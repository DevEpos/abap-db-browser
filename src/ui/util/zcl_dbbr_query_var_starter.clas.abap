CLASS zcl_dbbr_query_var_starter DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_variant_starter
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_variant_starter.
    METHODS constructor
      IMPORTING
        iv_query_id   TYPE string
        iv_variant_id TYPE zdbbr_variant_id.
  PROTECTED SECTION.
    METHODS load_variant REDEFINITION.

  PRIVATE SECTION.

    DATA mv_query_id TYPE string.
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory.
    DATA ms_query_info TYPE zdbbr_query_info.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mt_table_list TYPE zdbbr_query_data-tables.

    METHODS handle_join_tables.
    METHODS load_query.
ENDCLASS.



CLASS zcl_dbbr_query_var_starter IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_variant_id ).
    mv_query_id = iv_query_id.
    mr_query_f = NEW #( ).
  ENDMETHOD.


  METHOD handle_join_tables.

*.. add fields of join table to selfields output
    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table_info>) WHERE add_table <> ms_global_data-primary_table.
      create_table_fields( iv_tablename         = <ls_join_table_info>-add_table
                           if_conditional_table = <ls_join_table_info>-is_virtual ).

    ENDLOOP.
    mr_tabfield_list->update_tables( ).
    mr_tabfield_list->update_alias_names(  ). "mt_table_to_alias_map ).
  ENDMETHOD.

  METHOD load_variant.
    IF mv_variant_id = zif_dbbr_global_consts=>c_dummy_variant.
*.... Check if there is a default variant for the query
      DATA(lv_default_variant_id) = mr_variant_f->find_default_query_variant( iv_query_id = ms_query_info-query_id ).
      IF lv_default_variant_id IS NOT INITIAL.
        mv_variant_id = lv_default_variant_id.
      ENDIF.
    ENDIF.

    super->load_variant( ).
  ENDMETHOD.

  METHOD load_query.

    " 1) load query from database

    DATA(ls_query_data) = COND #( WHEN mv_variant_id = zif_dbbr_global_consts=>c_dummy_variant THEN
                                    mr_query_f->get_query( iv_query_name = CONV #( mv_query_id ) )
                                 ELSE
                                    mr_query_f->get_query_by_id( CONV #( mv_query_id ) ) ).

    ms_query_info = CORRESPONDING #( ls_query_data ).

    ms_join_def = ls_query_data-join_def.
    ms_global_data-primary_table = ms_query_info-primary_table.
    DATA(lt_query_selfields) = ls_query_data-fields.
    mt_table_list = ls_query_data-tables.

    fill_table( ).

    IF ms_join_def IS NOT INITIAL.
      handle_join_tables( ).
    ENDIF.

    mr_tabfield_list->set_table_list( CORRESPONDING #( mt_table_list ) ).
    mr_tabfield_list->clear_active_flag(
        if_clear_selection = abap_true
        if_clear_sort      = ms_query_info-has_sort_fields
        if_clear_output    = ms_query_info-has_output_fields
    ).

    " update selection fields from query
    mr_tabfield_list->initialize_iterator( ).
    WHILE mr_tabfield_list->has_more_lines( ).
      DATA(lr_current_field) = mr_tabfield_list->get_next_entry( ).

      ASSIGN lt_query_selfields[ tabname       = lr_current_field->tabname
                                  fieldname     = lr_current_field->fieldname
                                  is_text_field = lr_current_field->is_text_field ] TO FIELD-SYMBOL(<ls_query_selfield>).
      IF sy-subrc = 0.
        lr_current_field->selection_active = <ls_query_selfield>-selection_active.
        lr_current_field->selection_order = <ls_query_selfield>-selection_order.
        lr_current_field->output_active = <ls_query_selfield>-output_active.
        lr_current_field->output_order = <ls_query_selfield>-output_order.
        lr_current_field->sort_active = <ls_query_selfield>-sort_active.
        lr_current_field->sort_order = <ls_query_selfield>-sort_order.
        lr_current_field->sort_direction = <ls_query_selfield>-sort_direction.
      ENDIF.
    ENDWHILE.

    IF ms_query_info-formula IS NOT INITIAL.
      " validate formula
      TRY.
          DATA(lr_formula) = NEW zcl_dbbr_fe_validator(
              iv_formula   = ms_query_info-formula
              ir_tabfields = mr_tabfield_list
          )->validate( ).

          zcl_dbbr_formula_helper=>update_tabflds_from_formula(
              ir_tabfields      = mr_tabfield_list
              ir_formula        = lr_formula
              it_form_selfields = lt_query_selfields
          ).

        CATCH zcx_dbbr_formula_exception.
          mr_tabfield_list->clear_calculation_flag( ).
          mr_tabfield_list->delete_formula_fields( ).
          CLEAR ms_query_info-formula.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~execute_variant.
    show_start_progress_text( ).

    fill_data_from_variant( ).

    IF ms_query_info-formula IS NOT INITIAL.
      TRY.

          DATA(lr_formula) = NEW zcl_dbbr_fe_validator(
            iv_formula   =  ms_query_info-formula
            ir_tabfields = mr_tabfield_list
          )->validate( ).
        CATCH zcx_dbbr_formula_exception INTO DATA(lr_exc).
          lr_exc->zif_dbbr_exception_message~print( ).
          RETURN.
      ENDTRY.
    ENDIF.

    get_tabfields(
      IMPORTING
        er_tabfields     = DATA(lr_tabfields)
        er_tabfields_all = DATA(lr_tabfields_all)
    ).

    " create and start selection controller
    DATA(lr_controller) = zcl_dbbr_selection_controller=>create_controller(
       iv_entity_type        = zif_dbbr_c_selscreen_mode=>query
       iv_entity_id          = ms_query_info-query_name
       it_selection_fields   = mt_selfields
       it_multi_or           = mt_selfields_or
       is_technical_infos    = CORRESPONDING #( ms_global_data )
       it_selfields_multi    = mt_selfields_multi
       ir_tabfields          = lr_tabfields
       ir_tabfields_all      = lr_tabfields_all
       it_table_to_alias_map = mt_table_to_alias_map
       is_join_def           = ms_join_def
       it_exclude_function   = VALUE #(
         ( zif_dbbr_c_selection_functions=>leave_screen_with_layout )
         ( zif_dbbr_c_selection_functions=>transfer_filter_values   )
       )
       ir_formula            = COND #( WHEN lr_formula IS BOUND AND lr_formula->has_executable_code( ) THEN  lr_formula )
    ).

    lr_controller->execute_selection( ).
  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~initialize.
    load_query( ).
    load_variant( ).

    CHECK mv_variant_id <> zif_dbbr_global_consts=>c_dummy_variant.

    NEW zcl_dbbr_favmenu_factory( )->refresh_most_used(
        iv_entry     = ms_query_info-query_name
        iv_entry_raw = ms_query_info-query_name
        iv_type      = zif_dbbr_c_favmenu_type=>query
        iv_text      = ms_query_info-description
    ).
  ENDMETHOD.


ENDCLASS.
