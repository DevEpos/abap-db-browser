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
    METHODS: fill_primary_entity REDEFINITION.

  PRIVATE SECTION.

    DATA mv_query_id TYPE string.
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mt_table_list TYPE zdbbr_query_data-tables.
    DATA ms_query TYPE zdbbr_query_data.
    DATA: mo_formula TYPE REF TO zcl_dbbr_formula.

    "! <p class="shorttext synchronized" lang="en">Handle join entities</p>
    "!
    METHODS handle_join_tables.
    "! <p class="shorttext synchronized" lang="en">Load the query from the database</p>
    "!
    METHODS load_query.
    "! <p class="shorttext synchronized" lang="en">Fill necessary information for execution</p>
    "!
    METHODS fill_secondary_data.
ENDCLASS.



CLASS zcl_dbbr_query_var_starter IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_variant_id ).
    mv_query_id = iv_query_id.
    mr_query_f = NEW #( ).
  ENDMETHOD.


  METHOD handle_join_tables.

*.. add fields of join table to selfields output
    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table_info>).
      DATA(ls_join_entity) = VALUE zdbbr_entity_info(
        tabname            = <ls_join_table_info>-add_table
        tabname_alias      = <ls_join_table_info>-add_table_alias
        alias              = <ls_join_table_info>-add_table_alias_alv
        virtual_join_table = <ls_join_table_info>-is_virtual
        selection_order    = VALUE #( mt_table_list[ tabname_alias = <ls_join_table_info>-add_table_alias ]-selection_order OPTIONAL )
      ).
      IF <ls_join_table_info>-entity_type = zif_dbbr_c_entity_type=>cds_view.
        create_cds_fields( ls_join_entity ).
      ELSE.
        create_table_fields( ls_join_entity ).
      ENDIF.
    ENDLOOP.

    mo_tabfield_list->update_tables( ).
    mo_tabfield_list->update_alias_names(  ).
  ENDMETHOD.

  METHOD load_variant.
    IF mv_variant_id = zif_dbbr_global_consts=>c_dummy_variant.
*.... Check if there is a default variant for the query
      DATA(lv_default_variant_id) = zcl_dbbr_variant_factory=>find_default_query_variant( iv_query_id = ms_query-query_id ).
      IF lv_default_variant_id IS NOT INITIAL.
        mv_variant_id = lv_default_variant_id.
      ENDIF.
    ENDIF.

    super->load_variant( ).
  ENDMETHOD.


  METHOD fill_primary_entity.
    DATA(ls_entity) = VALUE zdbbr_entity_info(
        active_selection     = abap_true
        tabname              = ms_query-primary_table
        tabname_alias        = ms_query-primary_table_alias
        type                 = ms_query-entity_type
        selection_order      = VALUE #( mt_table_list[ tabname_alias = ms_query-primary_table_alias ]-selection_order OPTIONAL )
        fields_are_loaded    = abap_true
        is_primary           = abap_true
    ).

    IF ms_query-entity_type = zif_dbbr_c_entity_type=>cds_view.
      create_cds_fields( ls_entity ).
    ELSE.
      create_table_fields( ls_entity ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_secondary_data.

    IF ms_join_def IS NOT INITIAL.
      handle_join_tables( ).
    ENDIF.

    mo_tabfield_list->clear_active_flag(
        if_clear_selection = abap_true
        if_clear_sort      = ms_query-has_sort_fields
        if_clear_output    = ms_query-has_output_fields
    ).

    " update selection fields from query
    mo_tabfield_list->initialize_iterator( ).
    WHILE mo_tabfield_list->has_more_lines( ).
      DATA(lr_current_field) = mo_tabfield_list->get_next_entry( ).

      ASSIGN ms_query-fields[ tabname_alias = lr_current_field->tabname_alias
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

    IF ms_query-formula IS NOT INITIAL.
      TRY.
          mo_formula = NEW zcl_dbbr_fe_validator(
              iv_formula   = ms_query-formula
              ir_tabfields = mo_tabfield_list
          )->validate( ).

          zcl_dbbr_formula_helper=>update_tabflds_from_formula(
              ir_tabfields      = mo_tabfield_list
              ir_formula        = mo_formula
              it_form_selfields = ms_query-fields
          ).

        CATCH zcx_dbbr_formula_exception.
          mo_tabfield_list->clear_calculation_flag( ).
          mo_tabfield_list->delete_formula_fields( ).
          CLEAR ms_query-formula.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD load_query.

*.. load query from database
    ms_query = COND #( WHEN mv_variant_id = zif_dbbr_global_consts=>c_dummy_variant THEN
                                    mr_query_f->get_query( iv_query_name = CONV #( mv_query_id ) )
                                 ELSE
                                    mr_query_f->get_query_by_id( CONV #( mv_query_id ) ) ).

    ms_join_def = ms_query-join_def.
    mt_table_list = ms_query-tables.

  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~execute_variant.
    show_start_progress_text( ).

    fill_data_from_variant( ).

    get_tabfields(
      IMPORTING
        er_tabfields     = DATA(lr_tabfields)
        er_tabfields_all = DATA(lr_tabfields_all)
    ).

    " create and start selection controller
    DATA(lr_controller) = zcl_dbbr_selection_controller=>create_controller(
      VALUE #(
         entity_type        = zif_dbbr_c_selscreen_mode=>query
         entity_id          = ms_query-query_name
         selection_fields   = mt_selfields
         multi_or           = mt_selfields_or
         technical_infos    = CORRESPONDING #( ms_global_data )
         selfields_multi    = mt_selfields_multi
         tabfields          = lr_tabfields
         tabfields_all      = lr_tabfields_all
         join_def           = ms_join_def
         exclude_function   = VALUE #(
           ( zif_dbbr_c_selection_functions=>leave_screen_with_layout )
           ( zif_dbbr_c_selection_functions=>transfer_filter_values   )
         )
         formula            = COND #( WHEN mo_formula IS BOUND AND mo_formula->has_executable_code( ) THEN  mo_formula )
       )
    ).

    rf_no_data = lr_controller->execute_selection( ).
  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~initialize.
    load_query( ).
    fill_primary_entity( ).
    fill_secondary_data( ).
    load_variant( ).

    IF mv_variant_id <> zif_dbbr_global_consts=>c_dummy_variant OR
       ms_global_data-called_from_adt = abap_true.

      IF ms_global_data-called_from_adt = abap_true.
        zcl_dbbr_usersettings_factory=>update_start_settings(
          iv_entity_id   = ms_query-query_name
          iv_entity_type = zif_dbbr_c_entity_type=>query
        ).
      ENDIF.

      NEW zcl_dbbr_favmenu_factory( )->refresh_most_used(
          iv_entry     = ms_query-query_name
          iv_entry_raw = ms_query-query_name
          iv_type      = zif_dbbr_c_favmenu_type=>query
          iv_text      = ms_query-description
      ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
