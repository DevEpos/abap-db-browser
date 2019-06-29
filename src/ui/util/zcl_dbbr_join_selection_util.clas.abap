CLASS zcl_dbbr_join_selection_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selection_util
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS execute_selection
        REDEFINITION.
    METHODS get_entity_name
        REDEFINITION.
    METHODS handle_alv_ctx_menu_request
        REDEFINITION.
    METHODS init
        REDEFINITION.
    METHODS refresh_selection
        REDEFINITION.
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION.
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION .
  PROTECTED SECTION.

    METHODS create_from_clause
        REDEFINITION.

    "! <p class="shorttext synchronized" lang="en">Marks virtual join fields for later processing</p>
    "!
    METHODS mark_virt_join_selfields .
    "! <p class="shorttext synchronized" lang="en">Fill virtual join field in the output table</p>
    "!
    METHODS fill_virtual_join_fields
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Change CDS parameter values</p>
    "!
    METHODS change_parameters .
    "! <p class="shorttext synchronized" lang="en">Update parameters of entities with the given parameter</p>
    "!
    METHODS update_table_parameters
      IMPORTING
        is_param TYPE zdbbr_table_parameter.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_join_selection_util IMPLEMENTATION.

  METHOD execute_selection.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    determine_group_by_state( ).
    determine_aggregation_state( ).
    mark_virt_join_selfields( ).

    """ only count lines for current selection and display result
    IF mf_count_lines = abap_true.
      count_lines( ).
      RETURN.
    ENDIF.

    read_entity_infos( ).

    build_full_fieldnames( ).

    IF mf_join_is_active = abap_true AND
       line_exists( ms_join_def-tables[ is_virtual = abap_true ] ).
      mo_post_join_helper = zcl_dbbr_virtual_join_helper=>create(
        is_join_def   = ms_join_def
        ir_fields     = mo_tabfields
        ir_fields_all = mo_tabfields_all
      ).
    ENDIF.

    mo_tabfields->update_text_field_status( ).

    zcl_dbbr_addtext_helper=>prepare_text_fields(
      EXPORTING ir_fields    = mo_tabfields
      CHANGING  ct_add_texts = mt_add_texts
    ).

    create_from_clause( ).

    create_where_clause( ).

    create_group_by_clause( ).

    create_order_by_clause( ).

    create_field_catalog( ).

    create_select_clause( ).

    create_dynamic_table( ).

    IF mo_formula IS BOUND.
      TRY.
          mo_formula_calculator = zcl_dbbr_formula_calculator=>create(
              ir_formula            = mo_formula
              ir_tabfields          = mo_tabfields
              it_tab_components     = mt_dyntab_components
          ).
        CATCH zcx_dbbr_exception INTO DATA(lr_exception).
          lr_exception->zif_dbbr_exception_message~print( ).
      ENDTRY.
    ENDIF.

    CHECK select_data( ).

    " if no selection occurred, prevent screen visibility
    IF ms_control_info-number <= 0.
      raise_no_data_event( ).
      RETURN.
    ENDIF.

    CHECK fill_virtual_join_fields( ).

    execute_formula_for_lines( ).

    set_miscinfo_for_selected_data( ).

    RAISE EVENT selection_finished
      EXPORTING
         ef_first_select = abap_true.
  ENDMETHOD.

  METHOD fill_virtual_join_fields.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    result = abap_true.

*.. only continue if virtual join helper is bound
    CHECK mo_post_join_helper IS BOUND.

    ASSIGN mr_t_data->* TO <lt_table>.

    TRY.
        mo_post_join_helper->fill_cache_tables( <lt_table> ).
        result = mo_post_join_helper->process_table( mr_t_data ).
      CATCH zcx_dbbr_selection_common INTO DATA(lx_sql_error).
        CLEAR result.
        MESSAGE lx_sql_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD mark_virt_join_selfields.
    CHECK line_exists( ms_join_def-tables[ is_virtual = abap_true ] ).

    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE is_virtual = abap_true.
      LOOP AT <ls_join_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_join_field>).

        " update field
        DATA(lr_s_field)    = mo_tabfields->get_field_ref(
           iv_tabname_alias = <ls_join_field>-ref_table
           iv_fieldname     = <ls_join_field>-ref_field
        ).

        lr_s_field->needed_for_virtual_join = abap_true.
      ENDLOOP.
    ENDLOOP.

    mt_virtual_join_table_range = VALUE #(
      LET sign = 'I' option = 'EQ' IN
      FOR <ls_join_tab> IN ms_join_def-tables
      WHERE ( is_virtual = abap_true )
      ( sign   = sign
        option = option
        low    = <ls_join_tab>-add_table )
    ).

    LOOP AT mt_selection_fields ASSIGNING FIELD-SYMBOL(<ls_selfield>) WHERE tabname IN mt_virtual_join_table_range.
      <ls_selfield>-virtual_join_field = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_entity_name.
    result = |{ 'Join Selection' }|.
  ENDMETHOD.

  METHOD handle_alv_ctx_menu_request.
  ENDMETHOD.

  METHOD init.
    ms_control_info-primary_table = mv_entity_id.

    IF mt_param_values IS NOT INITIAL.
      zcl_dbbr_join_helper=>prefill_parameters(
        EXPORTING io_tabfields = mo_tabfields
        CHANGING  cs_join      = ms_join_def
      ).
    ENDIF.
  ENDMETHOD.

  METHOD create_from_clause.
    DATA: lv_handled_params TYPE i.

    IF mt_param_values IS NOT INITIAL.
      LOOP AT mt_param_values ASSIGNING FIELD-SYMBOL(<ls_param_value>).
        update_table_parameters( is_param = CORRESPONDING #( <ls_param_value> MAPPING param_name = fieldname param_value = low ) ).
      ENDLOOP.
    ENDIF.

    IF ms_join_def-tables IS INITIAL.
      IF mv_entity_type = zif_dbbr_c_entity_type=>cds_view AND
         ( sy-saprl < 750 OR ms_technical_info-use_ddl_view_for_select = abap_true ).
        DATA(lv_ddl_view) = zcl_dbbr_cds_view_factory=>get_ddl_for_entity_name( mv_entity_id ).
        DATA(lv_ddl_ddic_view) = zcl_dbbr_cds_view_factory=>read_ddl_ddic_view( lv_ddl_view ).
        mt_from = VALUE #( ( |{ lv_ddl_ddic_view }| ) ).
      ELSE.
        mt_from = VALUE #( ( |{ ms_control_info-primary_table }| ) ).
      ENDIF.
    ELSE.
      mt_from = zcl_dbbr_join_helper=>build_from_clause_for_join_def(
        if_use_ddl_for_select = ms_technical_info-use_ddl_view_for_select
        is_join_def           = ms_join_def
      ).
    ENDIF.
  ENDMETHOD.


  METHOD refresh_selection.
    CHECK select_data( if_refresh_only = abap_true ).

    IF ms_control_info-number = 0.
      raise_no_data_event( ).
      RETURN.
    ENDIF.

    CHECK fill_virtual_join_fields( ).

    execute_formula_for_lines( ).

    set_miscinfo_for_selected_data( ).

    RAISE EVENT selection_finished.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = VALUE #(
      ( LINES OF super->get_deactivated_functions( ) )
      ( zif_dbbr_c_selection_functions=>navigate_association )
    ).


    IF mt_param_values IS NOT INITIAL.
      DELETE result WHERE table_line = zif_dbbr_c_selection_functions=>change_cds_parameters.
    ENDIF.

    IF mf_aggregation = abap_true OR
       mf_group_by    = abap_true.

      result = VALUE #(
       BASE result
       ( zif_dbbr_c_selection_functions=>toggle_entity_info_header )
       ( zif_dbbr_c_selection_functions=>group_by_selected_columns )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_ui_function.
    super->handle_ui_function( CHANGING cv_function = cv_function ).

    CASE cv_function.

      WHEN zif_dbbr_c_selection_functions=>change_cds_parameters.
        change_parameters( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD change_parameters.
    DATA: lf_trigger_select TYPE abap_bool.

    DATA(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
        io_tabfields     = mo_tabfields_all
        iv_cds_view_name = mv_entity_id
        it_param_values  = VALUE #(
          FOR param IN mt_param_values WHERE ( is_parameter = abap_true ) ( name = param-fieldname value = param-low )
        )
    ).

    lo_param_popup->show( ).
    DATA(lt_new_param_values) = lo_param_popup->get_param_values( ).

    IF lt_new_param_values IS NOT INITIAL.
      LOOP AT lt_new_param_values ASSIGNING FIELD-SYMBOL(<ls_new_param>).
        ASSIGN mt_param_values[ fieldname = <ls_new_param>-name is_parameter = abap_true ] TO FIELD-SYMBOL(<ls_old_param>).
        IF sy-subrc = 0 AND
           <ls_old_param>-low <> <ls_new_param>-value.
          lf_trigger_select = abap_true.
          <ls_old_param>-low = <ls_new_param>-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lf_trigger_select = abap_true.
      CLEAR mt_from.
      create_from_clause( ).
      mr_select_program->update_from( mt_from ).
      refresh_selection( ).
    ENDIF.
  ENDMETHOD.

  METHOD update_table_parameters.
    FIELD-SYMBOLS: <ls_param> TYPE zdbbr_table_parameter.

    LOOP AT ms_join_def-parameters ASSIGNING <ls_param> WHERE param_name = is_param-param_name.
      <ls_param>-param_value = is_param-param_value.
    ENDLOOP.

    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      LOOP AT <ls_table>-parameters ASSIGNING <ls_param> WHERE param_name = is_param-param_name.
        <ls_param>-param_value = is_param-param_value.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
