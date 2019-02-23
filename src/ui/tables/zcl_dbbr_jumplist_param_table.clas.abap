CLASS zcl_dbbr_jumplist_param_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_table
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !it_jump_params        TYPE zdbbr_jumpparam_data_ui_itab
        !ir_query_fields       TYPE REF TO zcl_dbbr_tabfield_list
        !it_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab .
    METHODS delete_selected_params .
    METHODS param_id_f4 .
    METHODS param_value_f4 .

    METHODS validate
        REDEFINITION .
    METHODS zif_uitb_table~add_line
        REDEFINITION .
    METHODS zif_uitb_table~update_screen_attributes
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_table_fields,
        param_id    TYPE dynfnam VALUE 'GS_PARAM-PARAMETER_ID',
        param_value TYPE dynfnam VALUE 'GS_PARAM-PARAM_VALUE',
      END OF mc_table_fields.

    DATA mr_query_fields TYPE REF TO zcl_dbbr_tabfield_list.
    DATA mt_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab.
ENDCLASS.



CLASS zcl_dbbr_jumplist_param_table IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <ls_jump_param> TYPE zdbbr_jumpparam_data_ui.

    super->constructor( ).

    mr_query_fields = ir_query_fields.
    mt_table_to_alias_map = it_table_to_alias_map.

    " init some global data references from ui
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>jump_list_manager ).

    mr_table_data = lr_data_cache->get_data_ref( zif_dbbr_jumplist_var_ids=>c_t_params ).
    mr_current_line = lr_data_cache->get_data_ref( zif_dbbr_jumplist_var_ids=>c_s_param ).
    mr_tablecontrol = CAST #( lr_data_cache->get_data_ref( zif_dbbr_jumplist_var_ids=>c_params_tc ) ).

*... fill parameter table
    CAST zdbbr_jumpparam_data_ui_itab( mr_table_data )->* = it_jump_params.
    CLEAR CAST zdbbr_jumpparam_data_ui( mr_current_line )->*.

*... build alias fieldnames if needed
    LOOP AT CAST zdbbr_jumpparam_data_ui_itab( mr_table_data )->* ASSIGNING <ls_jump_param>.
      IF <ls_jump_param>-param_field <> space.
        DATA(lr_param_field) = mr_query_fields->get_field_ref(
            iv_tabname_alias = <ls_jump_param>-param_table
            iv_fieldname     = <ls_jump_param>-param_field
        ).
        <ls_jump_param>-value = lr_param_field->sql_fieldname.
      ELSE.
        <ls_jump_param>-value = <ls_jump_param>-param_value.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD delete_selected_params.
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_jumpparam_data_ui_itab.

    ASSIGN mr_table_data->* TO <lt_table>.

    DELETE <lt_table> WHERE marked = abap_true.
  ENDMETHOD.


  METHOD param_id_f4.
    DATA: lv_value TYPE se16n_value.
    FIELD-SYMBOLS: <ls_current_line> TYPE zdbbr_jumpparam_data_ui.

    ASSIGN mr_current_line->* TO <ls_current_line>.

    zcl_dbbr_f4_helper=>call_built_in_f4(
      EXPORTING
        iv_repid                = zif_dbbr_c_report_id=>jump_list_manager
        iv_selfield_name        = mc_table_fields-param_id
        iv_tablename            = 'TPARA'
        iv_fieldname            = 'PARAMID'
      CHANGING
        cv_value                = lv_value
    ).

    IF lv_value IS NOT INITIAL.
      <ls_current_line>-parameter_id = lv_value.
    ENDIF.

  ENDMETHOD.


  METHOD param_value_f4.
    FIELD-SYMBOLS: <ls_current_line> TYPE zdbbr_jumpparam_data_ui.

    DATA(lr_join_field_f4) = NEW zcl_dbbr_tabfield_tree_f4(
      iv_screen_title     = 'Value help for query Field'
      io_tree_node_filler = NEW zcl_dbbr_tabf_treeno_fill(
          ir_tabfield_list      = mr_query_fields
      )
    ).

    lr_join_field_f4->display_value_help(
      IMPORTING ev_chosen_field            = DATA(lv_field)
                ev_chosen_table_alias      = data(lv_tabname_alias)
                ev_chosen_field_with_alias = DATA(lv_field_with_alias) ).

    IF lv_field IS NOT INITIAL.
      ASSIGN mr_current_line->* TO <ls_current_line>.
      <ls_current_line>-value = lv_field_with_alias.
      <ls_current_line>-param_field = lv_field.
      <ls_current_line>-param_table = lv_tabname_alias.
    ENDIF.
  ENDMETHOD.


  METHOD validate.

    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_jumpparam_data_ui_itab.

    " disable validation for delete line action
    CHECK cv_function_code <> 'DELETE'.

    ASSIGN mr_table_data->* TO <lt_table>.

    DATA(lr_cursor) = zcl_uitb_cursor=>get_cursor( if_reset = abap_false ).

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_current_line>).
      DATA(lv_index) = sy-tabix.
      <ls_current_line>-has_error = abap_true.

      IF <ls_current_line>-parameter_id IS INITIAL.
        lr_cursor->set_line( lv_index ).
        lr_cursor->set_field( mc_table_fields-param_id ).
        " clear function upon error
        CLEAR cv_function_code.
        MESSAGE s066(zdbbr_info) DISPLAY LIKE 'E'.
        RETURN.
      ELSE. " check the validity of the parameter name

      ENDIF.

      " no fieldname/value for parameter supplied
      IF <ls_current_line>-value IS INITIAL.
        lr_cursor->set_line( lv_index ).
        lr_cursor->set_field( mc_table_fields-param_value ).
        " clear function upon error
        CLEAR cv_function_code.
        MESSAGE s067(zdbbr_info) DISPLAY LIKE 'E'.
        RETURN.
      ELSE. " check the validity of the value
        IF <ls_current_line>-value CP |'*'|.
          " clear tab/field fields for param value if a string literal was entered
          	 	  CLEAR : <ls_current_line>-param_field,
                          <ls_current_line>-param_table.
          <ls_current_line>-param_value = <ls_current_line>-value.
        ELSE.
          CLEAR: <ls_current_line>-param_value.
          TRY.
              DATA(ls_param_field) = mr_query_fields->get_field_by_sql_name( CONV #( <ls_current_line>-value ) ).
              <ls_current_line>-param_field = ls_param_field-fieldname.
              <ls_current_line>-param_table = ls_param_field-tabname_alias.
            CATCH cx_sy_itab_line_not_found.
              CLEAR cv_function_code.
              MESSAGE s014(zdbbr_exception) WITH <ls_current_line>-param_value DISPLAY LIKE 'E'.
              RETURN.
          ENDTRY.
        ENDIF.
      ENDIF.

      CLEAR <ls_current_line>-has_error.
    ENDLOOP.

    " confirm that there is at least one active parameter
    IF cv_function_code = 'ENTER' AND
       <lt_table> IS NOT INITIAL AND
       NOT line_exists( <lt_table>[ active = abap_true ] ).
      CLEAR cv_function_code.
      MESSAGE s068(zdbbr_info) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_table~add_line.
    DATA(lv_new_line) = super->zif_uitb_table~add_line( ).

    IF lv_new_line > 0.
      DATA(lr_cursor) = zcl_uitb_cursor=>get_cursor( if_reset = abap_false ).
      lr_cursor->set_line( lv_new_line ).
      lr_cursor->set_field( mc_table_fields-param_id ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_table~update_screen_attributes.
    DATA: ls_screen TYPE screen.
    FIELD-SYMBOLS: <ls_current_line> TYPE zdbbr_jumpparam_data_ui.

    super->zif_uitb_table~update_screen_attributes( ).

    ASSIGN mr_current_line->* TO <ls_current_line>.

    IF <ls_current_line>-parameter_id IS NOT INITIAL AND <ls_current_line>-has_error = abap_false.
      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-name = mc_table_fields-param_id.
          ls_screen-input = 0.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
