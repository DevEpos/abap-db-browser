CLASS zcl_dbbr_jumplist_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_table
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF mc_table_fields,
        jump_field          TYPE dynfnam VALUE 'GS_JUMPFIELD-JUMP_FIELD',
        jump_target         TYPE dynfnam VALUE 'GS_JUMPFIELD-JUMP_TARGET',
        criterion           TYPE dynfnam VALUE 'GS_JUMPFIELD-CRITERION',
        criterion_operation TYPE dynfnam VALUE 'GS_JUMPFIELD-CRIT_OPERATION',
        criterion_value     TYPE dynfnam VALUE 'GS_JUMPFIELD-CRIT_VALUE',
      END OF mc_table_fields.
    METHODS constructor
      IMPORTING
        !ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS delete_selected_lines.
    METHODS set_query_infos
      IMPORTING
        ir_tabfield_list      TYPE REF TO zcl_dbbr_tabfield_list
        it_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab
        if_join_is_active     TYPE boolean.
    METHODS zif_uitb_table~add_line
         REDEFINITION .
    METHODS zif_uitb_table~update_screen_attributes
         REDEFINITION .
    METHODS zif_uitb_table~pbo
         REDEFINITION .
    METHODS zif_uitb_table~get_current_line_value
         REDEFINITION.
    METHODS zif_uitb_table~update_fields
         REDEFINITION.
    METHODS jump_field_f4.
    METHODS jump_crit_f4.
    METHODS jump_target_f4.
    METHODS set_jump_destinations
      IMPORTING
        it_jump_destinations TYPE zdbbr_jumpdest_data_ui_itab.
    METHODS clear_is_copied_flags.
    METHODS copy_jump_field.
    METHODS validate
         REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_ui_param_details_button TYPE REF TO zdbbr_button.
    DATA mr_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list.
    DATA mr_query_tabfields TYPE REF TO zcl_dbbr_tabfield_list.
    DATA mt_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab.
    DATA mf_join_is_active TYPE boolean.
ENDCLASS.



CLASS ZCL_DBBR_JUMPLIST_TABLE IMPLEMENTATION.


  METHOD clear_is_copied_flags.
    FIELD-SYMBOLS: <lt_jump_fields> TYPE zdbbr_jumpdest_data_ui_itab.
    ASSIGN mr_table_data->* TO <lt_jump_fields>.

    LOOP AT <lt_jump_fields> ASSIGNING FIELD-SYMBOL(<ls_jump_field>).
      CLEAR: <ls_jump_field>-is_copied.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mr_tabfield_list = ir_tabfield_list.

    " init some global data references from ui
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>jump_list_manager ).

    mr_tablecontrol = CAST #( lr_data_cache->get_data_ref( zif_dbbr_jumplist_var_ids=>c_jump_fields_tc ) ).
    mr_table_data = CAST #( lr_data_cache->get_data_ref( zif_dbbr_jumplist_var_ids=>c_t_jumpfields ) ).
    mr_current_line = CAST #( lr_data_cache->get_data_ref( zif_dbbr_jumplist_var_ids=>c_s_jumpfield ) ).
    mr_ui_param_details_button = CAST #( lr_data_cache->get_data_ref( zif_dbbr_jumplist_var_ids=>c_bt_param_details ) ).

    CLEAR: CAST zdbbr_jumpdest_data_ui_itab( mr_table_data )->*,
           cast zdbbr_jumpdest_data_ui( mr_current_line )->*.
  ENDMETHOD.


  METHOD copy_jump_field.
    " check that only one row is marked
    FIELD-SYMBOLS: <lt_jumpdest> TYPE zdbbr_jumpdest_data_ui_itab.

    ASSIGN mr_table_data->* TO <lt_jumpdest>.

    DATA(lv_selection_count) = zcl_dbbr_appl_util=>get_tab_line_count(
       it_criteria   = VALUE #( ( crit = 'MARKED' compare = 'EQ' value = abap_true ) )
       it_table      = <lt_jumpdest>
    ).

    IF lv_selection_count <> 1.
      MESSAGE s069(zdbbr_info) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " copy the jump destination
    APPEND INITIAL LINE TO <lt_jumpdest> ASSIGNING FIELD-SYMBOL(<ls_copied_line>).
    ASSIGN <lt_jumpdest>[ marked = abap_true ] TO FIELD-SYMBOL(<ls_source_line>).
    <ls_source_line>-marked = abap_false.
    <ls_copied_line> = <ls_source_line>.
    CLEAR: <ls_copied_line>-jumpdest_id. " otherwise existing entry will be overridden
    <ls_copied_line>-is_copied = abap_true.
    DATA(lr_cursor) = zcl_uitb_cursor=>get_cursor( if_reset = abap_false ).
    lr_cursor->set_line( lines( <lt_jumpdest> ) ).
    lr_cursor->set_field( zcl_dbbr_jumplist_table=>mc_table_fields-jump_field ).
  ENDMETHOD.


  METHOD delete_selected_lines.
*&---------------------------------------------------------------------*
*& Description: Deletes the selected lines of the table
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_jumpdest_data_ui_itab.

    ASSIGN mr_table_data->* TO <lt_table>.

    DELETE <lt_table> WHERE marked = abap_true.
  ENDMETHOD.


  METHOD jump_crit_f4.
    FIELD-SYMBOLS: <ls_current_line> TYPE zdbbr_jumpdest_data_ui.

    DATA(lr_join_field_f4) = NEW zcl_dbbr_tabfield_tree_f4(
      iv_screen_title     = 'Value help for query Field'
      io_tree_node_filler = NEW zcl_dbbr_tabf_treeno_fill(
          ir_tabfield_list      = mr_query_tabfields
      )
    ).

    lr_join_field_f4->display_value_help(
      IMPORTING ev_chosen_field            = DATA(lv_field)
                ev_chosen_table            = DATA(lv_tabname)
                ev_chosen_field_with_alias = DATA(lv_field_with_alias) ).

    IF lv_field IS NOT INITIAL.
      ASSIGN mr_current_line->* TO <ls_current_line>.
      <ls_current_line>-criterion = lv_field_with_alias.
    ENDIF.
  ENDMETHOD.


  METHOD jump_field_f4.
    FIELD-SYMBOLS: <ls_current_line> TYPE zdbbr_jumpdest_data_ui.

    DATA(lr_join_field_f4) = NEW zcl_dbbr_tabfield_tree_f4(
      iv_screen_title     = 'Value help for query Field'
      io_tree_node_filler = NEW zcl_dbbr_tabf_treeno_fill(
          ir_tabfield_list      = mr_query_tabfields
      )
    ).

    lr_join_field_f4->display_value_help(
      IMPORTING ev_chosen_field            = DATA(lv_field)
                ev_chosen_table            = DATA(lv_tabname)
                ev_chosen_field_with_alias = DATA(lv_field_with_alias) ).

    IF lv_field IS NOT INITIAL.
      ASSIGN mr_current_line->* TO <ls_current_line>.
      <ls_current_line>-jump_field = lv_field_with_alias.
      <ls_current_line>-jump_source_field = lv_field.
      <ls_current_line>-jump_source_table = lv_tabname.
    ENDIF.
  ENDMETHOD.


  METHOD jump_target_f4.
    DATA: lv_value TYPE se16n_value.

    FIELD-SYMBOLS: <ls_current_line> TYPE zdbbr_jumpdest_data_ui.

    ASSIGN mr_current_line->* TO <ls_current_line>.

    zcl_dbbr_f4_helper=>call_built_in_f4(
      EXPORTING
        iv_repid                = zif_dbbr_c_report_id=>jump_list_manager
        iv_selfield_name        = mc_table_fields-jump_target
        iv_tablename            = 'TSTC'
        iv_fieldname            = 'TCODE'
      CHANGING
        cv_value                = lv_value
    ).

    <ls_current_line>-jump_target = lv_value.
  ENDMETHOD.


  METHOD set_jump_destinations.
    FIELD-SYMBOLS: <ls_jump_field> TYPE zdbbr_jumpdest_data_ui.

    CAST zdbbr_jumpdest_data_ui_itab( mr_table_data )->* = it_jump_destinations.

    LOOP AT CAST zdbbr_jumpdest_data_ui_itab( mr_table_data )->* ASSIGNING <ls_jump_field>.
      IF mf_join_is_active = abap_true.
        <ls_jump_field>-jump_field = mr_query_tabfields->get_field(
          iv_tabname = <ls_jump_field>-jump_source_table
          iv_fieldname = <ls_jump_field>-jump_source_field
        )-sql_fieldname.
      ELSE.
        <ls_jump_field>-jump_field = <ls_jump_field>-jump_source_field.
      ENDIF.

      IF <ls_jump_field>-crit_field IS NOT INITIAL.
        IF mf_join_is_active = abap_true.
          <ls_jump_field>-criterion = mr_query_tabfields->get_field(
            iv_tabname = <ls_jump_field>-crit_table
            iv_fieldname = <ls_jump_field>-crit_field
          )-sql_fieldname.
        ELSE.
          <ls_jump_field>-criterion = <ls_jump_field>-crit_field.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD set_query_infos.
    mt_table_to_alias_map = it_table_to_alias_map.
    mr_query_tabfields = ir_tabfield_list.
    mf_join_is_active = if_join_is_active.
  ENDMETHOD.


  METHOD validate.
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_jumpdest_data_ui_itab.

    DATA(lr_cursor) = zcl_uitb_cursor=>get_cursor( if_reset = abap_false ).

    " only perform validation if current line is filled
    CHECK cv_function_code <> 'DELETE'.

    ASSIGN mr_table_data->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_jump_field>).
      DATA(lv_index) = sy-tabix.

      " set flag which indicates error in the line
      <ls_jump_field>-has_error = abap_true.

      " mandatory field check
      IF <ls_jump_field>-jump_field IS INITIAL OR
         <ls_jump_field>-jump_target IS INITIAL.
        lr_cursor->set_line( lv_index ).

        lr_cursor->set_field( COND #( WHEN <ls_jump_field>-jump_field IS INITIAL THEN mc_table_fields-jump_field
                                      WHEN <ls_jump_field>-jump_target IS INITIAL THEN mc_table_fields-jump_target ) ).
        " clear function upon error
        CLEAR cv_function_code.
        MESSAGE s032(zdbbr_info) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      " other validations
      " 1) check if field exists
      TRY.
          DATA(ls_jump_field) = mr_query_tabfields->get_field_by_sql_name( iv_sql_fieldname = <ls_jump_field>-jump_field ).
          <ls_jump_field>-jump_source_field = ls_jump_field-fieldname.
          <ls_jump_field>-jump_source_table = ls_jump_field-tabname_alias.
        CATCH cx_sy_itab_line_not_found.
          lr_cursor->set_line( lv_index ).
          lr_cursor->set_field( mc_table_fields-jump_field ).
          CLEAR cv_function_code.
          MESSAGE s014(zdbbr_exception) WITH <ls_jump_field>-jump_field DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.

      " 2) check if criterion field exists
      IF <ls_jump_field>-criterion IS NOT INITIAL.
        TRY.
            DATA(ls_crit_field) = mr_query_tabfields->get_field_by_sql_name( iv_sql_fieldname = <ls_jump_field>-criterion ).
            <ls_jump_field>-crit_field = ls_crit_field-fieldname.
            <ls_jump_field>-crit_table = ls_crit_field-tabname_alias.
          CATCH cx_sy_itab_line_not_found.
            lr_cursor->set_line( lv_index ).
            lr_cursor->set_field( mc_table_fields-jump_field ).
            CLEAR cv_function_code.
            MESSAGE s014(zdbbr_exception) WITH <ls_jump_field>-criterion DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.
      ELSE.
        CLEAR: <ls_jump_field>-crit_field,
               <ls_jump_field>-crit_table.
      ENDIF.

      " 3) check if transaction (jump target exists)
      IF NOT zcl_dbbr_dictionary_helper=>is_transaction_valid( <ls_jump_field>-jump_target ).
        lr_cursor->set_line( lv_index ).
        lr_cursor->set_field( mc_table_fields-jump_target ).
        CLEAR cv_function_code.
        MESSAGE s070(zdbbr_info) WITH <ls_jump_field>-jump_target DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      " 4) TODO: check if there are duplicate entries

      " at this point the line is valid so error flag has can be reset
      CLEAR <ls_jump_field>-has_error.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_uitb_table~add_line.
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_jumpdest_data_ui_itab.

    rv_new_index = super->zif_uitb_table~add_line( ).

    ASSIGN mr_table_data->* TO <lt_table>.

    " set cursor to first field of new line
    DATA(lr_cursor) = zcl_uitb_cursor=>get_cursor( if_reset = abap_false ).
    lr_cursor->set_field( mc_table_fields-jump_field ).
    lr_cursor->set_line( lines( <lt_table> ) ).
  ENDMETHOD.


  METHOD zif_uitb_table~get_current_line_value.
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_jumpdest_data_ui_itab.

    zif_uitb_table~determine_current_line( ).

    IF mv_current_line <> 0.
      ASSIGN mr_table_data->* TO <lt_table>.
      es_line = <lt_table>[ mv_current_line ].
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_table~pbo.
    super->zif_uitb_table~pbo( ).
  ENDMETHOD.


  METHOD zif_uitb_table~update_fields.
    super->zif_uitb_table~update_fields( iv_function_code ).
  ENDMETHOD.


  METHOD zif_uitb_table~update_screen_attributes.
    DATA: ls_screen TYPE screen.

    DATA: lv_param_details_icon  TYPE iconname,
          lv_param_details_infot TYPE string,
          lv_param_details_text  TYPE string VALUE 'Param.'.

    FIELD-SYMBOLS: <ls_jump_field> TYPE zdbbr_jumpdest_data_ui.

    super->zif_uitb_table~update_screen_attributes( ).

    " handle icon creation for adjusting/creating parameters of jump field
    ASSIGN mr_current_line->* TO <ls_jump_field>.

    IF <ls_jump_field>-jump_field IS NOT INITIAL.

      IF <ls_jump_field>-parameters IS INITIAL.
        lv_param_details_icon = 'ICON_CREATE'.
        lv_param_details_infot = 'Create Parameter'.
      ELSE.
        lv_param_details_icon = 'ICON_CHANGE'.
        lv_param_details_infot = 'Edit Parameter'.
      ENDIF.

      zcl_dbbr_icon_handler=>create_icon(
        EXPORTING
          iv_icon_name = lv_param_details_icon
          iv_text      = lv_param_details_text
          iv_info      = lv_param_details_infot
        IMPORTING
          ev_push      = mr_ui_param_details_button->*
      ).

      " only disable change of key fields if no error occurred in line
      IF <ls_jump_field>-has_error = abap_false AND <ls_jump_field>-is_copied = abap_false.
        LOOP AT SCREEN INTO ls_screen.
          IF ls_screen-name = mc_table_fields-jump_field OR
             ls_screen-name = mc_table_fields-criterion OR
             ls_screen-name = mc_table_fields-criterion_operation OR
             ls_screen-name = mc_table_fields-criterion_value.
            ls_screen-input = 0.
            MODIFY screen FROM ls_screen.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ELSE.
      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-name = 'PARAM_DETAILS'.
          ls_screen-active = 0.
          MODIFY screen FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
