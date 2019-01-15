class ZCL_DBBR_JUMPLIST_CONTROLLER definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_SCREEN_CONTROLLER .

  methods CONSTRUCTOR
    importing
      !IV_query_ID type ZDBBR_query_ID
      !IR_TABLE type ref to ZCL_DBBR_JUMPLIST_TABLE .
  methods JUMP_FIELD_F4 .
  methods HAS_JUMP_DESTINATIONS
    returning
      value(RESULT) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

  constants:
    BEGIN OF mc_function_codes,
        save_jump_fields      TYPE sy-ucomm VALUE 'SAVE',
        select_all_fields     TYPE sy-ucomm VALUE 'SELECT_ALL',
        unselect_all_fields   TYPE sy-ucomm VALUE 'UNSLCT_ALL',
        create_new_jump_field TYPE sy-ucomm VALUE 'CREATE',
        delete_jump_fields    TYPE sy-ucomm VALUE 'DELETE',
        maDBBRin_parameters   TYPE sy-ucomm VALUE 'PARAM_DETAILS',
        copy_jump_destination TYPE sy-ucomm VALUE 'COPY',
      END OF mc_function_codes .
  data MR_TABLE type ref to ZCL_DBBR_JUMPLIST_TABLE .
  data MV_query_ID type ZDBBR_query_ID .
  data MR_query_TABFIELDS type ref to ZCL_DBBR_TABFIELD_LIST .
  data MS_query_INFO type ZDBBR_query_INFO .
  data MT_TABLE_TO_ALIAS_MAP type ZDBBR_TABLE_TO_ALIAS_MAP_ITAB .
  data MF_JOIN_IS_ACTIVE type XSDBOOLEAN .

  methods MAINTAIN_PARAMETERS .
  methods INITIALIZE .
  methods LOAD_query .
  methods SAVE_JUMP_FIELDS .
  methods ADD_NEW_JUMP_FIELD .
  methods COPY_JUMP_FIELD .
ENDCLASS.



CLASS ZCL_DBBR_JUMPLIST_CONTROLLER IMPLEMENTATION.


  METHOD add_new_jump_field.
    FIELD-SYMBOLS: <lt_table> TYPE ZDBBR_jumpdest_data_ui_itab.

    DATA(lv_new_line_index) = mr_table->ZIF_UITB_TABLE~add_line( ).

    IF lv_new_line_index <= 0.
      RETURN.
    ENDIF.

    DATA(lr_table_data_itab) = mr_table->ZIF_UITB_TABLE~get_table_data( ).

    ASSIGN lr_table_data_itab->* TO <lt_table>.

    " fill query id of new jump field
    DATA(lr_new_line) = REF #( <lt_table>[ lv_new_line_index ] ).
    lr_new_line->ref_query_id = mv_query_id.
    lr_new_line->is_active = abap_true.
    lr_new_line->jump_target_type = ZIF_DBBR_global_consts=>gc_jump_call_types-normal.
    lr_new_line->is_hotspot = abap_true.
    lr_new_line->skip_1st_screen = abap_true.
  ENDMETHOD.


  METHOD constructor.
    mr_table = ir_table.
    mv_query_id = iv_query_id.
  ENDMETHOD.


  METHOD copy_jump_field.
    mr_table->copy_jump_field( ).
  ENDMETHOD.


  method HAS_JUMP_DESTINATIONS.
    FIELD-SYMBOLS: <lt_data> type STANDARD TABLE.
    data(lr_t_data) = mr_table->zif_uitb_table~get_table_data( ).
    ASSIGN lr_t_data->* to <lt_data>.

    result = xsdbool( <lt_data> is ASSIGNED and <lt_data> is not INITIAL ).
  endmethod.


  METHOD initialize.
    " read the query from the database
    load_query( ).

    DATA(lr_jumpdest_f) = NEW ZCL_DBBR_jump_destination_f( ).
    " load existing jump fields
    mr_table->set_jump_destinations( lr_jumpdest_f->get_jump_destinations( mv_query_id ) ).
  ENDMETHOD.


  METHOD jump_field_f4.
    DATA(lr_join_field_f4) = NEW zcl_dbbr_tabfield_tree_f4(
      iv_screen_title     = 'Value help for query Field'
      ir_tree_node_filler = NEW zcl_dbbr_tabf_treeno_fill(
          ir_tabfield_list      = mr_query_tabfields
      )
    ).

    lr_join_field_f4->display_value_help(
      IMPORTING ev_chosen_field            = DATA(lv_field)
                ev_chosen_table            = DATA(lv_tabname)
                ev_chosen_field_with_alias = DATA(lv_field_with_alias) ).

  ENDMETHOD.


  METHOD load_query.
    mr_query_tabfields = NEW #( ).

    " 1) load query from database
    DATA(ls_query_data) = NEW zcl_dbbr_query_factory( )->get_query_by_id( mv_query_id ).

    ms_query_info = CORRESPONDING #( ls_query_data ).


    DATA(ls_join_def) = ls_query_data-join_def.
    mf_join_is_active = xsdbool( ls_join_def-tables IS NOT INITIAL ).
    DATA(lt_query_selfields) = ls_query_data-fields.

    DATA(lt_table_list) = CORRESPONDING zdbbr_entity_info_t( ls_query_data-tables ).

    LOOP AT lt_table_list ASSIGNING FIELD-SYMBOL(<ls_table>).
      DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( iv_tablename = <ls_table>-tabname ).
      <ls_table>-is_primary = xsdbool( <ls_table>-tabname = ls_query_data-primary_table ).
      <ls_table>-index = sy-tabix.
      <ls_table>-description = ls_table_info-ddtext.

*... create tabfield list entries for output fields
      LOOP AT lt_query_selfields ASSIGNING FIELD-SYMBOL(<ls_query_field>) WHERE tabname = <ls_table>-tabname
                                                                              AND output_active = abap_true
                                                                              AND is_formula_field = abap_false.
        DATA(ls_fieldinfo) = zcl_dbbr_dictionary_helper=>get_table_field_info(
          iv_tablename = <ls_query_field>-tabname
          iv_fieldname = <ls_query_field>-fieldname
        ).

        CHECK ls_fieldinfo IS NOT INITIAL.

        DATA(ls_tabfield) = CORRESPONDING zdbbr_tabfield_info_ui(
          ls_fieldinfo
          MAPPING is_key          = keyflag
                  ddic_order      = position
                  std_short_text  = scrtext_s
                  std_medium_text = scrtext_m
                  std_long_text   = scrtext_l
                  header_text     = reptext
        ).
        ls_tabfield-tabname_alias   = ls_tabfield-tabname.
        ls_tabfield-is_foreign_key  = xsdbool( ls_fieldinfo-checktable IS NOT INITIAL ).
        ls_tabfield-field_ddtext    = COND #( WHEN ls_fieldinfo-scrtext_l IS INITIAL THEN
                                                ls_fieldinfo-fieldtext
                                              ELSE
                                                ls_fieldinfo-scrtext_l ).
        ls_tabfield-is_numeric      = zcl_dbbr_dictionary_helper=>is_type_numeric( ls_fieldinfo-inttype )..

        mr_query_tabfields->add( REF #( ls_tabfield ) ).
      ENDLOOP.

*... add the table
      mr_query_tabfields->add_table( <ls_table> ).
    ENDLOOP.


    mr_query_tabfields->update_tables( ).
    mr_query_tabfields->update_alias_names( ).
    mr_query_tabfields->build_complete_fieldnames( ).

    mr_query_tabfields->sort_in_ddic_order( ).

    mr_table->set_query_infos(
        ir_tabfield_list      = mr_query_tabfields
        if_join_is_active     = mf_join_is_active
        it_table_to_alias_map = mt_table_to_alias_map
    ).

  ENDMETHOD.


  METHOD maintain_parameters.
    FIELD-SYMBOLS: <ls_current_line> TYPE zdbbr_jumpdest_data_ui.

    DATA(lr_current_line) = mr_table->zif_uitb_table~get_current_line_ref( ).
    IF lr_current_line IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN lr_current_line->* TO <ls_current_line>.

    DATA(lr_jumplist_param_table) = NEW zcl_dbbr_jumplist_param_table(
      ir_query_fields      = mr_query_tabfields
      it_table_to_alias_map = mt_table_to_alias_map
      it_jump_params        = CORRESPONDING #( <ls_current_line>-parameters )
    ).

    DATA(lr_jumplist_param_controller) = NEW zcl_dbbr_jumplist_param_ctlr(
      ir_table       = lr_jumplist_param_table
      iv_transaction = <ls_current_line>-jump_target
    ).

    lr_jumplist_param_controller->zif_uitb_screen_controller~call_screen( ).


    IF lr_jumplist_param_controller->zif_uitb_screen_controller~was_not_cancelled( ).
      " retrieve current list of param values
      <ls_current_line>-parameters = CORRESPONDING #( lr_jumplist_param_controller->get_parameters( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD save_jump_fields.
    FIELD-SYMBOLS: <lt_jump_definitions> TYPE ZDBBR_jumpdest_data_ui_itab.

    DATA(lr_jump_destinations) = mr_table->ZIF_UITB_TABLE~get_table_data( ).
    ASSIGN lr_jump_destinations->* TO <lt_jump_definitions>.

    NEW ZCL_DBBR_jump_destination_f( )->save_jump_destinations(
      EXPORTING
        iv_query_id          = mv_query_id
        if_update_query_flag = abap_true
      CHANGING
        ct_jump_destinations = <lt_jump_definitions>
    ).

    ZCL_DBBR_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    initialize( ).

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_jumplist_var_ids=>c_r_jumplist_controller
            global_ref    = me )
          ( variable_name = zif_dbbr_jumplist_var_ids=>c_r_jumplist_table
            global_ref    = mr_table )
        )
    ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~cancel.
    ZCL_DBBR_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~determine_cursor.
    zcl_uitb_cursor=>get_cursor( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>jump_list_manager.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_jump_list_manager.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~handle_user_command.
    DATA(lv_function) = cv_function_code.
    CLEAR cv_function_code.

    " clear copy attributes from table lines
    mr_table->clear_Is_copied_flags( ).

    CASE lv_function.
      WHEN ZIF_DBBR_global_consts=>gc_function_codes-leave_screen.
        ZCL_DBBR_screen_helper=>leave_screen( ).

      WHEN mc_function_codes-save_jump_fields.
        save_jump_fields( ).
      WHEN mc_function_codes-select_all_fields.

      WHEN mc_function_codes-unselect_all_fields.

      WHEN mc_function_codes-create_new_jump_field.
        add_new_jump_field( ).

      WHEN mc_function_codes-maDBBRin_parameters.
        maintain_parameters( ).

      WHEN mc_function_codes-delete_jump_fields.
        mr_table->delete_selected_lines( ).

      WHEN mc_function_codes-copy_jump_destination.
        copy_jump_field( ).

    ENDCASE.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~pbo.
    ZIF_UITB_SCREEN_CONTROLLER~set_status( ).

    " update cursor
    zcl_uitb_cursor=>refresh_cursor( ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~set_status.
    SET PF-STATUS 'JUMP_LIST_STATUS' OF PROGRAM zif_dbbr_c_report_id=>jump_list_manager.
    SET TITLEBAR 'JUMP_LIST' OF PROGRAM zif_dbbr_c_report_id=>jump_list_manager WITH ms_query_info-query_name.
  ENDMETHOD.
ENDCLASS.
