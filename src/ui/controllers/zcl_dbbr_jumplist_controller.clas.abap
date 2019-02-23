CLASS zcl_dbbr_jumplist_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    METHODS constructor
      IMPORTING
        !iv_query_id TYPE zdbbr_query_id
        !ir_table    TYPE REF TO zcl_dbbr_jumplist_table .
    METHODS jump_field_f4 .
    METHODS has_jump_destinations
      RETURNING
        VALUE(result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    CONSTANTS:
      BEGIN OF mc_function_codes,
        save_jump_fields      TYPE sy-ucomm VALUE 'SAVE',
        select_all_fields     TYPE sy-ucomm VALUE 'SELECT_ALL',
        unselect_all_fields   TYPE sy-ucomm VALUE 'UNSLCT_ALL',
        create_new_jump_field TYPE sy-ucomm VALUE 'CREATE',
        delete_jump_fields    TYPE sy-ucomm VALUE 'DELETE',
        madbbrin_parameters   TYPE sy-ucomm VALUE 'PARAM_DETAILS',
        copy_jump_destination TYPE sy-ucomm VALUE 'COPY',
      END OF mc_function_codes .
    DATA mr_table TYPE REF TO zcl_dbbr_jumplist_table .
    DATA mv_query_id TYPE zdbbr_query_id .
    DATA mr_query_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA ms_query_info TYPE zdbbr_query_info .
    DATA mt_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab .
    DATA mf_join_is_active TYPE xsdboolean .

    METHODS maintain_parameters .
    METHODS initialize .
    METHODS load_query .
    METHODS save_jump_fields .
    METHODS add_new_jump_field .
    METHODS copy_jump_field .
ENDCLASS.



CLASS zcl_dbbr_jumplist_controller IMPLEMENTATION.


  METHOD add_new_jump_field.
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_jumpdest_data_ui_itab.

    DATA(lv_new_line_index) = mr_table->zif_uitb_table~add_line( ).

    IF lv_new_line_index <= 0.
      RETURN.
    ENDIF.

    DATA(lr_table_data_itab) = mr_table->zif_uitb_table~get_table_data( ).

    ASSIGN lr_table_data_itab->* TO <lt_table>.

    " fill query id of new jump field
    DATA(lr_new_line) = REF #( <lt_table>[ lv_new_line_index ] ).
    lr_new_line->ref_query_id = mv_query_id.
    lr_new_line->is_active = abap_true.
    lr_new_line->jump_target_type = zif_dbbr_global_consts=>gc_jump_call_types-normal.
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


  METHOD has_jump_destinations.
    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.
    DATA(lr_t_data) = mr_table->zif_uitb_table~get_table_data( ).
    ASSIGN lr_t_data->* TO <lt_data>.

    result = xsdbool( <lt_data> IS ASSIGNED AND <lt_data> IS NOT INITIAL ).
  ENDMETHOD.


  METHOD initialize.
    " read the query from the database
    load_query( ).

    DATA(lr_jumpdest_f) = NEW zcl_dbbr_jump_destination_f( ).
    " load existing jump fields
    mr_table->set_jump_destinations( lr_jumpdest_f->get_jump_destinations( mv_query_id ) ).
  ENDMETHOD.


  METHOD jump_field_f4.
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
*.... Fetch short alias for table from the join definition
      IF mf_join_is_active = abap_true.
        IF <ls_table>-is_primary = abap_true.
          <ls_table>-alias = ls_join_def-primary_table_alias_alv.
        ELSE.
          <ls_table>-alias = VALUE #( ls_join_def-tables[ add_table_alias = <ls_table>-tabname_alias ]-add_table_alias_alv OPTIONAL ).
        ENDIF.
      ENDIF.

*.... create tabfield list entries for output fields
      LOOP AT lt_query_selfields ASSIGNING FIELD-SYMBOL(<ls_query_field>) WHERE tabname_alias    = <ls_table>-tabname_alias
                                                                            AND output_active    = abap_true
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
        ls_tabfield-fieldname_raw   = ls_tabfield-fieldname.
        ls_tabfield-tabname_alias   = <ls_query_field>-tabname_alias.
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
    FIELD-SYMBOLS: <lt_jump_definitions> TYPE zdbbr_jumpdest_data_ui_itab.

    DATA(lr_jump_destinations) = mr_table->zif_uitb_table~get_table_data( ).
    ASSIGN lr_jump_destinations->* TO <lt_jump_definitions>.

    NEW zcl_dbbr_jump_destination_f( )->save_jump_destinations(
      EXPORTING
        iv_query_id          = mv_query_id
        if_update_query_flag = abap_true
      CHANGING
        ct_jump_destinations = <lt_jump_definitions>
    ).

    zcl_dbbr_screen_helper=>leave_screen( ).
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


  METHOD zif_uitb_screen_controller~cancel.
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~determine_cursor.
    zcl_uitb_cursor=>get_cursor( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>jump_list_manager.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_jump_list_manager.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
    DATA(lv_function) = cv_function_code.
    CLEAR cv_function_code.

    " clear copy attributes from table lines
    mr_table->clear_is_copied_flags( ).

    CASE lv_function.
      WHEN zif_dbbr_global_consts=>gc_function_codes-leave_screen.
        zcl_dbbr_screen_helper=>leave_screen( ).

      WHEN mc_function_codes-save_jump_fields.
        save_jump_fields( ).
      WHEN mc_function_codes-select_all_fields.

      WHEN mc_function_codes-unselect_all_fields.

      WHEN mc_function_codes-create_new_jump_field.
        add_new_jump_field( ).

      WHEN mc_function_codes-madbbrin_parameters.
        maintain_parameters( ).

      WHEN mc_function_codes-delete_jump_fields.
        mr_table->delete_selected_lines( ).

      WHEN mc_function_codes-copy_jump_destination.
        copy_jump_field( ).

    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).

    " update cursor
    zcl_uitb_cursor=>refresh_cursor( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    SET PF-STATUS 'JUMP_LIST_STATUS' OF PROGRAM zif_dbbr_c_report_id=>jump_list_manager.
    SET TITLEBAR 'JUMP_LIST' OF PROGRAM zif_dbbr_c_report_id=>jump_list_manager WITH ms_query_info-query_name.
  ENDMETHOD.
ENDCLASS.
