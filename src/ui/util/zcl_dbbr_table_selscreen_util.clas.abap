CLASS zcl_dbbr_table_selscreen_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selscreen_util
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! CONSTRUCTOR
    METHODS constructor
      IMPORTING
        ir_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data
        iv_entity_type    TYPE zsat_entity_type DEFAULT zif_sat_c_entity_type=>table.

    METHODS check_edit_mode                                REDEFINITION.
    METHODS check_primary_entity                           REDEFINITION.
    METHODS get_entity_information                         REDEFINITION.
    METHODS get_title                                      REDEFINITION.
    METHODS set_custom_functions                           REDEFINITION.
    METHODS zif_dbbr_screen_util~get_deactivated_functions REDEFINITION.
    METHODS zif_dbbr_screen_util~handle_ui_function        REDEFINITION.
    METHODS clear                                          REDEFINITION.

  PROTECTED SECTION.
    DATA mv_tab_size_text TYPE string.
    DATA mf_is_view TYPE abap_bool.

    METHODS load_entity_internal REDEFINITION.
    METHODS fill_selection_mask  REDEFINITION.
    METHODS fill_primary_entity  REDEFINITION.

  PRIVATE SECTION.
    METHODS import_data_from_json.
ENDCLASS.


CLASS zcl_dbbr_table_selscreen_util IMPLEMENTATION.
  METHOD check_edit_mode.
    CHECK mo_data->mr_s_global_data->edit = abap_true.

    delete_join_definition( ).
  ENDMETHOD.

  METHOD clear.
    super->clear( ).
    mo_data->mr_v_selmask_entity_type->* = TEXT-001.
  ENDMETHOD.

  METHOD check_primary_entity.
    rf_success = abap_true.

    DATA(ls_table_info) = zcl_sat_ddic_repo_access=>get_table_info( mo_data->mr_s_global_data->primary_table ).
    IF ls_table_info IS INITIAL.
      rf_success = abap_false.
      RETURN.
    ENDIF.

    mf_is_view = xsdbool( ls_table_info-tabclass = 'VIEW' ).
    mo_data->mr_s_settings->disable_edit = mf_is_view.

    IF mf_is_view = abap_true.
      mo_data->mr_v_selmask_entity_type->* = TEXT-005.
    ELSE.
      mo_data->mr_v_selmask_entity_type->* = TEXT-004.
    ENDIF.

    mo_data->mr_s_global_data->client_dependent = ls_table_info-clidep.

    mo_data->mr_v_selmask_entity_text->* = ls_table_info-ddtext.

    " reset edit mode upon table change
    IF ls_table_info-mainflag = abap_true.
      " check if any kind of maintenance view exists
      SELECT COUNT( * ) FROM objs
        INTO @DATA(lv_maint_view_count)
        WHERE tabname    = @mo_data->mr_s_global_data->primary_table
          AND objecttype = 'V'.
      IF lv_maint_view_count > 0.
        MESSAGE s418(mo) WITH mo_data->mr_s_global_data->primary_table.
      ELSE.
        " check if edit flag should be set
        mo_data->mr_s_global_data->edit = mo_data->mr_s_global_data->maintain_entries.
      ENDIF.
    ELSE.
      mo_data->mr_s_global_data->edit = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ir_selscreen_data = ir_selscreen_data
                        iv_entity_type    = iv_entity_type ).
    mo_data->mr_v_selmask_entity_type->* = 'Table/View'(001).

    " .. Fill custom toolbar menu
    mo_custom_menu = NEW #( ).
    mo_custom_menu->add_function( fcode = zif_dbbr_c_selscreen_functions=>navigate_to_table_def
                                  text  = |{ 'Navigate to Table Definition'(002) }| ).
    mo_custom_menu->add_function( fcode = zif_dbbr_c_selscreen_functions=>cross_reference_table
                                  text  = |{ 'Where-Used-List for Table'(003) }| ).

    IF mo_data->mr_s_global_data->experimental_mode = abap_true.
      mo_custom_menu->add_function( fcode = zif_dbbr_c_selscreen_functions=>import_json_data
                                    text  = |{ 'Import Data from JSON' }| ).
    ENDIF.

    fill_toolbar( if_create_extended_search = abap_true ).
  ENDMETHOD.

  METHOD fill_selection_mask.
    IF mo_data->mr_s_join_def->tables IS NOT INITIAL.
      update_multi_selection_mask( ).
    ELSE.
      update_selection_mask( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_primary_entity.
    " ... set cursor to first line
    mo_data->mo_selection_table->zif_uitb_page_scroller~scroll_page_top( ).
    mo_data->mo_custom_f4_map->clear( ).

    check_edit_mode( ).

    " ... create selection fields for primary table
    create_table_fields( VALUE #( tabname          = is_primary_entity-tabname
                                  tabname_alias    = is_primary_entity-tabname_alias
                                  active_selection = abap_true
                                  is_primary       = abap_true
                                  selection_order  = is_primary_entity-selection_order ) ).

    CLEAR mv_tab_size_text.
  ENDMETHOD.

  METHOD get_entity_information.
    super->get_entity_information( IMPORTING ev_description = ev_description ).
    ev_type = COND #( WHEN mf_is_view = abap_true THEN zif_sat_c_entity_type=>view ELSE zif_sat_c_entity_type=>table ).
    ev_entity_raw = mo_data->mr_s_global_data->primary_table.
    ev_entity_id = ev_entity_raw.
    ev_entity =
    ev_entity_id
    .
  ENDMETHOD.

  METHOD get_title.
    IF has_content( ).
      result = |Table - { mo_data->mr_s_global_data->primary_table }{ mv_tab_size_text }|.
    ELSE.
      result = |Table Mode|.
    ENDIF.
  ENDMETHOD.

  METHOD load_entity_internal.
    CHECK mo_data->mr_s_global_data->primary_table IS NOT INITIAL.

    zcl_dbbr_screen_helper=>show_progress(
        iv_text     = |Selection Mask for Table { mo_data->mr_s_global_data->primary_table } is loading...|
        iv_progress = 1 ).
    clear_edit_flags( ).
    mo_data->set_join_active( abap_false ).
    CLEAR mo_data->mr_s_settings->*.

    IF NOT check_primary_entity( ).
      RETURN.
    ENDIF.

    fill_primary_entity( VALUE #( tabname = mo_data->mr_s_global_data->primary_table
                                  alias   = mo_data->mr_s_global_data->primary_table ) ).

    mo_data->mr_v_selmask_entity_name->* = mo_data->mr_s_global_data->primary_table.

    " determine table size
    IF zcl_dbbr_usersettings_factory=>should_read_db_size( ).
      zcl_dbbr_screen_helper=>show_progress( iv_text     = `Determining Table size...`
                                             iv_progress = 70 ).
      mv_tab_size_text = | (rows { zcl_dbbr_selection_helper=>read_db_size( mo_data->mr_s_global_data->primary_table ) NUMBER = USER })|.
    ENDIF.

    update_selection_mask( ).

    mv_entity_id = mo_data->mr_s_global_data->primary_table.

    mo_data->mr_s_entity_info->* = VALUE #( entity_id   = mo_data->mr_s_global_data->primary_table
                                            entity_type = mv_entity_type ).

    update_entity_type_sh( ).

    finish_loading( ).

    rf_entity_loaded = abap_true.
  ENDMETHOD.

  METHOD set_custom_functions.
    mo_data->clear_custom_functions( ).

    mo_data->mr_s_top_custom_menu->text = 'Table'(001).

    " ... fill custom functions
    mo_data->mr_s_entity_function1->text = 'Navigate to Table Definition'(002).
    mo_data->mr_s_entity_function2->text = 'Where-Used-List for Table'(003).
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = super->get_deactivated_functions( ).
    IF mo_data->is_join_active( ).
      result = VALUE #( BASE result
                        ( zif_dbbr_c_selscreen_functions=>delete_db_content )
                        ( zif_dbbr_c_selscreen_functions=>import_json_data ) ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_ui_function.
    super->handle_ui_function( CHANGING cv_function = cv_function ).

    CASE cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>navigate_to_table_def.
        zcl_dbbr_ddic_util=>navigate_to_table( mo_data->mr_s_global_data->primary_table ).
        CLEAR cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>import_json_data.
        import_data_from_json( ).
        CLEAR cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>cross_reference_table.
        zcl_dbbr_ddic_util=>cross_reference_table( mo_data->mr_s_global_data->primary_table ).
        CLEAR cv_function.

    ENDCASE.
  ENDMETHOD.

  METHOD import_data_from_json.
    TYPES x1024 TYPE x LENGTH 1024.

    DATA lv_useraction TYPE i.
    DATA lt_filenames TYPE filetable.
    DATA lv_filecount TYPE i.
    DATA lt_xjson_data TYPE STANDARD TABLE OF x1024.
    DATA lr_t_import TYPE REF TO data.

    FIELD-SYMBOLS <lt_import> TYPE STANDARD TABLE.

    cl_gui_frontend_services=>file_open_dialog( EXPORTING default_extension = '.json'
                                                          default_filename  = |{ mv_entity_id }_data.json|
                                                          file_filter       = '*.json'
                                                CHANGING  file_table        = lt_filenames
                                                          rc                = lv_filecount
                                                          user_action       = lv_useraction ).

    IF NOT (     lv_useraction = cl_gui_frontend_services=>action_ok
             AND lv_filecount  = 1 ).
      RETURN.
    ENDIF.

    DATA(lv_fullpath) = |{ lt_filenames[ 1 ]-filename }|.

    cl_gui_frontend_services=>gui_upload( EXPORTING filename = lv_fullpath
                                                    filetype = 'BIN'
                                          CHANGING  data_tab = lt_xjson_data ).

    DATA(lo_xml_reader) = cl_sxml_table_reader=>create( input = lt_xjson_data ).
    DATA(lo_import_struct_type) = cl_abap_typedescr=>describe_by_name( mv_entity_id ).
    DATA(lo_import_table_type) = cl_abap_tabledescr=>create( p_line_type = CAST #( lo_import_struct_type ) ).
    CREATE DATA lr_t_import TYPE HANDLE lo_import_table_type.

    ASSIGN lr_t_import->* TO <lt_import>.

    TRY.
        CALL TRANSFORMATION id
             SOURCE XML lo_xml_reader
             RESULT data = <lt_import>.

        IF <lt_import> IS NOT INITIAL.
          IF zcl_dbbr_appl_util=>popup_to_confirm(
                 iv_title     = 'Import Data'
                 iv_query     = |Are you sure you want to import { lines( <lt_import> ) } entries into\r\n| &&
                                |table { mv_entity_id }|
                 iv_icon_type = 'ICON_WARNING' )
             = '1'.

            INSERT (mv_entity_id) FROM TABLE <lt_import> ACCEPTING DUPLICATE KEYS.
            IF sy-dbcnt > 0.
              MESSAGE |{ sy-dbcnt } entries imported into table { mv_entity_id }| TYPE 'I'.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE |No valid entries found for import into table { mv_entity_id }| TYPE 'I'.
        ENDIF.
      CATCH cx_transformation_error INTO DATA(lx_transformation).
        MESSAGE lx_transformation->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
