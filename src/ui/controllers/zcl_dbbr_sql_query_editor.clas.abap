"! <p class="shorttext synchronized" lang="en">SQL Query Creator</p>
CLASS zcl_dbbr_sql_query_editor DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_composite_view.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        iv_query_name TYPE zsat_query_name OPTIONAL
        iv_query      TYPE string OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Returns the last saved query</p>
    "!
    METHODS get_last_saved_query
      RETURNING
        VALUE(rv_query_name) TYPE zsat_query_name.
  PROTECTED SECTION.
    DATA mf_standalone_mode TYPE abap_bool.
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar.
    DATA mo_query_f TYPE REF TO zcl_dbbr_query_factory.
    DATA mo_editor TYPE REF TO zcl_uitb_gui_code_editor.
    DATA ms_current_query TYPE zdbbr_query_data.

    DATA mf_title TYPE string.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
    METHODS handle_exit_request
        REDEFINITION.
    "! <p class="shorttext synchronized" lang="en">Checks the modification state of the editor</p>
    METHODS is_modified
      RETURNING
        VALUE(rf_modified) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Check Query for errors</p>
    "!
    METHODS check_query
      RETURNING
        VALUE(rf_ok) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Saves the current query</p>
    "!
    METHODS save_query.
    "! <p class="shorttext synchronized" lang="en">Tests the current query string</p>
    METHODS test_query.

    "! <p class="shorttext synchronized" lang="en">Protect the given lines</p>
    METHODS protect_lines
      IMPORTING
        it_lines TYPE cl_gui_sourceedit=>linetabs OPTIONAL.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        check                TYPE ui_func VALUE 'CHECK',
        toggle_side_bar      TYPE ui_func VALUE 'TOGGLE_SIDE_BAR',
        load_entities        TYPE ui_func VALUE 'LOAD_ENTITIES',
        focus_on_editor      TYPE ui_func VALUE 'FOCUS_ON_EDITOR',
        focus_on_sidebar     TYPE ui_func VALUE 'FOCUS_ON_SIDEBAR',
        test_query           TYPE ui_func VALUE 'TEST',
        insert_with_indent   TYPE ui_func VALUE 'INSERT_WITH_INDENT',
        format_source        TYPE ui_func VALUE 'PRETTY_PRINTER',
        show_code_completion TYPE ui_func VALUE 'CODE_COMPLETION',
      END OF c_functions.

    DATA mv_query_name TYPE zsat_query_name.
    DATA mo_query TYPE REF TO zcl_dbbr_sql_query.
    DATA mv_last_saved_content TYPE string.
    DATA mf_no_edit_allowed TYPE abap_bool.
    DATA mf_modified TYPE abap_bool.
    DATA mv_last_saved_query TYPE zsat_query_name.
    DATA mv_last_tested_query TYPE string.
    DATA mo_splitter TYPE REF TO zcl_uitb_gui_splitter_cont.
    DATA mo_side_bar TYPE REF TO zcl_dbbr_sqle_sidebar.
    METHODS toggle_side_bar.
    METHODS init_side_bar.
    METHODS load_entities
      IMPORTING
        it_entities TYPE zdbbr_tabname_range_itab.
    METHODS insert_from_clipboard.
ENDCLASS.



CLASS zcl_dbbr_sql_query_editor IMPLEMENTATION.

  METHOD constructor.

    super->constructor( iv_title = '' ).
    mf_title = |DB Browser - | && COND #( WHEN iv_query_name IS INITIAL THEN |Create Query| ELSE |Edit Query { iv_query_name }| ).
    mv_query_name = iv_query_name.
    mo_query_f = NEW #( ).

    IF mv_query_name IS NOT INITIAL.
      ms_current_query = mo_query_f->get_query(
        iv_query_name      = mv_query_name
        if_load_completely = abap_false ).
      mf_no_edit_allowed = xsdbool( ms_current_query-is_global = abap_false AND ms_current_query-created_by <> sy-uname ).
      IF mf_no_edit_allowed = abap_true.
        MESSAGE s069(zdbbr_exception) WITH ms_current_query-query_name DISPLAY LIKE 'E'.
      ENDIF.
    ELSEIF iv_query IS NOT INITIAL.
      ms_current_query-source = iv_query.
    ENDIF.

    mv_last_saved_content = ms_current_query-source.
  ENDMETHOD.

  METHOD create_content.
    mo_splitter = NEW zcl_uitb_gui_splitter_cont(
        iv_elements  = 2
        iv_mode      = zcl_uitb_gui_splitter_cont=>c_mode-cols
        io_parent    = io_container
        iv_size      = '410:*'
    ).
    mo_splitter->set_element_visibility( iv_element = 1 if_visible = abap_false ).
    create_control_toolbar(
      EXPORTING io_parent    = mo_splitter->get_container( 2 )
                it_button    = VALUE #(
                  ( function  = c_functions-toggle_side_bar
                    icon      = icon_toggle_display
                    quickinfo = |{ 'Toggle Side Bar'(006) }| )
                  ( butn_type = cntb_btype_sep )
                  ( function  = c_functions-load_entities
                    icon      = icon_tree
                    quickinfo = |{ 'Synchronize data sources'(007) }| )
                  ( butn_type = cntb_btype_sep )
                  ( function  = c_functions-check
                    icon      = icon_check
                    quickinfo = |{ 'Check Syntax'(001) }| )
                  ( function = c_functions-format_source
                    text     = |{ 'Pretty Printer'(003) }| )
                  ( butn_type = cntb_btype_sep )
                  ( function  = c_functions-insert_with_indent
                    icon      = icon_system_paste
                    quickinfo = |{ 'Insert with indentations'(009) }| )
                  ( butn_type = cntb_btype_sep )
                  ( function  = c_functions-test_query
                    icon      = icon_test
                    quickinfo = |{ 'Test Query'(002) }| )
                  ( butn_type = cntb_btype_sep )
                  ( function  = c_functions-show_code_completion
                    icon      = icon_abap
                    text      = |{ 'Code Completion'(004) }|
                    quickinfo = |{ 'Show Code completion results'(005) }| )
                )
      IMPORTING eo_toolbar   = mo_toolbar
                eo_client    = DATA(lo_container)
    ).

    DATA(lr_f_use_text_based_editor) = CAST abap_bool(
        zcl_uitb_data_cache=>get_instance(
            zif_dbbr_c_report_id=>user_settings )->get_data_ref( zif_dbbr_user_settings_ids=>c_deactvt_highltng_in_cqe ) ).

    mo_editor = NEW zcl_uitb_gui_code_editor(
      io_parent      = lo_container
      iv_source_type = COND #( WHEN lr_f_use_text_based_editor->* = abap_true THEN 'TEXT' ELSE 'ABAP' )
      iv_line_width  = 250
    ).

    mo_editor->set_text( iv_text = ms_current_query-source  ).

    IF mf_no_edit_allowed = abap_true.
      mo_editor->set_editable( abap_false ).
    ENDIF.

    toggle_side_bar( ).

  ENDMETHOD.

  METHOD zif_uitb_gui_composite_view~set_child_visibility.
*.. Let's see if we really need it here
  ENDMETHOD.

  METHOD zif_uitb_gui_composite_view~execute_command.
    zif_uitb_gui_command_handler~execute_command( io_command ).
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.

    CASE io_command->mv_function.

      WHEN zif_uitb_c_gui_screen=>c_functions-save.
        IF check_query( ).
          save_query( ).
        ENDIF.

      WHEN c_functions-test_query.
        IF check_query( ).
          test_query( ).
        ENDIF.

      WHEN c_functions-focus_on_editor.
        mo_editor->focus( ).

      WHEN c_functions-focus_on_sidebar.
        mo_side_bar->show_view( zcl_dbbr_sqle_sidebar=>c_views-data_source_browser ).

      WHEN c_functions-format_source.
        mo_editor->get_sel_position(
          IMPORTING ev_line = DATA(lv_current_line)
                    ev_pos  = DATA(lv_current_pos)
        ).
        mo_editor->format_source( ).
        mo_editor->set_sel_position( iv_line = lv_current_line
                                     iv_pos  = lv_current_pos ).

      WHEN c_functions-show_code_completion.
        mo_editor->show_completion_results( ).

      WHEN c_functions-check.
        check_query( ).

      WHEN c_functions-toggle_side_bar.
        toggle_side_bar( ).

      WHEN zcl_uitb_gui_code_editor=>c_command_ids-replace_content.
        ASSIGN io_command->mr_params->* TO FIELD-SYMBOL(<lv_content>).
        IF sy-subrc = 0.
          mo_editor->set_text( if_new_content = abap_true iv_text =  <lv_content> ).
        ENDIF.

      WHEN c_functions-load_entities.
        DATA(lt_entities_in_query) = zcl_dbbr_sql_query_parser=>get_entities_in_query( mo_editor->get_text( ) ).
        IF lt_entities_in_query IS NOT INITIAL.
          load_entities( lt_entities_in_query ).
        ENDIF.

      WHEN c_functions-insert_with_indent.
        insert_from_clipboard( ).

      WHEN zif_uitb_c_gui_screen=>c_functions-search.
        IF mo_side_bar IS BOUND AND mo_side_bar->zif_uitb_gui_control~has_focus( ).
          mo_side_bar->zif_uitb_content_searcher~search( ).
        ENDIF.

      WHEN zif_uitb_c_gui_screen=>c_functions-search_more.
        IF mo_side_bar IS BOUND AND mo_side_bar->zif_uitb_gui_control~has_focus( ).
          mo_side_bar->zif_uitb_content_searcher~search_next( ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->set_title( mf_title ).
    io_callback->map_fkey_functions( VALUE #(
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f1  mapped_function = c_functions-focus_on_editor      text = |{ 'Set focus to Editor' }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f9 mapped_function = c_functions-focus_on_sidebar     text = |{ 'Set focus to Sidebar' }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f2  mapped_function = c_functions-check                text = |{ TEXT-001 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f1 mapped_function = c_functions-format_source        text = |{ TEXT-003 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f8       mapped_function = c_functions-test_query           text = |{ TEXT-002 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f5  mapped_function = c_functions-load_entities        text = |{ TEXT-007 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f5       mapped_function = c_functions-show_code_completion text = |{ TEXT-004 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f6       mapped_function = c_functions-insert_with_indent   text = |{ TEXT-008 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f9       mapped_function = c_functions-toggle_side_bar      text = |{ TEXT-006 }| )
    ) ).

    IF mf_no_edit_allowed = abap_true.
      io_callback->deactivate_function( zif_uitb_c_gui_screen=>c_functions-save ).
    ENDIF.

    IF io_callback->is_first_screen_call( ).
      mo_editor->focus( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_exit_request.
    DATA: lv_prompt_query TYPE string.

    CHECK mf_standalone_mode = abap_false.

*... Check if content was modified
    IF mo_editor IS BOUND AND is_modified( ).
      IF ms_current_query-query_id IS NOT INITIAL.
        lv_prompt_query = |{ 'Save changes in query' } { ms_current_query-query_name }?|.
      ELSE.
        lv_prompt_query = |{ 'Do you want to save the current query before you leave' }?|.
      ENDIF.

      DATA(lv_prompt) = zcl_dbbr_appl_util=>popup_to_confirm(
        iv_title                 = 'Warning'
        iv_query                 = lv_prompt_query
        iv_icon_type             = 'ICON_WARNING'
      ).
      IF lv_prompt = '1'. " Yes
        IF check_query( ).
          save_query( ).
        ENDIF.
      ELSEIF lv_prompt = 'A'. " Cancel
        io_callback->cancel_exit( ).
      ENDIF.
    ENDIF.

*... Close the protocol if it is still open
    zcl_uitb_protocol=>get_instance( )->close_protocol( ).
  ENDMETHOD.

  METHOD is_modified.
    rf_modified = mo_editor->is_modified( ).
    CHECK rf_modified = abap_false.

    DATA(lv_text) = mo_editor->get_text( ).
    DATA(lv_text_upper) = to_upper( lv_text ).
    rf_modified = xsdbool( lv_text IS NOT INITIAL AND
                           ( lv_text_upper CS 'SELECT' OR
                             lv_text_upper CS 'WITH' ) AND
                           lv_text <> mv_last_saved_content ).
  ENDMETHOD.

  METHOD check_query.

    DATA(lo_protocol) = zcl_uitb_protocol=>get_instance( ).
    lo_protocol->close_protocol( ).
    lo_protocol->clear( ).

    DATA(lv_text) = mo_editor->get_text( ).

    IF lv_text IS INITIAL.
      MESSAGE |Editor contains no content| TYPE 'S'.
      RETURN.
    ENDIF.
    TRY.

        zcl_dbbr_screen_helper=>show_progress(
            iv_progress = 1
            iv_text     = |{ TEXT-001 }...|
        ).

        mo_query = NEW zcl_dbbr_sql_query_parser( lv_text )->parse( ).

        IF lo_protocol->has_messages( ).
          lo_protocol->show_protocol( ).
        ENDIF.
        rf_ok = abap_true.

      CATCH zcx_dbbr_sql_query_error INTO DATA(lx_parse_error).
        IF lx_parse_error->line_number > 0.
          mo_editor->select_row( lx_parse_error->line_number ).
        ENDIF.

        IF lx_parse_error->message IS NOT INITIAL.
          lo_protocol->add_error(
              iv_message     = lx_parse_error->message
              iv_line_number = CONV #( lx_parse_error->line_number )
          ).
        ELSE.
          lx_parse_error->zif_sat_exception_message~get_message( ).
          lo_protocol->add_error_from_sy( CONV #( lx_parse_error->line_number ) ).
        ENDIF.
        lo_protocol->show_protocol( ).
    ENDTRY.

  ENDMETHOD.


  METHOD save_query.
    IF ms_current_query-query_id IS NOT INITIAL.
      ms_current_query-source = mo_query->ms_data-source.
      ms_current_query-parameters = mo_query->mt_parameters.

      DATA(lv_query_id) = mo_query_f->save_query( ms_current_query ).
      IF lv_query_id IS NOT INITIAL.
        MESSAGE s022(zdbbr_info) WITH ms_current_query-query_name.
        mo_editor->set_unmodified( ).
        mv_last_saved_query = ms_current_query-query_name.
        mv_last_saved_content = ms_current_query-source.
      ENDIF.
    ELSE.
      DATA(lo_save_controller) = NEW zcl_dbbr_save_sql_query_ctrl(
        is_query_info       = VALUE #(
          query_name  = ms_current_query-query_name
          description = ms_current_query-description
          is_global   = ms_current_query-is_global
          source      = mo_query->ms_data-source
        )
        it_query_parameters = mo_query->mt_parameters
      ).

      lo_save_controller->show( ).

      IF lo_save_controller->was_saved( ).
        DATA(lv_saved_query_name) = lo_save_controller->get_query_name( ).
        mo_editor->set_unmodified( ).
        mv_last_saved_content = mo_query->ms_data-source.
        mv_last_saved_query =
        ms_current_query-query_name = lv_saved_query_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_last_saved_query.
    rv_query_name = mv_last_saved_query.
  ENDMETHOD.

  METHOD protect_lines.
    mo_editor->protect_lines( it_lines ).
  ENDMETHOD.

  METHOD test_query.
    DATA(lv_query) = mo_editor->get_text( ).
    IF lv_query IS INITIAL.
      MESSAGE |There is no query to test| TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
    zcl_dbbr_custom_query_tester=>test_query( lv_query ).

*.. Trigger history refresh
    IF mo_side_bar IS BOUND.
      mo_side_bar->zif_uitb_gui_composite_view~execute_command(
          NEW zcl_uitb_gui_simple_command( iv_function = zif_dbbr_c_sql_query_editor=>fc_refresh_history )
      ).
    ENDIF.

    mv_last_tested_query = lv_query.
  ENDMETHOD.


  METHOD toggle_side_bar.
    init_side_bar( ).
    mo_splitter->toggle_visibility( 1 ).
  ENDMETHOD.

  METHOD init_side_bar.
    CHECK mo_side_bar IS INITIAL.
    mo_side_bar = NEW #(
      io_parent_container = mo_splitter->get_container( 1 )
      io_parent_view      = me
    ).
  ENDMETHOD.


  METHOD load_entities.
    CHECK it_entities IS NOT INITIAL.
    init_side_bar( ).
    mo_side_bar->zif_uitb_gui_composite_view~execute_command(
        NEW zcl_uitb_gui_simple_command(
            iv_function = zif_dbbr_c_sql_query_editor=>fc_load_entity
            ir_params   = NEW zdbbr_tabname_range_itab( it_entities )
        )
    ).
    mo_side_bar->show_view( zcl_dbbr_sqle_sidebar=>c_views-data_source_browser ).
    IF NOT mo_splitter->is_element_visible( 1 ).
      mo_splitter->set_element_visibility( iv_element = 1 ).
    ENDIF.
  ENDMETHOD.


  METHOD insert_from_clipboard.
    TYPES: lty_clipboard TYPE c LENGTH 1000.
    DATA: lt_clipboard_data    TYPE TABLE OF lty_clipboard,
          lv_spaces            TYPE string,
          lv_clipboard_content TYPE string.

    cl_gui_frontend_services=>clipboard_import( IMPORTING data = lt_clipboard_data ).

    CHECK lt_clipboard_data IS NOT INITIAL.
    mo_editor->get_sel_position(
      IMPORTING ev_pos  = DATA(lv_pos)
    ).

    DO lv_pos - 1 TIMES.
      lv_spaces = | { lv_spaces }|.
    ENDDO.

    LOOP AT lt_clipboard_data ASSIGNING FIELD-SYMBOL(<lv_clipboard>) FROM 2.
      <lv_clipboard> = |{ lv_spaces }{ condense( <lv_clipboard> ) }|.
    ENDLOOP.

    CONCATENATE LINES OF lt_clipboard_data INTO lv_clipboard_content SEPARATED BY cl_abap_char_utilities=>newline.

    mo_editor->set_selected_text( lv_clipboard_content ).
  ENDMETHOD.

ENDCLASS.
