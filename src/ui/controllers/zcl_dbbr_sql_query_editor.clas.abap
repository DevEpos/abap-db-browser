"! <p class="shorttext synchronized" lang="en">SQL Query Creator</p>
CLASS zcl_dbbr_sql_query_editor DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        iv_query_name TYPE zdbbr_query_name OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Returns the last saved query</p>
    "!
    METHODS get_last_saved_query
      RETURNING
        VALUE(rv_query_name) TYPE zdbbr_query_name.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
    METHODS handle_exit_request
        REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        check                TYPE ui_func VALUE 'CHECK',
        focus_on_editor      TYPE ui_func VALUE 'FOCUS_ON_EDITOR',
        test_query           TYPE ui_func VALUE 'TEST',
        format_source        TYPE ui_func VALUE 'PRETTY_PRINTER',
        show_code_completion TYPE ui_func VALUE 'CODE_COMPLETION',
      END OF c_functions.

    DATA mo_toolbar TYPE REF TO cl_gui_toolbar.
    DATA mv_query_name TYPE zdbbr_query_name.
    DATA mo_query_f TYPE REF TO zcl_dbbr_query_factory.
    DATA mo_query TYPE REF TO zcl_dbbr_sql_query.
    DATA mo_editor TYPE REF TO zcl_uitb_gui_code_editor.
    DATA ms_current_query TYPE zdbbr_query_data.
    DATA mv_last_saved_content TYPE string.
    DATA mf_no_edit_allowed TYPE abap_bool.
    DATA mf_modified TYPE abap_bool.
    DATA mv_last_saved_query TYPE zdbbr_query_name.

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

ENDCLASS.



CLASS zcl_dbbr_sql_query_editor IMPLEMENTATION.

  METHOD constructor.

    super->constructor( |DB Browser - | && COND #( WHEN iv_query_name IS INITIAL THEN |Create Query| ELSE |Edit Query { iv_query_name }| ) ).
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
    ENDIF.

    mv_last_saved_content = ms_current_query-source.
  ENDMETHOD.

  METHOD create_content.
    create_control_toolbar(
      EXPORTING
        io_parent    = io_container
        it_button    = VALUE #(
          ( function  = c_functions-check
            icon      = icon_check
            quickinfo = |{ 'Check Syntax'(001) }| )
          ( function = c_functions-format_source
            text     = |{ 'Pretty Printer'(003) }| )
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
      IMPORTING
        eo_toolbar   = mo_toolbar
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

    mo_editor->set_text( ms_current_query-source ).

    IF mf_no_edit_allowed = abap_true.
      mo_editor->set_editable( abap_false ).
    ENDIF.

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
    ENDCASE.
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->map_fkey_functions( VALUE #(
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f1  mapped_function = c_functions-focus_on_editor      text = |{ 'Set focus to Editor' }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f2  mapped_function = c_functions-check                text = |{ TEXT-001 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f1 mapped_function = c_functions-format_source        text = |{ TEXT-003 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f8       mapped_function = c_functions-test_query           text = |{ TEXT-002 }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f5       mapped_function = c_functions-show_code_completion text = |{ TEXT-004 }| )
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

*... Check if content was modified
    IF mo_editor IS BOUND AND is_modified( ).
      IF ms_current_query-query_id IS NOT INITIAL.
        lv_prompt_query = |{ 'Save changes in query' } { ms_current_query-query_name }?|.
      ELSE.
        lv_prompt_query = |{ 'Do you want to save the current query before you leave' }?|.
      ENDIF.

      IF zcl_dbbr_appl_util=>popup_to_confirm(
            iv_title                 = 'Warning'
           iv_query                 = lv_prompt_query
           iv_icon_type             = 'ICON_WARNING'
         ) = '1'.

        IF check_query( ).
          save_query( ).
        ELSE.
          io_callback->cancel_exit( ).
        ENDIF.
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
          lx_parse_error->zif_dbbr_exception_message~get_message( ).
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


  METHOD test_query.
    DATA(lv_query) = mo_editor->get_text( ).
    IF lv_query IS INITIAL.
      MESSAGE |There is no query to test| TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
    zcl_dbbr_custom_query_tester=>test_query( lv_query ).
  ENDMETHOD.


ENDCLASS.
