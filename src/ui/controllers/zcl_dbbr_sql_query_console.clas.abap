"! <p class="shorttext synchronized" lang="en">Open SQL Query Console View</p>
CLASS zcl_dbbr_sql_query_console DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view .

    "! <p class="shorttext synchronized" lang="en">Will be called upon finished query syntax check</p>
    METHODS check_query_finished
      IMPORTING
        !p_task TYPE clike .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor .
    "! <p class="shorttext synchronized" lang="en">Will be called upon finished query execution</p>
    METHODS execute_query_finished
      IMPORTING
        !p_task TYPE clike .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_query,
        execution_time TYPE zdbbr_dec17_5,
        rows_retrieved TYPE sy-tabix,
        executed_query TYPE string,
        query          TYPE string,
      END OF ty_query .
    TYPES:
      BEGIN OF ty_fav_query,
        query_id    TYPE zdbbr_sql_query_id,
        description TYPE ddtext,
        query       TYPE string,
      END OF ty_fav_query .

    DATA mf_async_processing TYPE abap_bool .
    DATA mv_height TYPE i .
    "! <p class="shorttext synchronized" lang="en">Dynamic Query Result</p>
    DATA mr_query_result TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Query Result</p>
    DATA ms_query_result TYPE zdbbr_dp_table_data .
    "! <p class="shorttext synchronized" lang="en">Completion Check for asynchronous syntax check</p>
    DATA mf_async_finished TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Callback for Template Program</p>
    DATA mr_view TYPE REF TO zif_uitb_template_prog .
    "! <p class="shorttext synchronized" lang="en">ABAP Edit Control</p>
    DATA mr_abap_edit TYPE REF TO cl_gui_abapedit .
    DATA mr_splitter TYPE REF TO cl_gui_splitter_container .
    DATA mr_output_dock TYPE REF TO cl_gui_docking_container .
    DATA mr_output_alv TYPE REF TO zcl_uitb_alv .
    DATA mr_t_output TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Dynamic Documents: Input Element</p>
    DATA mr_row_input TYPE REF TO cl_dd_input_element .
    "! <p class="shorttext synchronized" lang="en">Dynamic Documents: Document</p>
    DATA mr_row_input_dd TYPE REF TO cl_dd_document .
    DATA mf_query_invalid TYPE abap_bool .
    DATA:
      mt_executed_queries TYPE TABLE OF ty_query .
    DATA:
      mt_favorite_queries TYPE TABLE OF ty_fav_query .
    DATA mv_last_executed_query TYPE string .

    "! <p class="shorttext synchronized" lang="en">Checks the entered query</p>
    METHODS check_query .
    METHODS count_lines .
    "! <p class="shorttext synchronized" lang="en">Create abap_edit as console</p>
    METHODS create_console .
    "! <p class="shorttext synchronized" lang="en">Create functions in tool bar</p>
    METHODS create_functions .
    METHODS create_input .
    "! <p class="shorttext synchronized" lang="en">Create splitter for console and query history</p>
    METHODS create_splitter .
    "! <p class="shorttext synchronized" lang="en">Logic for first screen call</p>
    METHODS do_on_first_call .
    "! <p class="shorttext synchronized" lang="en">Execute the entered query and display the results</p>
    METHODS execute_query .
    "! <p class="shorttext synchronized" lang="en">Exit event</p>
    METHODS on_exit
          FOR EVENT exit OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
    "! <p class="shorttext synchronized" lang="en">PAI event</p>
    METHODS on_pai
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          !er_callback
          !ev_function_id .
    "! <p class="shorttext synchronized" lang="en">PBO event</p>
    METHODS on_pbo
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
    "! <p class="shorttext synchronized" lang="en">Max Row Number enter event</p>
    METHODS on_row_number_input
         FOR EVENT entered OF cl_dd_input_element .
    "! <p class="shorttext synchronized" lang="en">Pretty Print query source</p>
    METHODS pretty_print_query .
    METHODS process_count_lines_result .
    "! <p class="shorttext synchronized" lang="en">Process Query result</p>
    METHODS process_query_result .

    "! <p class="shorttext synchronized" lang="en">Update history alv</p>
    METHODS update_history .
    "! <p class="shorttext synchronized" lang="en">Create dock and alv to display query result</p>
    METHODS update_output .
    "! <p class="shorttext synchronized" lang="en">Update column descriptions of query result ALV</p>
    METHODS update_result_columns .
ENDCLASS.


CLASS ZCL_DBBR_SQL_QUERY_CONSOLE IMPLEMENTATION.

  METHOD check_query.
    DATA: lt_table        TYPE STANDARD TABLE OF string,
          lv_query        TYPE string,
          ls_check_result TYPE zdbbr_dp_check_result.

    mr_abap_edit->get_text(
      IMPORTING
        table                  = lt_table
        is_modified            = DATA(lv_modified)
    ).

    CONCATENATE LINES OF lt_table INTO lv_query SEPARATED BY cl_abap_char_utilities=>cr_lf.

    CLEAR mf_async_finished.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |Query is being checked...| iv_progress = 1 ).

    CALL FUNCTION 'ZDBBR_EXECUTE_SQL_QUERY' STARTING NEW TASK 'QUERY_CHECK' DESTINATION 'NONE'
      CALLING check_query_finished ON END OF TASK
      EXPORTING
        iv_query        = lv_query
        if_syntax_check = abap_true.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL mf_async_finished = abap_true.

  ENDMETHOD.


  METHOD check_query_finished.
    DATA: ls_query_check_result TYPE zdbbr_dp_check_result,
          lv_start_position     TYPE string,
          lv_end_position       TYPE string,
          lv_start_line         TYPE string,
          lv_start_column       TYPE string.

    RECEIVE RESULTS FROM FUNCTION 'ZDBBR_EXECUTE_SQL_QUERY'
    IMPORTING
      es_check_result = ls_query_check_result.

    mf_query_invalid = abap_false.

    IF ls_query_check_result-message IS NOT INITIAL.
      mf_query_invalid = abap_true.
      SPLIT ls_query_check_result-position AT ';' INTO lv_start_position lv_end_position.
      IF lv_start_position IS NOT INITIAL.
        SPLIT lv_start_position AT ',' INTO lv_start_line lv_start_column.
        mr_abap_edit->set_sel_pos_in_line_centered(
          EXPORTING
            line                   = CONV #( lv_start_line )
            pos                    = CONV #( lv_start_column )
          EXCEPTIONS
            error_cntl_call_method = 1
            OTHERS                 = 2
        ).
        IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

*.... Show message
      MESSAGE |{ ls_query_check_result-message }| TYPE 'S' DISPLAY LIKE ls_query_check_result-message_severity.
    ENDIF.

    mf_async_finished = abap_true.

  ENDMETHOD.


  METHOD constructor.
    mr_view = zcl_uitb_templt_prog_callback=>create_template_program( 'DB Browser - SQL Console' ).

    create_functions( ).

    SET HANDLER:
      on_pai FOR mr_view,
      on_pbo FOR mr_view,
      on_exit FOR mr_view.
  ENDMETHOD.


  METHOD count_lines.
    DATA: lt_table        TYPE STANDARD TABLE OF string,
          ls_check_result TYPE zdbbr_dp_check_result.

*.. Check query before execution
    check_query( ).
    CHECK mf_query_invalid = abap_false.

    mr_abap_edit->get_text(
      IMPORTING
        table                  = lt_table
        is_modified            = DATA(lv_modified)
    ).

    CONCATENATE LINES OF lt_table INTO mv_last_executed_query SEPARATED BY cl_abap_char_utilities=>cr_lf.

    CLEAR: mf_async_finished,
           ms_query_result.

    DATA(lv_row_count) = CONV i( mr_row_input->value ).

    zcl_dbbr_screen_helper=>show_progress( iv_text = |Query is being executed...| iv_progress = 1 ).

    CALL FUNCTION 'ZDBBR_EXECUTE_SQL_QUERY' STARTING NEW TASK 'COUNT_EXEC' DESTINATION 'NONE'
      CALLING execute_query_finished ON END OF TASK
      EXPORTING
        iv_query      = mv_last_executed_query
        if_count_only = abap_true.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL mf_async_finished = abap_true.

    process_count_lines_result( ).

*.. set console editor to unmodified
    mr_abap_edit->set_textmodified_status( ).
  ENDMETHOD.


  METHOD create_console.
    DATA(lr_top_container) = mr_splitter->get_container( row = 2 column = 1 ).
    mr_abap_edit = NEW cl_gui_abapedit(
        parent           = lr_top_container
        max_number_chars = 140
    ).
    mr_abap_edit->init_completer( ).
    DATA(lr_parser) = mr_abap_edit->get_completer( ).

    SET HANDLER:
     lr_parser->handle_completion_request FOR mr_abap_edit,
     lr_parser->handle_insertion_request FOR mr_abap_edit,
     lr_parser->handle_quickinfo_request FOR mr_abap_edit.

    mr_abap_edit->register_event_completion( ).
    mr_abap_edit->register_event_quick_info( ).
    mr_abap_edit->register_event_insert_pattern( ).

    DATA: lt_text TYPE STANDARD TABLE OF char140.
    mr_abap_edit->set_text(
        table           = lt_text

    ).

    mr_abap_edit->set_local_contextmenu_mode( visible = 1 ).

    cl_gui_control=>set_focus( mr_abap_edit ).

  ENDMETHOD.


  METHOD create_functions.
    mr_view->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_ctrl_f2
        iv_text        = 'Check'
        iv_icon        = icon_check
    ).

    mr_view->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_f8
        iv_text        = 'Execute Query'
        iv_icon        = icon_execute_object
    ).

    IF sy-saprl >= 750.
      mr_view->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_shift_f1
        iv_text        = 'Pretty Printer'
      ).
    ENDIF.

    mr_view->add_function(
      iv_function_id = zif_uitb_template_prog=>c_func_f7
      iv_text        = 'Number of Entries'
    ).

  ENDMETHOD.


  METHOD create_input.
    mr_row_input_dd = NEW #( ).

    mr_row_input_dd->initialize_document( background_color = cl_dd_area=>col_background_level2 ).

*.. Fill the document
    mr_row_input_dd->add_form( IMPORTING formarea = DATA(lr_form) ).
    lr_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lr_form->add_text( text = 'Max. Rows: ' ).
    lr_form->add_gap( width = 2 ).
    lr_form->add_input_element(
      EXPORTING
        value         = |{ 100 }|
        size          = 15
      IMPORTING
        input_element = mr_row_input
    ).

*.. Register event handlers for form elements
    SET HANDLER:
      on_row_number_input FOR mr_row_input.

    mr_row_input_dd->merge_document( ).

    DATA(lr_container) = mr_splitter->get_container( column = 1 row = 1 ).
    mr_row_input_dd->display_document( parent = lr_container reuse_control = abap_true ).
  ENDMETHOD.


  METHOD create_splitter.
    mr_splitter = NEW #(
      parent  = mr_view->get_container( )
      columns = 1
      rows    = 3
    ).

    mr_splitter->set_row_mode( cl_gui_splitter_container=>mode_absolute ).

    mr_splitter->set_row_height( id = 1 height = 37 ).
    mr_splitter->set_row_height( id = 3 height = 0 ).

    mr_splitter->set_row_sash(
        id = 1
        type = cl_gui_splitter_container=>type_movable
        value = cl_gui_splitter_container=>false
    ).

  ENDMETHOD.


  METHOD do_on_first_call.
    create_splitter( ).
    create_input( ).
    create_console( ).
  ENDMETHOD.


  METHOD execute_query.
    DATA: lt_table        TYPE STANDARD TABLE OF string,
          ls_check_result TYPE zdbbr_dp_check_result.

*.. Check query before execution
    check_query( ).
    CHECK mf_query_invalid = abap_false.

    mr_abap_edit->get_text(
      IMPORTING
        table                  = lt_table
        is_modified            = DATA(lv_modified)
    ).

    CONCATENATE LINES OF lt_table INTO mv_last_executed_query SEPARATED BY cl_abap_char_utilities=>cr_lf.

    CLEAR: mf_async_finished,
           ms_query_result.

    DATA(lv_row_count) = CONV i( mr_row_input->value ).

    zcl_dbbr_screen_helper=>show_progress( iv_text = |Query is being executed...| iv_progress = 1 ).

    CALL FUNCTION 'ZDBBR_EXECUTE_SQL_QUERY' STARTING NEW TASK 'QUERY_EXEC' DESTINATION 'NONE'
      CALLING execute_query_finished ON END OF TASK
      EXPORTING
        iv_query     = mv_last_executed_query
        iv_row_count = lv_row_count.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL mf_async_finished = abap_true.

    process_query_result( ).

*.. set console editor to unmodified
    mr_abap_edit->set_textmodified_status( ).
  ENDMETHOD.


  METHOD execute_query_finished.
    DATA: ls_query_result TYPE zdbbr_dp_table_data.

    RECEIVE RESULTS FROM FUNCTION 'ZDBBR_EXECUTE_SQL_QUERY'
      IMPORTING
        es_query_result = ms_query_result.

    mf_async_finished = abap_true.
  ENDMETHOD.


  METHOD on_exit.
  ENDMETHOD.


  METHOD on_pai.
    CASE ev_function_id.

      WHEN zif_uitb_template_prog=>c_func_ctrl_f2.
        check_query( ).

      WHEN zif_uitb_template_prog=>c_func_f8.
        execute_query( ).

      WHEN zif_uitb_template_prog=>c_func_shift_f1.
        pretty_print_query( ).

      WHEN zif_uitb_template_prog=>c_func_f7.
        count_lines( ).

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.


  METHOD on_pbo.
    IF er_callback->is_first_screen_call( ).
      do_on_first_call( ).
    ENDIF.
  ENDMETHOD.


  METHOD on_row_number_input.
    execute_query( ).
  ENDMETHOD.


  METHOD pretty_print_query.
    DATA: lt_source       TYPE STANDARD TABLE OF string,
          lv_query        TYPE string,
          ls_check_result TYPE zdbbr_dp_check_result.

    mr_abap_edit->get_text(
      IMPORTING
        table                  = lt_source
        is_modified            = DATA(lv_modified)
    ).

    TRY.
        zcl_dbbr_pretty_printer=>format_source(
          CHANGING
            ct_source              = lt_source
        ).
        mr_abap_edit->set_text( table = lt_source ).
      CATCH cx_sedi_pretty_printer.    "
    ENDTRY.
  ENDMETHOD.


  METHOD process_count_lines_result.
    MESSAGE |Number of entries that meet the selection criteria: { ms_query_result-line_count NUMBER = USER }| TYPE 'I'.
  ENDMETHOD.


  METHOD process_query_result.
    DATA: lt_abap_comp_type TYPE cl_abap_structdescr=>component_table,
          lr_new_line       TYPE REF TO data,
          lr_type           TYPE REF TO cl_abap_datadescr.

    FIELD-SYMBOLS: <lt_result> TYPE table.

    CHECK ms_query_result IS NOT INITIAL.

*.. Check if an error occurred
    IF ms_query_result-message IS NOT INITIAL.
*.... Show message
      MESSAGE |{ ms_query_result-message }| TYPE 'S' DISPLAY LIKE ms_query_result-message_severity.
    ELSE.
      CLEAR mr_query_result.

      LOOP AT ms_query_result-columns ASSIGNING FIELD-SYMBOL(<ls_column>).
        DATA(ls_metadata) = <ls_column>-metadata.

        IF ls_metadata-rollname IS NOT INITIAL.
          lr_type = CAST #( cl_abap_typedescr=>describe_by_name( ls_metadata-rollname ) ).
        ELSE.
          CASE ls_metadata-typekind.

            WHEN cl_abap_typedescr=>typekind_date.
              lr_type = cl_abap_elemdescr=>get_d( ).

            WHEN cl_abap_typedescr=>typekind_string.
              lr_type = cl_abap_elemdescr=>get_string( ).

            WHEN cl_abap_typedescr=>typekind_time.
              lr_type = cl_abap_elemdescr=>get_t( ).

            WHEN cl_abap_typedescr=>typekind_clike OR
                 cl_abap_typedescr=>typekind_char.
              lr_type = cl_abap_elemdescr=>get_c( p_length = ls_metadata-length ).

            WHEN cl_abap_typedescr=>typekind_packed.
              lr_type = cl_abap_elemdescr=>get_p( p_length = ls_metadata-length p_decimals = CONV #( ls_metadata-decimals ) ).

            WHEN cl_abap_typedescr=>typekind_float.
              lr_type = cl_abap_elemdescr=>get_f( ).

            WHEN OTHERS.
          ENDCASE.
        ENDIF.

        lt_abap_comp_type = VALUE #(
          BASE lt_abap_comp_type
          ( name    = ls_metadata-name
            type    = lr_type )
        ).
      ENDLOOP.

      IF lt_abap_comp_type IS NOT INITIAL.
        TRY.
            DATA(lr_line_type) = cl_abap_structdescr=>create(
                p_components = lt_abap_comp_type
            ).
            DATA(lr_table_type) = cl_abap_tabledescr=>create(
                p_line_type = lr_line_type
            ).
            CREATE DATA mr_query_result TYPE HANDLE lr_table_type.
            ASSIGN mr_query_result->* TO <lt_result>.

*.......... fill query result table
            DO ms_query_result-line_count TIMES.
              DATA(lv_index) = sy-index.

              CREATE DATA lr_new_line TYPE HANDLE lr_line_type.
              ASSIGN lr_new_line->* TO FIELD-SYMBOL(<ls_new_line>).

              LOOP AT ms_query_result-columns ASSIGNING <ls_column>.
                ASSIGN COMPONENT <ls_column>-metadata-name OF STRUCTURE <ls_new_line> TO FIELD-SYMBOL(<lv_component>).
                <lv_component> = |{ <ls_column>-dataset[ lv_index ] }|.
              ENDLOOP.

              APPEND <ls_new_line> TO <lt_result>.
            ENDDO.

          CATCH cx_sy_struct_creation
                cx_sy_table_creation.
            MESSAGE |Error during type creation. Preview not possible| TYPE 'I' DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.
      ENDIF.

      update_history( ).

      update_output( ).
    ENDIF.
  ENDMETHOD.


  METHOD update_history.

    DATA(ls_executed_query) = VALUE ty_query(
        execution_time = CONV #( ms_query_result-query_execution_time )
        rows_retrieved = ms_query_result-line_count
        executed_query = ms_query_result-executed_query_string
        query          = mv_last_executed_query
    ).

    INSERT ls_executed_query INTO mt_executed_queries INDEX 1.

  ENDMETHOD.


  METHOD update_output.
    CHECK mr_query_result IS NOT INITIAL.

    IF mr_output_dock IS INITIAL.
      mr_output_dock = NEW #(
        lifetime = cl_gui_control=>lifetime_dynpro
        side     = cl_gui_docking_container=>dock_at_right
        ratio    = 50
*        extension = 5000
      ).

      mr_output_alv = zcl_uitb_alv=>create_alv(
          ir_data                 = mr_query_result
          ir_container            = mr_output_dock
      ).

      DATA(lr_selections) = mr_output_alv->get_selections( ).
      lr_selections->set_mode( value = zif_uitb_c_alv_selection=>cell ).

      DATA(lr_functions) = mr_output_alv->get_functions( ).
      lr_functions->set_default( ).
      lr_functions->set_function( zif_uitb_c_alv_functions=>filter_menu ).
      lr_functions->set_function( zif_uitb_c_alv_functions=>export_menu ).
      lr_functions->set_function( zif_uitb_c_alv_functions=>export_spreadsheet ).
      lr_functions->set_function( zif_uitb_c_alv_functions=>export_localfile ).
      lr_functions->set_function( zif_uitb_c_alv_functions=>export_html ).
      lr_functions->set_quickfilter( ).

      update_result_columns( ).
      mr_output_alv->display( ).
    ELSE.
      mr_output_alv->set_data( mr_query_result ).
      update_result_columns( ).
      mr_output_alv->refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD update_result_columns.
    DATA(lr_columns) = mr_output_alv->get_columns( ).
    CHECK lr_columns IS BOUND.

*.. Update column descriptions
    LOOP AT ms_query_result-columns ASSIGNING FIELD-SYMBOL(<ls_column>).
      DATA(lr_column) = lr_columns->get_column( |{ <ls_column>-metadata-name }| ).

      lr_column->set_descriptions( iv_long = |{ <ls_column>-metadata-name }| ).
      lr_column->set_tooltip( |{ <ls_column>-metadata-description }| ).
    ENDLOOP.

    lr_columns->set_optimized( ).
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    mr_view->show( ).
  ENDMETHOD.
ENDCLASS.
