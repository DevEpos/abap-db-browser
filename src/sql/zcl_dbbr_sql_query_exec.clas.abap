CLASS zcl_dbbr_sql_query_exec DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-EVENTS:
      "! <p class="shorttext synchronized" lang="en">Is called after asynchronous query finishes</p>
      query_finished
        EXPORTING
          VALUE(et_data_info) TYPE zdbbr_dp_col_metadata_t
          VALUE(ev_execution_time) TYPE string
          VALUE(ev_message) TYPE string
          VALUE(ev_message_type) TYPE sy-msgty
          VALUE(ev_line_count) TYPE zdbbr_no_of_lines
          VALUE(er_data) TYPE REF TO data.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Execute the entered query and display the results</p>
      execute_query
        IMPORTING
          io_query              TYPE REF TO zcl_dbbr_sql_query
          iv_row_count          TYPE i DEFAULT 100
          if_count_only         TYPE abap_bool OPTIONAL
          if_show_progress_text TYPE abap_bool DEFAULT abap_true
        EXPORTING
          et_data_info          TYPE zdbbr_dp_col_metadata_t
          ev_execution_time     TYPE string
          ev_message            TYPE string
          ev_message_type       TYPE sy-msgty
          ev_line_count         TYPE zdbbr_no_of_lines
          er_data               TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en">Execute the entered query asynchronoulsy</p>
      execute_query_async
        IMPORTING
          io_query              TYPE REF TO zcl_dbbr_sql_query
          iv_row_count          TYPE i DEFAULT 100
          if_count_only         TYPE abap_bool OPTIONAL
          if_show_progress_text TYPE abap_bool DEFAULT abap_true,
      "! <p class="shorttext synchronized" lang="en">Retrieves single column value from result</p>
      get_single_value_from_result
        IMPORTING
          it_result_info TYPE zdbbr_dp_col_metadata_t
          ir_t_data      TYPE REF TO data
        EXPORTING
          ev_value       TYPE any.
  PROTECTED SECTION.
    CLASS-METHODS:
      raise_query_finished
        IMPORTING
          it_data_info      TYPE zdbbr_dp_col_metadata_t
          iv_execution_time TYPE string
          iv_message        TYPE string
          iv_message_type   TYPE sy-msgty
          iv_line_count     TYPE zdbbr_no_of_lines
          ir_data           TYPE REF TO data.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_dbbr_sql_query_exec IMPLEMENTATION.

  METHOD execute_query.
    DATA: lt_table        TYPE STANDARD TABLE OF string,
          ls_check_result TYPE zdbbr_dp_check_result.

    DATA(lo_executor) = NEW lcl_query_executor(
      io_query              = io_query
      iv_row_count          = iv_row_count
      if_count_only         = if_count_only
      if_show_progress_text = if_show_progress_text ).
    lo_executor->execute_query(
      IMPORTING
        et_data_info      = et_data_info
        ev_message        = ev_message
        ev_message_type   = ev_message_type
        ev_execution_time = ev_execution_time
        ev_line_count     = ev_line_count
        er_data           = er_data ).
  ENDMETHOD.


  METHOD execute_query_async.
    DATA(lo_executor) = NEW lcl_query_async_executor(
      io_query              = io_query
      iv_row_count          = iv_row_count
      if_count_only         = if_count_only
      if_show_progress_text = if_show_progress_text ).
    lo_executor->execute_query( ).
  ENDMETHOD.


  METHOD get_single_value_from_result.
    FIELD-SYMBOLS: <lt_result>      TYPE table,
                   <la_result_line> TYPE any.

    ASSIGN ir_t_data->* TO <lt_result>.
    IF sy-subrc <> 0 OR lines( <lt_result> ) > 1.
      RETURN.
    ENDIF.

    ASSIGN <lt_result>[ 1 ] TO <la_result_line>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DESCRIBE FIELD <la_result_line> TYPE DATA(lv_line_type) COMPONENTS DATA(lv_comp_count).
    IF lv_line_type = 'u' AND lv_comp_count = 1.
      ASSIGN COMPONENT 1 OF STRUCTURE <la_result_line> TO FIELD-SYMBOL(<lv_value>).
      TRY.
          ev_value = CONV #( <lv_value> ).
        CATCH cx_conversion_failed.
      ENDTRY.
    ELSEIF lv_line_type NA 'vh'.
      ev_value = CONV #( <la_result_line> ).
    ENDIF.

  ENDMETHOD.


  METHOD raise_query_finished.
    RAISE EVENT query_finished
      EXPORTING
        et_data_info      = it_data_info
        ev_execution_time = iv_execution_time
        ev_message        = iv_message
        ev_message_type   = iv_message_type
        ev_line_count     = iv_line_count
        er_data           = ir_data.
  ENDMETHOD.

ENDCLASS.
