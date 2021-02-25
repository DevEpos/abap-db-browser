CLASS zcl_dbbr_sql_query_exec DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Execute the entered query and display the results</p>
    CLASS-METHODS execute_query
      IMPORTING
        io_query          TYPE REF TO zcl_dbbr_sql_query
        iv_row_count      TYPE i DEFAULT 100
        if_count_only     TYPE abap_bool OPTIONAL
      EXPORTING
        et_data_info      TYPE zdbbr_dp_col_metadata_t
        ev_execution_time TYPE string
        ev_message        TYPE string
        ev_message_type   TYPE sy-msgty
        ev_line_count     TYPE zdbbr_no_of_lines
        er_data           TYPE REF TO data.

    "! <p class="shorttext synchronized" lang="en">Retrieves single column value from result</p>
    CLASS-METHODS get_single_value_from_result
      IMPORTING
        it_result_info TYPE zdbbr_dp_col_metadata_t
        ir_t_data      TYPE REF TO data
      EXPORTING
        ev_value       TYPE any.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_dbbr_sql_query_exec IMPLEMENTATION.

  METHOD execute_query.
    DATA: lt_table        TYPE STANDARD TABLE OF string,
          ls_check_result TYPE zdbbr_dp_check_result.

    DATA(lo_executor) = NEW lcl_executor(
      io_query      = io_query
      iv_row_count  = iv_row_count
      if_count_only = if_count_only
    ).
    lo_executor->execute_query(
      IMPORTING
        et_data_info      = et_data_info
        ev_message        = ev_message
        ev_message_type   = ev_message_type
        ev_execution_time = ev_execution_time
        ev_line_count     = ev_line_count
        er_data           = er_data
    ).
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

    DESCRIBE FIELD <la_result_line> TYPE DATA(lv_line_type).
    IF lv_line_type CA 'uvh'.
      RETURN.
    ENDIF.

    WRITE <la_result_line> TO ev_value.

  ENDMETHOD.

ENDCLASS.
