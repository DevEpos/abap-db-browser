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
        er_data           TYPE REF TO data.
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
        ev_execution_time = ev_execution_time
        er_data           = er_data
    ).
  ENDMETHOD.


ENDCLASS.
