*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_executor DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        if_count_only TYPE abap_bool
        iv_row_count  TYPE i
        io_query      TYPE REF TO zcl_dbbr_sql_query.
    "! <p class="shorttext synchronized" lang="en">Execute the entered query and display the results</p>
    METHODS execute_query
      EXPORTING
        et_data_info      TYPE zdbbr_dp_col_metadata_t
        ev_execution_time TYPE string
        er_data           TYPE REF TO data.
    "! <p class="shorttext synchronized" lang="en">Will be called upon finished query execution</p>
    METHODS execute_query_finished
      IMPORTING
        !p_task TYPE clike .
  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Query Result</p>
    DATA ms_query_result TYPE zdbbr_dp_table_data .
    "! <p class="shorttext synchronized" lang="en">Completion Check for asynchronous syntax check</p>
    DATA mf_async_finished TYPE abap_bool .
    DATA mr_query_result TYPE REF TO data.
    DATA mf_count_only TYPE abap_bool.
    DATA mo_query TYPE REF TO zcl_dbbr_sql_query.
    DATA mv_row_count TYPE i.

    METHODS process_query_result.
ENDCLASS.
