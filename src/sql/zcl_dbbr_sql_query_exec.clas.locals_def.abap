*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_query_executor_base DEFINITION
  ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          if_count_only TYPE abap_bool
          iv_row_count  TYPE i
          io_query      TYPE REF TO zcl_dbbr_sql_query.
  PROTECTED SECTION.
    DATA:
      "! <p class="shorttext synchronized" lang="en">Query Result</p>
      ms_query_result TYPE zdbbr_dp_table_data,
      mr_query_result TYPE REF TO data,
      mf_count_only   TYPE abap_bool,
      mo_query        TYPE REF TO zcl_dbbr_sql_query,
      mv_row_count    TYPE i.

    METHODS:
      process_query_result.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_query_executor DEFINITION
  INHERITING FROM lcl_query_executor_base.
  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Execute the entered query and display the results</p>
      execute_query
        EXPORTING
          et_data_info      TYPE zdbbr_dp_col_metadata_t
          ev_execution_time TYPE string
          ev_message        TYPE string
          ev_message_type   TYPE sy-msgty
          ev_line_count     TYPE zdbbr_no_of_lines
          er_data           TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en">Will be called upon finished query execution</p>
      execute_query_finished
        IMPORTING
          p_task TYPE clike.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      "! <p class="shorttext synchronized" lang="en">Completion Check for asynchronous syntax check</p>
      mf_async_finished TYPE abap_bool.
ENDCLASS.

CLASS lcl_query_async_executor DEFINITION
  INHERITING FROM lcl_query_executor_base.
  PUBLIC SECTION.
    METHODS:
      execute_query,
      "! <p class="shorttext synchronized" lang="en">Will be called upon finished query execution</p>
      execute_query_finished
        IMPORTING
          p_task TYPE clike.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


class zcl_dbbr_sql_query_exec DEFINITION local friends lcl_query_async_executor.
