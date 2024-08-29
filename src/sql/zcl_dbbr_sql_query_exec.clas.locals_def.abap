*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_query_executor_base DEFINITION
  ABSTRACT.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        if_count_only         TYPE abap_bool
        if_show_progress_text TYPE abap_bool
        iv_row_count          TYPE i
        io_query              TYPE REF TO zcl_dbbr_sql_query.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_tab_field_type,
        typekind TYPE typekind,
        length   TYPE i,
        decimals TYPE i,
        type_ref TYPE REF TO cl_abap_elemdescr,
      END OF ty_s_tab_field_type,
      ty_t_tab_field_type TYPE HASHED TABLE OF ty_s_tab_field_type WITH UNIQUE KEY typekind length decimals.

    CONSTANTS c_dec_types_table TYPE tabname VALUE 'DDDDLDECTYPES'.

    "! <p class="shorttext synchronized">Query Result</p>
    DATA ms_query_result TYPE zdbbr_dp_table_data.
    DATA mr_query_result TYPE REF TO data.
    DATA mf_count_only TYPE abap_bool.
    DATA mf_show_progress_text TYPE abap_bool.
    DATA mo_query TYPE REF TO zcl_dbbr_sql_query.
    DATA mt_tab_field_types TYPE ty_t_tab_field_type.
    DATA mv_row_count TYPE i.

    METHODS process_query_result.

    METHODS get_fallback_type
      IMPORTING
        iv_type_kind  TYPE typekind
        iv_length     TYPE i OPTIONAL
        iv_decimals   TYPE i OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_elemdescr.
ENDCLASS.


CLASS lcl_query_executor DEFINITION
  INHERITING FROM lcl_query_executor_base.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Execute the entered query and display the results</p>
    METHODS execute_query
      EXPORTING
        et_data_info      TYPE zdbbr_dp_col_metadata_t
        ev_execution_time TYPE string
        ev_message        TYPE string
        ev_message_type   TYPE sy-msgty
        ev_line_count     TYPE zdbbr_no_of_lines
        er_data           TYPE REF TO data.

    "! <p class="shorttext synchronized">Will be called upon finished query execution</p>
    METHODS execute_query_finished
      IMPORTING
        p_task TYPE clike.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Completion Check for asynchronous syntax check</p>
    DATA mf_async_finished TYPE abap_bool.
ENDCLASS.


CLASS lcl_query_async_executor DEFINITION
  INHERITING FROM lcl_query_executor_base.

  PUBLIC SECTION.
    METHODS execute_query.

    "! <p class="shorttext synchronized">Will be called upon finished query execution</p>
    METHODS execute_query_finished
      IMPORTING
        p_task TYPE clike.
ENDCLASS.


CLASS zcl_dbbr_sql_query_exec DEFINITION LOCAL FRIENDS lcl_query_async_executor.
