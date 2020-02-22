"! <p class="shorttext synchronized" lang="en">Open SQL Console</p>
CLASS zcl_dbbr_sql_console DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_sql_query_editor
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_sqlquery_export_mem_id TYPE c LENGTH 15 VALUE 'SQLQUERY_EXPORT' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        iv_query TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_sql_console IMPLEMENTATION.
  METHOD constructor.

    super->constructor( iv_query = iv_query ).
    mf_standalone_mode = abap_true.
    mf_title = |{ 'DB Browser - SQL Console'(001) }|.

  ENDMETHOD.

ENDCLASS.
