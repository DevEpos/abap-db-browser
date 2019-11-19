"! <p class="shorttext synchronized" lang="en">Open SQL Console</p>
CLASS zcl_dbbr_sql_console DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_sql_query_editor
  CREATE PUBLIC .

  PUBLIC SECTION.
  methods constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_sql_console IMPLEMENTATION.
  METHOD constructor.

    super->constructor(  ).
    mf_standalone_mode = abap_true.
    mf_title = |{ 'DB Browser - SQL Console'(001) }|.
  ENDMETHOD.

ENDCLASS.
