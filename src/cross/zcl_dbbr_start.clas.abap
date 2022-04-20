"! <p class="shorttext synchronized" lang="en">Starts the DB Browser</p>
CLASS zcl_dbbr_start DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor .
    "! <p class="shorttext synchronized" lang="en">Starts DB Browser</p>
    METHODS start .
    "! <p class="shorttext synchronized" lang="en">Start Central Search Function</p>
    METHODS start_search .
    "! <p class="shorttext synchronized" lang="en">Starts the SQL Query Console</p>
    METHODS start_sql_console.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_start IMPLEMENTATION.


  METHOD constructor.
    zcl_sat_system_helper=>set_locale_language( ).
  ENDMETHOD.


  METHOD start.
    CALL FUNCTION 'ZDBBR_START'.
  ENDMETHOD.


  METHOD start_search.
    NEW zcl_dbbr_object_central_search( if_new_transaction_mode = abap_true )->show( ).
  ENDMETHOD.

  METHOD start_sql_console.
    DATA: lv_query TYPE string.
    IMPORT
      query = lv_query
    FROM MEMORY ID zcl_dbbr_sql_console=>c_sqlquery_export_mem_id.
    FREE MEMORY ID zcl_dbbr_sql_console=>c_sqlquery_export_mem_id.

    NEW zcl_dbbr_sql_console( iv_query = lv_query )->show( ).
  ENDMETHOD.

ENDCLASS.
