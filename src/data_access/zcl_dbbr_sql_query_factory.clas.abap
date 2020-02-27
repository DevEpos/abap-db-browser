"! <p class="shorttext synchronized" lang="en">Factory for queries</p>
CLASS zcl_dbbr_sql_query_factory DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      ty_t_query_history    TYPE TABLE OF zdbbrsqlqh WITH EMPTY KEY,
      ty_t_history_id_range TYPE RANGE OF zdbbr_sql_query_history_id.

    "! <p class="shorttext synchronized" lang="en">Retrieves SQL Query history for user</p>
    "!
    CLASS-METHODS get_history_entries
      IMPORTING
        iv_max_entries    TYPE i DEFAULT 50
      RETURNING
        VALUE(rt_history) TYPE ty_t_query_history.

    "! <p class="shorttext synchronized" lang="en">Creates history for Query</p>
    "!
    CLASS-METHODS create_history_entry
      IMPORTING
        iv_query_string      TYPE string
        iv_exec_time         TYPE zdbbr_query_exec_time OPTIONAL
      RETURNING
        VALUE(rv_history_id) TYPE zdbbrsqlqh-query_history_id.

    "! <p class="shorttext synchronized" lang="en">Updates execution time of query</p>
    CLASS-METHODS update_execution_time
      IMPORTING
        iv_query_id  TYPE zdbbr_sql_query_history_id
        iv_exec_time TYPE zdbbr_query_exec_time.
    "! <p class="shorttext synchronized" lang="en">Deletes query history entries</p>
    "!
    CLASS-METHODS delete_history_entries
      IMPORTING
        it_history_entries TYPE ty_t_history_id_range OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_dbbr_sql_query_factory IMPLEMENTATION.

  METHOD get_history_entries.
    SELECT
      FROM zdbbrsqlqh
      FIELDS *
      WHERE created_by = @sy-uname
      ORDER BY created_date DESCENDING,
               created_time DESCENDING
    INTO CORRESPONDING FIELDS OF TABLE @rt_history
      UP TO @iv_max_entries ROWS.
  ENDMETHOD.

  METHOD create_history_entry.
    DATA(ls_history_entry) = VALUE zdbbrsqlqh(
        query_history_id = zcl_sat_system_helper=>create_guid_22( )
        query_string     = iv_query_string
        execution_time   = iv_exec_time
        created_by       = sy-uname
        created_date     = sy-datum
        created_time     = sy-timlo
    ).
    INSERT zdbbrsqlqh FROM ls_history_entry.
    IF sy-subrc = 0.
      COMMIT WORK.
      rv_history_id = ls_history_entry-query_history_id.
    ENDIF.
  ENDMETHOD.

  METHOD delete_history_entries.
    DELETE FROM zdbbrsqlqh WHERE query_history_id IN it_history_entries.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD update_execution_time.
    UPDATE zdbbrsqlqh SET execution_time = iv_exec_time WHERE query_history_id = iv_query_id.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
