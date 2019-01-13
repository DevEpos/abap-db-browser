CLASS zcl_dbbr_sql_query_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_queries TYPE TABLE OF zdbbr_sqlquery WITH EMPTY KEY .

    METHODS get_favorites
      RETURNING
        VALUE(result) TYPE tt_queries .
    METHODS save_favorite
      IMPORTING
        !is_query    TYPE zdbbr_sqlquery
      RETURNING
        VALUE(rv_id) TYPE zdbbr_sql_query_id .
    METHODS delete_favorite
      IMPORTING
        !iv_query_id TYPE zdbbr_sql_query_id .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_SQL_QUERY_FACTORY IMPLEMENTATION.


  METHOD delete_favorite.
    DELETE FROM zdbbr_sqlquery WHERE query_id = iv_query_id.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD get_favorites.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE @result
      FROM zdbbr_sqlquery
      WHERE created_by = @sy-uname.
  ENDMETHOD.


  METHOD save_favorite.
    DATA(ls_query) = is_query.
    ls_query-query_id = zcl_dbbr_system_helper=>create_guid_22( ).

    INSERT zdbbr_sqlquery FROM ls_query.
    IF sy-subrc = 0.
      COMMIT WORK.
      rv_id = ls_query-query_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
