CLASS ZCL_DBBR_sql_query DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.
    CLASS-METHODS create_query
      IMPORTING
        ir_table_fields TYPE REF TO ZCL_DBBR_tablefield_list
        it_selection    TYPE ZDBBR_selfield_info_itab OPTIONAL
        it_selection_or TYPE ZDBBR_or_seltab_itab OPTIONAL.
    METHODS execute_single
      EXPORTING
        ev_result TYPE any.
    METHODS execute
      IMPORTING
        if_determine_max_count TYPE boolean OPTIONAL
      EXPORTING
        er_result              TYPE REF TO data.
    METHODS determine_line_count
      RETURNING
        VALUE(rv_count) TYPE sy-tabix.
  PROTECTED SECTION.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mt_select TYPE STANDARD TABLE OF string.
    DATA mt_from TYPE STANDARD TABLE OF string.
    DATA mt_where TYPE STANDARD TABLE OF string.
    DATA mt_group_by TYPE STANDARD TABLE OF string.
    DATA mt_order_by TYPE STANDARD TABLE OF string.

    METHODS create_select_part.
    METHODS create_where_part.
    METHODS create_group_by_part.
    METHODS create_order_by_part.
    METHODS create_from_part.
ENDCLASS.



CLASS ZCL_DBBR_sql_query IMPLEMENTATION.
  METHOD create_query.

  ENDMETHOD.

  METHOD constructor.

  ENDMETHOD.

  METHOD create_select_part.

  ENDMETHOD.

  METHOD create_where_part.

  ENDMETHOD.

  METHOD create_group_by_part.

  ENDMETHOD.

  METHOD create_order_by_part.

  ENDMETHOD.

  METHOD create_from_part.

  ENDMETHOD.

  METHOD execute_single.

  ENDMETHOD.

  METHOD execute.

  ENDMETHOD.

  METHOD determine_line_count.

  ENDMETHOD.

ENDCLASS.
