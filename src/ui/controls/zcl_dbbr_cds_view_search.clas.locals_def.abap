*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_sql_select DEFINITION DEFERRED.
CLASS lcl_sql_from DEFINITION DEFERRED.
CLASS lcl_sql_condition DEFINITION DEFERRED.
CLASS lcl_sql_group_by DEFINITION DEFERRED.
CLASS lcl_sql_order_by DEFINITION DEFERRED.

CLASS lcl_sql_query DEFINITION.
  PUBLIC SECTION.
    METHODS get_select
      RETURNING
        VALUE(result) TYPE REF TO lcl_sql_select.
    METHODS get_from
      RETURNING
        VALUE(result) TYPE REF TO lcl_sql_from.
    METHODS get_condition
      RETURNING
        VALUE(result) TYPE REF TO lcl_sql_condition.
    METHODS get_group_by
      RETURNING
        VALUE(result) TYPE REF TO lcl_sql_group_by.
    METHODS get_order_by
      RETURNING
        VALUE(result) TYPE REF TO lcl_sql_order_by.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mr_sql_select    TYPE REF TO lcl_sql_select,
      mr_sql_from      TYPE REF TO lcl_sql_from,
      mr_sql_condition TYPE REF TO lcl_sql_condition,
      mr_sql_group_by  TYPE REF TO lcl_sql_group_by,
      mr_sql_order_by  TYPE REF TO lcl_sql_order_by.
ENDCLASS.

CLASS lcl_sql_select DEFINITION.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_sql_from DEFINITION.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_sql_condition DEFINITION.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_sql_group_by DEFINITION.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_sql_order_by DEFINITION.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
