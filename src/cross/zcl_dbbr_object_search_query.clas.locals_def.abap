*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS cl_query_option_validator DEFINITION
ABSTRACT.

  PUBLIC SECTION.
    "! Validates the given option value
    METHODS validate
      IMPORTING
        iv_option TYPE string
        iv_value  TYPE string
      RAISING
        zcx_dbbr_object_search.

    "! Create validator reference regarding the search type
    CLASS-METHODS create_validator
      IMPORTING
        iv_type             TYPE zdbbr_obj_browser_mode
      RETURNING
        VALUE(rr_validator) TYPE REF TO cl_query_option_validator.
ENDCLASS.

CLASS cl_qov_cds DEFINITION
INHERITING FROM cl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.

ENDCLASS.

CLASS cl_qov_database_tab_view DEFINITION
INHERITING FROM cl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.
ENDCLASS.

CLASS cl_qov_query DEFINITION
INHERITING FROM cl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.

ENDCLASS.

CLASS cl_qov_package DEFINITION
INHERITING FROM cl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.

ENDCLASS.
