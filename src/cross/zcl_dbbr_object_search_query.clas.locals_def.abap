*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_exclusion_helper DEFINITION.

  PUBLIC SECTION.
    "! Removes the exclusion strings from the given value
    CLASS-METHODS remove_exclusion_string
      CHANGING
        cv_value TYPE string
        cv_sign  TYPE ddsign OPTIONAL.
ENDCLASS.
CLASS lcl_query_option_validator DEFINITION
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
        VALUE(rr_validator) TYPE REF TO lcl_query_option_validator.
ENDCLASS.

CLASS lcl_qov_cds DEFINITION
INHERITING FROM lcl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.

ENDCLASS.

CLASS lcl_qov_database_tab_view DEFINITION
INHERITING FROM lcl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.
ENDCLASS.

CLASS lcl_qov_query DEFINITION
INHERITING FROM lcl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.

ENDCLASS.

CLASS lcl_qov_package DEFINITION
INHERITING FROM lcl_query_option_validator.
  PUBLIC SECTION.
    METHODS: validate REDEFINITION.

ENDCLASS.
