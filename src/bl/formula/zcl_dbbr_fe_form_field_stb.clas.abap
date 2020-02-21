"! <p class="shorttext synchronized" lang="en">Statement builder for calculation field</p>
CLASS zcl_dbbr_fe_form_field_stb DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_stmnt_string_builder .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_dbbr_fe_form_field_stb IMPLEMENTATION.

  METHOD zif_dbbr_stmnt_string_builder~build_string.

    " $DEF <formula field> TYPE <type name>.
    cs_statement-stringform =
       |DATA { cs_statement-tokens[ 2 ]-str } TYPE { cs_statement-tokens[ 4 ]-str }.|.

  ENDMETHOD.

ENDCLASS.
