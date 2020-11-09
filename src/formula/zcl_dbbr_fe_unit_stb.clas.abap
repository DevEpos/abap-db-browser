"! <p class="shorttext synchronized" lang="en">Statment builder for UNIT field</p>
CLASS zcl_dbbr_fe_unit_stb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_stmnt_string_builder.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_unit_stb IMPLEMENTATION.

  METHOD zif_dbbr_stmnt_string_builder~build_string.

    " $UNIT <formula field> <row component>.
    cs_statement-stringform =
       |DATA { cs_statement-tokens[ 2 ]-str }u LIKE { cs_statement-tokens[ 3 ]-str }.|.

  ENDMETHOD.

ENDCLASS.
