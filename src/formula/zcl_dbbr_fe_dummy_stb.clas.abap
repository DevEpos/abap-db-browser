CLASS zcl_dbbr_fe_dummy_stb DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_stmnt_string_builder.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_dbbr_fe_dummy_stb IMPLEMENTATION.
  METHOD zif_dbbr_stmnt_string_builder~build_string.
    " nothing to do
  ENDMETHOD.
ENDCLASS.
