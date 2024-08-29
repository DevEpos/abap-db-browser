CLASS zcl_dbbr_fe_icon_tt_stb DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_stmnt_string_builder.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_dbbr_fe_icon_tt_stb IMPLEMENTATION.
  METHOD zif_dbbr_stmnt_string_builder~build_string.
    cs_statement-stringform =
       |DATA { cs_statement-tokens[ 2 ]-str } TYPE { zif_dbbr_c_fe_global=>c_icon_tt_type }.|.
  ENDMETHOD.
ENDCLASS.
