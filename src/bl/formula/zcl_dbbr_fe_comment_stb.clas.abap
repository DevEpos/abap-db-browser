"! <p class="shorttext synchronized" lang="en">String Builder for Formula Comments</p>
CLASS zcl_dbbr_fe_comment_stb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_stmnt_string_builder .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_comment_stb IMPLEMENTATION.
  METHOD zif_dbbr_stmnt_string_builder~build_string.
    LOOP AT cs_statement-tokens ASSIGNING FIELD-SYMBOL(<ls_token>).
      cs_statement-stringform = |{ cs_statement-stringform }{ <ls_token>-str }|.
      IF sy-tabix <> lines( cs_statement-tokens ).
        cs_statement-stringform = |{ cs_statement-stringform }{ cl_abap_char_utilities=>cr_lf }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
