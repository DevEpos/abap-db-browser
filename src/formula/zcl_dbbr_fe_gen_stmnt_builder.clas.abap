"! <p class="shorttext synchronized" lang="en">Generic statement builder</p>
CLASS zcl_dbbr_fe_gen_stmnt_builder DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_stmnt_string_builder .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_gen_stmnt_builder IMPLEMENTATION.


  METHOD zif_dbbr_stmnt_string_builder~build_string.

    DATA: lv_stringform            TYPE string,
          lv_stringform_subroutine TYPE string,
          lv_translated_token      TYPE string,
          lv_old_row               TYPE sy-tabix.

    LOOP AT cs_statement-tokens ASSIGNING FIELD-SYMBOL(<ls_token>).
      DATA(lv_token) =  <ls_token>-str.
      IF lv_token = '|'.
        lv_stringform = |{ lv_stringform }{ lv_token }|.
        lv_stringform_subroutine = |{ lv_stringform_subroutine }{ lv_token }|.
      ELSE.
        lv_stringform = |{ lv_stringform } { lv_token }|.

        IF <ls_token>-is_formula_field = abap_true.
          lv_token = |<{ zif_dbbr_c_global=>c_formula_alias }_{ lv_token }>|.
        ELSEIF <ls_token>-is_row_field = abap_true.
          lv_token = |<{ zcl_dbbr_formula_helper=>get_raw_row_field( lv_token ) }>|.
        ENDIF.

        IF lv_old_row = 0 OR
           lv_old_row = <ls_token>-row.
          lv_stringform_subroutine = |{ lv_stringform_subroutine } { lv_token }|.
        ELSE.
          lv_stringform_subroutine = |{ lv_stringform_subroutine }{ cl_abap_char_utilities=>cr_lf }    { lv_token }|.
        ENDIF.
      ENDIF.

      lv_old_row = <ls_token>-row.
    ENDLOOP.

    " close string with termination sign
    cs_statement-stringform = |{ lv_stringform }{ cs_statement-terminator }|.
    cs_statement-stringform_subroutine = |{ lv_stringform_subroutine }{ cs_statement-terminator }|.

  ENDMETHOD.
ENDCLASS.
