"! <p class="shorttext synchronized" lang="en">General validation for statement</p>
CLASS zcl_dbbr_fe_general_sv DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_statement_validator .

    METHODS constructor
      IMPORTING
        !ir_formula TYPE REF TO zcl_dbbr_formula .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_formula TYPE REF TO zcl_dbbr_formula .
ENDCLASS.



CLASS zcl_dbbr_fe_general_sv IMPLEMENTATION.


  METHOD constructor.

    mr_formula = ir_formula.

  ENDMETHOD.


  METHOD zif_dbbr_statement_validator~validate.

    IF cs_statement-type = 'K'.
      " check if keyword is valid
      IF NOT cs_statement-first_token_str IN zcl_dbbr_fe_templates=>gt_valid_keywords_range.
        RAISE EXCEPTION TYPE zcx_dbbr_fe_stmnt_valid_exc
          EXPORTING
            textid      = zcx_dbbr_fe_stmnt_valid_exc=>invalid_keyword_found
            msgv1       = |{ cs_statement-first_token_str }|
            msgv2       = |{ cs_statement-tokens[ 1 ]-row }|
            invalid_row = cs_statement-tokens[ 1 ]-row.
      ENDIF.
    ENDIF.

    LOOP AT cs_statement-tokens ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE type = 'I'.
      IF <ls_token>-str CP 'ROW-*'.
        <ls_token>-is_row_field = abap_true.
      ELSE.
        <ls_token>-is_formula_field = mr_formula->is_formula_field( |{ <ls_token>-str }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
