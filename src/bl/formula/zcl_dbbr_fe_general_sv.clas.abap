class ZCL_DBBR_FE_GENERAL_SV definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_STATEMENT_VALIDATOR .

  methods CONSTRUCTOR
    importing
      !IR_FORMULA type ref to ZCL_DBBR_FORMULA .
protected section.
private section.

  data MR_FORMULA type ref to ZCL_DBBR_FORMULA .
ENDCLASS.



CLASS ZCL_DBBR_FE_GENERAL_SV IMPLEMENTATION.


  method CONSTRUCTOR.

    mr_formula = ir_formula.

  endmethod.


  method ZIF_DBBR_STATEMENT_VALIDATOR~VALIDATE.


    IF cs_statement-type = 'K'.
      " check if keyword is valid
      IF NOT cs_statement-first_token_str IN ZCL_DBBR_fe_templates=>st_valid_keywords_range.
        RAISE EXCEPTION TYPE ZCX_DBBR_fe_stmnt_valid_exc
          EXPORTING
            textid          = ZCX_DBBR_fe_stmnt_valid_exc=>invalid_keyword_found
            msgv1           = |{ cs_statement-first_token_str }|
            msgv2           = |{ cs_statement-tokens[ 1 ]-row }|
            invalid_row     = cs_statement-tokens[ 1 ]-row.
      ENDIF.
    ENDIF.

    LOOP AT cs_statement-tokens ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE type = 'I'.
      IF <ls_token>-str CP 'ROW-*'.
        <ls_token>-is_row_field = abap_true.
      ELSE.
        <ls_token>-is_formula_field = mr_formula->is_formula_field( |{ <ls_token>-str }| ).
      ENDIF.
    ENDLOOP.

  endmethod.
ENDCLASS.
