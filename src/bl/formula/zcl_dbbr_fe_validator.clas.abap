class ZCL_DBBR_FE_VALIDATOR definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_FORMULA type STRING
      !IR_TABFIELDS type ref to ZCL_DBBR_TABFIELD_LIST .
  methods VALIDATE
    exporting
      !ET_DEFINED_FIELDS type ZIF_DBBR_FE_TYPES=>TT_FORM_FIELD
    returning
      value(RR_FORMULA) type ref to ZCL_DBBR_FORMULA
    raising
      ZCX_DBBR_FORMULA_EXCEPTION .
  PROTECTED SECTION.
private section.

  data MR_TABFIELDS type ref to ZCL_DBBR_TABFIELD_LIST .
  data MT_FORMULA_LINES type STRING_TABLE .
  data MV_FORMULA_STRING type STRING .
  data MT_FORMULA_STMNT type ZIF_DBBR_FE_TYPES=>TT_STATEMENT .
  data MR_FORMULA type ref to ZCL_DBBR_FORMULA .
  data MT_CALC_FIELDS type ZIF_DBBR_FE_TYPES=>TT_FORM_CALC_FIELD .

  methods TOKENIZE_FORMULA
    raising
      ZCX_DBBR_FE_STMNT_VALID_EXC .
  methods CHECK_ABAP_SYNTAX
    raising
      ZCX_DBBR_FORMULA_EXCEPTION .
  methods PROCESS_STATEMENTS
    raising
      ZCX_DBBR_FORMULA_EXCEPTION .
  methods GET_STATEMENTS_FROM_CODE
    importing
      !IT_KEYWORDS type CHAR20_T optional
    exporting
      !ET_FORMULA_LINES type ZIF_DBBR_FE_TYPES=>TT_STATEMENT
    raising
      ZCX_DBBR_FE_STMNT_VALID_EXC .
ENDCLASS.



CLASS ZCL_DBBR_FE_VALIDATOR IMPLEMENTATION.


  METHOD check_abap_syntax.
    DATA: lv_line TYPE i,
          lv_word TYPE string.

    " include formula lines
    DATA(lr_builder) = ZCL_DBBR_fe_form_builder=>get_builder_for_check(
        ir_formula   = mr_formula
        ir_tabfields = mr_tabfields
    ).

    lr_builder->build_formula( IMPORTING et_lines = DATA(lt_code) ).

    SELECT SINGLE * FROM trdir
    INTO @DATA(dir)
    WHERE name = @sy-repid.

    DATA: lv_message TYPE string.

    SYNTAX-CHECK FOR lt_code MESSAGE lv_message
                                LINE lv_line
                                WORD lv_word
                                DIRECTORY ENTRY dir.

    IF lv_message IS NOT INITIAL AND sy-subrc <> 0.
      ZCL_DBBR_appl_util=>split_string_for_message(
        EXPORTING iv_string = lv_message
        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                  ev_msgv2  = DATA(lv_msgv2)
                  ev_msgv3  = DATA(lv_msgv3)
                  ev_msgv4  = DATA(lv_msgv4)
      ).

      RAISE EXCEPTION TYPE ZCX_DBBR_formula_exception
        EXPORTING
          textid = ZCX_DBBR_formula_exception=>general_error
          msgv1  = lv_msgv1
          msgv2  = lv_msgv2
          msgv3  = lv_msgv3
          msgv4  = lv_msgv4.
    ENDIF.

    "<< subroutine pool was generated without error >>
  ENDMETHOD.


  METHOD constructor.
    mv_formula_string = iv_formula.
    mr_tabfields = ir_tabfields.
  ENDMETHOD.


  METHOD get_statements_from_code.
    DATA: lt_tokes   TYPE stokes_tab,
          lv_message TYPE string,
          lv_word    TYPE char80,
          lv_line    TYPE i,
          lt_stmnt   TYPE sstmnt_tab.

    SCAN ABAP-SOURCE mt_formula_lines TOKENS INTO     lt_tokes
                                      STATEMENTS INTO lt_stmnt
                                      MESSAGE INTO    lv_message
                                      WORD INTO       lv_word
                                      LINE INTO       lv_line.

    IF lv_message IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_DBBR_fe_stmnt_valid_exc
        EXPORTING
          textid      = ZCX_DBBR_fe_stmnt_valid_exc=>error_at_scan_source
          msgv1       = |{ lv_message }|
          msgv2       = |{ lv_word }|
          msgv3       = |{ lv_line }|
          invalid_row = lv_line.
    ENDIF.

    LOOP AT lt_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>) WHERE type <> 'P'. " exclude comments from further processing
      APPEND CORRESPONDING #( <ls_stmnt> ) TO et_formula_lines ASSIGNING FIELD-SYMBOL(<ls_new_statement>).

      DATA(lv_token_id) = 1.

      LOOP AT lt_tokes ASSIGNING FIELD-SYMBOL(<ls_tokes>) FROM <ls_stmnt>-from TO <ls_stmnt>-to.
        DATA(ls_token) = CORRESPONDING ZIF_DBBR_fe_types=>ty_token( <ls_tokes> ).
        ls_token-id = lv_token_id.

        <ls_new_statement>-tokens = VALUE #( BASE <ls_new_statement>-tokens ( ls_token ) ).
        ADD 1 TO lv_token_id.
      ENDLOOP.

      <ls_new_statement>-token_count = lines( <ls_new_statement>-tokens ).
    ENDLOOP.

  ENDMETHOD.


  METHOD process_statements.
    LOOP AT mt_formula_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>).

      <ls_stmnt>-first_token_str = <ls_stmnt>-tokens[ 1 ]-str.
      IF <ls_stmnt>-first_token_str CP '$*'.
        <ls_stmnt>-type = 'U'.
      ENDIF.

      CASE <ls_stmnt>-type.

        WHEN 'U'. " custom / unknown stmnt so formula field is possible
          NEW ZCL_DBBR_fe_custom_sv(
          )->ZIF_DBBR_statement_validator~validate( CHANGING cs_statement = <ls_stmnt> ).

          " Extract possible formula field (custom editor functions will be excluded)
          IF <ls_stmnt>-is_function_call = abap_false.
            DATA(lr_field_extractor) = ZCL_DBBR_fe_field_extractor=>get_extractor( <ls_stmnt>-first_token_str ).
            IF lr_field_extractor IS BOUND.
              mr_formula->add_field( lr_field_extractor->extract_field( <ls_stmnt> ) ).
            ENDIF.
          ELSE.
            IF <ls_stmnt>-first_token_str = zif_dbbr_c_fe_keywords=>set_cell_color OR
               <ls_stmnt>-first_token_str = zif_dbbr_c_fe_keywords=>set_row_color.
              mr_formula->set_color_column_needed( EXPORTING value = abap_true ).
            ENDIF.
          ENDIF.

        WHEN 'C' OR " Compute statements
             'K'.   " ABAP Keyword
          NEW ZCL_DBBR_fe_general_sv( mr_formula )->ZIF_DBBR_statement_validator~validate( CHANGING cs_statement = <ls_stmnt> ).
      ENDCASE.

      " build the stringform of the statement
      DATA(lr_string_builder) = ZCL_DBBR_fe_stmnt_str_builder=>get_string_builder( <ls_stmnt> ).
      lr_string_builder->build_string( CHANGING cs_statement = <ls_stmnt> ).

      mr_formula->add_stmnt( <ls_stmnt> ).
    ENDLOOP.

    " check if there is at least one formula field
    IF NOT mr_formula->has_executable_code( ).
      RAISE EXCEPTION TYPE ZCX_DBBR_formula_exception
        EXPORTING
          textid = ZCX_DBBR_formula_exception=>no_executable_lines.
    ENDIF.

  ENDMETHOD.


  METHOD tokenize_formula.
    " remove line breaks
    SPLIT mv_formula_string AT cl_abap_char_utilities=>cr_lf INTO TABLE mt_formula_lines.

    get_statements_from_code( IMPORTING et_formula_lines = mt_formula_stmnt ).

    CLEAR mt_formula_lines.

  ENDMETHOD.


  METHOD validate.
    TRY.
        CLEAR: mt_formula_lines,
               mt_formula_stmnt.

        mr_formula = NEW #( iv_formula   = mv_formula_string ).

        tokenize_formula( ).
        process_statements( ).
        check_abap_syntax( ).
        rr_formula = mr_formula.
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_appl_exc).
        clear mr_formula.
        RAISE EXCEPTION lr_appl_exc.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
