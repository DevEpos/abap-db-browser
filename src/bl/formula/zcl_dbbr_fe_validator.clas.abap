CLASS zcl_dbbr_fe_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_formula   TYPE string
        !ir_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS validate
      EXPORTING
        !et_defined_fields TYPE zif_dbbr_fe_types=>tt_form_field
      RETURNING
        VALUE(rr_formula)  TYPE REF TO zcl_dbbr_formula
      RAISING
        zcx_dbbr_formula_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mt_formula_lines TYPE string_table .
    DATA mv_formula_string TYPE string .
    DATA mt_formula_stmnt TYPE zif_dbbr_fe_types=>tt_statement .
    DATA mr_formula TYPE REF TO zcl_dbbr_formula .
    DATA mt_calc_fields TYPE zif_dbbr_fe_types=>tt_form_calc_field .

    METHODS tokenize_formula
      RAISING
        zcx_dbbr_fe_stmnt_valid_exc .
    METHODS check_abap_syntax
      RAISING
        zcx_dbbr_formula_exception .
    METHODS process_statements
      RAISING
        zcx_dbbr_formula_exception .
    METHODS get_statements_from_code
      IMPORTING
        !it_keywords      TYPE char20_t OPTIONAL
      EXPORTING
        !et_formula_lines TYPE zif_dbbr_fe_types=>tt_statement
      RAISING
        zcx_dbbr_fe_stmnt_valid_exc .
ENDCLASS.



CLASS zcl_dbbr_fe_validator IMPLEMENTATION.


  METHOD check_abap_syntax.
    DATA: lv_line TYPE i,
          lv_word TYPE string.

    " include formula lines
    DATA(lr_builder) = zcl_dbbr_fe_form_builder=>get_builder_for_check(
        ir_formula   = mr_formula
        ir_tabfields = mr_tabfields
    ).

    lr_builder->build_formula( IMPORTING et_lines         = DATA(lt_code)
                                         ev_starting_line = DATA(lv_starting_line) ).

    SELECT SINGLE * FROM trdir
    INTO @DATA(dir)
    WHERE name = @sy-repid.

    DATA: lv_message TYPE string.

    SYNTAX-CHECK FOR lt_code MESSAGE lv_message
                                LINE lv_line
                                WORD lv_word
                                DIRECTORY ENTRY dir.

    IF lv_message IS NOT INITIAL AND sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
        EXPORTING
*          invalid_row    = lv_line - lv_starting_line
          syntax_message = lv_message.
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
*                                      with COMMENTS.

    IF lv_message IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_dbbr_fe_stmnt_valid_exc
        EXPORTING
          textid      = zcx_dbbr_fe_stmnt_valid_exc=>error_at_scan_source
          msgv1       = |{ lv_message }|
          msgv2       = |{ lv_word }|
          msgv3       = |{ lv_line }|
          invalid_row = lv_line.
    ENDIF.

    LOOP AT lt_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>) WHERE type <> 'P'. " exclude comments from further processing
      APPEND CORRESPONDING #( <ls_stmnt> ) TO et_formula_lines ASSIGNING FIELD-SYMBOL(<ls_new_statement>).

      DATA(lv_token_id) = 1.

      LOOP AT lt_tokes ASSIGNING FIELD-SYMBOL(<ls_tokes>) FROM <ls_stmnt>-from TO <ls_stmnt>-to.
        DATA(ls_token) = CORRESPONDING zif_dbbr_fe_types=>ty_token( <ls_tokes> ).
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
          NEW zcl_dbbr_fe_custom_sv(
          )->zif_dbbr_statement_validator~validate( CHANGING cs_statement = <ls_stmnt> ).

          " Extract possible formula field (custom editor functions will be excluded)
          IF <ls_stmnt>-is_function_call = abap_false.
            DATA(lr_field_extractor) = zcl_dbbr_fe_field_extractor=>get_extractor( <ls_stmnt>-first_token_str ).
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
          NEW zcl_dbbr_fe_general_sv( mr_formula )->zif_dbbr_statement_validator~validate( CHANGING cs_statement = <ls_stmnt> ).
      ENDCASE.

      " build the stringform of the statement
      DATA(lr_string_builder) = zcl_dbbr_fe_stmnt_str_builder=>get_string_builder( <ls_stmnt> ).
      lr_string_builder->build_string( CHANGING cs_statement = <ls_stmnt> ).

      mr_formula->add_stmnt( <ls_stmnt> ).
    ENDLOOP.

    " check if there is at least one formula field
    IF NOT mr_formula->has_executable_code( ).
      RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
        EXPORTING
          textid = zcx_dbbr_formula_exception=>no_executable_lines.
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
      CATCH zcx_dbbr_formula_exception INTO DATA(lr_appl_exc).
        CLEAR mr_formula.
        RAISE EXCEPTION lr_appl_exc.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
