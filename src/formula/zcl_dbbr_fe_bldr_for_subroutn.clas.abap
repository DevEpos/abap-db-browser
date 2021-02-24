CLASS ZCL_DBBR_fe_bldr_for_subroutn DEFINITION
  PUBLIC
  INHERITING FROM ZCL_DBBR_fe_generic_form_bldr
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_DBBR_fe_formula_builder.

    METHODS constructor
      IMPORTING
        ir_formula    TYPE REF TO ZCL_DBBR_formula
        ir_tabfields  TYPE REF TO ZCL_DBBR_tabfield_list
        it_comp_types TYPE ZDBBR_abap_comp_type_itab.
  PRIVATE SECTION.
    DATA mr_formula   TYPE REF TO ZCL_DBBR_formula.
    DATA mr_tabfields TYPE REF TO ZCL_DBBR_tabfield_list.
    DATA mt_comp_types TYPE ZDBBR_abap_comp_type_itab.
    METHODS build_clear_form_fld_lines
      CHANGING
        ct_form TYPE string_table.
    METHODS include_row_and_form_fields
      IMPORTING
        it_stmnt TYPE ZIF_DBBR_fe_types=>tt_statement
      CHANGING
        ct_form  TYPE string_table.
ENDCLASS.



CLASS ZCL_DBBR_fe_bldr_for_subroutn IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mr_formula = ir_formula.
    mt_comp_types = it_comp_types.
    mr_tabfields = ir_tabfields.
  ENDMETHOD.

  METHOD ZIF_DBBR_fe_formula_builder~build_formula.
    et_lines = VALUE #( ( |REPORT Z_FORMULA.| )
                        ( ) ).

    " insert macro for field-symbol definition
    et_lines = VALUE #(
      BASE et_lines
      ( |DEFINE macro_row_comp.| )
      ( | ASSIGN ('ROW->&1') to FIELD-SYMBOL(<&1>).| )
      ( | CHECK sy-subrc = 0.| )
      ( |END-OF-DEFINITION.| )
      ( )
    ).


    mr_formula->get_statements( IMPORTING et_statements = DATA(lt_stmnt) ).
    et_lines = VALUE #( BASE et_lines ( |FORM { zif_dbbr_c_fe_global=>c_formula_subroutine_form } CHANGING row TYPE REF TO data.| ) ( ) ).

    " include try block to catch all kinds of exceptions during formula calculations
    et_lines = VALUE #( BASE et_lines ( |TRY.| ) ( ) ).

    include_row_and_form_fields( EXPORTING it_stmnt = lt_stmnt
                                 CHANGING  ct_form  = et_lines ).
    include_normal_statements( EXPORTING it_statements             = lt_stmnt
                                         if_use_subroutine_strings = abap_true
                               IMPORTING et_form                   = et_lines ).
    et_lines = VALUE #( BASE et_lines ( )
                                      ( |CATCH CX_ROOT.| ) ).

    " clear all formula field values
    build_clear_form_fld_lines( CHANGING ct_form = et_lines ).

    et_lines = VALUE #( BASE et_lines ( )
                                      ( |ENDTRY.| ) ).
    et_lines = VALUE #( BASE et_lines ( |ENDFORM.| ) ).

*    ZCL_SAT_SYSTEM_HELPER=>check_abap_syntax( et_lines ).
  ENDMETHOD.

  METHOD build_clear_form_fld_lines.
    mr_formula->get_formula_fields( IMPORTING et_fields = DATA(lt_fields) ).

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_form_field>).
      ct_form = VALUE #(
        BASE ct_form
        ( |  CLEAR <X_{ <ls_form_field>-field }>.| )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD include_row_and_form_fields.
    DATA: lt_row_fields TYPE string_table,
          lv_terminator TYPE char1.

    LOOP AT it_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>).
      lt_row_fields = VALUE #( BASE lt_row_fields
        FOR <ls_token> IN <ls_stmnt>-tokens
        WHERE ( is_formula_field = abap_true OR is_row_field = abap_true )
        ( COND #( WHEN <ls_token>-is_formula_field = abap_true THEN 'X_' && <ls_token>-str
                  WHEN <ls_token>-is_row_field = abap_true THEN ZCL_DBBR_formula_helper=>get_raw_row_field( <ls_token>-str ) ) )
      ).
    ENDLOOP.

    SORT lt_row_fields.
    DELETE ADJACENT DUPLICATES FROM lt_row_fields.

    IF mr_formula->is_color_column_needed( ).
      lt_row_fields = VALUE #( BASE lt_row_fields ( zif_dbbr_c_special_out_columns=>cell_col_row_color ) ).
    ENDIF.

    DATA(lv_lines) = lines( lt_row_fields ).
    LOOP AT lt_row_fields ASSIGNING FIELD-SYMBOL(<lv_row_field>).
      IF lv_lines = sy-tabix.
        lv_terminator = '.'.
      ELSE.
        lv_terminator = ','.
      ENDIF.

      IF sy-tabix = 1.
        ct_form = VALUE #( BASE ct_form
                           ( |macro_row_comp: { <lv_row_field> }{ lv_terminator }| ) ).
      ELSE.
        ct_form = VALUE #( BASE ct_form
                           ( |                { <lv_row_field> }{ lv_terminator }| ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
