CLASS zcl_dbbr_fe_generic_form_bldr DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS include_normal_statements
      IMPORTING
        it_statements             TYPE zif_dbbr_fe_types=>tt_statement
        if_use_subroutine_strings TYPE abap_bool OPTIONAL
      EXPORTING
        et_form                   TYPE string_table.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_generic_form_bldr IMPLEMENTATION.
  METHOD include_normal_statements.
    DATA: lt_lines TYPE string_table.

    et_form = VALUE #( BASE et_form
                       ( `*******************************************************************************` )
                       ( ) ).
    LOOP AT it_statements ASSIGNING FIELD-SYMBOL(<ls_statement>) WHERE is_form_stmnt = abap_false.
      IF if_use_subroutine_strings = abap_true.
        SPLIT <ls_statement>-stringform_subroutine AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.
        et_form = VALUE #( BASE et_form ( LINES OF lt_lines ) ).
      ELSE.
        IF <ls_statement>-stringform CS cl_abap_char_utilities=>cr_lf.
          SPLIT <ls_statement>-stringform AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.
          et_form = VALUE #( BASE et_form ( LINES OF lt_lines ) ).
        ELSE.
          et_form = VALUE #( BASE et_form ( <ls_statement>-stringform ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    et_form = VALUE #( BASE et_form
                       ( )
                       ( `*******************************************************************************` ) ).
  ENDMETHOD.
ENDCLASS.
