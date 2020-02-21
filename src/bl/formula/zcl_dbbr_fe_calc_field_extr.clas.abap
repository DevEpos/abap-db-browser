"! <p class="shorttext synchronized" lang="en">Extracts calculation field out formula</p>
CLASS zcl_dbbr_fe_calc_field_extr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_fe_field_extractor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_calc_field_extr IMPLEMENTATION.

  METHOD zif_dbbr_fe_field_extractor~extract_field.

    ASSIGN is_statement-tokens TO FIELD-SYMBOL(<lt_tokens>).

    DATA(lv_type_string) = <lt_tokens>[ 4 ]-str.

    IF contains( val = lv_type_string sub = '-' ).
      DATA(lv_type_ref_tab)      = substring_before( val = lv_type_string sub = '-' ).
      DATA(lv_type_ref_field)    = substring_after( val = lv_type_string sub = '-' ).
    ENDIF.

    rs_field = VALUE zif_dbbr_fe_types=>ty_form_field(
        field             = <lt_tokens>[ 2 ]-str
        type_ref_tab      = lv_type_ref_tab
        type_ref_field    = lv_type_ref_field
        type_name         = lv_type_string
    ).

  ENDMETHOD.

ENDCLASS.
