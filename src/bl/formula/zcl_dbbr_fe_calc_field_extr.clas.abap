class ZCL_DBBR_FE_CALC_FIELD_EXTR definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_FE_FIELD_EXTRACTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_CALC_FIELD_EXTR IMPLEMENTATION.


  method ZIF_DBBR_FE_FIELD_EXTRACTOR~EXTRACT_FIELD.

    ASSIGN is_statement-tokens TO FIELD-SYMBOL(<lt_tokens>).

    DATA(lv_type_string) = <lt_tokens>[ 4 ]-str.

    IF contains( val = lv_type_string sub = '-' ).
      DATA(lv_type_ref_tab)      = substring_before( val = lv_type_string sub = '-' ).
      DATA(lv_type_ref_field)    = substring_after( val = lv_type_string sub = '-' ).
    ENDIF.

    rs_field = VALUE ZIF_DBBR_fe_types=>ty_form_field(
        field             = <lt_tokens>[ 2 ]-str
        type_ref_tab      = lv_type_ref_tab
        type_ref_field    = lv_type_ref_field
        type_name         = lv_type_string
    ).

  endmethod.
ENDCLASS.
