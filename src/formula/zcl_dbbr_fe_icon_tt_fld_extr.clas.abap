class ZCL_DBBR_FE_ICON_TT_FLD_EXTR definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_FE_FIELD_EXTRACTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_ICON_TT_FLD_EXTR IMPLEMENTATION.


  method ZIF_DBBR_FE_FIELD_EXTRACTOR~EXTRACT_FIELD.

    ASSIGN is_statement-tokens TO FIELD-SYMBOL(<lt_tokens>).

    rs_field = VALUE ZIF_DBBR_fe_types=>ty_form_field(
        field             = <lt_tokens>[ 2 ]-str
        type_name         = zif_dbbr_c_fe_global=>c_icon_tt_type
    ).

  endmethod.
ENDCLASS.
