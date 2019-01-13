class ZCL_DBBR_FE_FIELD_DESCR_EXTR definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_FE_FIELD_EXTRACTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_FIELD_DESCR_EXTR IMPLEMENTATION.


  method ZIF_DBBR_FE_FIELD_EXTRACTOR~EXTRACT_FIELD.

    ASSIGN is_statement-tokens TO FIELD-SYMBOL(<lt_tokens>).

    rs_field = VALUE ZIF_DBBR_fe_types=>ty_form_field(
        field             = <lt_tokens>[ 2 ]-str
        is_description    = abap_true
        short_description = replace( val   = <lt_tokens>[ 3 ]-str
                                     regex = |'(.*)'|
                                     with  ='$1' )
        long_description  = COND #( WHEN is_statement-token_count > 3 THEN
                                      replace( val   = <lt_tokens>[ 4 ]-str
                                               regex = |'(.*)'|
                                               with  ='$1' ) )
    ).

  endmethod.
ENDCLASS.
