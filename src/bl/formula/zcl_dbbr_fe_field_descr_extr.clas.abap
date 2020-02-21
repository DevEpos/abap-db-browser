"! <p class="shorttext synchronized" lang="en">Extracts description of formula field</p>
CLASS zcl_dbbr_fe_field_descr_extr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_fe_field_extractor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_field_descr_extr IMPLEMENTATION.

  METHOD zif_dbbr_fe_field_extractor~extract_field.

    ASSIGN is_statement-tokens TO FIELD-SYMBOL(<lt_tokens>).

    rs_field = VALUE zif_dbbr_fe_types=>ty_form_field(
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

  ENDMETHOD.

ENDCLASS.
