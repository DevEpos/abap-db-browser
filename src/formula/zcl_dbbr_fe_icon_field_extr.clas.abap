CLASS zcl_dbbr_fe_icon_field_extr DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_fe_field_extractor.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_dbbr_fe_icon_field_extr IMPLEMENTATION.
  METHOD zif_dbbr_fe_field_extractor~extract_field.
    ASSIGN is_statement-tokens TO FIELD-SYMBOL(<lt_tokens>).

    rs_field = VALUE zif_dbbr_fe_types=>ty_form_field( field     = <lt_tokens>[ 2 ]-str
                                                       type_name = zif_dbbr_c_fe_global=>c_icon_type ).
  ENDMETHOD.
ENDCLASS.
