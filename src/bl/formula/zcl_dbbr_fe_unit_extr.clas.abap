"! <p class="shorttext synchronized" lang="en">Extracts information of UNIT assignment</p>
CLASS zcl_dbbr_fe_unit_extr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_fe_field_extractor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_unit_extr IMPLEMENTATION.

  METHOD zif_dbbr_fe_field_extractor~extract_field.

    rs_field = VALUE zif_dbbr_fe_types=>ty_form_field(
        field     = is_statement-tokens[ 2 ]-str
        is_unit   = abap_true
        unit_field = is_statement-tokens[ 3 ]-str
    ).

  ENDMETHOD.

ENDCLASS.
