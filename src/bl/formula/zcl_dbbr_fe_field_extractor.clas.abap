"! <p class="shorttext synchronized" lang="en">Extracts information for formula field</p>
CLASS zcl_dbbr_fe_field_extractor DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_extractor
      IMPORTING
        iv_token            TYPE string
      RETURNING
        VALUE(rr_extractor) TYPE REF TO zif_dbbr_fe_field_extractor
      RAISING
        zcx_dbbr_formula_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_field_extractor IMPLEMENTATION.

  METHOD get_extractor.
    CASE iv_token.

      WHEN zif_dbbr_c_fe_keywords=>define_field.
        rr_extractor = NEW zcl_dbbr_fe_calc_field_extr( ).

      WHEN zif_dbbr_c_fe_keywords=>define_icon.
        rr_extractor = NEW zcl_dbbr_fe_icon_field_extr( ).

      WHEN zif_dbbr_c_fe_keywords=>define_icon_quick.
        rr_extractor = NEW zcl_dbbr_fe_icon_tt_fld_extr( ).

      WHEN zif_dbbr_c_fe_keywords=>define_description.
        rr_extractor = NEW zcl_dbbr_fe_field_descr_extr( ).

      WHEN zif_dbbr_c_fe_keywords=>define_unit.
        rr_extractor = NEW zcl_dbbr_fe_unit_extr( ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
          EXPORTING
            textid = zcx_dbbr_formula_exception=>no_extractor_found
            msgv1  = |{ iv_token }|.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
