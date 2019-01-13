CLASS ZCL_DBBR_fe_field_extractor DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_extractor
      IMPORTING
        iv_token            TYPE string
      RETURNING
        VALUE(rr_extractor) TYPE REF TO ZIF_DBBR_fe_field_extractor
      RAISING
        ZCX_DBBR_formula_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_fe_field_extractor IMPLEMENTATION.
  METHOD get_extractor.
    CASE iv_token.

      WHEN zif_dbbr_c_fe_keywords=>define_field.
        rr_extractor = NEW ZCL_DBBR_fe_calc_field_extr( ).

      WHEN zif_dbbr_c_fe_keywords=>define_icon.
        rr_extractor = NEW ZCL_DBBR_fe_icon_field_extr( ).

      WHEN zif_dbbr_c_fe_keywords=>define_icon_quick.
        rr_extractor = NEW ZCL_DBBR_fe_icon_tt_fld_extr( ).

      WHEN zif_dbbr_c_fe_keywords=>define_description.
        rr_extractor = NEW ZCL_DBBR_fe_field_descr_extr( ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE ZCX_DBBR_formula_exception
          EXPORTING
            textid = ZCX_DBBR_formula_exception=>no_extractor_found
            msgv1  = |{ iv_token }|.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
