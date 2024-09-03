CLASS zcl_dbbr_variant DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_variant_info TYPE zdbbr_variant_info.

    METHODS get_variant_info
      RETURNING
        VALUE(result) TYPE zdbbr_variant_info.

  PRIVATE SECTION.
    DATA ms_variant_info TYPE zdbbr_variant_info.
ENDCLASS.


CLASS zcl_dbbr_variant IMPLEMENTATION.
  METHOD constructor.
    ms_variant_info = is_variant_info.
  ENDMETHOD.

  METHOD get_variant_info.
    result = ms_variant_info.
  ENDMETHOD.
ENDCLASS.
