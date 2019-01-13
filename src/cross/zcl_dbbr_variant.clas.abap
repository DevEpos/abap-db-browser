CLASS ZCL_DBBR_variant DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_variant_info TYPE ZDBBR_variant_info.
    METHODS get_variant_info
      RETURNING
        VALUE(result) TYPE ZDBBR_variant_info.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_variant_info TYPE ZDBBR_variant_info.
ENDCLASS.



CLASS ZCL_DBBR_variant IMPLEMENTATION.

  METHOD constructor.

    me->ms_variant_info = is_variant_info.

  ENDMETHOD.

  METHOD get_variant_info.
    result = ms_variant_info.
  ENDMETHOD.

ENDCLASS.
