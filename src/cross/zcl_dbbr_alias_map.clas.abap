CLASS zcl_dbbr_alias_map DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_alias
      IMPORTING
        iv_index        TYPE sy-index
      RETURNING
        VALUE(rv_alias) TYPE char2.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA mt_alias_map TYPE TABLE OF char2.
ENDCLASS.


CLASS zcl_dbbr_alias_map IMPLEMENTATION.
  METHOD class_constructor.
    DATA lv_alias_prefix TYPE char1.

    DATA(lv_alphabet) = |ABCDEFGHIJKLMNOPQRSTUVY|.
    DATA(lv_alphabet_length) = strlen( lv_alphabet ).

    DO lv_alphabet_length TIMES.
      DATA(lv_index) = sy-index - 1.
      mt_alias_map = VALUE #( BASE mt_alias_map
                              ( |{ lv_alias_prefix }{ lv_alphabet+lv_index(1) }| ) ).
    ENDDO.

    DO lv_alphabet_length TIMES.
      DATA(lv_prefix_index) = sy-index - 1.
      lv_alias_prefix = lv_alphabet+lv_prefix_index(1).

      DO lv_alphabet_length TIMES.
        lv_index = sy-index - 1.
        mt_alias_map = VALUE #( BASE mt_alias_map
                                ( |{ lv_alias_prefix }{ lv_alphabet+lv_index(1) }| ) ).
      ENDDO.

    ENDDO.
  ENDMETHOD.

  METHOD get_alias.
    rv_alias = VALUE #( mt_alias_map[ iv_index ] OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
