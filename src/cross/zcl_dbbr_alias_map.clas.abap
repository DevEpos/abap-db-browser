class ZCL_DBBR_ALIAS_MAP definition
  public
  create public .

public section.

  class-methods GET_ALIAS
    importing
      !IV_INDEX type SY-INDEX
    returning
      value(RV_ALIAS) type CHAR2 .
  class-methods CLASS_CONSTRUCTOR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      mt_alias_map TYPE TABLE OF char2.
ENDCLASS.



CLASS ZCL_DBBR_ALIAS_MAP IMPLEMENTATION.


  METHOD class_constructor.
    DATA: lv_alias_prefix TYPE char1.

    DATA(lv_alphabet) = |ABCDEFGHIJKLMNOPQRSTUVY|.
    DATA(lv_alphabet_length) = strlen( lv_alphabet ).

    DO lv_alphabet_length TIMES.
      DATA(lv_index) = sy-index - 1.
      mt_alias_map = VALUE #( BASE mt_alias_map
       ( |{ lv_alias_prefix }{ lv_alphabet+lv_index(1) }| )
      ).
    ENDDO.

    DO lv_alphabet_length TIMES.
      DATA(lv_prefix_index) = sy-index - 1.
      lv_alias_prefix = lv_alphabet+lv_prefix_index(1).

      DO lv_alphabet_length TIMES.
        lv_index = sy-index - 1.
        mt_alias_map = VALUE #( BASE mt_alias_map
         ( |{ lv_alias_prefix }{ lv_alphabet+lv_index(1) }| )
        ).
      ENDDO.

    ENDDO.



  ENDMETHOD.


  METHOD get_alias.

    rv_alias = value #( mt_alias_map[ iv_index ] optional ).

  ENDMETHOD.
ENDCLASS.
