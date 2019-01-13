CLASS zcl_dbbr_package_list DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_list.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_packages TYPE zdbbr_package_t.
ENDCLASS.



CLASS zcl_dbbr_package_list IMPLEMENTATION.
  METHOD zif_uitb_list~add.
    TRY.
        mt_packages = VALUE #( BASE mt_packages ( CAST #( ir_element ) ) ).
      CATCH cx_sy_move_cast_error.
        " TODO: raise specific exception that passed object cannot be added to list.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_uitb_list~clear.
    CLEAR mt_packages.
  ENDMETHOD.

  METHOD zif_uitb_list~size.
    rv_size = lines( mt_packages ).
  ENDMETHOD.

  METHOD zif_uitb_list~get_iterator.
    rr_iterator = zcl_uitb_list_iterator=>create( ir_list = me ).
  ENDMETHOD.

  METHOD zif_uitb_list~get_element.
    TRY.
        rr_element = mt_packages[ iv_index ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_uitb_element_not_found
          EXPORTING
            textid = zcx_uitb_element_not_found=>index_access
            index  = iv_index.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_uitb_list~remove.
    " not possible
  ENDMETHOD.

  METHOD zif_uitb_list~remove_at.
    " not possible
  ENDMETHOD.

ENDCLASS.
