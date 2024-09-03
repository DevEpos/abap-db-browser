CLASS zcl_dbbr_favmenu_evt_handler DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_dbbr_favorites_tree.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_favmenu_evt_handler.

  PRIVATE SECTION.
    DATA mr_favmenu_tree TYPE REF TO zcl_dbbr_favorites_tree.

    METHODS constructor
      IMPORTING
        ir_favmenu_tree TYPE REF TO zcl_dbbr_favorites_tree.
ENDCLASS.


CLASS zcl_dbbr_favmenu_evt_handler IMPLEMENTATION.
  METHOD constructor.
    mr_favmenu_tree = ir_favmenu_tree.
  ENDMETHOD.

  METHOD zif_dbbr_favmenu_evt_handler~add_favorite.
    mr_favmenu_tree->create_new_favorite( iv_fav_type    = iv_type
                                          iv_favorite    = iv_favorite
                                          iv_description = iv_description ).
  ENDMETHOD.
ENDCLASS.
