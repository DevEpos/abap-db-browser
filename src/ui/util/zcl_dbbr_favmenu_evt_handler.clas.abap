CLASS zcl_dbbr_favmenu_evt_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_dbbr_favorites_tree.

  PUBLIC SECTION.

    INTERFACES zif_dbbr_favmenu_evt_handler .
  PROTECTED SECTION.
private section.

  data MR_FAVMENU_TREE type ref to ZCL_DBBR_FAVORITES_TREE .

  methods CONSTRUCTOR
    importing
      !IR_FAVMENU_TREE type ref to ZCL_DBBR_FAVORITES_TREE .
ENDCLASS.



CLASS ZCL_DBBR_FAVMENU_EVT_HANDLER IMPLEMENTATION.


  METHOD constructor.
    mr_favmenu_tree = ir_favmenu_tree.
  ENDMETHOD.


  METHOD zif_dbbr_favmenu_evt_handler~add_favorite.
    mr_favmenu_tree->create_new_favorite(
        iv_fav_type    = iv_type
        iv_favorite    = iv_favorite
        iv_description = iv_description
    ).
  ENDMETHOD.
ENDCLASS.
