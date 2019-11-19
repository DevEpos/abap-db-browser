"! <p class="shorttext synchronized" lang="en">Helper for Trees</p>
CLASS zcl_dbbr_tree_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Get correct icon for a tree node</p>
    "! @parameter iv_favmenu_type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_tree_node_icon
      IMPORTING
        !iv_favmenu_type TYPE ZSAT_FAVMENU_TYPE
      RETURNING
        VALUE(result)    TYPE tv_image .
    "! <p class="shorttext synchronized" lang="en">Checks if the node is holds a db browser variant</p>
    "!
    "! @parameter ir_node | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rf_variant_node | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS is_variant_node
      IMPORTING
        ir_node                TYPE REF TO zcl_uitb_ctm_node
      RETURNING
        VALUE(rf_variant_node) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Execute default variant for node</p>
    "!
    "! @parameter ir_node | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS exec_default_variant_for_node
      IMPORTING
        ir_node TYPE ref to zcl_uitb_ctm_node.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_tree_helper IMPLEMENTATION.

  METHOD get_tree_node_icon.
    result = SWITCH #(
      iv_favmenu_type
      WHEN zif_dbbr_c_favmenu_type=>query THEN
         zif_dbbr_c_icon=>query
      WHEN zif_dbbr_c_favmenu_type=>table THEN
         zif_dbbr_c_icon=>database_table
      WHEN zif_dbbr_c_favmenu_type=>view THEN
         zif_dbbr_c_icon=>database_view
      WHEN zif_dbbr_c_favmenu_type=>cds_view THEN
         zif_dbbr_c_icon=>cds_view
      ELSE
         zif_dbbr_c_icon=>folder
    ).
  ENDMETHOD.

  METHOD is_variant_node.
    CHECK ir_node IS BOUND.

    DATA(lr_user_object) = ir_node->get_user_object( ).

    CHECK lr_user_object IS BOUND.

    TRY.
        CAST zcl_dbbr_variant( lr_user_object ).
        rf_variant_node = abap_true.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.

  METHOD exec_default_variant_for_node.

  ENDMETHOD.

ENDCLASS.
