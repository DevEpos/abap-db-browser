CLASS zcl_dbbr_tree_node_wrapper DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES node_itab TYPE STANDARD TABLE OF REF TO zcl_dbbr_tree_node_wrapper WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        iv_node_key        TYPE tm_nodekey
        it_items           TYPE treemlitab
        is_node_properties TYPE treemlnodt
        if_is_folder       TYPE abap_bool.

    METHODS is_variant_node
      RETURNING
        VALUE(rf_is_variant) TYPE abap_bool.

    METHODS set_parent
      IMPORTING
        ir_parent TYPE REF TO zcl_dbbr_tree_node_wrapper.

    METHODS get_parent
      RETURNING
        VALUE(rr_parent) TYPE REF TO zcl_dbbr_tree_node_wrapper.

    METHODS set_prev_sibling
      IMPORTING
        ir_prev_sibling TYPE REF TO zcl_dbbr_tree_node_wrapper.

    METHODS get_prev_sibling
      RETURNING
        VALUE(rr_prev_sibling) TYPE REF TO zcl_dbbr_tree_node_wrapper.

    METHODS set_next_sibling
      IMPORTING
        ir_next_sibling TYPE REF TO zcl_dbbr_tree_node_wrapper.

    METHODS get_next_sibling
      RETURNING
        VALUE(rr_next_sibling) TYPE REF TO zcl_dbbr_tree_node_wrapper.

    METHODS set_children
      IMPORTING
        it_children TYPE node_itab.

    METHODS get_children
      RETURNING
        VALUE(rt_children) TYPE node_itab.

    METHODS get_node_key
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey.

    METHODS get_node_properties
      RETURNING
        VALUE(rs_node_properties) TYPE treemlnodt.

    METHODS get_items
      RETURNING
        VALUE(rt_items) TYPE treemlitab.

    METHODS has_children
      RETURNING
        VALUE(rf_has_children) TYPE boolean.

    METHODS is_folder
      RETURNING
        VALUE(rf_is_folder) TYPE boolean.

    METHODS get_favmenu_data
      RETURNING
        VALUE(rs_favmenu_data) TYPE zdbbr_favmenu.

    METHODS set_favmenu_data
      IMPORTING
        is_favmenu_data TYPE zdbbr_favmenu.

  PRIVATE SECTION.
    DATA mv_node_key TYPE tm_nodekey.
    DATA ms_node_properties TYPE treemlnodt.
    DATA mt_items TYPE treemlitab.
    DATA mr_parent TYPE REF TO zcl_dbbr_tree_node_wrapper.
    DATA mr_previous_sibling TYPE REF TO zcl_dbbr_tree_node_wrapper.
    DATA mr_next_sibling TYPE REF TO zcl_dbbr_tree_node_wrapper.
    DATA mt_children TYPE node_itab.
    DATA mf_is_folder TYPE abap_bool.
ENDCLASS.


CLASS zcl_dbbr_tree_node_wrapper IMPLEMENTATION.
  METHOD constructor.
    mv_node_key = iv_node_key.
    mt_items = it_items.
    ms_node_properties = is_node_properties.
    mf_is_folder = if_is_folder.
  ENDMETHOD.

  METHOD get_children.
    rt_children = mt_children.
  ENDMETHOD.

  METHOD get_favmenu_data.
    IF ms_node_properties-userobject IS NOT INITIAL.
      rs_favmenu_data = CAST zcl_dbbr_favmenu_entry( ms_node_properties-userobject )->get_favmenu_data( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_items.
    rt_items = mt_items.
  ENDMETHOD.

  METHOD get_next_sibling.
    rr_next_sibling = mr_next_sibling.
  ENDMETHOD.

  METHOD get_node_key.
    rv_node_key = mv_node_key.
  ENDMETHOD.

  METHOD get_node_properties.
    rs_node_properties = ms_node_properties.
  ENDMETHOD.

  METHOD get_parent.
    rr_parent = mr_parent.
  ENDMETHOD.

  METHOD get_prev_sibling.
    rr_prev_sibling = mr_previous_sibling.
  ENDMETHOD.

  METHOD has_children.
    rf_has_children = xsdbool( mt_children IS NOT INITIAL ).
  ENDMETHOD.

  METHOD is_folder.
*    rf_is_folder = xsdbool( ms_node_properties-exp_image = icon_folder or ms_node_properties-isfolder = abap_true ).
    rf_is_folder = mf_is_folder.
  ENDMETHOD.

  METHOD is_variant_node.
    CHECK ms_node_properties-userobject IS BOUND.

    TRY.
        CAST zcl_dbbr_variant( ms_node_properties-userobject ).
        rf_is_variant = abap_true.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.

  METHOD set_children.
    mt_children = it_children.
  ENDMETHOD.

  METHOD set_favmenu_data.
    ms_node_properties-userobject = NEW zcl_dbbr_favmenu_entry( is_favmenu_data ).
  ENDMETHOD.

  METHOD set_next_sibling.
    mr_next_sibling = ir_next_sibling.
  ENDMETHOD.

  METHOD set_parent.
    mr_parent = ir_parent.
  ENDMETHOD.

  METHOD set_prev_sibling.
    mr_previous_sibling = ir_prev_sibling.
  ENDMETHOD.
ENDCLASS.
