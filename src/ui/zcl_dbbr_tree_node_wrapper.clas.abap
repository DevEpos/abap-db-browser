class ZCL_DBBR_TREE_NODE_WRAPPER definition
  public
  final
  create public .

public section.

  types:
    node_itab TYPE STANDARD TABLE OF REF TO ZCL_DBBR_tree_node_wrapper WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IV_NODE_KEY type TM_NODEKEY
      !IT_ITEMS type TREEMLITAB
      !IS_NODE_PROPERTIES type TREEMLNODT
      !IF_IS_FOLDER type ABAP_BOOL .
  methods IS_VARIANT_NODE
    returning
      value(RF_IS_VARIANT) type ABAP_BOOL .
  methods SET_PARENT
    importing
      !IR_PARENT type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  methods GET_PARENT
    returning
      value(RR_PARENT) type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  methods SET_PREV_SIBLING
    importing
      !IR_PREV_SIBLING type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  methods GET_PREV_SIBLING
    returning
      value(RR_PREV_SIBLING) type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  methods SET_NEXT_SIBLING
    importing
      !IR_NEXT_SIBLING type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  methods GET_NEXT_SIBLING
    returning
      value(RR_NEXT_SIBLING) type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  methods SET_CHILDREN
    importing
      !IT_CHILDREN type NODE_ITAB .
  methods GET_CHILDREN
    returning
      value(RT_CHILDREN) type NODE_ITAB .
  methods GET_NODE_KEY
    returning
      value(RV_NODE_KEY) type TM_NODEKEY .
  methods GET_NODE_PROPERTIES
    returning
      value(RS_NODE_PROPERTIES) type TREEMLNODT .
  methods GET_ITEMS
    returning
      value(RT_ITEMS) type TREEMLITAB .
  methods HAS_CHILDREN
    returning
      value(RF_HAS_CHILDREN) type BOOLEAN .
  methods IS_FOLDER
    returning
      value(RF_IS_FOLDER) type BOOLEAN .
  methods GET_FAVMENU_DATA
    returning
      value(RS_FAVMENU_DATA) type ZDBBR_FAVMENU .
  methods SET_FAVMENU_DATA
    importing
      !IS_FAVMENU_DATA type ZDBBR_FAVMENU .
  PROTECTED SECTION.
private section.

  data MV_NODE_KEY type TM_NODEKEY .
  data MS_NODE_PROPERTIES type TREEMLNODT .
  data MT_ITEMS type TREEMLITAB .
  data MR_PARENT type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  data MR_PREVIOUS_SIBLING type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  data MR_NEXT_SIBLING type ref to ZCL_DBBR_TREE_NODE_WRAPPER .
  data MT_CHILDREN type NODE_ITAB .
  data MF_IS_FOLDER type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_DBBR_TREE_NODE_WRAPPER IMPLEMENTATION.


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
      rs_favmenu_data = CAST ZCL_DBBR_favmenu_entry( ms_node_properties-userobject )->get_favmenu_data( ).
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
        CAST ZCL_DBBR_variant( ms_node_properties-userobject ).
        rf_is_variant = abap_true.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_children.
    mt_children = it_children.
  ENDMETHOD.


  METHOD set_favmenu_data.
    ms_node_properties-userobject = NEW ZCL_DBBR_favmenu_entry( is_favmenu_data ).
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
