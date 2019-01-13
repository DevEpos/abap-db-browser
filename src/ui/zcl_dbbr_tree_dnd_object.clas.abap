class ZCL_DBBR_TREE_DND_OBJECT definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_NODE_KEYS type TREEMNOTAB .
  methods HAS_MORE_KEYS
    returning
      value(RF_HAS_MORE) type BOOLEAN .
  methods GET_NEXT_NODE
    returning
      value(RV_NODE_KEY) type TM_NODEKEY .
  methods GET_ALL_KEYS
    returning
      value(RT_KEYS) type TREEMNOTAB .
protected section.
private section.

  data MT_NODE_KEYS type TREEMNOTAB .
ENDCLASS.



CLASS ZCL_DBBR_TREE_DND_OBJECT IMPLEMENTATION.


  method CONSTRUCTOR.

    mt_node_keys = it_node_keys.

  endmethod.


  method GET_ALL_KEYS.

    rt_keys = mt_node_keys.

  endmethod.


  method GET_NEXT_NODE.

    CHECK has_more_keys( ).

    rv_node_key = mt_node_keys[ 1 ].
    DELETE mt_node_keys INDEX 1.

  endmethod.


  method HAS_MORE_KEYS.

    rf_has_more = xsdbool( mt_node_keys IS NOT INITIAL ).

  endmethod.
ENDCLASS.
