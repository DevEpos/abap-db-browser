CLASS zcl_dbbr_tree_dnd_object DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_node_keys TYPE treemnotab.

    METHODS has_more_keys
      RETURNING
        VALUE(rf_has_more) TYPE boolean.

    METHODS get_next_node
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey.

    METHODS get_all_keys
      RETURNING
        VALUE(rt_keys) TYPE treemnotab.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_node_keys TYPE treemnotab.
ENDCLASS.


CLASS zcl_dbbr_tree_dnd_object IMPLEMENTATION.
  METHOD constructor.
    mt_node_keys = it_node_keys.
  ENDMETHOD.

  METHOD get_all_keys.
    rt_keys = mt_node_keys.
  ENDMETHOD.

  METHOD get_next_node.
    CHECK has_more_keys( ).

    rv_node_key = mt_node_keys[ 1 ].
    DELETE mt_node_keys INDEX 1.
  ENDMETHOD.

  METHOD has_more_keys.
    rf_has_more = xsdbool( mt_node_keys IS NOT INITIAL ).
  ENDMETHOD.
ENDCLASS.
