INTERFACE zif_dbbr_tree_node_filler
  PUBLIC .

  TYPES:
    BEGIN OF ty_node_map,
      node_key        TYPE tm_nodekey,
      tabname         TYPE tabname,
      fieldname       TYPE fieldname,
      alias_fieldname TYPE zdbbr_fieldname_with_alias,
    END OF ty_node_map .
  TYPES:
    tt_node_map TYPE STANDARD TABLE OF ty_node_map WITH KEY node_key .

  METHODS fill_node_item_tables
    IMPORTING
      !ir_nodes          TYPE REF TO zcl_uitb_ctm_nodes
    RETURNING
      VALUE(rt_node_map) TYPE tt_node_map .
ENDINTERFACE.
