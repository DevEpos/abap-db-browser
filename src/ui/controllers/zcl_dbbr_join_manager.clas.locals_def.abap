*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: lty_node_type TYPE char20.

TYPES:
  BEGIN OF lty_node_map,
    node_key  TYPE tm_nodekey,
    tabname   TYPE tabname,
    node_type TYPE lty_node_type,
  END OF lty_node_map.

TYPES: ltt_node_type_range TYPE RANGE OF lty_node_type.

CONSTANTS:
  BEGIN OF c_node_type,
    table           TYPE lty_node_type VALUE 'TABLE',
    table_filters   TYPE lty_node_type VALUE 'TABLE_FILTERS',
    table_fields    TYPE lty_node_type VALUE 'TABLE_FIELDS',
    table_field     TYPE lty_node_type VALUE 'TABLE_FIELD',
    table_filter    TYPE lty_node_type VALUE 'TABLE_FILTER',
    table_filter_or TYPE lty_node_type VALUE 'TABLE_FILTER_OR',
    or_filter_group TYPE lty_node_type VALUE 'OR_FILTER_GROUP',
  END OF c_node_type.

INTERFACE lif_tree_node_events.
  EVENTS request_deletion
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey.
ENDINTERFACE.

CLASS lcl_join_table DEFINITION DEFERRED.
CLASS lcl_join DEFINITION DEFERRED.

CLASS lcl_join_field DEFINITION
  FRIENDS lcl_join_table
          lcl_join.
  PUBLIC SECTION.
    INTERFACES lif_tree_node_events.

    DATA mv_node_key TYPE tm_nodekey READ-ONLY.
    DATA mv_table TYPE tabname READ-ONLY.
    METHODS constructor
      IMPORTING
        is_field    TYPE zdbbr_joinfld
        iv_table    TYPE tabname
        iv_node_key TYPE tm_nodekey.
    METHODS get_field
      RETURNING
        VALUE(result) TYPE zdbbr_joinfld.
    METHODS update_field
      IMPORTING
        value             TYPE zdbbr_joinfld
      RETURNING
        VALUE(rf_changed) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_field TYPE zdbbr_joinfld.
    DATA mf_changed TYPE abap_bool.
ENDCLASS.

CLASS lcl_join_filter DEFINITION
  FRIENDS lcl_join_table
          lcl_join.
  PUBLIC SECTION.
    INTERFACES lif_tree_node_events.

    DATA mv_node_key TYPE tm_nodekey READ-ONLY.
    DATA mv_table TYPE tabname READ-ONLY.

    METHODS constructor
      IMPORTING
        is_filter   TYPE zdbbr_joinfil
        iv_table    TYPE tabname
        iv_node_key TYPE tm_nodekey.
    METHODS set_and_or
      IMPORTING
        value TYPE vsconj DEFAULT zif_dbbr_c_selection_condition=>and.
    METHODS set_or_group_node
      IMPORTING
        iv_node_key TYPE tm_nodekey.
    METHODS get_filter
      RETURNING
        VALUE(result) TYPE zdbbr_joinfil.
    METHODS update_filter
      IMPORTING
        value             TYPE zdbbr_joinfil
      RETURNING
        VALUE(rf_changed) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_filter TYPE zdbbr_joinfil.
    DATA mv_or_group_node TYPE tm_nodekey.
    DATA mf_changed TYPE abap_bool.
ENDCLASS.

CLASS lcl_join_table DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_tree_node_events.
    DATA mv_node_key TYPE tm_nodekey READ-ONLY.
    DATA mv_tabname TYPE tabname READ-ONLY.

    METHODS constructor
      IMPORTING
        is_join_table TYPE zdbbr_joint
        iv_node_key   TYPE tm_nodekey.
    METHODS clear_offset_from_fields.
    METHODS has_changes
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS to_structure
      IMPORTING
        ir_tree         TYPE REF TO zcl_uitb_column_tree_model
      RETURNING
        VALUE(rs_table) TYPE zdbbr_join_table_ui.
    METHODS get_tab_info
      RETURNING
        VALUE(result) TYPE zdbbr_joint.
    METHODS delete_dependent
      IMPORTING
        iv_table TYPE tabname.
    METHODS set_tab_info
      IMPORTING
        value             TYPE zdbbr_joint
      RETURNING
        VALUE(rf_changed) TYPE abap_bool.
    METHODS add_filter
      IMPORTING
        ir_filter         TYPE REF TO lcl_join_filter
        ir_filter_node     type ref to zcl_uitb_ctm_node.
    METHODS add_field
      IMPORTING
        ir_field          TYPE REF TO lcl_join_field
        ir_field_node     type ref to zcl_uitb_ctm_node.
    METHODS has_references_to_table
      IMPORTING
        iv_table                     TYPE tabname
      RETURNING
        VALUE(rf_dependencies_exist) TYPE abap_bool.
    METHODS delete_filter
      IMPORTING
        iv_node_key TYPE tm_nodekey.
    METHODS delete_field
      IMPORTING
        iv_node_key TYPE tm_nodekey.
    METHODS validate.
    METHODS clear_changed_flag.
    "! Delete all filter conditions in this join table
    METHODS delete_filters.
    "! Delete all field conditions in this join table
    METHODS delete_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF mty_filter_cond,
        node_key   TYPE tm_nodekey,
        filter_ref TYPE REF TO lcl_join_filter,
      END OF mty_filter_cond.
    TYPES:
      BEGIN OF mty_field_cond,
        node_key  TYPE tm_nodekey,
        field_ref TYPE REF TO lcl_join_field,
      END OF mty_field_cond.
    DATA: ms_table_info TYPE zdbbr_joint.
    DATA: mt_filter_cond TYPE STANDARD TABLE OF mty_filter_cond WITH KEY node_key.
    DATA: mt_field_cond TYPE STANDARD TABLE OF mty_field_cond WITH KEY node_key.
    DATA mf_changed TYPE abap_bool.
ENDCLASS.

CLASS lcl_join DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_tree_node_events.

    DATA mv_primary_table TYPE tabname READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_primary_table TYPE tabname.
    METHODS has_changes
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS to_structure
      IMPORTING
        ir_tree            TYPE REF TO zcl_uitb_column_tree_model
      RETURNING
        VALUE(rs_join_def) TYPE zdbbr_join_def.
    METHODS get_possible_tables_for_f4
      IMPORTING
        iv_table         TYPE tabname
      RETURNING
        VALUE(rt_tables) TYPE zdbbr_tabname_range_itab.
    METHODS has_table
      IMPORTING
        iv_table         TYPE tabname
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    METHODS get_table
      IMPORTING
        iv_table        TYPE tabname
      RETURNING
        VALUE(rr_table) TYPE REF TO lcl_join_table.
    METHODS add_table
      IMPORTING
        ir_table TYPE REF TO lcl_join_table.
    METHODS delete_table
      IMPORTING
        iv_table TYPE tabname.
    METHODS delete_all_tables.
    METHODS clear_changed_flag.
    METHODS validate.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF mty_table,
        tabname  TYPE tabname,
        node_key TYPE tm_nodekey,
        tab_ref  TYPE REF TO lcl_join_table,
      END OF mty_table.

    DATA mt_tables TYPE STANDARD TABLE OF mty_table WITH EMPTY KEY.
    DATA mf_changed TYPE abap_bool.
ENDCLASS.
