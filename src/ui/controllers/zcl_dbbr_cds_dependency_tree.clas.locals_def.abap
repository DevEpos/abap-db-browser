*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CONSTANTS:
  BEGIN OF c_functions,
    close_code_view            TYPE ui_func VALUE 'CLOSE',
    toggle_tree                TYPE ui_func VALUE 'TOGGLE_TREE',
    toggle_alv                 TYPE ui_func VALUE 'TOGGLE_ALV',
    maximize_code_view         TYPE ui_func VALUE 'MAXIMIZE_CODE_VIEW',
    focus_on_tree              TYPE ui_func VALUE 'FOCUS',
    expand_all                 TYPE ui_func VALUE 'EXPANDALL',
    collapse_all               TYPE ui_func VALUE 'COLLAPSEALL',
    show_metrics               TYPE ui_func VALUE 'SHOW_METRICS',
    open_with_adt              TYPE ui_func VALUE 'ADTJUMP',
    options_menu               TYPE ui_func VALUE 'OPTIONS',
    open_in_new_window         TYPE ui_func VALUE 'OPENINDBBRSNEWWIN',
    show_ddl_source            TYPE ui_func VALUE 'SHOWSOURCE',
    exec_with_dbbrs            TYPE ui_func VALUE 'EXECWIHTDBBRS',
    exec_with_dbbrs_new_window TYPE ui_func VALUE 'EXECWIHTDBBRSNEW',
  END OF c_functions.

TYPES:
  BEGIN OF lty_s_command_info,
    entity_id   TYPE zsat_entity_id,
    entity_type TYPE zsat_entity_type,
    is_cds      TYPE abap_bool,
  END OF lty_s_command_info.

CLASS lcl_usage_alv DEFINITION.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        io_dependency_tree TYPE REF TO zcl_dbbr_cds_dependency_tree
        iv_cds_view_name   TYPE zsat_cds_view_name
        io_parent          TYPE REF TO cl_gui_container.

    "! <p class="shorttext synchronized">Sets function in ALV</p>
    METHODS set_alv_function
      IMPORTING
        iv_function TYPE ui_func.

    "! <p class="shorttext synchronized">Retrieve the selected entity id</p>
    METHODS get_selected_entity
      EXPORTING
        ev_entity_id   TYPE zsat_entity_id
        ef_is_cds      TYPE abap_bool
        ev_entity_type TYPE zsat_entity_type.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_dependency_usage.
             INCLUDE TYPE zcl_sat_cds_usage_analyzer=>ty_dependency.
    TYPES:   type_icon TYPE char40.
    TYPES: END OF ty_s_dependency_usage.

    DATA mt_usages TYPE STANDARD TABLE OF ty_s_dependency_usage.
    DATA mo_dependency_tree TYPE REF TO zcl_dbbr_cds_dependency_tree.
    DATA mo_alv TYPE REF TO zcl_uitb_alv.

    "! <p class="shorttext synchronized">Handler for ALV user command</p>
    METHODS on_user_command
      FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
        ev_function
        ev_tag.

    "! <p class="shorttext synchronized">Creates ALV for usages in CDS dependency tree</p>
    METHODS create_and_fill_usage_alv
      IMPORTING
        io_container TYPE REF TO cl_gui_container.

    "! <p class="shorttext synchronized">Handler for ALV double click event</p>
    METHODS on_double_click
      FOR EVENT double_click OF zcl_uitb_alv_events
      IMPORTING
        ev_column
        ev_row.

    "! <p class="shorttext synchronized">Handler for ALV context menu</p>
    METHODS on_context_menu
      FOR EVENT context_menu OF zcl_uitb_alv_events
      IMPORTING
        er_menu.
ENDCLASS.
