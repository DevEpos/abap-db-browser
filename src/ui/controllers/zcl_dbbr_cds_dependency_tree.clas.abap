"! <p class="shorttext synchronized" lang="en">Dependency Tree for CDS View</p>
CLASS zcl_dbbr_cds_dependency_tree DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_c_object_browser .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        iv_cds_view        TYPE zsat_cds_view_name
        if_opened_from_adt TYPE abap_bool OPTIONAL.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
    METHODS zif_uitb_gui_screen~show
        REDEFINITION.

    ALIASES c_node_type
      FOR zif_dbbr_c_object_browser~c_tree_node_type .
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
  PRIVATE SECTION.
    TYPES ty_bitmask TYPE x LENGTH 32.
    CONSTANTS:
      BEGIN OF c_dbl_click_action,
        function_prefix            TYPE string VALUE 'DBL_CLICK_ACTION_',
        adt_navigation             TYPE zdbbr_action VALUE 1,
        open_in_db_browser         TYPE zdbbr_action VALUE 2,
        exec_with_dbbrs            TYPE zdbbr_action VALUE 3,
        exec_with_dbbrs_new_window TYPE zdbbr_action VALUE 4,
      END OF c_dbl_click_action.

    TYPES:
      BEGIN OF ty_node_map,
        node_key      TYPE tm_nodekey,
        entity_id     TYPE zsat_entity_id,
        entity_id_raw TYPE zsat_entity_id,
        entity_type   TYPE zsat_entity_type,
        sql_name      TYPE tabname,
      END OF ty_node_map .

    CONSTANTS c_relation_col TYPE tv_itmname VALUE 'RELATION' ##NO_TEXT.
    CONSTANTS c_sql_name_col TYPE tv_itmname VALUE 'DBOBJECT' ##NO_TEXT.
    CONSTANTS c_object_type_col TYPE tv_itmname VALUE 'OBJTYPE' ##NO_TEXT.
    CONSTANTS c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT.
    CONSTANTS c_fill_text_func TYPE sy-ucomm VALUE 'FILLTXT' ##no_text.
    CONSTANTS c_metrics_provider_class TYPE classname VALUE 'CL_DD_DDL_META_NUM_COLLECTOR' ##NO_TEXT.

    CONSTANTS:
      BEGIN OF c_check_mask,
        incl_associations TYPE ty_bitmask VALUE '00000004',
      END OF c_check_mask.

    CLASS-DATA:
      BEGIN OF gs_metrics_settings,
        settings_loaded   TYPE abap_bool,
        has_bitmask_param TYPE abap_bool,
      END OF gs_metrics_settings.
    DATA mv_dbl_click_action TYPE char1.
    DATA mf_tree_mode TYPE abap_bool VALUE abap_true.
    DATA mf_opened_from_adt TYPE abap_bool.
    DATA mo_options_menu TYPE REF TO cl_ctmenu.
    DATA mo_usage_alv TYPE REF TO lcl_usage_alv.
    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model .
    DATA mv_view_name TYPE zsat_cds_view_name .
    DATA mt_node_map TYPE STANDARD TABLE OF ty_node_map WITH KEY node_key.
    DATA ms_dependencies TYPE zcl_sat_cds_dep_analyzer=>ty_s_dependency_graph_node.
    DATA ms_metrics_map TYPE cl_dd_ddl_meta_num_collector=>entity2number_map.
    DATA: mo_code_view TYPE REF TO zcl_uitb_abap_code_view,
          mo_splitter  TYPE REF TO zcl_uitb_gui_splitter_cont,
          mv_theme     TYPE zuitb_code_viewer_theme,
          mo_switch    TYPE REF TO zcl_uitb_gui_switch_container.

    METHODS get_metrics_calculator
      RETURNING
        VALUE(ro_metrics_calculator) TYPE REF TO cl_dd_ddl_meta_num_collector.
    "! <p class="shorttext synchronized" lang="en">Add new node to the tree</p>
    METHODS add_node
      IMPORTING
        is_node        TYPE cl_ddls_dependency_visitor=>ty_s_dependency_graph_node
        iv_parent_node TYPE tm_nodekey OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Create tree model</p>
    METHODS create_tree
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Fetches CDS dependencies</p>
    METHODS fetch_dependencies
      RETURNING
        VALUE(rf_dependencies_read) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Retrieve Text for constant</p>
    METHODS get_relation_text
      IMPORTING
        iv_constant    TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Get text for object type</p>
    METHODS get_object_type_text
      IMPORTING
        iv_constant    TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Get icon for node type</p>
    METHODS get_icon_for_node
      IMPORTING
        iv_type        TYPE string
      RETURNING
        VALUE(rv_icon) TYPE tv_image.
    METHODS fill_entity_texts.
    METHODS show_complexity_metrics.
    "! <p class="shorttext synchronized" lang="en">Handler for requesting a context menu for a node</p>
    METHODS on_node_context_menu_request
          FOR EVENT node_context_menu_request OF zif_uitb_tree_model_events
      IMPORTING
          !er_menu
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for when the context menu entry was chosen</p>
    METHODS on_node_context_menu_select
          FOR EVENT node_context_menu_select OF zif_uitb_tree_model_events
      IMPORTING
          !ev_fcode
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for double click on node</p>
    METHODS on_node_double_click
          FOR EVENT node_double_click OF zif_uitb_tree_model_events
      IMPORTING
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Trigger Show CDS Source Code</p>
    METHODS do_show_cds_source
      IMPORTING
        ir_params TYPE REF TO data OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Show CDS Source Code</p>
    METHODS show_cds_source
      IMPORTING
        iv_cds_name TYPE zsat_entity_id.
    "! <p class="shorttext synchronized" lang="en">Trigger Show Content of DB Entity</p>
    METHODS do_show_content
      IMPORTING
        if_new_task TYPE abap_bool OPTIONAL
        ir_params   TYPE REF TO data OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Show Content of DB Entity</p>
    METHODS show_content
      IMPORTING
        if_new_task    TYPE abap_bool OPTIONAL
        iv_entity_id   TYPE zsat_entity_id
        iv_entity_type TYPE zsat_entity_type.

    "! <p class="shorttext synchronized" lang="en">Creates code viewer control</p>
    METHODS create_code_viewer
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    METHODS load_cds_source_into_view
      IMPORTING
        iv_code TYPE string.
    "! <p class="shorttext synchronized" lang="en">Creates context menu for options</p>
    METHODS modify_options_ctx_menu.
    METHODS adjust_dbl_click_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO cl_ctmenu.
    METHODS update_settings
      IMPORTING
        iv_dbl_click_action TYPE zdbbr_action.
    "! <p class="shorttext synchronized" lang="en">Switch between tree and alv mode</p>
    METHODS toggle_usage_alv.
    METHODS toggle_dependency_tree.
    "! <p class="shorttext synchronized" lang="en">Trigger ADT jump</p>
    METHODS do_open_with_adt
      IMPORTING
        ir_params TYPE REF TO data.
    "! <p class="shorttext synchronized" lang="en">Jump to ADT with selected entity</p>
    METHODS open_with_adt
      IMPORTING
        iv_entity_id   TYPE zsat_entity_id
        iv_entity_type TYPE zsat_entity_type.
    "! <p class="shorttext synchronized" lang="en">Open DB Browser in new window for entity</p>
    METHODS open_db_browser_new_window
      IMPORTING
        iv_entity_id   TYPE zsat_entity_id
        iv_entity_type TYPE zsat_entity_type.
    "! <p class="shorttext synchronized" lang="en">Trigger Open DB Browser in new window for entity</p>
    METHODS do_open_in_new_window
      IMPORTING
        ir_params TYPE REF TO data.

ENDCLASS.



CLASS zcl_dbbr_cds_dependency_tree IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_title = |{ 'SQL Dependency Tree for'(002) } { iv_cds_view }| ).
    mv_view_name = iv_cds_view.
    mf_opened_from_adt = if_opened_from_adt.
    mv_dbl_click_action = zcl_dbbr_usersettings_factory=>get_settings( )-dep_tree_dbl_click_action.
    IF mv_dbl_click_action IS INITIAL OR mv_dbl_click_action = 0.
      mv_dbl_click_action = c_dbl_click_action-open_in_db_browser.
    ENDIF.
  ENDMETHOD.


  METHOD add_node.
    DATA: lv_entity_type TYPE zsat_entity_type.

    IF is_node-type = zcl_sat_cds_dep_analyzer=>c_node_type-cds_view OR
         is_node-type = zcl_sat_cds_dep_analyzer=>c_node_type-cds_table_function OR
         is_node-type = zcl_sat_cds_dep_analyzer=>c_node_type-cds_db_view.
      lv_entity_type = zif_sat_c_entity_type=>cds_view.
    ELSEIF is_node-type = zcl_sat_cds_dep_analyzer=>c_node_type-table.
      lv_entity_type = zif_sat_c_entity_type=>table.
    ELSEIF is_node-type = zcl_sat_cds_dep_analyzer=>c_node_type-view.
      lv_entity_type = zif_sat_c_entity_type=>view.
    ENDIF.

    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(lo_node) = lo_nodes->add_node(
        iv_relative_node_key = iv_parent_node
        iv_image             = get_icon_for_node( is_node-type )
        iv_expanded_image    = get_icon_for_node( is_node-type )
        it_item_table        = VALUE #(
          ( item_name = mo_tree->c_hierarchy_column
            font      = cl_item_tree_model=>item_font_prop
            class     = cl_item_tree_model=>item_class_text
            text      = |{ COND #(
              WHEN is_node-user_defined_entity_name IS NOT INITIAL THEN is_node-user_defined_entity_name
              ELSE is_node-name ) }| )
          ( item_name = c_hierarchy_node2
            font      = cl_item_tree_model=>item_font_prop
            class     = cl_item_tree_model=>item_class_text
            style     = zif_uitb_c_ctm_style=>inverted_gray )
          ( item_name = c_sql_name_col
            font      = cl_item_tree_model=>item_font_prop
            class     = cl_item_tree_model=>item_class_text
            text      = |{ COND #( WHEN lv_entity_type IS NOT INITIAL THEN is_node-name ) }| )
          ( item_name = c_relation_col
            font      = cl_item_tree_model=>item_font_prop
            class     = cl_item_tree_model=>item_class_text
            text      = get_relation_text( is_node-relation ) )
          ( item_name = c_object_type_col
            font      = cl_item_tree_model=>item_font_prop
            class     = cl_item_tree_model=>item_class_text
            text      = get_object_type_text( is_node-type ) )
        )
    ).

    IF lv_entity_type IS NOT INITIAL.
      mt_node_map = VALUE #( BASE mt_node_map
        ( node_key      = lo_node->mv_node_key
          entity_id     = COND #( WHEN is_node-entity_name IS NOT INITIAL THEN is_node-entity_name ELSE is_node-name )
          entity_id_raw = is_node-entity_name
          entity_type   = lv_entity_type
          sql_name      = is_node-name )
      ).
    ENDIF.


    IF is_node-children IS BOUND.
      LOOP AT CAST cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes( is_node-children )->* ASSIGNING FIELD-SYMBOL(<ls_child_node>).
        add_node(
            is_node        = <ls_child_node>
            iv_parent_node = lo_node->mv_node_key
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.




  METHOD create_content.
    mo_splitter = NEW zcl_uitb_gui_splitter_cont(
      iv_elements = 2
      iv_mode     = zcl_uitb_gui_splitter_cont=>c_mode-cols
      io_parent   = io_container
    ).

    mo_splitter->set_element_visibility( iv_element = 2 if_visible = abap_false ).
    create_tree( mo_splitter->get_container( 1 ) ).
    create_code_viewer( mo_splitter->get_container( 2 ) ).

  ENDMETHOD.

  METHOD do_before_dynpro_output.
    IF mf_tree_mode = abap_true.
      io_callback->set_title( |{ 'SQL Dependency Tree for'(002) } { mv_view_name }| ).
    ELSE.
      io_callback->set_title( |{ 'SQL Used Data Sources Analysis for'(054) } { mv_view_name }| ).
    ENDIF.

    io_callback->deactivate_function( zif_uitb_c_gui_screen=>c_functions-save ).
    DATA(lt_fkey_map) = VALUE zif_uitb_ty_gui_screen=>ty_t_fkey_map(
        ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f1       mapped_function = c_functions-focus_on_tree               text = |{ 'Set Focus to Tree' }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f4       mapped_function = c_functions-expand_all                  text = |{ 'Expand all nodes' }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f4 mapped_function = c_functions-collapse_all                text = |{ 'Collapse all nodes' }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f5            mapped_function = c_functions-exec_with_dbbrs             text = |{ 'Show DB Content'(046) }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f6            mapped_function = c_functions-exec_with_dbbrs_new_window  text = |{ 'Show DB Content in new Task'(048) }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f7            mapped_function = c_functions-show_ddl_source             text = |{ 'Show CDS Source Code'(044) }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f9            mapped_function = c_functions-show_metrics                text = |{ 'Show Complexity Metrics'(032) }| )
    ).

    IF mo_splitter IS BOUND  AND
       mo_splitter->is_element_visible( 2 ).

      lt_fkey_map = VALUE #( BASE lt_fkey_map
        ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f5       mapped_function = c_functions-maximize_code_view          text = |{ 'Maximize Code Viewer'(050) }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f5 mapped_function = c_functions-close_code_view             text = |{ 'Close Code Viewer'(051) }| )
      ).
    ENDIF.

    io_callback->map_fkey_functions( lt_fkey_map ).

    IF io_callback->is_first_screen_call( ).
      zcl_uitb_screen_util=>set_function_code( c_fill_text_func ).
      mo_tree->zif_uitb_gui_control~focus( ).
    ENDIF.
  ENDMETHOD.

  METHOD create_code_viewer.
    create_control_toolbar(
      EXPORTING
        io_parent = io_container
        it_button = VALUE #(
          ( function = c_functions-close_code_view
            icon     = icon_close
            quickinfo = |{ 'Close Code Viewer'(051) }| )
          ( function = c_functions-maximize_code_view
            icon     = icon_view_expand_horizontal
            quickinfo = |{ 'Maximize Code Viewer'(050) }| )
        )
      IMPORTING
        eo_toolbar = DATA(lo_toolbar)
        eo_client  = DATA(lo_container)
    ).
    IF mv_theme IS INITIAL.
      mv_theme = CAST zdbbr_user_settings_a( zcl_dbbr_usersettings_factory=>get_current_settings( ) )->code_viewer_theme.
    ENDIF.
    mo_code_view = NEW zcl_uitb_abap_code_view(
      io_parent_container = lo_container
      iv_theme            = mv_theme
    ).
  ENDMETHOD.


  METHOD create_tree.
*.. Create switch for to toggle between tree and ALV on the left side
    mo_switch = NEW zcl_uitb_gui_switch_container(
      io_parent   = io_container
    ).

    create_control_toolbar(
      EXPORTING
        io_parent    = mo_switch->add_child( iv_id = 'TREE')
        it_button    = VALUE #(
          ( function  = c_functions-expand_all
            quickinfo = |{ 'Expand all nodes' }|
            icon      = icon_expand_all )
          ( function  = c_functions-collapse_all
            quickinfo = |{ 'Collapse all nodes' }|
            icon      = icon_collapse_all )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-show_ddl_source
            quickinfo = |{ 'Show CDS Source Code'(044) }|
            icon      = icon_abap
            text      = |{ 'CDS Source'(045) }| )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-exec_with_dbbrs
            quickinfo = |{ 'Show DB Content'(046) }|
            icon      = icon_table_settings
            text      = |{ 'Content'(047) }|             )
          ( function  = c_functions-exec_with_dbbrs_new_window
            quickinfo = |{ 'Show DB Content in new Task'(048) }|
            icon      = icon_table_settings
            text      = |{ 'Content (New Task)'(049) }|          )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-toggle_alv
            text      = |{ 'Analyze Usages' }|
            icon      = icon_active_inactive )
          ( function  = c_functions-show_metrics
            quickinfo = |{ 'Show Complexity Metrics'(032) }|
            icon      = icon_calculation
            text      = |{ 'Metrics'(033) }|                )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-options_menu
            butn_type = cntb_btype_menu
            text      = |{ 'Options'(052) }|
            quickinfo = |{ 'Show options menu'(053) }|
            icon      = icon_settings )
        )
      IMPORTING
        eo_toolbar   = DATA(mo_toolbar)
        eo_client    = DATA(lo_left_container)
    ).

    modify_options_ctx_menu( ).

    mo_toolbar->set_static_ctxmenu(
      EXPORTING
        fcode   = c_functions-options_menu
        ctxmenu = mo_options_menu
      EXCEPTIONS
        OTHERS  = 1 ).

    mo_tree = NEW zcl_uitb_column_tree_model(
      ir_parent           = lo_left_container
      is_hierarchy_header = VALUE #(
        heading = 'Entity Name'(035)
        width   = 110
      )
      if_auto_node_key    = abap_true
      iv_selection_mode   = cl_tree_model=>node_sel_mode_multiple
    ).

    SET HANDLER:
      on_node_context_menu_request FOR mo_tree->get_events( ),
      on_node_context_menu_select FOR mo_tree->get_events( ),
      on_node_double_click FOR mo_tree->get_events( ).

*.. Create further columns
    DATA(lo_columns) = mo_tree->get_columns( ).
    lo_columns->add_hierarchy_column( c_hierarchy_node2 ).
    lo_columns->add_column(
        iv_colname        = c_relation_col
        iv_width          = 20
        iv_header_text    = |{ 'SQL Relation'(034) }|
    ).
    lo_columns->add_column(
        iv_colname        = c_sql_name_col
        iv_width          = 40
        iv_header_text    = |{ 'SQL Name'(001) }|
    ).
    lo_columns->add_column(
        iv_colname        = c_object_type_col
        iv_width          = 30
        iv_header_text    = |{ 'Object Type'(036) }|
    ).

    zcl_uitb_screen_util=>show_progress( iv_progress = 50 iv_text = |{ 'Building Dependency Tree...'(003) }| ).
    add_node(
      is_node = ms_dependencies
    ).

    mo_tree->get_nodes( )->expand_root_nodes( ).

    mo_tree->create_tree_control( ).
    mo_tree->get_nodes( )->set_first_root_node_as_top( ).
  ENDMETHOD.

  METHOD adjust_dbl_click_menu.
    ro_menu = NEW cl_ctmenu( ).

    ro_menu->add_function(
        fcode     = |{ c_dbl_click_action-function_prefix }{ c_dbl_click_action-open_in_db_browser }|
        text      = |{ 'Open with DB Browser' }|
        checked   = xsdbool( mv_dbl_click_action = c_dbl_click_action-open_in_db_browser )
    ).
    ro_menu->add_function(
        fcode     = |{ c_dbl_click_action-function_prefix }{ c_dbl_click_action-adt_navigation }|
        text      = |{ TEXT-042 }|
        checked   = xsdbool( mv_dbl_click_action = c_dbl_click_action-adt_navigation )
    ).
    ro_menu->add_separator( ).
    ro_menu->add_function(
        fcode     = |{ c_dbl_click_action-function_prefix }{ c_dbl_click_action-exec_with_dbbrs_new_window }|
        text      = |{ TEXT-039 }|
        checked   = xsdbool( mv_dbl_click_action = c_dbl_click_action-exec_with_dbbrs_new_window )
    ).
    ro_menu->add_function(
        fcode     = |{ c_dbl_click_action-function_prefix }{ c_dbl_click_action-exec_with_dbbrs }|
        text      = |{ TEXT-040 }|
        checked   = xsdbool( mv_dbl_click_action = c_dbl_click_action-exec_with_dbbrs )
    ).
  ENDMETHOD.

  METHOD modify_options_ctx_menu.
    IF mo_options_menu IS INITIAL.
      mo_options_menu = NEW #( ).
    ENDIF.

    mo_options_menu->clear( ).

    DATA(lo_double_click_setting_menu) = adjust_dbl_click_menu( ).
    mo_options_menu->add_submenu(
        menu        = lo_double_click_setting_menu
        text        = |{ 'Action on Node double click' }|
    ).
  ENDMETHOD.


  METHOD fetch_dependencies.
    rf_dependencies_read = abap_true.

    zcl_uitb_screen_util=>show_progress( iv_progress = 1 iv_text = |{ 'Analyzing CDS Dependencies...'(004) }| ).
    ms_dependencies = zcl_sat_cds_dep_analyzer=>analyze_dependency( iv_cds_view_name = mv_view_name ).
    IF ms_dependencies IS INITIAL.
      rf_dependencies_read = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD fill_entity_texts.
    zcl_uitb_screen_util=>show_progress( iv_progress = 75 iv_text = |{ 'Loading Descriptions...'(037) }| ).

    SELECT entity, description
      FROM zsat_i_databaseentity
      FOR ALL ENTRIES IN @mt_node_map
      WHERE entity = @mt_node_map-entity_id
    INTO TABLE @DATA(lt_texts).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lo_nodes) = mo_tree->get_nodes( ).

    LOOP AT mt_node_map ASSIGNING FIELD-SYMBOL(<ls_node_map>).
      ASSIGN lt_texts[ entity = <ls_node_map>-entity_id ] TO FIELD-SYMBOL(<ls_text>).
      CHECK sy-subrc = 0.

      DATA(lo_node) = lo_nodes->get_node( <ls_node_map>-node_key ).
      lo_node->get_item( c_hierarchy_node2 )->set_text( |{ <ls_text>-description }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_icon_for_node.
    rv_icon = SWITCH #(
      iv_type
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-table THEN zif_dbbr_c_icon=>database_table
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-cds_view THEN zif_dbbr_c_icon=>cds_view
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-cds_db_view THEN zif_dbbr_c_icon=>cds_view
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-view THEN zif_dbbr_c_icon=>database_view
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-cds_table_function THEN zif_dbbr_c_icon=>table_settings
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-external_view THEN zif_dbbr_c_icon=>external_source
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-select THEN ''
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-union THEN zif_dbbr_c_icon=>union
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-union_all THEN zif_dbbr_c_icon=>union
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-result THEN zif_dbbr_c_icon=>sql_result ).
  ENDMETHOD.


  METHOD get_object_type_text.
    CASE iv_constant.

      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-table.
        rv_text = |{ 'Database Table (TABL)'(005) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-cds_view.
        rv_text = |{ 'CDS View (STOB)'(006) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-cds_db_view.
        rv_text = |{ 'CDS Database View (VIEW)'(007) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-view.
        rv_text = |{ 'View (VIEW)'(008) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-cds_table_function.
        rv_text = |{ 'CDS Table Function'(009) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_node_type-external_view.
        rv_text = |{ 'External View'(010) }|.

    ENDCASE.
  ENDMETHOD.


  METHOD get_relation_text.
    CASE iv_constant.
      WHEN zcl_sat_cds_dep_analyzer=>c_relation_type-from.
        rv_text = |{ 'From'(014) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_relation_type-inner_join.
        rv_text = |{ 'Inner Join'(015) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_relation_type-left_outer_join.
        rv_text = |{ 'Left Outer Join'(016) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_relation_type-right_outer_join.
        rv_text = |{ 'Right Outer Join'(017) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_relation_type-select.
        rv_text = |{ 'Select'(011) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_relation_type-union.
        rv_text = |{ 'Union'(012) }|.
      WHEN zcl_sat_cds_dep_analyzer=>c_relation_type-union_all.
        rv_text = |{ 'Union All'(013) }|.
    ENDCASE.
  ENDMETHOD.

  METHOD load_cds_source_into_view.
    mo_code_view->show_document(
        iv_code = iv_code
    ).

    mo_splitter->set_element_visibility( iv_element = 2 if_visible = abap_true ).
  ENDMETHOD.

  METHOD on_node_context_menu_request.
    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    er_menu->add_function(
        fcode = c_functions-open_with_adt
        text  = |{ 'Open with ADT'(042) }|
    ).

    er_menu->add_function(
        fcode = c_functions-open_in_new_window
        text  = |{ 'Open in new Window'(041) }|
    ).
    er_menu->add_separator( ).
    er_menu->add_function(
        fcode = c_functions-exec_with_dbbrs
        text  = |{ 'Show Content'(040) }|
    ).
    er_menu->add_function(
        fcode = c_functions-exec_with_dbbrs_new_window
        text  = |{ 'Show Content (New Task)'(039) }|
    ).
    IF <ls_node_map>-entity_type = zif_sat_c_entity_type=>cds_view.
      er_menu->add_separator( ).
      er_menu->add_function(
          fcode = c_functions-show_ddl_source
          text  = |{ 'Show DDL Source Code'(043) }|
      ).
    ENDIF.

  ENDMETHOD.


  METHOD on_node_context_menu_select.
    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    CHECK sy-subrc = 0.

    CASE ev_fcode.

      WHEN c_functions-open_with_adt.
        open_with_adt( EXPORTING iv_entity_id = <ls_node_map>-entity_id
                                 iv_entity_type = <ls_node_map>-entity_type ).

      WHEN c_functions-open_in_new_window.
        open_db_browser_new_window( iv_entity_id = <ls_node_map>-entity_id
                                    iv_entity_type = <ls_node_map>-entity_type ).

      WHEN c_functions-exec_with_dbbrs_new_window.
        show_content(
            if_new_task    = abap_true
            iv_entity_id   = <ls_node_map>-entity_id
            iv_entity_type = <ls_node_map>-entity_type
        ).

      WHEN c_functions-exec_with_dbbrs.
        show_content(
            if_new_task    = abap_false
            iv_entity_id   = <ls_node_map>-entity_id
            iv_entity_type = <ls_node_map>-entity_type
        ).

      WHEN c_functions-show_ddl_source.
        show_cds_source( iv_cds_name = <ls_node_map>-entity_id ).
    ENDCASE.
  ENDMETHOD.


  METHOD on_node_double_click.
    DATA: lv_fcode TYPE ui_func.

    DATA(lr_node_map) = REF #( mt_node_map[ node_key = ev_node_key ] OPTIONAL ).
    IF lr_node_map IS INITIAL.
*.... if the node is just a folder toggle the expansion state

      RETURN.
    ENDIF.

    CASE mv_dbl_click_action.

      WHEN c_dbl_click_action-open_in_db_browser.
        leave_screen( ).

        zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
            iv_entity_id   = lr_node_map->entity_id
            iv_entity_type = lr_node_map->entity_type
        ).
        RETURN.

      WHEN c_dbl_click_action-exec_with_dbbrs.
        lv_fcode = c_functions-exec_with_dbbrs.

      WHEN c_dbl_click_action-exec_with_dbbrs_new_window.
        lv_fcode = c_functions-exec_with_dbbrs_new_window.

      WHEN c_dbl_click_action-adt_navigation.
        open_with_adt(
            iv_entity_id   = lr_node_map->entity_id
            iv_entity_type = lr_node_map->entity_type
        ).
    ENDCASE.

    IF lv_fcode IS NOT INITIAL.
      on_node_context_menu_select(
          ev_fcode    = lv_fcode
          ev_node_key = ev_node_key
      ).
    ENDIF.

  ENDMETHOD.


  METHOD do_show_cds_source.
    IF ir_params IS BOUND.
      ASSIGN CAST lty_s_command_info( ir_params )->* TO FIELD-SYMBOL(<ls_command_info>).
      IF sy-subrc = 0 AND <ls_command_info>-entity_type = zif_sat_c_entity_type=>cds_view.
        show_cds_source( <ls_command_info>-entity_id ).
      ENDIF.
    ELSEIF mf_tree_mode = abap_false.
      mo_usage_alv->get_selected_entity( IMPORTING ev_entity_id   = DATA(lv_alv_entity_id)
                                                   ef_is_cds      = DATA(lf_is_cds) ).
      CHECK lf_is_cds = abap_true.
      show_cds_source( lv_alv_entity_id ).
    ELSE.
      DATA(lt_selected_nodes) = mo_tree->get_selections( )->get_selected_nodes( ).
      CHECK lines( lt_selected_nodes ) = 1.

      ASSIGN mt_node_map[ node_key    = lt_selected_nodes[ 1 ]->mv_node_key
                          entity_type = zif_sat_c_entity_type=>cds_view ] TO FIELD-SYMBOL(<ls_node_map>).
      CHECK sy-subrc = 0.
      on_node_context_menu_select(
          ev_fcode    = c_functions-show_ddl_source
          ev_node_key = <ls_node_map>-node_key
      ).
      show_cds_source( iv_cds_name = <ls_node_map>-entity_id ).
    ENDIF.
  ENDMETHOD.

  METHOD show_cds_source.
    TRY.
        DATA(lv_source) = zcl_sat_cds_view_factory=>read_ddls_source( iv_cds_name ).
        load_cds_source_into_view( EXPORTING iv_code = lv_source ).
      CATCH zcx_sat_application_exc INTO DATA(lx_app_error).
        lx_app_error->zif_sat_exception_message~print( ).
    ENDTRY.
  ENDMETHOD.


  METHOD show_complexity_metrics.
    IF ms_metrics_map IS INITIAL.
      zcl_uitb_screen_util=>show_progress(
        iv_progress = 1
        iv_text = |{ 'Calculating Metrics...'(038) }| ).

      DATA(lo_metrics_calculator) = get_metrics_calculator( ).
      lo_metrics_calculator->visitddlsource( iv_dsname = to_upper( mv_view_name ) ).
      ms_metrics_map = lo_metrics_calculator->getnumbermap( ).
    ENDIF.

    ASSIGN ms_metrics_map[ entity = to_upper( mv_view_name ) ]-numbers TO FIELD-SYMBOL(<ls_metrics>).
    CHECK sy-subrc = 0.

    zcl_uitb_popup_help_viewer=>create(
      iv_title        = |{ 'Complexity Metrics for'(018) } { mv_view_name }|
      iv_style        = |td:first-child \{ width: 200px; \} td:nth-child(2) | &&
                        |\{ color: Gray; width: 50px; text-align: right; \}|
      ir_html_content = NEW zcl_uitb_html_content(
        )->add_heading(
          iv_text = |{ 'Used Data Sources'(019) }|
        )->start_table(
        )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'Database tables:'(020) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numtabs NUMBER = USER }|
        )->end_table_row( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'Database views:'(021) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numviews NUMBER = USER }|
        )->end_table_row( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'CDS views with parameters'(022) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numviewswithparamter NUMBER = USER }|
        )->end_table_row( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'CDS table functions'(023) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numtablefunctions NUMBER = USER }|
        )->end_table_row( )->end_table(
        )->add_heading(
          iv_text = |{ 'SQL Operations'(024) }|
        )->start_table( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'JOIN Operations:'(025) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numjoins NUMBER = USER }|
        )->end_table_row( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'UNION Operations:'(026) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numunions NUMBER = USER }|
        )->end_table_row( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'GROUP BY clauses'(027) }|
        )->add_table_body_cell(
          iv_text = |{ <ls_metrics>-numgroupby NUMBER = USER }|
        )->end_table_row( )->end_table(
        )->add_heading(
          iv_text = |{ 'Performance Related Function Calls and Operations'(028) }|
        )->start_table( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'Function calls:'(029) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numfunctioncalls NUMBER = USER }|
        )->end_table_row( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'Explicit type CAST operations'(030) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numexplicitcasts NUMBER = USER }|
        )->end_table_row( )->start_table_row(
        )->add_table_body_cell(
          iv_text =   |{ 'CASE expressions:'(031) }|
        )->add_table_body_cell(
          iv_text =   |{ <ls_metrics>-numcase NUMBER = USER }|
        )->end_table_row( )->end_table( )
    )->show(
    ).
  ENDMETHOD.


  METHOD do_show_content.
    IF ir_params IS BOUND.
      ASSIGN CAST lty_s_command_info( ir_params )->* TO FIELD-SYMBOL(<ls_command_info>).
      IF sy-subrc = 0.
        show_content(
            if_new_task    = if_new_task
            iv_entity_id   = <ls_command_info>-entity_id
            iv_entity_type = <ls_command_info>-entity_type
        ).
      ENDIF.
    ELSEIF mf_tree_mode = abap_false.
      mo_usage_alv->get_selected_entity( IMPORTING ev_entity_id   = DATA(lv_alv_entity_id)
                                                   ev_entity_type = DATA(lv_alv_entity_type) ).
      show_content(
          if_new_task    = if_new_task
          iv_entity_id   = lv_alv_entity_id
          iv_entity_type = lv_alv_entity_type
      ).
    ELSE.
      DATA(lt_selected_nodes) = mo_tree->get_selections( )->get_selected_nodes( ).
      CHECK lines( lt_selected_nodes ) = 1.

      ASSIGN mt_node_map[ node_key = lt_selected_nodes[ 1 ]->mv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
      CHECK sy-subrc = 0.
      show_content(
          if_new_task    = if_new_task
          iv_entity_id   = <ls_node_map>-entity_id
          iv_entity_type = <ls_node_map>-entity_type
      ).
    ENDIF.
  ENDMETHOD.

  METHOD show_content.
    IF if_new_task = abap_true.
      CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_EXEC_JUMP'
        EXPORTING
          iv_entity_id       = iv_entity_id
          iv_entity_type     = iv_entity_type
          if_skip_selscreen  = abap_true
          if_load_parameters = abap_true.
    ELSE.
      DATA(lo_variant_starter) = zcl_dbbr_variant_starter_fac=>create_variant_starter(
          iv_variant_id        = zif_dbbr_c_global=>c_dummy_variant
          iv_entity_type       = iv_entity_type
          iv_variant_entity_id = CONV #( iv_entity_id )
      ).

      lo_variant_starter->initialize( ).
      TRY.
          lo_variant_starter->execute_variant( ).
        CATCH zcx_dbbr_variant_error INTO DATA(lx_variant_error).
          lx_variant_error->show_message( iv_message_type = 'S' ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_command_handler~execute_command.
    CASE io_command->mv_function.

      WHEN c_functions-maximize_code_view.
        IF mo_splitter->is_element_visible( 1 ).
          mo_splitter->set_element_visibility( iv_element = 1 if_visible = abap_false ).
        ELSE.
          mo_splitter->set_element_visibility( iv_element = 1 if_visible = abap_true ).
        ENDIF.

      WHEN c_functions-close_code_view.
        mo_splitter->set_element_visibility( iv_element = 2 if_visible = abap_false ).
        IF NOT mo_splitter->is_element_visible( 1 ).
          mo_splitter->set_element_visibility( iv_element = 1 if_visible = abap_true ).
        ENDIF.

      WHEN c_functions-open_with_adt.
        do_open_with_adt( io_command->mr_params ).

      WHEN c_functions-toggle_tree.
        toggle_dependency_tree( ).
        zcl_uitb_screen_util=>set_function_code( ).

      WHEN c_functions-toggle_alv.
        toggle_usage_alv( ).
        zcl_uitb_screen_util=>set_function_code( ).

      WHEN c_functions-focus_on_tree.
        mo_tree->zif_uitb_gui_control~focus( ).

      WHEN c_fill_text_func.
        fill_entity_texts( ).

      WHEN c_functions-show_metrics.
        show_complexity_metrics( ).

      WHEN c_functions-show_ddl_source.
        do_show_cds_source( io_command->mr_params ).

      WHEN c_functions-open_in_new_window.
        do_open_in_new_window( io_command->mr_params ).

      WHEN c_functions-exec_with_dbbrs.
        do_show_content( ir_params = io_command->mr_params  ).

      WHEN c_functions-exec_with_dbbrs_new_window.
        do_show_content( if_new_task = abap_true
                      ir_params   = io_command->mr_params ).

      WHEN zif_uitb_c_gui_screen=>c_functions-search.
        IF mf_tree_mode = abap_true.
          DATA(ls_result) = mo_tree->get_search( )->find( ).
        ELSE.
          mo_usage_alv->set_alv_function( zif_uitb_c_alv_functions=>find ).
        ENDIF.

      WHEN zif_uitb_c_gui_screen=>c_functions-search_more.
        IF mf_tree_mode = abap_true.
          ls_result = mo_tree->get_search( )->find_next( ).
        ELSE.
          mo_usage_alv->set_alv_function( zif_uitb_c_alv_functions=>find_more ).
        ENDIF.

      WHEN zif_uitb_c_gui_screen=>c_functions-page_top.
        mo_tree->scroll_to( cl_tree_model=>scroll_home ).

      WHEN zif_uitb_c_gui_screen=>c_functions-page_up.
        mo_tree->scroll_to( cl_tree_model=>scroll_up_page ).

      WHEN zif_uitb_c_gui_screen=>c_functions-page_down.
        mo_tree->scroll_to( cl_tree_model=>scroll_down_page ).

      WHEN zif_uitb_c_gui_screen=>c_functions-page_bottom.
        mo_tree->scroll_to( cl_tree_model=>scroll_end ).

      WHEN c_functions-collapse_all.
        mo_tree->get_nodes( )->collapse_selected_nodes( ).

      WHEN c_functions-expand_all.
        mo_tree->get_nodes( )->expand_selected_nodes( ).

      WHEN OTHERS.
        IF io_command->mv_function CP |{ c_dbl_click_action-function_prefix }*|.
          DATA(lv_new_action) = CONV zdbbr_action(
            substring( val = io_command->mv_function off = strlen( c_dbl_click_action-function_prefix ) )
          ).
          update_settings( EXPORTING iv_dbl_click_action = lv_new_action ).
        ENDIF.

    ENDCASE.

    IF ls_result IS NOT INITIAL.
      mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_screen~show.
    CHECK fetch_dependencies( ).

    super->show( ).
  ENDMETHOD.


  METHOD update_settings.
    IF iv_dbl_click_action <> mv_dbl_click_action.
      mv_dbl_click_action = iv_dbl_click_action.
      DATA(ls_settings) = zcl_dbbr_usersettings_factory=>get_settings( ).
      ls_settings-dep_tree_dbl_click_action = mv_dbl_click_action.
      zcl_dbbr_usersettings_factory=>save_settings( ls_settings ).
      MESSAGE s093(zdbbr_info).
      modify_options_ctx_menu( ).

*.... Update global DB Browser settings as well
      CAST zdbbr_user_settings_a( zcl_dbbr_usersettings_factory=>get_current_settings( ) )->dep_tree_dbl_click_action = mv_dbl_click_action.
    ENDIF.
  ENDMETHOD.


  METHOD toggle_usage_alv.
    mf_tree_mode = abap_false.

    IF mo_usage_alv IS INITIAL.
      zcl_uitb_screen_util=>show_progress( iv_progress = 1 iv_text = |{ 'Analyzing Usages' }| ).
      mo_usage_alv = NEW #(
        io_dependency_tree = me
        io_parent          = mo_switch->add_child( iv_id = 'ALV' )
        iv_cds_view_name   = mv_view_name
      ).
    ENDIF.

    mo_switch->set_child_visible( iv_id = 'TREE' if_visible = abap_false ).
    mo_switch->set_child_visible( iv_id = 'ALV' if_visible = abap_true ).

  ENDMETHOD.



  METHOD toggle_dependency_tree.
    mf_tree_mode = abap_true.
    mo_switch->set_child_visible( iv_id = 'TREE' if_visible = abap_true ).
    mo_switch->set_child_visible( iv_id = 'ALV' if_visible = abap_false ).
  ENDMETHOD.


  METHOD open_with_adt.
    TRY.
        zcl_sat_adt_util=>jump_adt(
            iv_obj_name     = |{ iv_entity_id }|
            iv_obj_type     = SWITCH #(
                iv_entity_type
                WHEN zif_sat_c_entity_type=>cds_view THEN 'DDLS'
                WHEN zif_sat_c_entity_type=>table    THEN 'TABD'
                WHEN zif_sat_c_entity_type=>view     THEN 'VIEW'
            )
        ).
      CATCH zcx_sat_adt_error INTO DATA(lx_adt_error).
      CATCH zcx_sat_data_read_error INTO DATA(lx_data_read_error).
    ENDTRY.

  ENDMETHOD.

  METHOD do_open_with_adt.
    IF ir_params IS BOUND.
      ASSIGN CAST lty_s_command_info( ir_params )->* TO FIELD-SYMBOL(<ls_command_info>).
      IF sy-subrc = 0.
        open_with_adt(
            iv_entity_id   = <ls_command_info>-entity_id
            iv_entity_type = <ls_command_info>-entity_type
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD do_open_in_new_window.
    IF ir_params IS BOUND.
      ASSIGN CAST lty_s_command_info( ir_params )->* TO FIELD-SYMBOL(<ls_command_info>).
      IF sy-subrc = 0.
        open_db_browser_new_window(
            iv_entity_id   = <ls_command_info>-entity_id
            iv_entity_type = <ls_command_info>-entity_type
        ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD open_db_browser_new_window.
    CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_OPEN_JUMP'
      EXPORTING
        iv_entity_id       = iv_entity_id
        iv_entity_type     = iv_entity_type
        if_load_parameters = abap_true.
  ENDMETHOD.


  METHOD get_metrics_calculator.
    IF gs_metrics_settings-settings_loaded = abap_false.
      DATA(lo_metrics_coll_typeref) = CAST cl_abap_classdescr(
        cl_abap_typedescr=>describe_by_name( c_metrics_provider_class ) ).
      gs_metrics_settings-has_bitmask_param =
        xsdbool( line_exists( lo_metrics_coll_typeref->methods[
          name = 'CONSTRUCTOR' ]-parameters[
            name = 'BITMASK' ] ) ).
      gs_metrics_settings-settings_loaded = abap_true.
    ENDIF.

    IF gs_metrics_settings-has_bitmask_param = abap_true.
      CREATE OBJECT ro_metrics_calculator TYPE (c_metrics_provider_class)
        EXPORTING
          descend = abap_true
          bitmask = c_check_mask-incl_associations.
    ELSE.
      CREATE OBJECT ro_metrics_calculator TYPE (c_metrics_provider_class)
        EXPORTING
          descend = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
