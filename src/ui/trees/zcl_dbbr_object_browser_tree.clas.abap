"! <p class="shorttext synchronized" lang="en">Repository tree for Navigator</p>
CLASS zcl_dbbr_object_browser_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_gui_control .
    INTERFACES zif_uitb_gui_view.
    INTERFACES zif_uitb_content_searcher .
    INTERFACES zif_dbbr_c_object_browser.
    INTERFACES zif_sat_c_object_search .

    ALIASES c_node_type
      FOR zif_dbbr_c_object_browser~c_tree_node_type .
    ALIASES c_search_option
      FOR zif_sat_c_object_search~c_search_option .
    ALIASES focus
      FOR zif_uitb_gui_control~focus .
    ALIASES has_focus
      FOR zif_uitb_gui_control~has_focus .

    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        io_parent      TYPE REF TO cl_gui_container
        io_parent_view TYPE REF TO zif_uitb_gui_composite_view.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_user_data,
        entity_id     TYPE zsat_entity_id,
        entity_id_raw TYPE zsat_entity_id,
        node_type     TYPE i,
        devclass      TYPE devclass,
        owner         TYPE username,
      END OF ty_s_user_data.

    TYPES:
      BEGIN OF ty_s_fav_func_query_map,
        function TYPE ui_func,
        query    TYPE string,
        type     TYPE zdbbr_obj_browser_mode,
      END OF ty_s_fav_func_query_map.

    TYPES:
      BEGIN OF ty_s_history,
        id         TYPE ui_func,
        query      TYPE REF TO zif_sat_object_search_query,
        is_current TYPE abap_bool,
      END OF ty_s_history.
    TYPES: ty_t_history TYPE STANDARD TABLE OF ty_s_history.

    DATA mt_history_stack TYPE ty_t_history.
    DATA mt_fav_func_query_map TYPE STANDARD TABLE OF ty_s_fav_func_query_map.
    DATA mf_use_and_for_filter_opt TYPE abap_bool.

    CONSTANTS: c_max_history TYPE i VALUE 25.
    CONSTANTS c_text_class TYPE i VALUE cl_item_tree_model=>item_class_text.

    CONSTANTS:
      BEGIN OF c_functions,
        open_with_adt              TYPE ui_func VALUE 'ADTJUMP',
        show_help                  TYPE ui_func VALUE 'HELP',
        search                     TYPE ui_func VALUE 'SEARCH',
        open_in_new_window         TYPE ui_func VALUE 'OPENINDBBRSNEWWIN',
        exec_with_dbbrs            TYPE ui_func VALUE 'EXECWIHTDBBRS',
        exec_with_dbbrs_new_window TYPE ui_func VALUE 'EXECWIHTDBBRSNEW',
        add_favorite               TYPE ui_func VALUE 'ADDFAV' ##NO_TEXT,
        edit_favorites             TYPE ui_func VALUE 'EDITFAV' ##NO_TEXT,
        to_parent                  TYPE ui_func VALUE 'TO_PARENT' ##NO_TEXT,
        favorite_dropdown          TYPE ui_func VALUE 'FAVMENU' ##NO_TEXT,
        use_and_instead_of_or      TYPE ui_func VALUE 'USEAND' ##NO_TEXT,
        search_settings_menu       TYPE ui_func VALUE 'SEARCHSETTINGS_M' ##NO_TEXT,
        show_ddl_source            TYPE ui_func VALUE 'SHOWSOURCE' ##no_text,
        analyze_dependencies       TYPE ui_func VALUE 'ANALYZEDEP' ##no_text,
        my_cds_views_search        TYPE ui_func VALUE 'MY_CDSVIEWS',
        my_db_tables_views_search  TYPE ui_func VALUE 'MY_DB_TABLES_VIEWS_SEARCH',
        my_queries_search          TYPE ui_func VALUE 'MY_QUERIES_SEARCH',
        expand_all                 TYPE ui_func VALUE 'EXPAND_ALL',
        collapse_all               TYPE ui_func VALUE 'COLLAPSE_ALL',
        previous_history           TYPE ui_func VALUE 'PREVIOUS_HISTORY',
        next_history               TYPE ui_func VALUE 'NEXT_HISTORY',
        find                       TYPE ui_func VALUE 'FIND',
        find_next                  TYPE ui_func VALUE 'FIND_NEXT',
        import_queries             TYPE ui_func VALUE 'IMPORT_QUERIES',
        export_queries             TYPE ui_func VALUE 'EXPORT_QUERIES',
        used_in_select_from        TYPE ui_func VALUE 'USED_IN_FROM',
        used_as_assoc              TYPE ui_func VALUE 'USED_AS_ASSOC',
        add_package_to_query       TYPE ui_func VALUE 'ADD_PACK_TO_QUERY',
        add_root_package_to_query  TYPE ui_func VALUE 'ADD_ROOT_PACK_TO_QUERY',
      END OF c_functions .
    CONSTANTS c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT.
    CONSTANTS c_hierarchy_node3 TYPE tv_itmname VALUE 'HIER3' ##NO_TEXT.
    CONSTANTS c_top_node TYPE tm_nodekey VALUE '00001' ##NO_TEXT.
    TYPES:
      BEGIN OF ty_s_api_state,
        api_state   TYPE string,
        description TYPE string,
      END OF ty_s_api_state.
    CLASS-DATA gt_api_state_texts TYPE STANDARD TABLE OF ty_s_api_state.
    DATA mo_input_dd TYPE REF TO cl_dd_document .
    DATA mo_parent TYPE REF TO cl_gui_container .
    DATA mo_query_f TYPE REF TO zcl_dbbr_query_factory .
    DATA mo_search_input TYPE REF TO cl_dd_input_element .
    DATA mo_search_type_select TYPE REF TO cl_dd_select_element .
    DATA mo_splitter TYPE REF TO zcl_uitb_gui_splitter_cont.
    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model .
    DATA mo_html_control TYPE REF TO cl_gui_html_viewer.
    DATA mo_variant_f TYPE REF TO zcl_dbbr_variant_factory .
    DATA mo_parent_view TYPE REF TO zif_uitb_gui_composite_view.

    DATA:
      BEGIN OF ms_search_input,
        package             TYPE string,
        cds_view            TYPE string,
        database_table_view TYPE string,
        query               TYPE string,
      END OF ms_search_input .
    DATA mo_search_query TYPE REF TO zif_sat_object_search_query.
    DATA mv_current_search_type TYPE zdbbr_obj_browser_mode .
    DATA mo_favorite_dd_menu TYPE REF TO cl_ctmenu .
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar.
    DATA mv_theme TYPE zuitb_code_viewer_theme.
    DATA: mo_settings_menu TYPE REF TO cl_ctmenu.

    "! <p class="shorttext synchronized" lang="en">Clears tree of all nodes</p>
    "!
    METHODS clear_tree .
    "! <p class="shorttext synchronized" lang="en">Create sub nodes of database table/view</p>
    METHODS create_db_table_subnodes
      IMPORTING
        io_db_tab_view_node TYPE REF TO zcl_uitb_ctm_node
        is_entity           TYPE zsat_entity.
    "! <p class="shorttext synchronized" lang="en">Create tree node</p>
    METHODS create_node
      IMPORTING
        iv_parent_node     TYPE tm_nodekey OPTIONAL
        if_folder          TYPE abap_bool OPTIONAL
        if_expander        TYPE abap_bool OPTIONAL
        iv_hier1_item_text TYPE tm_itemtxt
        iv_image           TYPE tv_image OPTIONAL
        iv_expanded_image  TYPE tv_image OPTIONAL
        is_hier2_item      TYPE treemcitem OPTIONAL
        is_hier3_item      TYPE treemcitem OPTIONAL
        ir_user_data       TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(ro_node)     TYPE REF TO zcl_uitb_ctm_node.

    "! <p class="shorttext synchronized" lang="en">Create input controls for search type and entity</p>
    "! @parameter iv_initial_value | <p class="shorttext synchronized" lang="en">the initial value</p>
    METHODS create_input_dd
      IMPORTING
        !iv_initial_value TYPE string OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for multiple cds views</p>
    "!
    METHODS create_multiple_cds_view_tree
      IMPORTING
        !it_cds_view_header TYPE zsat_entity_t
        !io_parent_node     TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for multiple packages</p>
    "!
    METHODS create_multiple_db_tab_tree
      IMPORTING
        !it_table_definition TYPE zsat_entity_t
        !io_parent_node      TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for multiple packages</p>
    "!
    METHODS create_multiple_package_tree
      IMPORTING
        !it_package_def TYPE zcl_dbbr_package_factory=>tt_package .
    "! <p class="shorttext synchronized" lang="en">Create query sub tree</p>
    "!
    METHODS create_query_sub_tree
      IMPORTING
        !iv_query      TYPE zsat_query_name
        !iv_query_node TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Create query tree</p>
    "!
    METHODS create_query_tree
      IMPORTING
        !it_queries TYPE zsat_entity_t.
    "! <p class="shorttext synchronized" lang="en">Create tree for single cds view</p>
    "!
    METHODS create_single_cds_view_tree
      IMPORTING
        !io_cds_view TYPE REF TO zcl_sat_cds_view
        !io_node     TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for single table/view</p>
    "!
    METHODS create_single_db_tab_tree
      IMPORTING
        !is_table_info TYPE zsat_entity .
    "! <p class="shorttext synchronized" lang="en">Build Tree for a single package</p>
    "!
    METHODS create_single_package_tree
      IMPORTING
        !io_package TYPE REF TO if_package
        !io_node    TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Creates splitter</p>
    METHODS create_splitter .
    "! <p class="shorttext synchronized" lang="en">Create sub nodes for single CDS node</p>
    "!
    METHODS create_subnodes_for_cds
      IMPORTING
        !io_cds_view      TYPE REF TO zcl_sat_cds_view
        !io_cds_view_node TYPE REF TO zcl_uitb_ctm_node.
    "! <p class="shorttext synchronized" lang="en">Creates Tree</p>
    "!
    METHODS create_tree
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Expand a single CDS View</p>
    "!
    METHODS expand_cds_view_node
      IMPORTING
        !iv_cds_view TYPE zsat_cds_view_name
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand db tabs subnode of package node</p>
    "!
    METHODS expand_package_dbtabs_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand ddls subnode of package node</p>
    "!
    METHODS expand_package_ddls_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand the package</p>
    "!
    METHODS expand_package_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand views subnode of package node</p>
    "!
    METHODS expand_package_views_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand a single Query</p>
    "!
    METHODS expand_query_node
      IMPORTING
        !iv_query    TYPE zsat_entity_id
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand a single Database view</p>
    "!
    METHODS expand_view_node
      IMPORTING
        !iv_view     TYPE zsat_entity_id
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Create foreign key table nodes for the given table</p>
    METHODS expand_foreign_key_tables
      IMPORTING
        iv_tabname  TYPE tabname
        iv_node_key TYPE tm_nodekey.
    " <p class="shorttext synchronized" lang="en">Expands a single database table</p>
    METHODS expand_db_table
      IMPORTING
        iv_tabname  TYPE tabname
        iv_node_key TYPE tm_nodekey.
    "! <p class="shorttext synchronized" lang="en">Expands properties node of db table/view</p>
    METHODS expand_dbtab_view_properties
      IMPORTING
        iv_tabname   TYPE tabname
        iv_node_type TYPE i
        iv_node_key  TYPE tm_nodekey.
    "! <p class="shorttext synchronized" lang="en">Expand technical settings node of db table node</p>
    METHODS expand_tech_settings_node
      IMPORTING
        iv_tabname  TYPE tabname
        iv_node_key TYPE tm_nodekey.
    "! <p class="shorttext synchronized" lang="en">Fill favorites dropdown</p>
    "!
    METHODS fill_favorite_dd_menu .

    METHODS node_image_for_node_type
      IMPORTING
        !iv_node_type   TYPE i
      RETURNING
        VALUE(rv_image) TYPE tv_image .
    "! <p class="shorttext synchronized" lang="en">Handler for DISPLAY_OBJECT_LIST event</p>
    "!
    METHODS on_display_object_list
      FOR EVENT display_object_list OF zcl_dbbr_selscr_nav_events
      IMPORTING
        !ev_entity_id
        !ev_entity_type .
    "! <p class="shorttext synchronized" lang="en">Handler for when children are to be loaded lazily</p>
    "!
    METHODS on_expand_no_children
      FOR EVENT expand_no_children OF zif_uitb_tree_model_events
      IMPORTING
        !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for External object search request</p>
    "!
    METHODS on_external_object_search_req
      FOR EVENT object_search OF zcl_dbbr_selscr_nav_events
      IMPORTING
        ev_object_type
        ev_search_query
        ef_close_popup.
    "! <p class="shorttext synchronized" lang="en">Handler for requesting a context menu for a node</p>
    "!
    METHODS on_node_context_menu_request
      FOR EVENT node_context_menu_request OF zif_uitb_tree_model_events
      IMPORTING
        !er_menu
        !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for when the context menu entry was chosen</p>
    "!
    METHODS on_node_context_menu_select
      FOR EVENT node_context_menu_select OF zif_uitb_tree_model_events
      IMPORTING
        !ev_fcode
        !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for double click on node</p>
    "!
    METHODS on_node_double_click
      FOR EVENT node_double_click OF zif_uitb_tree_model_events
      IMPORTING
        !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for ENTER key press on node</p>
    "!
    METHODS on_node_enter_key
      FOR EVENT node_keypress OF zif_uitb_tree_model_events
      IMPORTING
        !ev_key
        !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for performing the search</p>
    "!
    METHODS on_perform_search
        FOR EVENT clicked OF cl_dd_button_element .
    "! <p class="shorttext synchronized" lang="en">Enter Handler for serach input</p>
    "!
    METHODS on_search_input_enter
        FOR EVENT entered OF cl_dd_input_element .
    "! <p class="shorttext synchronized" lang="en">Dropdown Selection changed handler</p>
    "!
    METHODS on_search_type_selected
        FOR EVENT selected OF cl_dd_select_element .
    "! <p class="shorttext synchronized" lang="en">Handler for pressed toolbar button</p>
    "!
    METHODS on_toolbar_button
      FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        fcode.
    "! <p class="shorttext synchronized" lang="en">Show superordinate tree of current object</p>
    "!
    METHODS show_superordinate_tree .
    "! <p class="shorttext synchronized" lang="en">Trigger a new search</p>
    "!
    METHODS trigger_new_search
      RETURNING
        VALUE(rf_success) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Update toolbar for tree</p>
    "!
    METHODS update_toolbar
      IMPORTING
        if_multiple_tree TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Trigger my object search for objects of current user</p>
    "!
    METHODS trigger_my_objects_search
      IMPORTING
        iv_fcode TYPE ui_func.
    "! <p class="shorttext synchronized" lang="en">Exports the selected queries</p>
    "!
    METHODS export_queries.

    "! <p class="shorttext synchronized" lang="en">Trigger query from history</p>
    "!
    METHODS show_historic_query.
    "! <p class="shorttext synchronized" lang="en">Adds new historic entry</p>
    "!
    METHODS add_history_entry.

    "! <p class="shorttext synchronized" lang="en">Update status of history navigation buttons</p>
    METHODS update_history_buttons.
    "! <p class="shorttext synchronized" lang="en">Navigate to previous history entry</p>
    METHODS go_to_previous_historic_entry.
    "! <p class="shorttext synchronized" lang="en">Navigate to next history entry</p>
    METHODS go_to_next_historic_entry.
    METHODS go_to_historic_entry
      IMPORTING
        iv_function TYPE ui_func.
    "! <p class="shorttext synchronized" lang="en">Edit search favorites of Object Browser</p>
    METHODS edit_favorites.
    "! <p class="shorttext synchronized" lang="en">Add current query to search favorites</p>
    METHODS add_favorite.
    "! <p class="shorttext synchronized" lang="en">Trigger query execution from favorite entry</p>
    METHODS trigger_query_from_favorite
      IMPORTING
        iv_function TYPE ui_func.
    "! <p class="shorttext synchronized" lang="en">Show result count message in status bar</p>
    METHODS show_results_message
      IMPORTING
        iv_result_count TYPE i
        iv_max_count    TYPE i.
    "! <p class="shorttext synchronized" lang="en">Check if the given node type is a package sub node</p>
    METHODS is_package_sub_node
      IMPORTING
        iv_node_type  TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Adapt html input and display doc</p>
    METHODS adapt_and_display_html_inp
      IMPORTING
        if_assign_parent TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Updates the search settings menu</p>
    METHODS update_search_settings_menu.
    METHODS parse_query
      IMPORTING
        iv_query        TYPE string
        iv_browser_mode TYPE zdbbr_obj_browser_mode
      RETURNING
        VALUE(ro_query) TYPE REF TO zif_sat_object_search_query
      RAISING
        zcx_sat_object_search.
    METHODS search_objects
      EXPORTING
        et_results TYPE zsat_entity_t
      RAISING
        zcx_sat_object_search.
    METHODS map_to_dbbr_query_type
      IMPORTING
        iv_sat_type            TYPE zif_sat_ty_object_search=>ty_search_type
      RETURNING
        VALUE(rv_browser_type) TYPE zdbbr_obj_browser_mode.
    METHODS map_to_sat_query_type
      IMPORTING
        iv_browser_type    TYPE zdbbr_obj_browser_mode
      RETURNING
        VALUE(rv_sat_type) TYPE zif_sat_ty_object_search=>ty_search_type.
ENDCLASS.



CLASS zcl_dbbr_object_browser_tree IMPLEMENTATION.


  METHOD class_constructor.
    DATA(lt_api_states) = cl_ris_adt_res_release_states=>get_all( ).
    gt_api_state_texts = VALUE #( FOR api IN lt_api_states ( api_state = api-name description = api-description )
                                  ( api_state = zif_sat_c_cds_api_state=>not_released description = 'Not Released'(045) ) ).
  ENDMETHOD.

  METHOD adapt_and_display_html_inp.
    DATA: lt_events TYPE cntl_simple_events.

    mo_input_dd->merge_document( ).

    LOOP AT mo_input_dd->html_table ASSIGNING FIELD-SYMBOL(<html>) WHERE line CS `<body`.
      <html>-line = replace( val = <html>-line sub = `<body` with = `<body scroll="no"` ).
      EXIT.
    ENDLOOP.

    IF if_assign_parent = abap_true.
      mo_html_control = NEW cl_gui_html_viewer(
        parent = mo_splitter->get_container( 1 )
      ).
      mo_input_dd->html_control = mo_html_control.
      mo_input_dd->html_control->set_ui_flag(
        cl_gui_html_viewer=>uiflag_no3dborder +
        cl_gui_html_viewer=>uiflag_noiemenu +
        cl_gui_html_viewer=>uiflag_use_sapgui_charset
      ).

      lt_events = VALUE #( ( eventid = mo_html_control->m_id_sapevent ) ).
      mo_html_control->set_registered_events( lt_events ).
    ENDIF.

    mo_input_dd->display_document(
        reuse_control      = abap_true
        reuse_registration = abap_true
    ).
  ENDMETHOD.

  METHOD clear_tree.
    mo_tree->get_nodes( )->delete_all_nodes( ).
***    CLEAR: mt_node_map.
  ENDMETHOD.


  METHOD constructor.
    mo_parent = io_parent.
    mo_parent_view = io_parent_view.
    mo_query_f = NEW #( ).
    create_splitter( ).

    TRY.
        create_tree( ).
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
        lx_tree_error->zif_uitb_exception_message~print(
            iv_msg_type = 'X'
        ).
    ENDTRY.
    create_input_dd( ).

    SET HANDLER:
        on_display_object_list,
        on_external_object_search_req.

    cl_gui_cfw=>flush( ).

    update_toolbar( ).
  ENDMETHOD.


  METHOD create_input_dd.
    mo_input_dd = NEW cl_dd_document( ).

*.. get global data for setting some initial values
    DATA(lo_s_global_data) = CAST zdbbr_global_data( zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main )->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).

    mo_input_dd->initialize_document(  no_margins = abap_true ).

*.. Fill the document
    mo_input_dd->add_form( IMPORTING formarea = DATA(lo_form) ).
    lo_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lo_form->add_select_element(
      EXPORTING
        tooltip        = |{ 'Object Category'(006) }|
        options        = VALUE #(
          ( value = zif_dbbr_c_object_browser_mode=>cds_view            text = 'CDS View'(002) )
          ( value = zif_dbbr_c_object_browser_mode=>database_table_view text = 'Database Table/View'(003) )
          ( value = zif_dbbr_c_object_browser_mode=>query               text = 'Query'(008) )
          ( value = zif_dbbr_c_object_browser_mode=>package             text = 'Package'(001) )
        )
      IMPORTING
        select_element = mo_search_type_select
    ).

    mo_search_type_select->set_value( COND #( WHEN lo_s_global_data->initial_obj_brws_mode IS NOT INITIAL THEN lo_s_global_data->initial_obj_brws_mode ELSE zif_dbbr_c_object_browser_mode=>cds_view ) ).
    mv_current_search_type = mo_search_type_select->value.

    lo_form->line_with_layout( end = abap_true ).
    lo_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lo_form->add_input_element(
      EXPORTING
        size          = 50    " Length of Input Field
        tooltip       = |{ 'Enter name of Entity'(004) }|
        maxlength     = 200    " Maximum Permitted Text Entry Length
      IMPORTING
        input_element = mo_search_input
    ).
    mo_search_input->set_value( |{ iv_initial_value }| ).

    lo_form->add_button(
      EXPORTING
        sap_icon = |ICON_DISPLAY|
        tooltip  = |{ 'Perform search'(007) }|
      IMPORTING
        button   = DATA(lo_search_button)
    ).

    lo_form->line_with_layout( end = abap_true ).

*.. Register event handlers for form elements
    SET HANDLER:
      on_search_input_enter FOR mo_search_input,
      on_perform_search FOR lo_search_button,
      on_search_type_selected FOR mo_search_type_select.

    adapt_and_display_html_inp( if_assign_parent = abap_true ).

  ENDMETHOD.


  METHOD create_node.
    DATA: ls_item TYPE treemcitem.

    DATA(lv_image) = COND #( WHEN iv_image IS INITIAL AND if_folder = abap_false THEN zif_dbbr_c_icon=>no_icon ELSE iv_image ).
    DATA(lv_expanded_image) = COND #( WHEN iv_expanded_image IS INITIAL THEN lv_image ELSE iv_expanded_image ).

    DATA(lt_items) = VALUE treemcitab(
       ( class     = cl_item_tree_model=>item_class_text
         item_name = mo_tree->c_hierarchy_column
         text      = iv_hier1_item_text )
    ).
    IF is_hier2_item IS NOT INITIAL.
      ls_item = is_hier2_item.
      ls_item-item_name = c_hierarchy_node2.
      IF ls_item-class = 0.
        ls_item-class = c_text_class.
      ENDIF.
      lt_items = VALUE #( BASE lt_items ( ls_item ) ).
    ENDIF.

    IF is_hier3_item IS NOT INITIAL.
      ls_item = is_hier3_item.
      ls_item-item_name = c_hierarchy_node3.
      IF ls_item-class = 0.
        ls_item-class = c_text_class.
      ENDIF.
      lt_items = VALUE #( BASE lt_items ( ls_item ) ).
    ENDIF.

    TRY.
        ro_node = mo_tree->get_nodes( )->add_node(
            if_folder            = if_folder
            iv_relative_node_key = iv_parent_node
            iv_image             = lv_image
            iv_expanded_image    = lv_expanded_image
            if_expander          = if_expander
            if_use_prop_font     = abap_true
            it_item_table        = lt_items
            ir_user_data         = ir_user_data
        ).
      CATCH zcx_uitb_tree_error INTO DATA(lx_error).
*...... Application cannot handle this exception so force exit
        lx_error->zif_uitb_exception_message~print(
            iv_msg_type = 'X'
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD create_multiple_cds_view_tree.
    CHECK it_cds_view_header IS NOT INITIAL.

    IF io_parent_node IS INITIAL.
      clear_tree( ).
    ENDIF.

*.. Create nodes/subnodes for multiple cds views
*... Only the top nodes of each view will be loaded at first
    DATA(lo_nodes) = mo_tree->get_nodes( ).

    LOOP AT it_cds_view_header ASSIGNING FIELD-SYMBOL(<ls_cds_header>).
      DATA(lo_cds_view_node) = create_node(
          if_folder            = abap_true
          iv_parent_node       = COND #( WHEN io_parent_node IS NOT INITIAL THEN io_parent_node->mv_node_key )
          iv_image             = zif_dbbr_c_icon=>cds_view
          if_expander          = abap_true
          iv_hier1_item_text   = |{ <ls_cds_header>-entity_id_raw }|
          is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = <ls_cds_header>-description )
          ir_user_data  = NEW ty_s_user_data(
              entity_id     = <ls_cds_header>-entity_id
              entity_id_raw = <ls_cds_header>-entity_id_raw
              node_type     = c_node_type-cds_view
          )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD create_multiple_db_tab_tree.
    DATA: lv_node_image TYPE tv_image,
          lv_node_type  TYPE i.

    CHECK it_table_definition IS NOT INITIAL.

    IF io_parent_node IS INITIAL.
      clear_tree( ).
    ENDIF.

*.. Create nodes/subnodes for multiple tables/views
*... Only the top nodes of each view will be loaded at first
    DATA(lo_nodes) = mo_tree->get_nodes( ).

    LOOP AT it_table_definition ASSIGNING FIELD-SYMBOL(<ls_tabname>).
      IF <ls_tabname>-entity_type = zif_sat_c_entity_type=>view.
        lv_node_image = zif_dbbr_c_icon=>database_view.
        lv_node_type = c_node_type-view.
      ELSE.
        lv_node_image = zif_dbbr_c_icon=>database_table.
        lv_node_type = c_node_type-dbtable.
      ENDIF.

      DATA(lo_table_node) = create_node(
        iv_parent_node     = COND #( WHEN io_parent_node IS NOT INITIAL THEN io_parent_node->mv_node_key )
        if_folder          = abap_true
        if_expander        = abap_true
        iv_hier1_item_text = |{ <ls_tabname>-entity_id }|
        iv_image           = lv_node_image
        iv_expanded_image  = lv_node_image
        is_hier2_item      = VALUE #( text = <ls_tabname>-description style = zif_uitb_c_ctm_style=>inverted_gray )
        ir_user_data       = NEW ty_s_user_data(
          entity_id = <ls_tabname>-entity_id
          node_type = lv_node_type
        )
      ).

      create_db_table_subnodes(
        EXPORTING io_db_tab_view_node = lo_table_node
                  is_entity          = <ls_tabname>
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_db_table_subnodes.
    CHECK io_db_tab_view_node IS BOUND.

    DATA(lo_nodes) = mo_tree->get_nodes( ).

*.. Create properties nodes
    DATA(lo_properties_node) = create_node(
        iv_parent_node     = io_db_tab_view_node->mv_node_key
        if_folder          = abap_true
        if_expander        = abap_true
        iv_hier1_item_text = |{ TEXT-017 }|
        ir_user_data       = NEW ty_s_user_data(
           entity_id = is_entity-entity_id
           node_type = COND #( WHEN is_entity-entity_type = zif_sat_c_entity_type=>view THEN
                                 c_node_type-view_properties
                               ELSE c_node_type-dbtable_properties )
        )
    ).

    IF is_entity-entity_type = zif_sat_c_entity_type=>table.
      create_node(
        iv_parent_node     = io_db_tab_view_node->mv_node_key
        if_folder          = abap_true
        if_expander        = abap_true
        iv_hier1_item_text = |{ 'Technical Settings'(037) }|
        ir_user_data       = NEW ty_s_user_data(
          entity_id = is_entity-entity_id
          node_type = c_node_type-dbtable_tech_settings
        )
      ).
      create_node(
        iv_parent_node     = io_db_tab_view_node->mv_node_key
        if_folder          = abap_true
        if_expander        = abap_true
        iv_hier1_item_text = |{ 'Foreign Key Relations'(036) }|
        ir_user_data       = NEW ty_s_user_data(
          entity_id = is_entity-entity_id
          node_type = c_node_type-dbtable_foreign_key
        )
      ).
    ELSE.
      create_node(
        iv_parent_node     = io_db_tab_view_node->mv_node_key
        if_folder          = abap_true
        if_expander        = abap_true
        iv_hier1_item_text = |{ 'Base Tables'(038) }|
        ir_user_data       = NEW ty_s_user_data(
          entity_id = is_entity-entity_id
          node_type = c_node_type-view_tables
        )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD create_multiple_package_tree.
    CHECK it_package_def IS NOT INITIAL.

    clear_tree( ).

*.. Create nodes/subnodes for multiple packages
*... Only the top nodes of each view will be loaded at first
    DATA(lo_nodes) = mo_tree->get_nodes( ).

    LOOP AT it_package_def ASSIGNING FIELD-SYMBOL(<ls_package>).
      DATA(lo_package_node) = create_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>package
          if_expander          = abap_true
          iv_hier1_item_text   = |{ <ls_package>-package }|
          is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = <ls_package>-ddtext )
          ir_user_data         = NEW ty_s_user_data(
            entity_id = <ls_package>-package
            node_type = c_node_type-package
          )
      ).

    ENDLOOP.
  ENDMETHOD.


  METHOD create_query_sub_tree.
    expand_query_node(
        iv_query    = iv_query
        iv_node_key = iv_query_node
    ).
  ENDMETHOD.


  METHOD create_query_tree.
    clear_tree( ).

    DATA(lo_nodes) = mo_tree->get_nodes( ).
    LOOP AT it_queries ASSIGNING FIELD-SYMBOL(<ls_query>).
      DATA(lo_query_node) = create_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>query
          if_expander          = abap_true
          iv_hier1_item_text   = |{ <ls_query>-entity_id }|
          is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = <ls_query>-description )
          ir_user_data         = NEW ty_s_user_data(
            entity_id = <ls_query>-entity_id
            node_type = c_node_type-query
          )
      ).

      IF lines( it_queries ) = 1.
        create_query_sub_tree(
            iv_query      = <ls_query>-entity_id
            iv_query_node = lo_query_node->mv_node_key
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_single_cds_view_tree.
    DATA: lv_node_type  TYPE i,
          lv_node_image TYPE tv_image.

*.. Create node/subnodes for a single cds view
    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(lo_cds_view_node) = io_node.

*.. If node is bound, expand the node to load the sub entities
    IF lo_cds_view_node IS INITIAL.

      clear_tree( ).

      DATA(lv_cds_name_raw) = io_cds_view->get_header( )-entityname_raw.

      lo_cds_view_node = create_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>cds_view
          iv_hier1_item_text   = |{ lv_cds_name_raw }|
          is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = io_cds_view->get_description( ) )
          ir_user_data         = NEW ty_s_user_data(
            entity_id     = io_cds_view->mv_view_name
            entity_id_raw = lv_cds_name_raw
            node_type     = c_node_type-cds_view
          )
      ).
    ENDIF.

*.. Create Sub nodes
    create_subnodes_for_cds(
      io_cds_view      = io_cds_view
      io_cds_view_node = lo_cds_view_node
    ).

    lo_nodes->expand_node(
        iv_node_key = lo_cds_view_node->mv_node_key
    ).
  ENDMETHOD.


  METHOD create_single_db_tab_tree.
    DATA: lv_node_image TYPE tv_image,
          lv_node_type  TYPE i.

    clear_tree( ).

*.. Create nodes/subnodes for a single database table/view
    DATA(lo_nodes) = mo_tree->get_nodes( ).
    IF is_table_info-entity_type = zif_sat_c_entity_type=>view.
      lv_node_image = zif_dbbr_c_icon=>database_view.
      lv_node_type = c_node_type-view.
    ELSE.
      lv_node_image = zif_dbbr_c_icon=>database_table.
      lv_node_type = c_node_type-dbtable.
    ENDIF.

    DATA(lo_table_node) = create_node(
      if_folder            = abap_true
      iv_image             = lv_node_image
      iv_hier1_item_text   = |{ is_table_info-entity_id }|
      is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = is_table_info-description )
      ir_user_data         = NEW ty_s_user_data(
        entity_id = is_table_info-entity_id
        node_type = lv_node_type
      )
    ).

    create_db_table_subnodes(
        io_db_tab_view_node = lo_table_node
        is_entity           = is_table_info
    ).

    mo_tree->get_nodes( )->expand_node( lo_table_node->mv_node_key ).

  ENDMETHOD.


  METHOD create_single_package_tree.
    DATA(lo_nodes) = mo_tree->get_nodes( ).

    DATA(lo_package_node) = io_node.
    IF lo_package_node IS INITIAL.
      clear_tree( ).

*.. Create nodes/subnodes for a single cds view
      lo_package_node = create_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>package
          iv_hier1_item_text   = |{ io_package->package_name }|
          is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = io_package->short_text )
          ir_user_data         = NEW ty_s_user_data(
            entity_id = io_package->package_name
            node_type = c_node_type-package
          )
      ).
    ENDIF.

*.. Add sub nodes for subpackages, cds view, views and database tables
    io_package->get_sub_packages(
        IMPORTING  e_sub_packages = DATA(lt_sub_packages)
        EXCEPTIONS OTHERS = 1
    ).
    IF lt_sub_packages IS NOT INITIAL.
      LOOP AT lt_sub_packages ASSIGNING FIELD-SYMBOL(<lo_subpackage>).
        DATA(lo_subpackage_node) = create_node(
            if_folder            = abap_true
            iv_parent_node       = lo_package_node->mv_node_key
            iv_image             = zif_dbbr_c_icon=>package
            if_expander          = abap_true
            iv_hier1_item_text   = |{ <lo_subpackage>->package_name }|
            is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = <lo_subpackage>->short_text )
            ir_user_data         = NEW ty_s_user_data(
              entity_id = <lo_subpackage>->package_name
              node_type = c_node_type-package
            )
        ).
      ENDLOOP.
    ENDIF.

    io_package->get_elements( IMPORTING e_elements = DATA(lt_elements) EXCEPTIONS OTHERS = 1 ).
    IF lt_elements IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lf_has_ddls) = abap_false.

    LOOP AT lt_elements ASSIGNING FIELD-SYMBOL(<lo_element>).
      IF <lo_element>->dev_elem_type = 'DDLS'.
        lf_has_ddls = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lf_has_ddls = abap_true.
      DATA(lo_cdsviews_node) = create_node(
          if_folder            = abap_true
          iv_parent_node       = lo_package_node->mv_node_key
          if_expander          = abap_true
          iv_hier1_item_text   = |{ 'Data Definitions'(014) }|
          ir_user_data         = NEW ty_s_user_data(
            entity_id = io_package->package_name
            node_type = c_node_type-pak_ddl_folder
          )
      ).
    ENDIF.

*.. Check table/view count in package
    SELECT *
      FROM zsat_i_databaseentityaggr
      WHERE developmentpackage = @io_package->package_name
    INTO TABLE @DATA(lt_entity_count_in_package).

    IF line_exists( lt_entity_count_in_package[ type = zif_sat_c_entity_type=>view ] ).
      DATA(lo_views_node) = create_node(
          if_folder            = abap_true
          iv_parent_node       = lo_package_node->mv_node_key
          if_expander          = abap_true
          iv_hier1_item_text   = |{ 'Views'(020) }|
          ir_user_data         = NEW ty_s_user_data(
            entity_id = io_package->package_name
            node_type = c_node_type-pak_view_folder
          )
      ).
    ENDIF.


    IF line_exists( lt_entity_count_in_package[ type = zif_sat_c_entity_type=>table ] ).
      DATA(lo_database_tables_node) = create_node(
          if_folder            = abap_true
          iv_parent_node       = lo_package_node->mv_node_key
          if_expander          = abap_true
          iv_hier1_item_text   = |{ 'Database Tables'(021) }|
          ir_user_data         = NEW ty_s_user_data(
            entity_id = io_package->package_name
            node_type = c_node_type-pak_dtab_folder
          )
      ).
    ENDIF.

*... Expand top package node
    mo_tree->get_nodes( )->expand_node( lo_package_node->mv_node_key ).
  ENDMETHOD.


  METHOD create_splitter.
    mo_splitter = NEW zcl_uitb_gui_splitter_cont(
        iv_elements = 2
        iv_size     = '50:*'
        io_parent   = mo_parent
      )->set_all_sash_properties(
        if_visible = abap_false ).
  ENDMETHOD.


  METHOD create_subnodes_for_cds.

    DATA: lv_node_type              TYPE i,
          lo_api_states_parent_node TYPE REF TO zcl_uitb_ctm_node,
          lv_node_image             TYPE tv_image.

    DATA(lo_nodes) = mo_tree->get_nodes( ).

*.. Create "Properties" Sub node
    DATA(lo_cds_properties_node) = create_node(
       iv_parent_node     = io_cds_view_node->mv_node_key
       if_folder          = abap_true
       iv_hier1_item_text = |{ 'Properties'(017) }|
       ir_user_data       = NEW ty_s_user_data(
         entity_id = io_cds_view->mv_view_name
         node_type = c_node_type-cds_properties
       )
    ).

    DATA(ls_tadio_info) = io_cds_view->get_tadir_info( ).
    create_node(
       iv_parent_node     = lo_cds_properties_node->mv_node_key
       iv_image           = zif_dbbr_c_icon=>user_menu
       iv_hier1_item_text = |{ 'Responsible'(018) }|
       is_hier2_item      = VALUE #( text = ls_tadio_info-created_by style = zif_uitb_c_ctm_style=>inverted_blue )
       ir_user_data       = NEW ty_s_user_data(
          node_type = c_node_type-cds_owner_prop
          owner     = ls_tadio_info-created_by
       )
    ).
    create_node(
       iv_parent_node     = lo_cds_properties_node->mv_node_key
       iv_image           = zif_dbbr_c_icon=>date
       iv_hier1_item_text = |{ 'Created On'(033) }|
       is_hier2_item      = VALUE #( text = |{ ls_tadio_info-created_date DATE = USER }| style = zif_uitb_c_ctm_style=>inverted_blue )
    ).
    create_node(
       iv_parent_node     = lo_cds_properties_node->mv_node_key
       iv_image           = zif_dbbr_c_icon=>package
       iv_hier1_item_text = |{ 'Package'(001) }|
       is_hier2_item      = VALUE #( text = ls_tadio_info-devclass style = zif_uitb_c_ctm_style=>inverted_blue )
       ir_user_data       = NEW ty_s_user_data(
          node_type = c_node_type-cds_package_prop
          devclass  = ls_tadio_info-devclass
       )
    ).

*.. Insert nodes for found API states
*.. Create "API States" Sub node
    DATA(lt_api_states) = io_cds_view->get_api_states( ).
    IF lines( lt_api_states ) > 1.
      DATA(lo_cds_api_states_node) = create_node(
        iv_parent_node     = lo_cds_properties_node->mv_node_key
        if_folder          = abap_true
        iv_hier1_item_text = |{ 'API States'(019) }|
        ir_user_data       = NEW ty_s_user_data(
          entity_id = io_cds_view->mv_view_name
          node_type = c_node_type-cds_api_states
        )
      ).

      lo_api_states_parent_node = lo_cds_api_states_node.
    ELSE.
      lo_api_states_parent_node = lo_cds_properties_node.
    ENDIF.

    LOOP AT lt_api_states ASSIGNING FIELD-SYMBOL(<lv_api_state>).
      CASE <lv_api_state>.

        WHEN zif_sat_c_cds_api_state=>not_released.
          DATA(lf_not_released) = abap_true.
      ENDCASE.

      DATA(lv_icon) = COND tv_image( WHEN lf_not_released = abap_true THEN zif_dbbr_c_icon=>incomplete ELSE zif_dbbr_c_icon=>checked ).

      create_node(
        iv_parent_node     = lo_api_states_parent_node->mv_node_key
        iv_image           = lv_icon
        iv_hier1_item_text = |{ gt_api_state_texts[ api_state = <lv_api_state> ]-description }|
      ).
    ENDLOOP.

*.. Create "FROM PART" Sub node
    DATA(lt_base_tables) = io_cds_view->get_base_tables( ).
    IF lt_base_tables IS NOT INITIAL.
      DATA(lo_cds_select_node) = create_node(
        iv_parent_node     = io_cds_view_node->mv_node_key
        if_folder          = abap_true
        iv_hier1_item_text = |{ 'Select From'(012) }|
        ir_user_data         = NEW ty_s_user_data(
          entity_id = io_cds_view->mv_view_name
          node_type = c_node_type-cds_tables
        )
      ).

      LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
        lv_node_type = COND #(
          WHEN <ls_base_table>-table_kind = zif_sat_c_entity_type=>table AND
               <ls_base_table>-is_db_view = abap_true                           THEN c_node_type-view
          WHEN <ls_base_table>-table_kind = zif_sat_c_entity_type=>table AND
               <ls_base_table>-is_db_view = abap_false                          THEN c_node_type-dbtable
          WHEN <ls_base_table>-table_kind = zif_sat_c_entity_type=>cds_view    THEN c_node_type-cds_view
        ).
        lv_node_image = node_image_for_node_type( lv_node_type ).

        DATA(lo_cds_base_table_node) = create_node(
          iv_parent_node        = lo_cds_select_node->mv_node_key
          iv_image              = lv_node_image
          if_expander           = xsdbool( lv_node_type <> c_node_type-dbtable )
          if_folder             = abap_true
          iv_hier1_item_text    = |{ <ls_base_table>-entityname_raw }|
          is_hier2_item         = VALUE #( text = <ls_base_table>-description style = zif_uitb_c_ctm_style=>inverted_gray )
          ir_user_data         = NEW ty_s_user_data(
            entity_id     = <ls_base_table>-entityname
            entity_id_raw = <ls_base_table>-entityname_raw
            node_type     = lv_node_type
          )
        ).

        IF lv_node_type = c_node_type-dbtable.
          create_db_table_subnodes(
            io_db_tab_view_node = lo_cds_base_table_node
            is_entity           = VALUE #(
              entity_id     = <ls_base_table>-entityname
              entity_id_raw = <ls_base_table>-entityname_raw
              entity_type   = zif_sat_c_entity_type=>table
            )
          ).
        ENDIF.

      ENDLOOP.

    ENDIF.

*.. Create "Associations" Sub node
    DATA(lt_assoc) = io_cds_view->get_associations( ).
    IF lt_assoc IS NOT INITIAL.
      DATA(lo_cds_assoc_node) = create_node(
         if_folder            = abap_true
         iv_parent_node       = io_cds_view_node->mv_node_key
         iv_hier1_item_text   = |{ 'Associations'(013) }|
         ir_user_data         = NEW ty_s_user_data(
           entity_id = io_cds_view->mv_view_name
           node_type = c_node_type-cds_assocs
         )
      ).

      LOOP AT lt_assoc ASSIGNING FIELD-SYMBOL(<ls_assoc>).
        lv_node_type = SWITCH #( <ls_assoc>-kind
           WHEN zif_sat_c_cds_assoc_type=>entity OR
                zif_sat_c_cds_assoc_type=>table_function THEN c_node_type-cds_view
           WHEN zif_sat_c_cds_assoc_type=>table OR
                zif_sat_c_cds_assoc_type=>view           THEN c_node_type-dbtable
        ).
        lv_node_image = node_image_for_node_type( lv_node_type ).

        create_node(
            if_folder            = abap_true
            iv_parent_node       = lo_cds_assoc_node->mv_node_key
            if_expander          = abap_true
            iv_image             = lv_node_image
            iv_hier1_item_text   = |{ <ls_assoc>-raw_name }|
            is_hier2_item        = VALUE #( text = |({ <ls_assoc>-ref_cds_view_raw })| style = zif_uitb_c_ctm_style=>inverted_blue )
            ir_user_data         = NEW ty_s_user_data(
              entity_id     = <ls_assoc>-ref_cds_view
              entity_id_raw = <ls_assoc>-ref_cds_view_raw
              node_type     = lv_node_type
            )
        ).

        IF <ls_assoc>-kind = zif_sat_c_cds_assoc_type=>table.
          IF lv_node_type = c_node_type-dbtable.
            create_db_table_subnodes(
              io_db_tab_view_node = lo_cds_base_table_node
              is_entity           = VALUE #(
                entity_id     = <ls_assoc>-ref_cds_view
                entity_id_raw = <ls_assoc>-ref_cds_view_raw
                entity_type   = zif_sat_c_entity_type=>table
              )
            ).
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD create_tree.
    DATA: lt_events TYPE cntl_simple_events.

    CHECK mo_tree IS INITIAL.

    mo_favorite_dd_menu = NEW cl_ctmenu( ).
    fill_favorite_dd_menu( ).

    zcl_uitb_gui_helper=>create_control_toolbar(
      EXPORTING
        io_parent    = mo_splitter->get_container( 2 )
        it_button    = VALUE #(
          ( function = c_functions-previous_history
            icon     = icon_arrow_left
            quickinfo = |{ 'Show previous tree' }|
            butn_type = cntb_btype_dropdown )
          ( function = c_functions-next_history
            icon     = icon_arrow_right
            quickinfo = |{ 'Show next tree' }|
            butn_type = cntb_btype_dropdown )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-expand_all
            icon      = icon_expand_all
            quickinfo = |{ 'Expand all Nodes' }| )
          ( function  = c_functions-collapse_all
            icon      = icon_collapse_all
            quickinfo = |{ 'Collapse all Nodes' }| )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-find
            icon      = icon_search
            quickinfo = |{ 'Find' }| )
          ( function  = c_functions-find_next
            icon      = icon_search_next
            quickinfo = |{ 'Find next' }| )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-to_parent
            icon      = icon_previous_hierarchy_level
            quickinfo = |{ 'Superordinate Object List'(011) }| )
          ( function  = c_functions-search_settings_menu
            icon      = icon_settings
            butn_type = cntb_btype_menu
            quickinfo = 'Search Settings' )
          ( function  = c_functions-import_queries
            icon      = icon_import
            butn_type = cntb_btype_first
            quickinfo = |{ 'Import Queries' }| )
          ( function  = c_functions-export_queries
            icon      = icon_export
            butn_type = cntb_btype_last
            quickinfo = |{ 'Export Queries' }| )
          ( function  = c_functions-favorite_dropdown
            icon      = icon_system_favorites
            quickinfo = |{ 'Search Favorites'(026) }|
            butn_type = cntb_btype_menu )
          ( butn_type = cntb_btype_sep )
          ( function  = 'HELP'
            icon      = icon_information
            quickinfo = |{ 'Show Help' }| )
        )
      IMPORTING
        eo_toolbar   = mo_toolbar
        eo_client    = DATA(lo_container)
    ).

    update_search_settings_menu( ).

    mo_toolbar->set_button_state( fcode = c_functions-next_history     enabled = abap_false ).
    mo_toolbar->set_button_state( fcode = c_functions-previous_history enabled = abap_false ).

    mo_toolbar->set_static_ctxmenu(
      EXPORTING  fcode   = c_functions-search_settings_menu
                 ctxmenu = mo_settings_menu
      EXCEPTIONS OTHERS  = 1
    ).
    mo_toolbar->set_static_ctxmenu(
      EXPORTING  fcode   = c_functions-favorite_dropdown
                 ctxmenu = mo_favorite_dd_menu
      EXCEPTIONS OTHERS  = 1
    ).
    SET HANDLER: on_toolbar_button FOR mo_toolbar.

    mo_tree = NEW zcl_uitb_column_tree_model(
        ir_parent           = lo_container
        if_auto_node_key    = abap_true
        is_hierarchy_header = VALUE #(
          heading = 'Object Name'(010)
        )
        iv_selection_mode   = cl_tree_model=>node_sel_mode_multiple
    ).

    mo_tree->get_columns( )->add_hierarchy_column( iv_colname = c_hierarchy_node2 ).
    mo_tree->get_columns( )->add_hierarchy_column( iv_colname = c_hierarchy_node3 ).

    mo_tree->create_tree_control( ).

    DATA(lo_events) = mo_tree->get_events( ).

    lo_events->add_key_for_keypress( cl_tree_model=>key_delete ).

*.. set event handler methods to tree control
    SET HANDLER:
      on_node_context_menu_request FOR lo_events,
      on_node_context_menu_select FOR lo_events,
      on_node_double_click FOR lo_events,
      on_node_enter_key FOR lo_events,
      on_expand_no_children FOR lo_events.
  ENDMETHOD.

  METHOD expand_db_table.
    DATA(ls_tabname) = zcl_sat_ddic_repo_access=>get_table_info( iv_tablename = iv_tabname ).
    create_db_table_subnodes(
        io_db_tab_view_node = mo_tree->get_nodes( )->get_node( iv_node_key )
        is_entity           = VALUE #( )
    ).
  ENDMETHOD.


  METHOD expand_dbtab_view_properties.
    DATA: lv_date      TYPE string,
          lv_date_text TYPE string.

    SELECT SINGLE *
      FROM zsat_i_databaseentity
      WHERE entity = @iv_tabname
    INTO @DATA(ls_entity).

    CHECK sy-subrc = 0.

    create_node(
        iv_parent_node     = iv_node_key
        iv_hier1_item_text = |{ 'Responsible'(018) }|
        iv_image           = zif_dbbr_c_icon=>user_menu
        is_hier2_item      = VALUE #( text = |{ ls_entity-createdby }| class = c_text_class style = zif_uitb_c_ctm_style=>inverted_blue )
        ir_user_data       = NEW ty_s_user_data(
            node_type = COND #( WHEN iv_node_type = c_node_type-dbtable_properties THEN c_node_type-dbtable_owner_prop ELSE c_node_type-view_owner_prop )
            owner     = ls_entity-createdby
        )
    ).
    IF ls_entity-createddate IS INITIAL.
      lv_date = |{ ls_entity-changeddate DATE = USER }|.
      lv_date_text = |{ 'Last Changed on'(039) }|.
    ELSE.
      lv_date = |{ ls_entity-createddate DATE = USER }|.
      lv_date_text = |{ 'Created On'(033) }|.
    ENDIF.

    create_node(
        iv_parent_node     = iv_node_key
        iv_hier1_item_text = lv_date_text
        iv_image           = zif_dbbr_c_icon=>date
        is_hier2_item      = VALUE #( text = lv_date class = c_text_class style = zif_uitb_c_ctm_style=>inverted_blue )
    ).
    create_node(
        iv_parent_node     = iv_node_key
        iv_hier1_item_text = |{ 'Package'(001) }|
        iv_image           = zif_dbbr_c_icon=>package
        is_hier2_item      = VALUE #( text = |{ ls_entity-developmentpackage }| class = c_text_class style = zif_uitb_c_ctm_style=>inverted_blue )
        ir_user_data       = NEW ty_s_user_data(
            node_type = COND #( WHEN iv_node_type = c_node_type-dbtable_properties THEN c_node_type-dbtable_package_prop ELSE c_node_type-view_package_prop )
            devclass  = ls_entity-developmentpackage
        )
    ).

    mo_tree->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.

  METHOD is_package_sub_node.
    result = SWITCH #(
      iv_node_type
      WHEN c_node_type-cds_package_prop OR
           c_node_type-dbtable_package_prop OR
           c_node_type-view_package_prop THEN abap_true
    ).
  ENDMETHOD.

  METHOD expand_foreign_key_tables.
    DATA(lt_foreign_key_tables) = zcl_sat_ddic_repo_access=>get_foreign_key_tables( iv_tabname = iv_tabname ).

    CHECK lt_foreign_key_tables IS NOT INITIAL.

    create_multiple_db_tab_tree(
        it_table_definition = lt_foreign_key_tables
        io_parent_node      = mo_tree->get_nodes( )->get_node( iv_node_key )
    ).

    mo_tree->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_cds_view_node.
*.. load cds view entity
    TRY.
        create_single_cds_view_tree(
            io_cds_view = zcl_sat_cds_view_factory=>read_cds_view( iv_cds_view )
            io_node     = mo_tree->get_nodes( )->get_node( iv_node_key )
        ).
      CATCH zcx_sat_data_read_error ##needed.
    ENDTRY.
  ENDMETHOD.


  METHOD expand_package_dbtabs_node.
    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading DB Tables of Package'(023) } { iv_package }| iv_progress = 1 ).

    DATA(lt_dbtabs) = zcl_sat_ddic_repo_access=>find_database_tab_view(
        iv_package = iv_package
        if_all     = abap_true
        iv_type    = zif_sat_c_entity_type=>table
    ).

    create_multiple_db_tab_tree(
        it_table_definition = lt_dbtabs
        io_parent_node      = mo_tree->get_nodes( )->get_node( iv_node_key )
    ).

    mo_tree->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_package_ddls_node.
    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading DDLS of Package'(022) } { iv_package }| iv_progress = 1 ).
    DATA(lt_header_in_package) = zcl_sat_cds_view_factory=>find_cds_views(
      iv_package  = iv_package
      iv_max_rows = 0
    ).

    create_multiple_cds_view_tree(
        it_cds_view_header = lt_header_in_package
        io_parent_node     = mo_tree->get_nodes( )->get_node( iv_node_key )
    ).

    mo_tree->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_package_node.
    cl_package=>load_package( EXPORTING i_package_name = iv_package IMPORTING e_package = DATA(lo_package) EXCEPTIONS OTHERS = 1 ).
    IF lo_package IS BOUND.
      create_single_package_tree(
          io_package = lo_package
          io_node    = mo_tree->get_nodes( )->get_node( iv_node_key )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD expand_package_views_node.
    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading DB Views of Package'(024) } { iv_package }| iv_progress = 1 ).
    DATA(lt_views) = zcl_sat_ddic_repo_access=>find_database_tab_view(
        iv_package = iv_package
        if_all     = abap_true
        iv_type    = zif_sat_c_entity_type=>view
    ).

    create_multiple_db_tab_tree(
        it_table_definition = lt_views
        io_parent_node      = mo_tree->get_nodes( )->get_node( iv_node_key )
    ).

    mo_tree->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_query_node.
    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(lo_node) = lo_nodes->get_node( iv_node_key ).

    DATA(ls_query) = mo_query_f->get_query( iv_query_name = iv_query if_load_completely = abap_false ).

*.. Create properties node of query
*.. Create "Properties" Sub node
    DATA(lo_query_props_node) = create_node(
       if_folder            = abap_true
       iv_parent_node       = iv_node_key
       iv_hier1_item_text   = |{ TEXT-017 }|
       ir_user_data         = NEW ty_s_user_data(
         entity_id = iv_query
         node_type = c_node_type-query_properties
       )
    ).

    create_node(
        iv_parent_node       = lo_query_props_node->mv_node_key
        iv_image             = zif_dbbr_c_icon=>user_menu
        iv_hier1_item_text   = |{ TEXT-018 }|
        is_hier2_item        = VALUE #( text = |{ ls_query-created_by }| style = zif_uitb_c_ctm_style=>inverted_blue )
        ir_user_data         = NEW ty_s_user_data(
            entity_id     = ls_query-query_name
            node_type     = c_node_type-query_owner_prop
            owner         = ls_query-created_by
        )
    ).
    create_node(
        iv_parent_node       = lo_query_props_node->mv_node_key
        iv_image             = zif_dbbr_c_icon=>date
        iv_hier1_item_text   = |{ TEXT-033 }|
        is_hier2_item        = VALUE #( text = |{ ls_query-created_date DATE = USER }| style = zif_uitb_c_ctm_style=>inverted_blue )
    ).
    create_node(
        iv_parent_node       = lo_query_props_node->mv_node_key
        iv_image             = |{ icon_settings }|
        iv_hier1_item_text   = |{ 'Type' }|
        is_hier2_item        = VALUE #( text = |{ COND #( WHEN ls_query-source IS INITIAL THEN 'Default' ELSE 'Custom' ) }| style = zif_uitb_c_ctm_style=>inverted_blue )
    ).

*.. Retrieve base tables of view
    DATA(lt_base_tables) = NEW zcl_dbbr_query_factory( )->get_tables_of_query( iv_query ).

    IF lt_base_tables IS NOT INITIAL.

      DATA(lo_query_entities_node) = create_node(
         if_folder            = abap_true
         iv_parent_node       = lo_node->mv_node_key
         iv_hier1_item_text   = |{ 'Entities' }|
      ).

      LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
        DATA(lv_node_type) = SWITCH #(
          <ls_base_table>-entity_type
          WHEN zif_sat_c_entity_type=>table    THEN c_node_type-dbtable
          WHEN zif_sat_c_entity_type=>view     THEN c_node_type-view
          WHEN zif_sat_c_entity_type=>cds_view THEN c_node_type-cds_view
        ).
        DATA(lv_image) = node_image_for_node_type( lv_node_type ).
        DATA(lo_base_table_node) = create_node(
            if_folder            = abap_true
            iv_parent_node       = lo_query_entities_node->mv_node_key
            iv_image             = lv_image
            if_expander          = xsdbool( lv_node_type = c_node_type-cds_view )
            iv_hier1_item_text   = |{ <ls_base_table>-entity_id_raw }|
            is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = <ls_base_table>-description )
            ir_user_data         = NEW ty_s_user_data(
              entity_id = <ls_base_table>-entity_id
              node_type = lv_node_type
            )
        ).

        IF lv_node_type <> c_node_type-cds_view.
          create_db_table_subnodes(
              io_db_tab_view_node = lo_base_table_node
              is_entity           = <ls_base_table>
          ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    lo_nodes->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_view_node.
    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(lo_node) = lo_nodes->get_node( iv_node_key ).

*.. Retrieve base tables of view
    DATA(lt_base_tables) = zcl_sat_ddic_repo_access=>find_base_tables_of_view( iv_view_name = iv_view ).

    LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
      DATA(lv_node_type) = SWITCH #(
        <ls_base_table>-entity_type
        WHEN zif_sat_c_entity_type=>table THEN c_node_type-dbtable
        WHEN zif_sat_c_entity_type=>view  THEN c_node_type-view
      ).
      DATA(lv_image) = node_image_for_node_type( lv_node_type ).
      DATA(lo_base_table_node) = create_node(
          if_folder            = abap_true
          iv_parent_node       = iv_node_key
          iv_image             = lv_image
          iv_expanded_image    = lv_image
          iv_hier1_item_text   = |{ <ls_base_table>-entity_id }|
          is_hier2_item        = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = <ls_base_table>-description )
          ir_user_data         = NEW ty_s_user_data(
            entity_id = <ls_base_table>-entity_id
            node_type = lv_node_type
          )
      ).
      create_db_table_subnodes(
          io_db_tab_view_node = lo_base_table_node
          is_entity           = <ls_base_table>
      ).
    ENDLOOP.

    lo_nodes->expand_node( iv_node_key ).
  ENDMETHOD.

  METHOD expand_tech_settings_node.
    SELECT SINGLE *
     FROM zsat_i_dbtabletechsettings
     WHERE tablename = @iv_tabname
    INTO @DATA(ls_tech_settings).

    CHECK sy-subrc = 0.

    create_node(
      iv_parent_node     = iv_node_key
      iv_hier1_item_text = |{ 'Delivery Class' }|
      is_hier2_item      = VALUE #( text = |'{ ls_tech_settings-deliveryclass }'| style = zif_uitb_c_ctm_style=>inverted_blue )
      is_hier3_item      = COND #(
        WHEN ls_tech_settings-deliveryclasstext IS NOT INITIAL THEN
          VALUE #( text = |{ ls_tech_settings-deliveryclasstext }| style = zif_uitb_c_ctm_style=>inverted_gray )
      )
    ).
    create_node(
      iv_parent_node     = iv_node_key
      iv_hier1_item_text = |{ 'Size Category' }|
      is_hier2_item      = VALUE #( text = |'{ ls_tech_settings-sizecategory }'| style = zif_uitb_c_ctm_style=>inverted_blue )
      is_hier3_item      = COND #(
        WHEN ls_tech_settings-sizecategorytext IS NOT INITIAL THEN
          VALUE #( text = |{ ls_tech_settings-sizecategorytext }| style = zif_uitb_c_ctm_style=>inverted_gray )
      )
    ).
    create_node(
      iv_parent_node     = iv_node_key
      iv_hier1_item_text = |{ 'Data Class' }|
      is_hier2_item      = VALUE #( text = |'{ ls_tech_settings-dataclass }'| style = zif_uitb_c_ctm_style=>inverted_blue )
      is_hier3_item      = COND #(
        WHEN ls_tech_settings-dataclasstext IS NOT INITIAL THEN
          VALUE #( text = |{ ls_tech_settings-dataclasstext }| style = zif_uitb_c_ctm_style=>inverted_gray )
      )
    ).
    create_node(
      iv_parent_node     = iv_node_key
      iv_hier1_item_text = |{ 'Buffering' }|
      is_hier2_item      = VALUE #( text = |'{ ls_tech_settings-bufferingindicator }'| style = zif_uitb_c_ctm_style=>inverted_blue )
      is_hier3_item      = COND #(
        WHEN ls_tech_settings-bufferingindicatortext IS NOT INITIAL THEN
          VALUE #( text = |{ ls_tech_settings-bufferingindicatortext }| style = zif_uitb_c_ctm_style=>inverted_gray )
      )
    ).

    mo_tree->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.

  METHOD export_queries.
    DATA: lt_query_to_export  TYPE zdbbr_query_info_ui_itab,
          lt_query_name_range TYPE RANGE OF zsat_query_name.

    DATA(lt_selected_nodes) = mo_tree->get_selections( )->get_selected_nodes( ).
    IF lt_selected_nodes IS INITIAL.
      MESSAGE |Select at least one Query| TYPE 'S'.
      RETURN.
    ENDIF.

*.. Only collect nodes of type query
    LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lr_node>).
      DATA(lr_user_data) = CAST ty_s_user_data( <lr_node>->get_user_data( ) ).
      CHECK lr_user_data IS BOUND.

      IF lr_user_data->node_type = c_node_type-query.
        lt_query_name_range = VALUE #( BASE lt_query_name_range ( sign = 'I' option = 'EQ' low = lr_user_data->entity_id ) ).
      ENDIF.
    ENDLOOP.

    IF lt_query_name_range IS INITIAL.
      MESSAGE |There are no Query nodes in the current selection. Export was cancelled| TYPE 'S'.
      RETURN.
    ENDIF.

*.. Collect ids for the query names
    SELECT query_id
      FROM zdbbr_queryh
      WHERE query_name IN @lt_query_name_range
    INTO CORRESPONDING FIELDS OF TABLE @lt_query_to_export.

    NEW zcl_dbbr_query_exporter(
      it_query_info = lt_query_to_export
    )->export_data( ).
  ENDMETHOD.

  METHOD fill_favorite_dd_menu.
    CLEAR: mt_fav_func_query_map.

    mo_favorite_dd_menu->clear( ).
    mo_favorite_dd_menu->add_function(
        fcode = c_functions-add_favorite
        text  = |{ 'Add'(027) }|
    ).
    mo_favorite_dd_menu->add_function(
        fcode = c_functions-edit_favorites
        text  = |{ 'Edit...'(028) }|
    ).
    mo_favorite_dd_menu->add_separator( ).
    mo_favorite_dd_menu->add_function(
        fcode = c_functions-my_db_tables_views_search
        text  = |{ 'My Database Tables/Views ' }|
    ).
    mo_favorite_dd_menu->add_function(
        fcode = c_functions-my_cds_views_search
        text  = |{ 'My CDS Views' }|
    ).
    mo_favorite_dd_menu->add_function(
        fcode = c_functions-my_queries_search
        text  = |{ 'My Queries' }|
    ).
    mo_favorite_dd_menu->add_separator( ).

    DATA(lt_favorites) = zcl_dbbr_ob_fav_factory=>get_favorites( ).
    DATA(lv_fav_func_counter) = 0.

    LOOP AT lt_favorites ASSIGNING FIELD-SYMBOL(<ls_fav>)
      GROUP BY <ls_fav>-entity_type
      ASSIGNING FIELD-SYMBOL(<lv_type>).

      DATA(lo_submenu) = NEW cl_ctmenu( ).
      LOOP AT GROUP <lv_type> ASSIGNING FIELD-SYMBOL(<ls_favorite>).
        ADD 1 TO lv_fav_func_counter.
        DATA(lv_favorite_func) = CONV ui_func( |FAV:{ lv_fav_func_counter }| ).
        mt_fav_func_query_map = VALUE #( BASE mt_fav_func_query_map
          ( function = lv_favorite_func
            type     = <ls_favorite>-entity_type
            query    = <ls_favorite>-search_string )
        ).
        lo_submenu->add_function(
            fcode = lv_favorite_func
            text  = <ls_favorite>-favorite_name
        ).
      ENDLOOP.

      mo_favorite_dd_menu->add_submenu(
          menu = lo_submenu
          text = SWITCH gui_text(
            <lv_type>
            WHEN zif_dbbr_c_object_browser_mode=>cds_view THEN 'CDS View'
            WHEN zif_dbbr_c_object_browser_mode=>database_table_view THEN 'Database Table/View'
            WHEN zif_dbbr_c_object_browser_mode=>query THEN 'Query'
            WHEN zif_dbbr_c_object_browser_mode=>package THEN 'Package'
          )
      ).

    ENDLOOP.
  ENDMETHOD.

  METHOD node_image_for_node_type.
    rv_image = SWITCH #( iv_node_type
      WHEN c_node_type-dbtable  THEN zif_dbbr_c_icon=>database_table
      WHEN c_node_type-view     THEN zif_dbbr_c_icon=>database_view
      WHEN c_node_type-cds_view THEN zif_dbbr_c_icon=>cds_view
    ).
  ENDMETHOD.


  METHOD on_display_object_list.
    CHECK: ev_entity_id IS NOT INITIAL,
           ev_entity_type IS NOT INITIAL.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading Object List...'(016) }| iv_progress = 1 ).

    mo_search_type_select->set_value(
      SWITCH #( ev_entity_type
        WHEN zif_sat_c_entity_type=>cds_view THEN zif_dbbr_c_object_browser_mode=>cds_view
        WHEN zif_sat_c_entity_type=>view  OR
             zif_sat_c_entity_type=>table    THEN zif_dbbr_c_object_browser_mode=>database_table_view
        WHEN zif_sat_c_entity_type=>query    THEN zif_dbbr_c_object_browser_mode=>query
      )
    ).
    mv_current_search_type = mo_search_type_select->value.
    mo_search_input->set_value( |{ ev_entity_id }| ).

    update_toolbar( ).

    adapt_and_display_html_inp( ).

    TRY.
        IF mo_search_query IS INITIAL OR
           mo_search_query->mv_query <> ev_entity_id. " OR
*           mo_search_query->mv_type <> ev_entity_type.

          mo_search_query = parse_query(
           iv_query         = |{ ev_entity_id }|
           iv_browser_mode  = mv_current_search_type
          ).
          CHECK mo_search_query->has_search_terms( ).

          add_history_entry( ).
        ENDIF.

        trigger_new_search( ).

*...... Display object browser in parent if not already visible
        mo_parent_view->set_child_visibility( me ).
      CATCH zcx_sat_object_search.
    ENDTRY.

  ENDMETHOD.


  METHOD on_expand_no_children.
*... Check which action should be depending on the node type
    CHECK mo_tree->get_nodes( )->node_has_user_data( ev_node_key ).

    DATA(lo_node) = mo_tree->get_nodes( )->get_node( ev_node_key ).
    DATA(lr_user_data) = CAST ty_s_user_data( lo_node->get_user_data( ) ).

    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading Tree Nodes...'(015) }| iv_progress = 1 ).

    CASE lr_user_data->node_type.

      WHEN c_node_type-cds_view.
        expand_cds_view_node(
           iv_node_key = ev_node_key
           iv_cds_view = lr_user_data->entity_id
        ).

      WHEN c_node_type-package.
        expand_package_node(
            iv_package  = lr_user_data->entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-pak_dtab_folder.
        expand_package_dbtabs_node(
            iv_package  = lr_user_data->entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-pak_ddl_folder.
        expand_package_ddls_node(
            iv_package  = lr_user_data->entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-pak_view_folder.
        expand_package_views_node(
            iv_package  = lr_user_data->entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-dbtable_foreign_key.
        expand_foreign_key_tables(
          iv_tabname  = lr_user_data->entity_id
          iv_node_key = ev_node_key
        ).

      WHEN c_node_type-dbtable_tech_settings.
        expand_tech_settings_node(
          iv_tabname  = lr_user_data->entity_id
          iv_node_key = ev_node_key
        ).

      WHEN c_node_type-dbtable_properties OR
           c_node_type-view_properties.
        expand_dbtab_view_properties(
          iv_tabname   = lr_user_data->entity_id
          iv_node_type = lr_user_data->node_type
          iv_node_key  = ev_node_key
        ).

      WHEN c_node_type-view_tables.
        expand_view_node(
           iv_view     = lr_user_data->entity_id
           iv_node_key = ev_node_key
        ).

      WHEN c_node_type-query.
        expand_query_node(
           iv_query    = lr_user_data->entity_id
           iv_node_key = ev_node_key
        ).
    ENDCASE.

  ENDMETHOD.


  METHOD on_external_object_search_req.
    TRY.
        IF mo_search_query IS INITIAL OR
           mo_search_query->mv_query <> ev_search_query. " OR
*           mo_search_query->mv_type <> ev_object_type.


          mv_current_search_type = ev_object_type.

          mo_search_query = parse_query(
            iv_query         = |{ ev_search_query }|
            iv_browser_mode  = mv_current_search_type
          ).
          add_history_entry( ).
        ENDIF.
      CATCH zcx_sat_object_search INTO DATA(lx_appl_error).
        lx_appl_error->show_message( iv_message_type = 'S' ).
        RETURN.
    ENDTRY.

    IF trigger_new_search( ).

      mo_search_type_select->set_value( |{ mv_current_search_type }| ).
      mo_search_input->set_value( |{ ev_search_query }| ).

      update_toolbar( ).

      adapt_and_display_html_inp( ).
      focus( ).

      IF ef_close_popup = abap_true.
        zcl_dbbr_selscr_nav_events=>raise_close_object_srch_dialog( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    DATA(lt_selected_nodes) = mo_tree->get_selections( )->get_selected_nodes( ).
    IF lines( lt_selected_nodes ) = 1.
      DATA(lo_selected_node) = lt_selected_nodes[ 1 ].
      DATA(lr_user_data) = lo_selected_node->get_user_data( ).
      IF lr_user_data IS INITIAL.
        RETURN.
      ENDIF.

      ASSIGN CAST ty_s_user_data( lr_user_data )->* TO FIELD-SYMBOL(<ls_user_data>).

      CASE <ls_user_data>-node_type.

        WHEN c_node_type-cds_view OR
             c_node_type-dbtable OR
             c_node_type-view OR
             c_node_type-query.

          IF <ls_user_data>-node_type <> c_node_type-query.
            er_menu->add_function(
                fcode = c_functions-open_with_adt
                text  = |{ 'Open with ADT'(029) }|
            ).
          ENDIF.

          er_menu->add_function(
              fcode = c_functions-open_in_new_window
              text  = |{ 'Open in new Window'(030) }|
          ).
          er_menu->add_separator( ).
          er_menu->add_function(
              fcode = c_functions-exec_with_dbbrs
              text  = |{ 'Show Content'(031) }|
          ).
          er_menu->add_function(
              fcode = c_functions-exec_with_dbbrs_new_window
              text  = |{ 'Show Content (New Task)'(032) }|
          ).
          IF <ls_user_data>-node_type = c_node_type-cds_view.
            er_menu->add_separator( ).
            er_menu->add_function(
               fcode = c_functions-show_ddl_source
               text  = |{ 'Show DDL Source Code'(034) }|
            ).
            er_menu->add_function(
               fcode = c_functions-analyze_dependencies
               text  = |{ 'Open with Dependency Analyzer'(035) }|
            ).
          ENDIF.

          IF <ls_user_data>-node_type <> c_node_type-query.
            er_menu->add_separator( ).
            er_menu->add_function(
                fcode = c_functions-used_in_select_from
                text  = |{ 'Show uses in FROM of CDS Views'(040) }|
            ).
            er_menu->add_function(
                fcode = c_functions-used_as_assoc
                text  = |{ 'Show uses as Associations of CDS Views'(041) }|
            ).
          ENDIF.

        WHEN OTHERS.
          IF is_package_sub_node( <ls_user_data>-node_type ).
            er_menu->add_function(
              fcode = c_functions-add_package_to_query
              text  = |{ 'Add package to current Query'(042) }|
            ).
            er_menu->add_function(
              fcode = c_functions-add_root_package_to_query
              text  = |{ 'Add root package to current Query'(043) }|
            ).
          ENDIF.

      ENDCASE.
    ELSE.
*.... Several nodes are selected
      LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lo_node>).

      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    DATA: lf_no_data TYPE abap_bool.

    DATA(lr_user_data) = mo_tree->get_nodes( )->get_node( ev_node_key )->get_user_data( ).
    CHECK lr_user_data IS BOUND.

    ASSIGN CAST ty_s_user_data( lr_user_data )->* TO FIELD-SYMBOL(<ls_user_data>).

    DATA(lv_entity_type) = SWITCH #(
        <ls_user_data>-node_type
        WHEN c_node_type-cds_view THEN zif_sat_c_entity_type=>cds_view
        WHEN c_node_type-dbtable  THEN zif_sat_c_entity_type=>table
        WHEN c_node_type-view     THEN zif_sat_c_entity_type=>table
        WHEN c_node_type-query    THEN zif_sat_c_entity_type=>query
    ).

    CASE ev_fcode.

      WHEN c_functions-open_with_adt.

        TRY.
            zcl_sat_adt_util=>jump_adt(
                iv_obj_name     = SWITCH #( <ls_user_data>-node_type
                    WHEN c_node_type-cds_view THEN
                        zcl_sat_cds_view_factory=>get_ddl_for_entity_name( <ls_user_data>-entity_id )
                    ELSE
                        <ls_user_data>-entity_id
                )
                iv_obj_type     = SWITCH #(
                    <ls_user_data>-node_type
                    WHEN c_node_type-cds_view THEN 'DDLS'
                    WHEN c_node_type-dbtable  THEN 'TABD'
                    WHEN c_node_type-view     THEN 'VIEW'
                )
            ).
          CATCH zcx_sat_adt_error INTO DATA(lx_adt_error).
          CATCH zcx_sat_data_read_error INTO DATA(lx_data_read_error).
        ENDTRY.

      WHEN c_functions-open_in_new_window.
        CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_OPEN_JUMP'
          EXPORTING
            iv_entity_id       = <ls_user_data>-entity_id
            iv_entity_type     = lv_entity_type
            if_load_parameters = abap_true.

      WHEN c_functions-exec_with_dbbrs_new_window.
*...... Check if there is an actual result to show
*        IF zcl_sat_ddic_repo_access=>exists_data_for_entity( <ls_user_data>-entity_id ).
        CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_EXEC_JUMP'
          EXPORTING
            iv_entity_id       = <ls_user_data>-entity_id
            iv_entity_type     = lv_entity_type
            if_skip_selscreen  = abap_true
            if_load_parameters = abap_true.
*        ELSE.
*          MESSAGE i086(zdbbr_info).
*        ENDIF.

      WHEN c_functions-exec_with_dbbrs.
        DATA(lo_variant_starter) = zcl_dbbr_variant_starter_fac=>create_variant_starter(
            iv_variant_id        = zif_dbbr_c_global=>c_dummy_variant
            iv_entity_type       = lv_entity_type
            iv_variant_entity_id = CONV #( <ls_user_data>-entity_id )
        ).

        lo_variant_starter->initialize( ).
        TRY.
            lo_variant_starter->execute_variant( ).
          CATCH zcx_dbbr_variant_error INTO DATA(lx_variant_error).
            lx_variant_error->show_message( iv_message_type = 'S' ).
        ENDTRY.

      WHEN c_functions-show_ddl_source.
        IF mv_theme IS INITIAL.
          mv_theme = CAST zdbbr_user_settings_a( zcl_dbbr_usersettings_factory=>get_current_settings( ) )->code_viewer_theme.
        ENDIF.

        TRY.
            DATA(lv_source) = zcl_sat_cds_view_factory=>read_ddls_source( <ls_user_data>-entity_id ).
            zcl_uitb_abap_code_viewer=>show_code(
                iv_title = |DDL Source { <ls_user_data>-entity_id_raw }|
                iv_code  = lv_source
                iv_theme = mv_theme
            ).
          CATCH zcx_sat_application_exc INTO DATA(lx_app_error).
            lx_app_error->zif_sat_exception_message~print( ).
        ENDTRY.

      WHEN c_functions-analyze_dependencies.
        zcl_uitb_screen_util=>set_current_command( NEW zcl_dbbr_dep_tree_cmd_exec( <ls_user_data>-entity_id_raw ) ).
        zcl_uitb_screen_util=>raise_gui_command( ).

      WHEN c_functions-used_as_assoc.
        on_external_object_search_req(
            ev_object_type  = zif_dbbr_c_object_browser_mode=>cds_view
            ev_search_query = |assoc:{ <ls_user_data>-entity_id }|
            ef_close_popup  = abap_false
        ).

      WHEN c_functions-used_in_select_from.
        on_external_object_search_req(
            ev_object_type  = zif_dbbr_c_object_browser_mode=>cds_view
            ev_search_query = |from:{ <ls_user_data>-entity_id }|
            ef_close_popup  = abap_false
        ).

      WHEN c_functions-add_package_to_query.
        on_external_object_search_req(
           ev_object_type  = |{ mo_search_type_select->value }|
           ev_search_query = |{ mo_search_input->value } package:{ <ls_user_data>-devclass }|
           ef_close_popup  = abap_false
        ).

      WHEN c_functions-add_root_package_to_query.
        cl_pak_package_queries=>get_root_package(
          EXPORTING im_package      = <ls_user_data>-devclass
          IMPORTING ev_root_package = DATA(lv_root_pack)
          EXCEPTIONS OTHERS         = 1
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
          RETURN.
        ENDIF.
        on_external_object_search_req(
           ev_object_type  = |{ mo_search_type_select->value }|
           ev_search_query = |{ mo_search_input->value } package:{ lv_root_pack }|
           ef_close_popup  = abap_false
        ).
    ENDCASE.
  ENDMETHOD.


  METHOD on_node_double_click.
    DATA(lo_node) = mo_tree->get_nodes( )->get_node( ev_node_key ).
    CHECK lo_node IS BOUND.
    DATA(lr_user_data) = lo_node->get_user_data( ).
    CHECK lr_user_data IS BOUND.

    ASSIGN CAST ty_s_user_data( lr_user_data )->* TO FIELD-SYMBOL(<ls_user_data>).

    IF <ls_user_data>-node_type = c_node_type-cds_view OR
       <ls_user_data>-node_type = c_node_type-query OR
       <ls_user_data>-node_type = c_node_type-dbtable OR
       <ls_user_data>-node_type = c_node_type-view.

      zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
          iv_entity_id   = <ls_user_data>-entity_id
          iv_entity_type = SWITCH #(
            <ls_user_data>-node_type
            WHEN c_node_type-cds_view THEN zif_sat_c_entity_type=>cds_view
            WHEN c_node_type-query    THEN zif_sat_c_entity_type=>query
            WHEN c_node_type-dbtable  THEN zif_sat_c_entity_type=>table
            WHEN c_node_type-view     THEN zif_sat_c_entity_type=>table
          )
      ).
    ELSEIF <ls_user_data>-node_type = c_node_type-cds_tables OR
           <ls_user_data>-node_type = c_node_type-cds_assocs OR
           <ls_user_data>-node_type = c_node_type-cds_properties.
      mo_tree->get_nodes( )->toggle_node( ev_node_key ).

*... Trigger new owner:/package: queries depending on the entity
    ELSEIF <ls_user_data>-node_type = c_node_type-dbtable_owner_prop OR
           <ls_user_data>-node_type = c_node_type-dbtable_package_prop OR
           <ls_user_data>-node_type = c_node_type-query_owner_prop OR
           <ls_user_data>-node_type = c_node_type-view_package_prop OR
           <ls_user_data>-node_type = c_node_type-view_owner_prop OR
           <ls_user_data>-node_type = c_node_type-cds_owner_prop OR
           <ls_user_data>-node_type = c_node_type-cds_package_prop.

      DATA(lv_query_param) = SWITCH #(
        <ls_user_data>-node_type
        WHEN c_node_type-dbtable_package_prop OR
             c_node_type-view_package_prop    OR
             c_node_type-cds_package_prop        THEN |{ 'package:' }|
        ELSE |{ 'owner:' }|
      ).
      DATA(lv_query_param_value) = SWITCH #(
        <ls_user_data>-node_type
        WHEN c_node_type-dbtable_package_prop OR
             c_node_type-view_package_prop    OR
             c_node_type-cds_package_prop        THEN |{ <ls_user_data>-devclass }|
        ELSE |{ <ls_user_data>-owner }|
      ).
      on_external_object_search_req(
          ev_object_type  = SWITCH #( <ls_user_data>-node_type
            WHEN c_node_type-cds_owner_prop OR
                 c_node_type-cds_package_prop THEN zif_dbbr_c_object_browser_mode=>cds_view
            WHEN c_node_type-query_owner_prop THEN zif_dbbr_c_object_browser_mode=>query
            ELSE zif_dbbr_c_object_browser_mode=>database_table_view
          )
          ev_search_query = |{ lv_query_param }{ lv_query_param_value }|
          ef_close_popup  = abap_false
      ).
    ENDIF.
  ENDMETHOD.


  METHOD on_node_enter_key.
    on_node_double_click( ev_node_key ).
  ENDMETHOD.


  METHOD on_perform_search.
    CHECK mo_search_input->value IS NOT INITIAL.

    IF mo_search_query IS INITIAL OR
       mo_search_query->mv_query <> mo_search_input->value. " OR
*       mo_search_query->mv_type <> mo_search_type_select->value.

      TRY.
          mo_search_query = parse_query(
            iv_query         = |{ mo_search_input->value }|
            iv_browser_mode  = mv_current_search_type
          ).
          add_history_entry( ).
        CATCH zcx_sat_object_search INTO DATA(lx_parse_error).
          MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.

    ENDIF.

    trigger_new_search( ).
  ENDMETHOD.


  METHOD on_search_input_enter.
    CHECK mo_search_input->value IS NOT INITIAL.

    DATA(lv_current_search_type) = map_to_sat_query_type( |{ mo_search_type_select->value }| ).

    IF mo_search_query IS INITIAL OR
       mo_search_query->mv_query <> mo_search_input->value OR
       mo_search_query->mv_type <> lv_current_search_type.

      TRY.
          mo_search_query = parse_query(
            iv_query         = |{ mo_search_input->value }|
            iv_browser_mode  = mv_current_search_type
          ).
          add_history_entry( ).
        CATCH zcx_sat_object_search INTO DATA(lx_parse_error).
          MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.
    ENDIF.

    trigger_new_search( ).
  ENDMETHOD.


  METHOD on_search_type_selected.
    IF mv_current_search_type <> mo_search_type_select->value.
      CASE mv_current_search_type.

        WHEN zif_dbbr_c_object_browser_mode=>cds_view.
          ms_search_input-cds_view = mo_search_input->value.

        WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
          ms_search_input-database_table_view = mo_search_input->value.

        WHEN zif_dbbr_c_object_browser_mode=>package.
          ms_search_input-package = mo_search_input->value.

        WHEN zif_dbbr_c_object_browser_mode=>query.
          ms_search_input-query = mo_search_input->value.
      ENDCASE.
    ELSE.
      RETURN.
    ENDIF.

    mv_current_search_type = mo_search_type_select->value.
    mo_search_type_select->set_value( |{ mv_current_search_type }| ).

    CASE mv_current_search_type.

      WHEN zif_dbbr_c_object_browser_mode=>cds_view.
        mo_search_input->set_value( |{ ms_search_input-cds_view }| ).

      WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
        mo_search_input->set_value( |{ ms_search_input-database_table_view }| ).

      WHEN zif_dbbr_c_object_browser_mode=>package.
        mo_search_input->set_value( |{ ms_search_input-package }| ).

      WHEN zif_dbbr_c_object_browser_mode=>query.
        mo_search_input->set_value( |{ ms_search_input-query }| ).

    ENDCASE.

    adapt_and_display_html_inp( ).

    update_toolbar( ).
  ENDMETHOD.


  METHOD on_toolbar_button.
    CASE fcode.

      WHEN c_functions-collapse_all.
        DATA(lt_selected_nodes) = mo_tree->get_selections( )->get_selected_nodes( ).
        LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lo_node>).
          mo_tree->get_nodes( )->collapse_node(
              iv_node_key       = <lo_node>->mv_node_key
              if_collapse_subtree = abap_true
          ).
        ENDLOOP.

      WHEN c_functions-search.
        NEW zcl_dbbr_obj_brws_search_sc( )->show( ).

      WHEN c_functions-show_help.
        zcl_dbbr_help_repository=>show_help( zcl_dbbr_help_repository=>c_help_id-object_search ).

      WHEN c_functions-use_and_instead_of_or.
        zcl_uitb_appl_util=>toggle( CHANGING value = mf_use_and_for_filter_opt ).
        update_search_settings_menu( ).

      WHEN c_functions-expand_all.
        lt_selected_nodes = mo_tree->get_selections( )->get_selected_nodes( ).
        LOOP AT lt_selected_nodes ASSIGNING <lo_node>.
          mo_tree->get_nodes( )->expand_node(
              iv_node_key       = <lo_node>->mv_node_key
          ).
        ENDLOOP.

      WHEN c_functions-to_parent.
        show_superordinate_tree( ).

      WHEN c_functions-my_cds_views_search OR
           c_functions-my_db_tables_views_search OR
           c_functions-my_queries_search.
        trigger_my_objects_search( EXPORTING iv_fcode = fcode ).

      WHEN c_functions-import_queries.
        NEW zcl_dbbr_query_importer( )->import_data( ).

      WHEN c_functions-export_queries.
        export_queries( ).

      WHEN c_functions-previous_history.
        go_to_previous_historic_entry( ).

      WHEN c_functions-next_history.
        go_to_next_historic_entry( ).

      WHEN c_functions-add_favorite.
        add_favorite( ).

      WHEN c_functions-edit_favorites.
        edit_favorites( ).

      WHEN OTHERS.
        IF fcode CP 'HIST*'.
          go_to_historic_entry( EXPORTING iv_function = fcode ).
        ELSEIF fcode CP 'FAV:*'.
          trigger_query_from_favorite( EXPORTING iv_function = fcode ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD show_superordinate_tree.
    DATA(lo_nodes) = mo_tree->get_nodes( ).

    CHECK lines( lo_nodes->get_root_node_keys( ) ) = 1.

    DATA(lo_top_node) = lo_nodes->get_top_node( ).
    DATA(lr_user_data) = CAST ty_s_user_data( lo_top_node->get_user_data( ) ).

    IF lr_user_data->node_type = c_node_type-cds_view OR
       lr_user_data->node_type = c_node_type-dbtable OR
       lr_user_data->node_type = c_node_type-view.

      SELECT SINGLE developmentpackage
        FROM zsat_i_databaseentity
        WHERE entity = @lr_user_data->entity_id
      INTO @DATA(lv_package).

    ELSEIF lr_user_data->node_type = c_node_type-package.
      SELECT SINGLE parentcl
        FROM tdevc
        WHERE devclass = @lr_user_data->entity_id
      INTO @lv_package.
    ELSE.
      RETURN.
    ENDIF.

    IF lv_package IS INITIAL.
      MESSAGE s085(zdbbr_info).
      RETURN.
    ENDIF.

    zcl_dbbr_screen_helper=>show_progress( iv_progress = 1 iv_text = |{ 'Loading parent package...'(025) }| ).

    cl_package=>load_package( EXPORTING i_package_name = lv_package IMPORTING e_package = DATA(lo_package) ).

    create_single_package_tree(
        io_package = lo_package
    ).


*.. Update object browser type and search field input
    mo_search_input->set_value( |{ lv_package }| ).
    mv_current_search_type = zif_dbbr_c_object_browser_mode=>package.
    mo_search_type_select->set_value( |{ zif_dbbr_c_object_browser_mode=>package }| ).
    TRY.
        mo_search_query = parse_query(
          iv_query         = |{ lv_package }|
          iv_browser_mode  = zif_dbbr_c_object_browser_mode=>package
        ).
      CATCH zcx_sat_object_search.
    ENDTRY.
    add_history_entry( ).

    adapt_and_display_html_inp( ).

  ENDMETHOD.

  METHOD add_history_entry.
    CHECK mo_search_query IS BOUND.

    LOOP AT mt_history_stack ASSIGNING FIELD-SYMBOL(<ls_history>).
      CLEAR: <ls_history>-is_current.
    ENDLOOP.

    INSERT VALUE ty_s_history(
        id         = |HIST{ lines( mt_history_stack ) + 1 }|
        query      = mo_search_query
        is_current = abap_true
    ) INTO mt_history_stack INDEX 1.

    IF lines( mt_history_stack ) > c_max_history.
      DELETE mt_history_stack FROM c_max_history + 1.
    ENDIF.

    update_history_buttons( ).
  ENDMETHOD.


  METHOD trigger_new_search.
    DATA: lt_search_result TYPE zsat_entity_t,
          lv_found_lines   TYPE sy-tabix.

    CLEAR rf_success.
    zcl_dbbr_screen_helper=>show_progress( iv_progress = 1 iv_text     = 'Searching...' ).

    TRY.
        CASE mv_current_search_type.

*........ New CDS View Search
          WHEN zif_dbbr_c_object_browser_mode=>cds_view.
            search_objects( IMPORTING et_results = lt_search_result ).
            lv_found_lines = lines( lt_search_result ).
            show_results_message( iv_result_count = lv_found_lines  iv_max_count = mo_search_query->mv_max_rows ).
            IF lv_found_lines = 0.
              clear_tree( ).
            ELSEIF lv_found_lines = 1.
              create_single_cds_view_tree( EXPORTING io_cds_view = zcl_sat_cds_view_factory=>read_cds_view( |{ lt_search_result[ 1 ]-entity_id }| ) ).
            ELSEIF lv_found_lines > 1.
              create_multiple_cds_view_tree( EXPORTING it_cds_view_header = lt_search_result ).
            ENDIF.

*........ New Package search
          WHEN zif_dbbr_c_object_browser_mode=>package.
            IF mo_search_query->mv_query CS '*'.
              DATA(lt_packages) = zcl_dbbr_package_factory=>find_packages( |{ mo_search_query->mv_query }| ).
              IF lines( lt_packages ) = 50.
                MESSAGE s084(zdbbr_info) WITH 50.
              ENDIF.
              create_multiple_package_tree( lt_packages ).
            ELSE.
              create_single_package_tree( zcl_dbbr_package_factory=>get_package( |{ mo_search_query->mv_query }| ) ).
            ENDIF.

*........ New Database Table/View search
          WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
            search_objects( IMPORTING et_results = lt_search_result ).
            lv_found_lines = lines( lt_search_result ).
            show_results_message( iv_result_count = lv_found_lines  iv_max_count = mo_search_query->mv_max_rows ).
            IF lv_found_lines = 0.
              clear_tree( ).
            ELSEIF lv_found_lines = 1.
              create_single_db_tab_tree( lt_search_result[ 1 ] ).
            ELSEIF lv_found_lines > 1.
              create_multiple_db_tab_tree( it_table_definition = lt_search_result ).
            ENDIF.

*........ New Query search
          WHEN zif_dbbr_c_object_browser_mode=>query.
            search_objects( IMPORTING et_results = lt_search_result ).
            lv_found_lines = lines( lt_search_result ).
            show_results_message( iv_result_count = lv_found_lines  iv_max_count = mo_search_query->mv_max_rows ).
            IF lv_found_lines = 0.
              clear_tree( ).
            ELSEIF lv_found_lines = 1.
              create_query_tree( lt_search_result ).
            ELSEIF lv_found_lines > 1.
              create_query_tree( lt_search_result ).
            ENDIF.
        ENDCASE.

        update_toolbar( ).
        rf_success = abap_true.
      CATCH zcx_dbbr_application_exc INTO DATA(lx_appl_error).
        lx_appl_error->zif_sat_exception_message~print( iv_display_type = 'E' ).
      CATCH zcx_sat_application_exc INTO DATA(lx_sat_appl_error).
        lx_sat_appl_error->zif_sat_exception_message~print( iv_display_type = 'E' ).
    ENDTRY.
  ENDMETHOD.

  METHOD show_results_message.
    IF iv_result_count = 0.
      MESSAGE s086(zdbbr_info).
    ELSEIF iv_result_count = 1.
      MESSAGE s094(zdbbr_info).
    ELSEIF iv_result_count > 1.
      IF iv_result_count >= iv_max_count.
        MESSAGE s084(zdbbr_info) WITH iv_max_count.
      ELSE.
        MESSAGE s087(zdbbr_info) WITH iv_result_count.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD update_toolbar.
    DATA(lo_nodes) = mo_tree->get_nodes( ).

    IF mv_current_search_type = zif_dbbr_c_object_browser_mode=>query.
      DATA(lf_import_export_visible) = abap_true.
    ENDIF.

    DATA(lf_to_parent_enabled) = abap_true.
    IF lines( lo_nodes->get_root_node_keys( ) ) <> 1.
      lf_to_parent_enabled = abap_false.
    ELSE.
*... disable also if a query is displayed
      DATA(lo_top_node) = lo_nodes->get_top_node( ).
      DATA(lr_user_data) = CAST ty_s_user_data( lo_top_node->get_user_data( ) ).
      TRY.
          IF lr_user_data->node_type = c_node_type-query.
            lf_to_parent_enabled = abap_false.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

    mo_toolbar->set_button_state(
      EXPORTING  enabled = lf_to_parent_enabled
                 fcode   = c_functions-to_parent
      EXCEPTIONS OTHERS  = 1
    ).

    mo_toolbar->set_button_visible(
      EXPORTING  visible = lf_import_export_visible
                 fcode   = c_functions-import_queries
      EXCEPTIONS OTHERS  = 1
    ).
    mo_toolbar->set_button_visible(
      EXPORTING  visible = lf_import_export_visible
                 fcode   = c_functions-export_queries
      EXCEPTIONS OTHERS  = 1
    ).

  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.
    DATA(ls_result) = mo_tree->get_search( )->find( ).
    IF ls_result IS NOT INITIAL.
      mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    DATA(ls_result) = mo_tree->get_search( )->find_next( ).
    IF ls_result IS NOT INITIAL.
      mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    mo_tree->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    rf_has_focus = mo_tree->zif_uitb_gui_control~has_focus( ).
  ENDMETHOD.

  METHOD trigger_my_objects_search.
    DATA: lv_search_type TYPE zdbbr_obj_browser_mode.

    CASE iv_fcode.

      WHEN c_functions-my_cds_views_search.
        lv_search_type = zif_dbbr_c_object_browser_mode=>cds_view.

      WHEN c_functions-my_db_tables_views_search.
        lv_search_type = zif_dbbr_c_object_browser_mode=>database_table_view.

      WHEN c_functions-my_queries_search.
        lv_search_type = zif_dbbr_c_object_browser_mode=>query.
    ENDCASE.

    mo_search_input->set_value( |owner:me| ).
    mo_search_type_select->set_value( |{ lv_search_type }| ).
    mv_current_search_type = lv_search_type.
    adapt_and_display_html_inp( ).
    on_search_input_enter( ).
  ENDMETHOD.


  METHOD update_history_buttons.
    DATA: lf_previous        TYPE abap_bool,
          lv_function_text   TYPE gui_text,
          lv_function_string TYPE char256,
          lf_next            TYPE abap_bool.

    FIELD-SYMBOLS: <ls_history> LIKE LINE OF mt_history_stack.

    DEFINE set_function_text.
      lv_function_string = SWITCH #( <ls_history>-query->mv_type
          WHEN zif_dbbr_c_object_browser_mode=>cds_view THEN `CDS: `
          WHEN zif_dbbr_c_object_browser_mode=>database_table_view THEN `DB Tab/View: `
          WHEN zif_dbbr_c_object_browser_mode=>package THEN `Package: `
          WHEN zif_dbbr_c_object_browser_mode=>query THEN `Query: `
      ) && <ls_history>-query->mv_query.
      lv_function_text = lv_function_string.
      IF strlen( lv_function_string ) > 40.
        lv_function_text+37 = '...'.
      ENDIF.
    END-OF-DEFINITION.

    DATA(lv_history_lines) = lines( mt_history_stack ).

    IF lv_history_lines < 2.
      lf_previous = abap_false.
      lf_next = abap_false.
    ELSE.
*.... Get current history cursor from table
      DATA(lv_current_entry) = line_index( mt_history_stack[ is_current = abap_true ] ).
      ASSERT lv_current_entry <> 0.

      IF lv_current_entry = 1.
        lf_previous = abap_true.
        lf_next = abap_false.
      ELSEIF lv_current_entry > 1 AND lv_current_entry < lv_history_lines.
        lf_previous = abap_true.
        lf_next = abap_true.
      ELSEIF lv_current_entry = lv_history_lines.
        lf_next = abap_true.
        lf_previous = abap_false.
      ENDIF.
    ENDIF.

    IF lf_next = abap_true.
      DATA(lo_next_menu) = NEW cl_ctmenu( ).
      LOOP AT mt_history_stack ASSIGNING <ls_history> TO lv_current_entry - 1.
        set_function_text.
        lo_next_menu->add_function(
            fcode = <ls_history>-id
            text  = lv_function_text
            insert_at_the_top = abap_true
        ).
      ENDLOOP.
      IF sy-subrc = 0.
        mo_toolbar->set_static_ctxmenu( fcode = c_functions-next_history ctxmenu = lo_next_menu ).
      ENDIF.
    ENDIF.

    IF lf_previous = abap_true.
      DATA(lo_previous_menu) = NEW cl_ctmenu( ).
      LOOP AT mt_history_stack ASSIGNING <ls_history> FROM lv_current_entry + 1.
        set_function_text.
        lo_previous_menu->add_function(
            fcode = <ls_history>-id
            text  = lv_function_text
        ).
      ENDLOOP.
      IF sy-subrc = 0.
        mo_toolbar->set_static_ctxmenu( fcode = c_functions-previous_history ctxmenu = lo_previous_menu ).
      ENDIF.
    ENDIF.

    mo_toolbar->set_button_state( fcode = c_functions-next_history     enabled = lf_next ).
    mo_toolbar->set_button_state( fcode = c_functions-previous_history enabled = lf_previous ).
  ENDMETHOD.

  METHOD show_historic_query.
    CHECK mo_search_query IS BOUND.

    mv_current_search_type = map_to_dbbr_query_type( mo_search_query->mv_type ).
    mo_search_type_select->set_value( |{ mv_current_search_type }| ).
    mo_search_input->set_value( |{ mo_search_query->mv_query }| ).

    update_toolbar( ).

    adapt_and_display_html_inp( ).
    focus( ).

    trigger_new_search( ).
  ENDMETHOD.

  METHOD go_to_previous_historic_entry.
    DATA(lv_current_entry) = line_index( mt_history_stack[ is_current = abap_true ] ).
    CHECK lv_current_entry <> 0.

    ADD 1 TO lv_current_entry.

    IF lv_current_entry <= lines( mt_history_stack ).
      mt_history_stack[ is_current = abap_true ]-is_current = abap_false.
      DATA(lr_new_entry) = REF #( mt_history_stack[ lv_current_entry ] ).
      lr_new_entry->is_current = abap_true.
      mo_search_query = lr_new_entry->query.
    ENDIF.

    update_history_buttons( ).
    show_historic_query( ).
  ENDMETHOD.

  METHOD go_to_next_historic_entry.
    DATA(lv_current_entry) = line_index( mt_history_stack[ is_current = abap_true ] ).
    CHECK lv_current_entry <> 0.

    SUBTRACT 1 FROM lv_current_entry.

    IF lv_current_entry >= 1.
      mt_history_stack[ is_current = abap_true ]-is_current = abap_false.
      DATA(lr_new_entry) = REF #( mt_history_stack[ lv_current_entry ] ).
      lr_new_entry->is_current = abap_true.
      mo_search_query = lr_new_entry->query.
    ENDIF.

    update_history_buttons( ).
    show_historic_query( ).
  ENDMETHOD.


  METHOD go_to_historic_entry.
    ASSIGN mt_history_stack[ id = iv_function ] TO FIELD-SYMBOL(<ls_history_entry>).
    CHECK sy-subrc = 0.

    ASSIGN mt_history_stack[ is_current = abap_true ] TO FIELD-SYMBOL(<ls_current_entry>).
    CHECK sy-subrc = 0.

    CLEAR: <ls_current_entry>-is_current.
    <ls_history_entry>-is_current = abap_true.
    mo_search_query = <ls_history_entry>-query.
    update_history_buttons( ).
    show_historic_query( ).
  ENDMETHOD.


  METHOD edit_favorites.
    NEW zcl_dbbr_ob_fav_manager( )->show(
        iv_top    = 5
        iv_left   = 10
        iv_width  = 120
        iv_height = 15
    ).
    fill_favorite_dd_menu( ).
  ENDMETHOD.


  METHOD add_favorite.
    DATA: lf_cancelled TYPE abap_bool.

    CHECK mo_search_input->value <> space.

*.. Parse the query again to prevent storing favorite with invalid query
    TRY.
        parse_query(
            iv_query        = |{ mo_search_input->value }|
            iv_browser_mode = mv_current_search_type
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_parse_error).
        MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    DATA(lo_popup) = zcl_uitb_pgv_factory=>create_single_field_popup(
        iv_title = |{ 'Enter name for storing the favorite' }|
        is_field = VALUE #(
          tabname   = 'ZDBBR_OBJBRSFAV'
          fieldname = 'FAVORITE_NAME'
          field_obl = abap_true
          value     = mo_search_input->value )
      )->show( ).

    IF NOT lo_popup->cancelled( ).
      DATA(lv_result) = lo_popup->get_first_field_value( ).

      IF zcl_dbbr_ob_fav_factory=>create_favorite(
          iv_query = |{ mo_search_input->value }|
          iv_type  = mv_current_search_type
          iv_name  = CONV #( lv_result ) ).
*.. Refill favorite menu as a new entry exists now
        fill_favorite_dd_menu( ).

        MESSAGE s090(zdbbr_info) WITH lv_result.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD trigger_query_from_favorite.
    DATA(ls_map_entry) = VALUE #( mt_fav_func_query_map[ function = iv_function ] OPTIONAL ).
    CHECK ls_map_entry IS NOT INITIAL.

    TRY.
        mo_search_query = parse_query(
          iv_query        = ls_map_entry-query
          iv_browser_mode = ls_map_entry-type
        ).
        show_historic_query( ).
        add_history_entry( ).
      CATCH zcx_sat_object_search INTO DATA(lx_parse_error).
        MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD parse_query.
    DATA(lv_search_type) = map_to_sat_query_type( iv_browser_mode ).
    DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance(
                                                           iv_contract = 'zif_sat_search_engine' ) ).
    ro_query = lo_search_engine->parse_query(
        iv_search_query = iv_query
        iv_search_type  = lv_search_type
    ).
  ENDMETHOD.

  METHOD update_search_settings_menu.
    IF mo_settings_menu IS INITIAL.
      mo_settings_menu = NEW cl_ctmenu( ).
    ELSE.
      mo_settings_menu->clear( ).
    ENDIF.

    mo_settings_menu->add_function(
        fcode   = c_functions-use_and_instead_of_or
        text    = |{ 'Use "AND" for search options'(044) }|
        checked = mf_use_and_for_filter_opt
    ).
    CLEAR mo_search_query.
  ENDMETHOD.

  METHOD search_objects.
    DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance(
                                  iv_contract = 'zif_sat_search_engine'
                                  iv_filter   = |{ mo_search_query->mv_type }| )
                                ).
    lo_search_engine->search_objects_by_query(
      EXPORTING io_query                = mo_search_query
                is_search_engine_params = VALUE #( use_and_cond_for_options = mf_use_and_for_filter_opt )
      IMPORTING et_results              = et_results
    ).

  ENDMETHOD.

  METHOD map_to_dbbr_query_type.
    rv_browser_type = SWITCH #( iv_sat_type
      WHEN zif_sat_c_object_search=>c_search_type-cds_view    THEN zif_dbbr_c_object_browser_mode=>cds_view
      WHEN zif_sat_c_object_search=>c_search_type-db_tab_view THEN zif_dbbr_c_object_browser_mode=>database_table_view
      WHEN zif_dbbr_c_object_browser=>c_search_type-query     THEN zif_dbbr_c_object_browser_mode=>query
      WHEN zif_dbbr_c_object_browser=>c_search_type-package   THEN zif_dbbr_c_object_browser_mode=>package
    ).
  ENDMETHOD.

  METHOD map_to_sat_query_type.
    rv_sat_type = SWITCH #( iv_browser_type
      WHEN zif_dbbr_c_object_browser_mode=>cds_view THEN zif_sat_c_object_search=>c_search_type-cds_view
      WHEN zif_dbbr_c_object_browser_mode=>database_table_view THEN zif_sat_c_object_search=>c_search_type-db_tab_view
      WHEN zif_dbbr_c_object_browser_mode=>query THEN zif_dbbr_c_object_browser=>c_search_type-query
      WHEN zif_dbbr_c_object_browser_mode=>package THEN zif_dbbr_c_object_browser=>c_search_type-package
    ).
  ENDMETHOD.

ENDCLASS.
