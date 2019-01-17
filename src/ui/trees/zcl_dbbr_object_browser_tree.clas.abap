"! <p class="shorttext synchronized" lang="en">Repository tree for Navigator</p>
CLASS zcl_dbbr_object_browser_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_gui_control .
    INTERFACES zif_uitb_content_searcher .
    INTERFACES zif_dbbr_c_object_browser .
    INTERFACES zif_dbbr_ty_object_browser .
    INTERFACES zif_dbbr_c_object_browser_mode.

    ALIASES c_node_type
      FOR zif_dbbr_c_object_browser~c_tree_node_type .
    ALIASES c_search_option
      FOR zif_dbbr_c_object_browser~c_search_option .
    ALIASES focus
      FOR zif_uitb_gui_control~focus .
    ALIASES has_focus
      FOR zif_uitb_gui_control~has_focus .
    ALIASES ty_search
      FOR zif_dbbr_ty_object_browser~ty_search .

    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !ir_parent TYPE REF TO cl_gui_container .

  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mr_control
      FOR zif_uitb_gui_control~mr_control .

    TYPES:
      BEGIN OF ty_node_map,
        node_key  TYPE tm_nodekey,
        entity_id TYPE zdbbr_entity_id,
        node_type TYPE i,
      END OF ty_node_map .
    TYPES:
      BEGIN OF mty_node_data.
        INCLUDE TYPE treemsnod.
    TYPES: items TYPE treemcitab.
    TYPES: END OF mty_node_data .

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
      END OF c_functions .
    CONSTANTS c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT.
    CONSTANTS c_hierarchy_node3 TYPE tv_itmname VALUE 'HIER3' ##NO_TEXT.
    CONSTANTS c_top_node TYPE tm_nodekey VALUE '00001' ##NO_TEXT.
    CLASS-DATA gt_api_state_texts TYPE ddfixvalues .
    DATA mr_input_dd TYPE REF TO cl_dd_document .
    DATA mr_parent TYPE REF TO cl_gui_container .
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory .
    DATA mr_search_input TYPE REF TO cl_dd_input_element .
    DATA mr_search_type_select TYPE REF TO cl_dd_select_element .
    DATA mr_splitter TYPE REF TO cl_gui_splitter_container .
    DATA mr_tree_model TYPE REF TO zcl_uitb_column_tree_model .
    DATA mr_variant_f TYPE REF TO zcl_dbbr_variant_factory .
    DATA:
      BEGIN OF ms_search_input,
        package             TYPE string,
        cds_view            TYPE string,
        database_table_view TYPE string,
        query               TYPE string,
      END OF ms_search_input .
    DATA mr_search_query TYPE REF TO zcl_dbbr_object_search_query.
    DATA mt_node_map TYPE SORTED TABLE OF ty_node_map WITH UNIQUE KEY node_key .
    DATA mv_current_search_type TYPE zdbbr_obj_browser_mode .
    DATA mr_favorite_dd_menu TYPE REF TO cl_ctmenu .

    "! <p class="shorttext synchronized" lang="en">Clears tree of all nodes</p>
    "!
    METHODS clear_tree .
    "! Create input controls for search type and entity
    "! @parameter iv_initial_value | the initial value
    METHODS create_input_dd
      IMPORTING
        !iv_initial_value TYPE string OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for multiple cds views</p>
    "!
    "! @parameter it_cds_view_header | <p class="shorttext synchronized" lang="en">List of cds view headers</p>
    "! @parameter ir_parent_node | <p class="shorttext synchronized" lang="en">Parent node reference</p>
    METHODS create_multiple_cds_view_tree
      IMPORTING
        !it_cds_view_header TYPE zdbbr_entity_t
        !ir_parent_node     TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for multiple packages</p>
    "!
    "! @parameter it_table_definition | <p class="shorttext synchronized" lang="en">List of table definitions</p>
    "! @parameter ir_parent_node | <p class="shorttext synchronized" lang="en">Parent node reference</p>
    METHODS create_multiple_db_tab_tree
      IMPORTING
        !it_table_definition TYPE zdbbr_entity_t
        !ir_parent_node      TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for multiple packages</p>
    "!
    "! @parameter it_package_def | <p class="shorttext synchronized" lang="en"></p>
    METHODS create_multiple_package_tree
      IMPORTING
        !it_package_def TYPE zcl_dbbr_package_factory=>tt_package .
    "! <p class="shorttext synchronized" lang="en">Create query sub tree</p>
    "!
    "! @parameter iv_query | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_query_node | <p class="shorttext synchronized" lang="en"></p>
    METHODS create_query_sub_tree
      IMPORTING
        !iv_query      TYPE zdbbr_query_name
        !iv_query_node TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Create query tree</p>
    "!
    "! @parameter it_queries | <p class="shorttext synchronized" lang="en"></p>
    METHODS create_query_tree
      IMPORTING
        !it_queries TYPE zdbbr_query_info_itab .
    "! <p class="shorttext synchronized" lang="en">Create tree for single cds view</p>
    "!
    "! @parameter ir_cds_view | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_node | <p class="shorttext synchronized" lang="en"></p>
    METHODS create_single_cds_view_tree
      IMPORTING
        !ir_cds_view TYPE REF TO zcl_dbbr_cds_view
        !ir_node     TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create tree for single table/view</p>
    "!
    "! @parameter is_table_info | <p class="shorttext synchronized" lang="en"></p>
    METHODS create_single_db_tab_tree
      IMPORTING
        !is_table_info TYPE zdbbr_entity .
    "! <p class="shorttext synchronized" lang="en">Build Tree for a single package</p>
    "!
    "! @parameter ir_package | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_node | <p class="shorttext synchronized" lang="en"></p>
    METHODS create_single_package_tree
      IMPORTING
        !ir_package TYPE REF TO if_package
        !ir_node    TYPE REF TO zcl_uitb_ctm_node OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Creates splitter</p>
    METHODS create_splitter .
    "! <p class="shorttext synchronized" lang="en">Create sub nodes for single CDS node</p>
    "!
    "! @parameter ir_cds_view | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_cds_view_node | <p class="shorttext synchronized" lang="en">Reference to CDS View node</p>
    "! @raising zcx_uitb_tree_error | <p class="shorttext synchronized" lang="en">General Error in tree processing</p>
    METHODS create_subnodes_for_cds
      IMPORTING
        !ir_cds_view      TYPE REF TO zcl_dbbr_cds_view
        !ir_cds_view_node TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Creates Tree</p>
    METHODS create_tree .
    "! <p class="shorttext synchronized" lang="en">Expand a single CDS View</p>
    "!
    "! @parameter iv_cds_view | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS expand_cds_view_node
      IMPORTING
        !iv_cds_view TYPE zdbbr_cds_view_name
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand db tabs subnode of package node</p>
    "!
    "! @parameter iv_package | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS expand_package_dbtabs_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand ddls subnode of package node</p>
    "!
    "! @parameter iv_package | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS expand_package_ddls_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand the package</p>
    "!
    "! @parameter iv_package | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS expand_package_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand views subnode of package node</p>
    "!
    "! @parameter iv_package | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS expand_package_views_node
      IMPORTING
        !iv_package  TYPE devclass
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand a single Query</p>
    "!
    "! @parameter iv_query | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS expand_query_node
      IMPORTING
        !iv_query    TYPE zdbbr_entity_id
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Expand a single Database view</p>
    "!
    "! @parameter iv_view | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS expand_view_node
      IMPORTING
        !iv_view     TYPE zdbbr_entity_id
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Fill favorites dropdown</p>
    "!
    METHODS fill_favorite_dd_menu .
    "! <p class="shorttext synchronized" lang="en">Fill toolbar for tree</p>
    METHODS fill_toolbar .
    METHODS node_image_for_node_type
      IMPORTING
        !iv_node_type   TYPE i
      RETURNING
        VALUE(rv_image) TYPE tv_image .
    "! <p class="shorttext synchronized" lang="en">Handler for DISPLAY_OBJECT_LIST event</p>
    "!
    "! @parameter ev_entity_id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ev_entity_type | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_display_object_list
          FOR EVENT display_object_list OF zcl_dbbr_selscr_nav_events
      IMPORTING
          !ev_entity_id
          !ev_entity_type .
    "! <p class="shorttext synchronized" lang="en">Handler for when children are to be loaded lazily</p>
    METHODS on_expand_no_children
          FOR EVENT expand_no_children OF zif_uitb_tree_model_events
      IMPORTING
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for External object search request</p>
    "!
    "! @parameter ev_object_type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ev_search_query | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ef_close_popup | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_external_object_search_req
          FOR EVENT object_search OF zcl_dbbr_selscr_nav_events
      IMPORTING
          ev_object_type
          ev_search_query
          ef_close_popup.
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
    "! <p class="shorttext synchronized" lang="en">Handler for ENTER key press on node</p>
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
    METHODS on_search_input_enter
        FOR EVENT entered OF cl_dd_input_element .
    "! <p class="shorttext synchronized" lang="en">Dropdown Selection changed handler</p>
    METHODS on_search_type_selected
        FOR EVENT selected OF cl_dd_select_element .
    "! <p class="shorttext synchronized" lang="en">Handler for pressed toolbar button</p>
    METHODS on_toolbar_button
          FOR EVENT function_selected OF zif_uitb_toolbar_events
      IMPORTING
          !ev_fcode .
    "! <p class="shorttext synchronized" lang="en">Show superordinate tree of current object</p>
    "!
    METHODS show_superordinate_tree .
    "! <p class="shorttext synchronized" lang="en">Trigger a new search</p>
    "!
    "! @parameter rf_success | <p class="shorttext synchronized" lang="en">'X' if an error ocurred</p>
    METHODS trigger_new_search
      RETURNING
        VALUE(rf_success) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Update toolbar for tree</p>
    "!
    "! @parameter if_multiple_tree | <p class="shorttext synchronized" lang="en">Tree contains multiple entries</p>
    METHODS update_toolbar
      IMPORTING
        !if_multiple_tree TYPE abap_bool OPTIONAL .

ENDCLASS.



CLASS zcl_dbbr_object_browser_tree IMPLEMENTATION.


  METHOD class_constructor.
    gt_api_state_texts = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE zdbbr_cds_api_state( ) ) )->get_ddic_fixed_values( ).
  ENDMETHOD.


  METHOD clear_tree.
    mr_tree_model->get_nodes( )->delete_all_nodes( ).
    CLEAR: mt_node_map.
  ENDMETHOD.


  METHOD constructor.
    mr_parent = ir_parent.
    create_splitter( ).
    create_tree( ).
    create_input_dd( ).

    SET HANDLER:
        on_display_object_list,
        on_external_object_search_req.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD create_input_dd.
    mr_input_dd = NEW cl_dd_document( ).

*.. get global data for setting some initial values
    DATA(lr_s_global_data) = CAST zdbbr_global_data( zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main )->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).

    mr_input_dd->initialize_document(  no_margins = abap_true ).

*.. Fill the document
    mr_input_dd->add_form( IMPORTING formarea = DATA(lr_form) ).
    lr_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lr_form->add_select_element(
      EXPORTING
        tooltip        = |{ 'Object Category'(006) }|
        options        = VALUE #(
          ( value = zif_dbbr_c_object_browser_mode=>cds_view            text = 'CDS View'(002) )
          ( value = zif_dbbr_c_object_browser_mode=>database_table_view text = 'Database Table/View'(003) )
          ( value = zif_dbbr_c_object_browser_mode=>query               text = 'Query'(008) )
          ( value = zif_dbbr_c_object_browser_mode=>package             text = 'Package'(001) )
        )
      IMPORTING
        select_element = mr_search_type_select
    ).

    mr_search_type_select->set_value( COND #( WHEN lr_s_global_data->initial_obj_brws_mode IS NOT INITIAL THEN lr_s_global_data->initial_obj_brws_mode ELSE zif_dbbr_c_object_browser_mode=>cds_view ) ).
    mv_current_search_type = mr_search_type_select->value.

    lr_form->line_with_layout( end = abap_true ).
    lr_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lr_form->add_input_element(
      EXPORTING
        size          = 50    " Length of Input Field
        tooltip       = |{ 'Enter name of Entity'(004) }|
        maxlength     = 200    " Maximum Permitted Text Entry Length
      IMPORTING
        input_element = mr_search_input
    ).
    mr_search_input->set_value( |{ iv_initial_value }| ).

    lr_form->add_button(
      EXPORTING
        sap_icon = |ICON_DISPLAY|
        tooltip  = |{ 'Perform search'(007) }|
      IMPORTING
        button   = DATA(lr_search_button)
    ).

    lr_form->line_with_layout( end = abap_true ).

*.. Register event handlers for form elements
    SET HANDLER:
      on_search_input_enter FOR mr_search_input,
      on_perform_search FOR lr_search_button,
      on_search_type_selected FOR mr_search_type_select.

    mr_input_dd->merge_document( ).

    mr_input_dd->display_document( parent        = mr_splitter->get_container( row = 1 column = 1 )
                                   reuse_control = abap_true ).

  ENDMETHOD.


  METHOD create_multiple_cds_view_tree.
    CHECK it_cds_view_header IS NOT INITIAL.

    IF ir_parent_node IS INITIAL.
      clear_tree( ).
    ENDIF.

*.. Create nodes/subnodes for multiple cds views
*... Only the top nodes of each view will be loaded at first
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).

    LOOP AT it_cds_view_header ASSIGNING FIELD-SYMBOL(<ls_cds_header>).
      DATA(lr_cds_view_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_relative_node_key = COND #( WHEN ir_parent_node IS NOT INITIAL THEN ir_parent_node->mv_node_key )
          iv_image             = zif_dbbr_c_icon=>cds_view
          iv_expanded_image    = zif_dbbr_c_icon=>cds_view
          if_expander          = abap_true
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = <ls_cds_header>-entity_id_raw )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = <ls_cds_header>-description )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_cds_view_node->mv_node_key
          entity_id = <ls_cds_header>-entity_id
          node_type = c_node_type-cds_view )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD create_multiple_db_tab_tree.
    DATA: lv_node_image TYPE tv_image,
          lv_node_type  TYPE i.

    CHECK it_table_definition IS NOT INITIAL.

    IF ir_parent_node IS INITIAL.
      clear_tree( ).
    ENDIF.

*.. Create nodes/subnodes for multiple tables/views
*... Only the top nodes of each view will be loaded at first
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).

    LOOP AT it_table_definition ASSIGNING FIELD-SYMBOL(<ls_tabname>).
      DATA(lf_expander) = abap_false.
      IF <ls_tabname>-entity_type = zif_dbbr_c_entity_type=>view.
        lv_node_image = zif_dbbr_c_icon=>database_view.
        lv_node_type = c_node_type-view.
        lf_expander = abap_true.
      ELSE.
        lv_node_image = zif_dbbr_c_icon=>database_table.
        lv_node_type = c_node_type-dbtable.
      ENDIF.

      DATA(lr_table_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_image             = lv_node_image
          iv_relative_node_key = COND #( WHEN ir_parent_node IS NOT INITIAL THEN ir_parent_node->mv_node_key )
          iv_expanded_image    = lv_node_image
          if_expander          = lf_expander
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = <ls_tabname>-entity_id )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = <ls_tabname>-description )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_table_node->mv_node_key
          entity_id = <ls_tabname>-entity_id
          node_type = lv_node_type )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD create_multiple_package_tree.
    CHECK it_package_def IS NOT INITIAL.

    clear_tree( ).

*.. Create nodes/subnodes for multiple packages
*... Only the top nodes of each view will be loaded at first
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).

    LOOP AT it_package_def ASSIGNING FIELD-SYMBOL(<ls_package>).
      DATA(lr_package_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>package
          iv_expanded_image    = zif_dbbr_c_icon=>package
          if_expander          = abap_true
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = <ls_package>-package )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = <ls_package>-ddtext )
          )
      ).

      mt_node_map = VALUE #( BASE mt_node_map
        ( node_key  = lr_package_node->mv_node_key
          entity_id = <ls_package>-package
          node_type = c_node_type-package )
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
    CHECK mr_search_query IS BOUND.

    IF mr_search_query->mv_search_string CS '*'.
      DATA(lf_wild_card_search) = abap_true.
      CHECK it_queries IS NOT INITIAL.
    ELSE.
      IF it_queries IS INITIAL.
        MESSAGE s050(zdbbr_exception) WITH mr_search_query->mv_search_string DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.

    clear_tree( ).

    DATA(lr_nodes) = mr_tree_model->get_nodes( ).
    LOOP AT it_queries ASSIGNING FIELD-SYMBOL(<ls_query>).
      DATA(lr_query_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>query
          iv_expanded_image    = zif_dbbr_c_icon=>query
          if_expander          = COND #( WHEN lf_wild_card_search = abap_true THEN abap_true ELSE abap_false )
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = <ls_query>-query_name )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = <ls_query>-description )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_query_node->mv_node_key
          entity_id = <ls_query>-query_name
          node_type = c_node_type-query )
      ).

      IF lf_wild_card_search = abap_false.
        create_query_sub_tree(
            iv_query      = <ls_query>-query_name
            iv_query_node = lr_query_node->mv_node_key
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_single_cds_view_tree.
    DATA: lv_node_type  TYPE i,
          lv_node_image TYPE tv_image.

*.. Create node/subnodes for a single cds view
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).
    DATA(lr_cds_view_node) = ir_node.

*.. If node is bound, expand the node to load the sub entities
    IF lr_cds_view_node IS INITIAL.

      clear_tree( ).

      lr_cds_view_node = lr_nodes->add_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>cds_view
          iv_expanded_image    = zif_dbbr_c_icon=>cds_view
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = ir_cds_view->get_header( )-entityname_raw )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = ir_cds_view->get_description( ) )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_cds_view_node->mv_node_key
          entity_id = ir_cds_view->mv_view_name
          node_type = c_node_type-cds_view )
      ).
    ENDIF.

*.. Create Sub nodes
    create_subnodes_for_cds(
      ir_cds_view      = ir_cds_view
      ir_cds_view_node = lr_cds_view_node
    ).

    lr_nodes->expand_node(
        iv_node_key = lr_cds_view_node->mv_node_key
    ).
  ENDMETHOD.


  METHOD create_single_db_tab_tree.
    DATA: lv_node_image TYPE tv_image,
          lv_node_type  TYPE i.

    clear_tree( ).

*.. Create nodes/subnodes for a single database table/view
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).
    IF is_table_info-entity_type = zif_dbbr_c_entity_type=>view.
      lv_node_image = zif_dbbr_c_icon=>database_view.
      lv_node_type = c_node_type-view.
    ELSE.
      lv_node_image = zif_dbbr_c_icon=>database_table.
      lv_node_type = c_node_type-dbtable.
    ENDIF.

    DATA(lr_table_node) = lr_nodes->add_node(
        if_folder            = abap_true
        iv_image             = lv_node_image
        iv_expanded_image    = lv_node_image
*        ir_user_object       =
        it_item_table        = VALUE #(
          ( class     = cl_item_tree_model=>item_class_text
            item_name = mr_tree_model->c_hierarchy_column
            text      = is_table_info-entity_id )
          ( class     = cl_item_tree_model=>item_class_text
            item_name = c_hierarchy_node2
            style     = zif_uitb_c_ctm_style=>inverted_gray
            text      = is_table_info-description )
        )
    ).
    mt_node_map = VALUE #(
      BASE mt_node_map
      ( node_key  = lr_table_node->mv_node_key
        entity_id = is_table_info-entity_id
        node_type = lv_node_type )
    ).

    IF is_table_info-entity_type = zif_dbbr_c_entity_type=>view.
      expand_view_node(
          iv_view     = is_table_info-entity_id
          iv_node_key = lr_table_node->mv_node_key
      ).
    ENDIF.

  ENDMETHOD.


  METHOD create_single_package_tree.
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).

    DATA(lr_package_node) = ir_node.
    IF lr_package_node IS INITIAL.
      clear_tree( ).

*.. Create nodes/subnodes for a single cds view
      lr_package_node = lr_nodes->add_node(
          if_folder            = abap_true
          iv_image             = zif_dbbr_c_icon=>package
          iv_expanded_image    = zif_dbbr_c_icon=>package
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = ir_package->package_name )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = ir_package->short_text )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_package_node->mv_node_key
          entity_id = ir_package->package_name
          node_type = c_node_type-package )
      ).
    ENDIF.

*.. Add sub nodes for subpackages, cds view, views and database tables
    ir_package->get_sub_packages(
        IMPORTING  e_sub_packages = DATA(lt_sub_packages)
        EXCEPTIONS OTHERS = 1
    ).
    IF lt_sub_packages IS NOT INITIAL.
      LOOP AT lt_sub_packages ASSIGNING FIELD-SYMBOL(<lr_subpackage>).
        DATA(lr_subpackage_node) = lr_nodes->add_node(
            if_folder            = abap_true
            iv_relative_node_key = lr_package_node->mv_node_key
            iv_image             = zif_dbbr_c_icon=>package
            iv_expanded_image    = zif_dbbr_c_icon=>package
            if_expander          = abap_true
            it_item_table        = VALUE #(
              ( class     = cl_item_tree_model=>item_class_text
                item_name = mr_tree_model->c_hierarchy_column
                text      = <lr_subpackage>->package_name )
              ( class     = cl_item_tree_model=>item_class_text
                item_name = c_hierarchy_node2
                style     = zif_uitb_c_ctm_style=>inverted_gray
                text      = <lr_subpackage>->short_text )
            )
        ).
        mt_node_map = VALUE #(
          BASE mt_node_map
          ( node_key  = lr_subpackage_node->mv_node_key
            entity_id = <lr_subpackage>->package_name
            node_type = c_node_type-package )
        ).
      ENDLOOP.
    ENDIF.

    ir_package->get_elements( IMPORTING e_elements = DATA(lt_elements) EXCEPTIONS OTHERS = 1 ).
    IF lt_elements IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lf_has_ddls) = abap_false.

    LOOP AT lt_elements ASSIGNING FIELD-SYMBOL(<lr_element>).
      IF <lr_element>->dev_elem_type = 'DDLS'.
        lf_has_ddls = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lf_has_ddls = abap_true.
      DATA(lr_cdsviews_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_relative_node_key = lr_package_node->mv_node_key
          if_expander          = abap_true
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = |{ 'Data Definitions'(014) }| )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_cdsviews_node->mv_node_key
          entity_id = ir_package->package_name
          node_type = c_node_type-pak_ddl_folder )
      ).
    ENDIF.

*.. Check table/view count in package
    SELECT *
      FROM zdbbr_i_databaseentityaggr( p_language = @sy-langu )
      WHERE developmentpackage = @ir_package->package_name
    INTO TABLE @DATA(lt_entity_count_in_package).

    IF line_exists( lt_entity_count_in_package[ type = zif_dbbr_c_entity_type=>view ] ).
      DATA(lr_views_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_relative_node_key = lr_package_node->mv_node_key
          if_expander          = abap_true
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = |{ 'Views'(020) }| )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_views_node->mv_node_key
          entity_id = ir_package->package_name
          node_type = c_node_type-pak_view_folder )
      ).
    ENDIF.


    IF line_exists( lt_entity_count_in_package[ type = zif_dbbr_c_entity_type=>table ] ).
      DATA(lr_database_tables_node) = lr_nodes->add_node(
            if_folder            = abap_true
            iv_relative_node_key = lr_package_node->mv_node_key
            if_expander          = abap_true
            it_item_table        = VALUE #(
              ( class     = cl_item_tree_model=>item_class_text
                item_name = mr_tree_model->c_hierarchy_column
                text      = |{ 'Database Tables'(021) }| )
            )
        ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_database_tables_node->mv_node_key
          entity_id = ir_package->package_name
          node_type = c_node_type-pak_dtab_folder )
      ).
    ENDIF.

*... Expand top package node
    mr_tree_model->get_nodes( )->expand_node( lr_package_node->mv_node_key ).
  ENDMETHOD.


  METHOD create_splitter.
    mr_splitter = NEW cl_gui_splitter_container(
        parent      = mr_parent
        rows        = 2
        columns     = 1
    ).

    mr_splitter->set_row_height( id = 1 height = 50 ).
    mr_splitter->set_row_sash(
      id    = 1
      type  = cl_gui_splitter_container=>type_movable
      value = cl_gui_splitter_container=>false
    ).
    mr_splitter->set_row_sash(
      id    = 1
      type  = cl_gui_splitter_container=>type_sashvisible
      value = cl_gui_splitter_container=>false
    ).

    mr_splitter->set_row_mode( cl_gui_splitter_container=>mode_absolute ).

  ENDMETHOD.


  METHOD create_subnodes_for_cds.

    DATA: lv_node_type              TYPE i,
          lr_api_states_parent_node TYPE REF TO zcl_uitb_ctm_node,
          lv_node_image             TYPE tv_image.

    DATA(lr_nodes) = mr_tree_model->get_nodes( ).

*.. Create "Properties" Sub node
    DATA(lr_cds_properties_node) = lr_nodes->add_node(
       if_folder            = abap_true
       iv_relative_node_key = ir_cds_view_node->mv_node_key
       it_item_table        = VALUE #(
         ( class     = cl_item_tree_model=>item_class_text
           item_name = mr_tree_model->c_hierarchy_column
           text      = 'Properties'(017) )
       )
    ).

    mt_node_map = VALUE #(
      BASE mt_node_map
      ( node_key  = lr_cds_properties_node->mv_node_key
        entity_id = ir_cds_view->mv_view_name
        node_type = c_node_type-cds_properties )
    ).

    DATA(ls_tadir_info) = ir_cds_view->get_tadir_info( ).
*.. Insert node for name of author
    lr_nodes->add_node(
        iv_relative_node_key = lr_cds_properties_node->mv_node_key
        iv_image             = zif_dbbr_c_icon=>user_menu
        iv_expanded_image    = zif_dbbr_c_icon=>user_menu
        it_item_table        = VALUE #(
          ( class     = cl_item_tree_model=>item_class_text
            text      = |{ 'Responsible'(018) }|
            item_name = mr_tree_model->c_hierarchy_column )
          ( class     = cl_item_tree_model=>item_class_text
            text      = |{ ls_tadir_info-created_by }|
            item_name = c_hierarchy_node2
            style     = zif_uitb_c_ctm_style=>inverted_blue )
        )
    ).
*.. Insert node for created date
    lr_nodes->add_node(
        iv_relative_node_key = lr_cds_properties_node->mv_node_key
        iv_image             = zif_dbbr_c_icon=>date
        iv_expanded_image    = zif_dbbr_c_icon=>date
        it_item_table        = VALUE #(
          ( class     = cl_item_tree_model=>item_class_text
            text      = |{ 'Created On'(033) }|
            item_name = mr_tree_model->c_hierarchy_column )
          ( class     = cl_item_tree_model=>item_class_text
            text      = |{ ls_tadir_info-created_date DATE = USER }|
            item_name = c_hierarchy_node2
            style     = zif_uitb_c_ctm_style=>inverted_blue )
        )
    ).

*.. Insert nodes for found API states
*.. Create "API States" Sub node
    DATA(lt_api_states) = ir_cds_view->get_api_states( ).
    IF lines( lt_api_states ) > 1.
      DATA(lr_cds_api_states_node) = lr_nodes->add_node(
         if_folder            = abap_true
         iv_relative_node_key = lr_cds_properties_node->mv_node_key
         it_item_table        = VALUE #(
           ( class     = cl_item_tree_model=>item_class_text
             item_name = mr_tree_model->c_hierarchy_column
             text      = |{ 'API States'(019) }| )
         )
      ).
      lr_api_states_parent_node = lr_cds_api_states_node.

      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_cds_api_states_node->mv_node_key
          entity_id = ir_cds_view->mv_view_name
          node_type = c_node_type-cds_api_states )
      ).
    ELSE.
      lr_api_states_parent_node = lr_cds_properties_node.
    ENDIF.

    LOOP AT lt_api_states ASSIGNING FIELD-SYMBOL(<lv_api_state>).
      CASE <lv_api_state>.

        WHEN zif_dbbr_c_cds_api_state=>not_released.
          DATA(lf_not_released) = abap_true.
      ENDCASE.

      DATA(lv_icon) = COND tv_image( WHEN lf_not_released = abap_true THEN zif_dbbr_c_icon=>incomplete ELSE zif_dbbr_c_icon=>checked ).

      DATA(lr_api_state_node) = lr_nodes->add_node(
         if_folder            = abap_false
         iv_relative_node_key = lr_api_states_parent_node->mv_node_key
         iv_image             = lv_icon
         iv_expanded_image    = lv_icon
         it_item_table        = VALUE #(
           ( class     = cl_item_tree_model=>item_class_text
             item_name = mr_tree_model->c_hierarchy_column
             text      = gt_api_state_texts[ low = <lv_api_state> ]-ddtext )
         )
     ).
    ENDLOOP.

*.. Create "FROM PART" Sub node
    DATA(lt_base_tables) = ir_cds_view->get_base_tables( ).
    IF lt_base_tables IS NOT INITIAL.
      DATA(lr_cds_select_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_relative_node_key = ir_cds_view_node->mv_node_key
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = 'Select From'(012) )
          )
      ).

      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_cds_select_node->mv_node_key
          entity_id = ir_cds_view->mv_view_name
          node_type = c_node_type-cds_tables )
      ).

      LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
        lv_node_type = COND #(
          WHEN <ls_base_table>-table_kind = zif_dbbr_c_entity_type=>table AND
               <ls_base_table>-is_db_view = abap_true                           THEN c_node_type-view
          WHEN <ls_base_table>-table_kind = zif_dbbr_c_entity_type=>table AND
               <ls_base_table>-is_db_view = abap_false                          THEN c_node_type-dbtable
          WHEN <ls_base_table>-table_kind = zif_dbbr_c_entity_type=>cds_view    THEN c_node_type-cds_view
        ).
        lv_node_image = node_image_for_node_type( lv_node_type ).

        DATA(lr_cds_base_table_node) = lr_nodes->add_node(
            if_folder            = abap_true
            iv_relative_node_key = lr_cds_select_node->mv_node_key
            iv_image             = lv_node_image
            iv_expanded_image    = lv_node_image
            if_expander          = xsdbool( lv_node_type <> c_node_type-dbtable )
            it_item_table        = VALUE #(
              ( class     = cl_item_tree_model=>item_class_text
                item_name = mr_tree_model->c_hierarchy_column
                text      = <ls_base_table>-entityname_raw )
              ( class     = cl_item_tree_model=>item_class_text
                item_name = c_hierarchy_node2
                style     = zif_uitb_c_ctm_style=>inverted_gray
                text      = <ls_base_table>-description )
            )
        ).

        mt_node_map = VALUE #(
         BASE mt_node_map
         ( node_key  = lr_cds_base_table_node->mv_node_key
           entity_id = <ls_base_table>-entityname
           node_type = lv_node_type )
       ).
      ENDLOOP.

    ENDIF.

*.. Create "Associations" Sub node
    DATA(lt_assoc) = ir_cds_view->get_associations( ).
    IF lt_assoc IS NOT INITIAL.
      DATA(lr_cds_assoc_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_relative_node_key = ir_cds_view_node->mv_node_key
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = 'Associations'(013) )
          )
      ).

      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_cds_assoc_node->mv_node_key
          entity_id = ir_cds_view->mv_view_name
          node_type = c_node_type-cds_assocs )
      ).

      LOOP AT lt_assoc ASSIGNING FIELD-SYMBOL(<ls_assoc>).
        lv_node_type = SWITCH #( <ls_assoc>-kind
           WHEN zif_dbbr_c_cds_assoc_type=>entity OR
                zif_dbbr_c_cds_assoc_type=>table_function THEN c_node_type-cds_view
           WHEN zif_dbbr_c_cds_assoc_type=>table OR
                zif_dbbr_c_cds_assoc_type=>view           THEN c_node_type-dbtable
        ).
        lv_node_image = node_image_for_node_type( lv_node_type ).

        DATA(lr_cds_assoc_view_node) = lr_nodes->add_node(
            if_folder            = abap_true
            iv_relative_node_key = lr_cds_assoc_node->mv_node_key
            if_expander          = abap_true
            iv_image             = lv_node_image
            iv_expanded_image    = lv_node_image
            it_item_table        = VALUE #(
              ( class     = cl_item_tree_model=>item_class_text
                item_name = mr_tree_model->c_hierarchy_column
                text      = |{ <ls_assoc>-raw_name }| )
              ( class     = cl_item_tree_model=>item_class_text
                text      = |({ <ls_assoc>-ref_cds_view_raw })|
                item_name = c_hierarchy_node2
                style     = zif_uitb_c_ctm_style=>inverted_blue )
            )
        ).
        mt_node_map = VALUE #(
          BASE mt_node_map
          ( node_key  = lr_cds_assoc_view_node->mv_node_key
            entity_id = <ls_assoc>-ref_cds_view
            node_type = lv_node_type )
        ).


      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD create_tree.
    DATA: lt_events TYPE cntl_simple_events.

    CHECK mr_tree_model IS INITIAL.

    mr_tree_model = NEW zcl_uitb_column_tree_model(
        ir_parent           = mr_splitter->get_container( row = 2 column = 1 )
        if_auto_node_key    = abap_true

        is_hierarchy_header = VALUE #(
          heading = 'Object Name'(010)
*          width   = 50
        )
        if_with_toolbar     = abap_true
        iv_selection_mode   = cl_tree_model=>node_sel_mode_multiple
    ).

    mr_tree_model->get_columns( )->add_hierarchy_column( iv_colname = c_hierarchy_node2 ).
    mr_tree_model->get_columns( )->add_hierarchy_column( iv_colname = c_hierarchy_node3 ).

    fill_toolbar( ).

    mr_tree_model->create_tree_control( ).

    DATA(lr_events) = mr_tree_model->get_events( ).

    lr_events->add_key_for_keypress( cl_tree_model=>key_delete ).

*.. set event handler methods to tree control
    SET HANDLER:
      on_node_context_menu_request FOR lr_events,
      on_node_context_menu_select FOR lr_events,
      on_node_double_click FOR lr_events,
      on_node_enter_key FOR lr_events,
      on_expand_no_children FOR lr_events.
  ENDMETHOD.


  METHOD expand_cds_view_node.
*.. load cds view entity
    TRY.
        create_single_cds_view_tree(
            ir_cds_view = zcl_dbbr_cds_view_factory=>read_cds_view( iv_cds_view )
            ir_node     = mr_tree_model->get_nodes( )->get_node( iv_node_key )
        ).
      CATCH zcx_dbbr_data_read_error ##needed.
    ENDTRY.
  ENDMETHOD.


  METHOD expand_package_dbtabs_node.
    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading DB Tables of Package'(023) } { iv_package }| iv_progress = 1 ).

    DATA(lt_dbtabs) = zcl_dbbr_dictionary_helper=>find_database_tab_view(
        iv_package = iv_package
        if_all     = abap_true
        iv_type    = zif_dbbr_c_entity_type=>table
    ).

    create_multiple_db_tab_tree(
        it_table_definition = lt_dbtabs
        ir_parent_node      = mr_tree_model->get_nodes( )->get_node( iv_node_key )
    ).

    mr_tree_model->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_package_ddls_node.
    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading DDLS of Package'(022) } { iv_package }| iv_progress = 1 ).
    DATA(lt_header_in_package) = zcl_dbbr_cds_view_factory=>find_cds_views(
      iv_package  = iv_package
      iv_max_rows = 0
    ).

    create_multiple_cds_view_tree(
        it_cds_view_header = lt_header_in_package
        ir_parent_node     = mr_tree_model->get_nodes( )->get_node( iv_node_key )
    ).

    mr_tree_model->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_package_node.
    cl_package=>load_package( EXPORTING i_package_name = iv_package IMPORTING e_package = DATA(lr_package) EXCEPTIONS OTHERS = 1 ).
    IF lr_package IS BOUND.
      create_single_package_tree(
          ir_package = lr_package
          ir_node    = mr_tree_model->get_nodes( )->get_node( iv_node_key )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD expand_package_views_node.
    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading DB Views of Package'(024) } { iv_package }| iv_progress = 1 ).
    DATA(lt_views) = zcl_dbbr_dictionary_helper=>find_database_tab_view(
        iv_package = iv_package
        if_all     = abap_true
        iv_type    = zif_dbbr_c_entity_type=>view
    ).

    create_multiple_db_tab_tree(
        it_table_definition = lt_views
        ir_parent_node      = mr_tree_model->get_nodes( )->get_node( iv_node_key )
    ).

    mr_tree_model->get_nodes( )->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_query_node.
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).
    DATA(lr_node) = lr_nodes->get_node( iv_node_key ).

*.. Retrieve base tables of view
    DATA(lt_base_tables) = NEW zcl_dbbr_query_factory( )->get_tables_of_query( iv_query ).
    LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
      DATA(lv_node_type) = SWITCH #(
        <ls_base_table>-entity_type
        WHEN zif_dbbr_c_entity_type=>table THEN c_node_type-dbtable
        WHEN zif_dbbr_c_entity_type=>view  THEN c_node_type-view
      ).
      DATA(lv_image) = node_image_for_node_type( lv_node_type ).
      DATA(lr_base_table_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_relative_node_key = iv_node_key
          iv_image             = lv_image
          iv_expanded_image    = lv_image
          if_expander          = COND #( WHEN lv_node_type = c_node_type-view THEN abap_true )
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = <ls_base_table>-entity_id )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = <ls_base_table>-description )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_base_table_node->mv_node_key
          entity_id = <ls_base_table>-entity_id
          node_type = lv_node_type
        )
      ).
    ENDLOOP.

    lr_nodes->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD expand_view_node.
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).
    DATA(lr_node) = lr_nodes->get_node( iv_node_key ).

*.. Retrieve base tables of view
    DATA(lt_base_tables) = zcl_dbbr_dictionary_helper=>find_base_tables_of_view( iv_view_name = iv_view ).
    LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
      DATA(lv_node_type) = SWITCH #(
        <ls_base_table>-entity_type
        WHEN zif_dbbr_c_entity_type=>table THEN c_node_type-dbtable
        WHEN zif_dbbr_c_entity_type=>view  THEN c_node_type-view
      ).
      DATA(lv_image) = node_image_for_node_type( lv_node_type ).
      DATA(lr_base_table_node) = lr_nodes->add_node(
          if_folder            = abap_true
          iv_relative_node_key = iv_node_key
          iv_image             = lv_image
          iv_expanded_image    = lv_image
*          if_expander          = abap_true
          it_item_table        = VALUE #(
            ( class     = cl_item_tree_model=>item_class_text
              item_name = mr_tree_model->c_hierarchy_column
              text      = <ls_base_table>-entity_id )
            ( class     = cl_item_tree_model=>item_class_text
              item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              text      = <ls_base_table>-description )
          )
      ).
      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key  = lr_base_table_node->mv_node_key
          entity_id = <ls_base_table>-entity_id
          node_type = lv_node_type
        )
      ).
    ENDLOOP.

    lr_nodes->expand_node( iv_node_key ).
  ENDMETHOD.


  METHOD fill_favorite_dd_menu.
    mr_favorite_dd_menu->clear( ).
    mr_favorite_dd_menu->add_function(
        fcode = c_functions-add_favorite
        text  = |{ 'Add'(027) }|
    ).
    mr_favorite_dd_menu->add_function(
        fcode = c_functions-edit_favorites
        text  = |{ 'Edit...'(028) }|
    ).

  ENDMETHOD.


  METHOD fill_toolbar.
    DATA(lr_tb_model) = mr_tree_model->get_toolbar( ).

    lr_tb_model->add_expander_buttons( ).
    lr_tb_model->add_separator( ).
    lr_tb_model->add_search_buttons( ).
    lr_tb_model->add_separator( ).
    lr_tb_model->add_button(
        iv_fcode     = c_functions-to_parent
        iv_icon      = icon_previous_hierarchy_level
        iv_quickinfo = |{ 'Superordinate Object List'(011) }|
    ).
    lr_tb_model->add_button(
        iv_fcode     = c_functions-favorite_dropdown
        iv_icon      = icon_system_favorites
        iv_quickinfo = |{ 'Favorites'(026) }|
        iv_butn_type = cntb_btype_menu
    ).
    mr_favorite_dd_menu = NEW cl_ctmenu( ).
    fill_favorite_dd_menu( ).
    lr_tb_model->add_button_menu( iv_function = c_functions-favorite_dropdown ir_menu = mr_favorite_dd_menu ).
    lr_tb_model->add_separator( ).
    lr_tb_model->add_button(
        iv_fcode     = 'HELP'
        iv_icon      = icon_information
        iv_quickinfo = 'Show input help'
    ).

    SET HANDLER: on_toolbar_button FOR lr_tb_model.
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

    mr_search_type_select->set_value(
      SWITCH #( ev_entity_type
        WHEN zif_dbbr_c_entity_type=>cds_view THEN zif_dbbr_c_object_browser_mode=>cds_view
        WHEN zif_dbbr_c_entity_type=>view  OR
             zif_dbbr_c_entity_type=>table    THEN zif_dbbr_c_object_browser_mode=>database_table_view
        WHEN zif_dbbr_c_entity_type=>query    THEN zif_dbbr_c_object_browser_mode=>query
      )
    ).
    mv_current_search_type = mr_search_type_select->value.
    mr_search_input->set_value( |{ ev_entity_id }| ).

    update_toolbar( ).

    mr_input_dd->merge_document( ).

    mr_input_dd->display_document(
        reuse_control      = abap_true
        reuse_registration = abap_true
    ).

    TRY.
        mr_search_query = zcl_dbbr_object_search_query=>parse_query_string(
           iv_query       = |{ ev_entity_id }|
           iv_search_type = mv_current_search_type
        ).
        CHECK mr_search_query->has_search_string( ).
        trigger_new_search( ).
      CATCH zcx_dbbr_object_search.
    ENDTRY.

  ENDMETHOD.


  METHOD on_expand_no_children.
*... Check which action should be depending on the node type
    DATA(ls_node_map) = VALUE #( mt_node_map[ node_key = ev_node_key ] OPTIONAL ).
    IF ls_node_map IS INITIAL.
      RETURN.
    ENDIF.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ 'Loading Tree Nodes...'(015) }| iv_progress = 1 ).

    CASE ls_node_map-node_type.

      WHEN c_node_type-cds_view.
        expand_cds_view_node(
           iv_node_key = ev_node_key
           iv_cds_view = ls_node_map-entity_id
        ).

      WHEN c_node_type-package.
        expand_package_node(
            iv_package  = ls_node_map-entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-pak_dtab_folder.
        expand_package_dbtabs_node(
            iv_package  = ls_node_map-entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-pak_ddl_folder.
        expand_package_ddls_node(
            iv_package  = ls_node_map-entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-pak_view_folder.
        expand_package_views_node(
            iv_package  = ls_node_map-entity_id
            iv_node_key = ev_node_key
        ).

      WHEN c_node_type-dbtable.

      WHEN c_node_type-view.
        expand_view_node(
           iv_view     = ls_node_map-entity_id
           iv_node_key = ev_node_key
        ).

      WHEN c_node_type-query.
        expand_query_node(
           iv_query    = ls_node_map-entity_id
           iv_node_key = ev_node_key
        ).
    ENDCASE.

  ENDMETHOD.


  METHOD on_external_object_search_req.
    mv_current_search_type = ev_object_type.

    TRY.
        mr_search_query = zcl_dbbr_object_search_query=>parse_query_string(
           iv_query       = |{ ev_search_query }|
           iv_search_type = mv_current_search_type
        ).
      CATCH zcx_dbbr_application_exc INTO DATA(lx_appl_error).
        lx_appl_error->show_message( iv_message_type = 'S' ).
        RETURN.
    ENDTRY.

    IF trigger_new_search( ).

      mr_search_type_select->set_value( |{ mv_current_search_type }| ).
      mr_search_input->set_value( |{ ev_search_query }| ).

      update_toolbar( ).

      mr_input_dd->merge_document( ).

      mr_input_dd->display_document(
          reuse_control      = abap_true
          reuse_registration = abap_true
      ).
      focus( ).

      IF ef_close_popup = abap_true.
        zcl_dbbr_selscr_nav_events=>raise_close_object_srch_dialog( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    DATA(lt_selected_nodes) = mr_tree_model->get_selections( )->get_selected_nodes( ).
    IF lines( lt_selected_nodes ) = 1.
      DATA(lr_selected_node) = lt_selected_nodes[ 1 ].
      ASSIGN mt_node_map[ node_key = lr_selected_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CASE <ls_node_map>-node_type.

        WHEN c_node_type-cds_view OR
             c_node_type-dbtable OR
             c_node_type-view OR
             c_node_type-query.

          IF <ls_node_map>-node_type <> c_node_type-query.
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
        WHEN OTHERS.
      ENDCASE.
    ELSE.
*.... Several nodes are selected
      LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lr_node>).
        ASSIGN mt_node_map[ node_key = <lr_node>->mv_node_key ] TO <ls_node_map>.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    FIELD-SYMBOLS: <ls_node_map> TYPE ty_node_map.

    ASSIGN mt_node_map[ node_key = ev_node_key ] TO <ls_node_map>.
    CHECK sy-subrc = 0.

    DATA(lv_entity_type) = SWITCH #(
        <ls_node_map>-node_type
        WHEN c_node_type-cds_view THEN zif_dbbr_c_entity_type=>cds_view
        WHEN c_node_type-dbtable  THEN zif_dbbr_c_entity_type=>table
        WHEN c_node_type-view     THEN zif_dbbr_c_entity_type=>table
    ).

    CASE ev_fcode.

      WHEN c_functions-open_with_adt.

        TRY.
            zcl_dbbr_adt_util=>jump_adt(
                iv_obj_name     = SWITCH #( <ls_node_map>-node_type
                    WHEN c_node_type-cds_view THEN
                        zcl_dbbr_cds_view_factory=>get_ddl_for_entity_name( <ls_node_map>-entity_id )
                    ELSE
                        <ls_node_map>-entity_id
                )
                iv_obj_type     = SWITCH #(
                    <ls_node_map>-node_type
                    WHEN c_node_type-cds_view THEN 'DDLS'
                    WHEN c_node_type-dbtable  THEN 'TABD'
                    WHEN c_node_type-view     THEN 'VIEW'
                )
            ).
          CATCH zcx_dbbr_adt_error INTO DATA(lx_adt_error).
          CATCH zcx_dbbr_data_read_error INTO DATA(lx_data_read_error).
        ENDTRY.

      WHEN c_functions-open_in_new_window.
        CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_OPEN_JUMP'
          EXPORTING
            iv_entity_id       = <ls_node_map>-entity_id
            iv_entity_type     = lv_entity_type
            if_load_parameters = abap_true.

      WHEN c_functions-exec_with_dbbrs_new_window.
        CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_EXEC_JUMP'
          EXPORTING
            iv_entity_id       = <ls_node_map>-entity_id
            iv_entity_type     = lv_entity_type
            if_skip_selscreen  = abap_true
            if_load_parameters = abap_true.

      WHEN c_functions-exec_with_dbbrs.
        DATA(lr_variant_starter) = zcl_dbbr_variant_starter_fac=>create_variant_starter(
            iv_variant_id        = zif_dbbr_global_consts=>c_dummy_variant
            iv_entity_type       = lv_entity_type
            iv_variant_entity_id = CONV #( <ls_node_map>-entity_id )
        ).

        lr_variant_starter->initialize( ).
        TRY.
            lr_variant_starter->execute_variant( ).
          CATCH zcx_dbbr_variant_error INTO DATA(lx_variant_error).
            lx_variant_error->show_message( ).
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


  METHOD on_node_double_click.
    DATA(lr_s_node_map) = REF #( mt_node_map[ node_key = ev_node_key ] OPTIONAL ).
    IF lr_s_node_map IS INITIAL.
*.... if the node is just a folder toggle the expansion state

      RETURN.
    ENDIF.

    IF lr_s_node_map->node_type = c_node_type-cds_view OR
       lr_s_node_map->node_type = c_node_type-query OR
       lr_s_node_map->node_type = c_node_type-dbtable OR
       lr_s_node_map->node_type = c_node_type-view.

      zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
          iv_entity_id   = lr_s_node_map->entity_id
          iv_entity_type = SWITCH #(
            lr_s_node_map->node_type
            WHEN c_node_type-cds_view THEN zif_dbbr_c_entity_type=>cds_view
            WHEN c_node_type-query    THEN zif_dbbr_c_entity_type=>query
            WHEN c_node_type-dbtable  THEN zif_dbbr_c_entity_type=>table
            WHEN c_node_type-view     THEN zif_dbbr_c_entity_type=>table
          )
      ).
    ELSEIF lr_s_node_map->node_type = c_node_type-cds_tables OR
           lr_s_node_map->node_type = c_node_type-cds_assocs OR
           lr_s_node_map->node_type = c_node_type-cds_properties.
      mr_tree_model->get_nodes( )->toggle_node( ev_node_key ).
    ENDIF.
  ENDMETHOD.


  METHOD on_node_enter_key.
    on_node_double_click( ev_node_key ).
  ENDMETHOD.


  METHOD on_perform_search.
    CHECK mr_search_input->value IS NOT INITIAL.

    TRY.
        mr_search_query = zcl_dbbr_object_search_query=>parse_query_string(
           iv_query       = |{ mr_search_input->value }|
           iv_search_type = mv_current_search_type
        ).
      CATCH zcx_dbbr_application_exc INTO DATA(lx_parse_error).
        MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    trigger_new_search( ).
  ENDMETHOD.


  METHOD on_search_input_enter.
    CHECK mr_search_input->value IS NOT INITIAL.

    TRY.
        mr_search_query = zcl_dbbr_object_search_query=>parse_query_string(
           iv_query       = |{ mr_search_input->value }|
           iv_search_type = mv_current_search_type
        ).
      CATCH zcx_dbbr_application_exc INTO DATA(lx_parse_error).
        MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    trigger_new_search( ).
  ENDMETHOD.


  METHOD on_search_type_selected.
    CASE mv_current_search_type.

      WHEN zif_dbbr_c_object_browser_mode=>cds_view.
        ms_search_input-cds_view = mr_search_input->value.

      WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
        ms_search_input-database_table_view = mr_search_input->value.

      WHEN zif_dbbr_c_object_browser_mode=>package.
        ms_search_input-package = mr_search_input->value.

      WHEN zif_dbbr_c_object_browser_mode=>query.
        ms_search_input-query = mr_search_input->value.
    ENDCASE.

    mv_current_search_type = mr_search_type_select->value.
    mr_search_type_select->set_value( |{ mv_current_search_type }| ).


    CASE mv_current_search_type.

      WHEN zif_dbbr_c_object_browser_mode=>cds_view.
        mr_search_input->set_value( |{ ms_search_input-cds_view }| ).

      WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
        mr_search_input->set_value( |{ ms_search_input-database_table_view }| ).

      WHEN zif_dbbr_c_object_browser_mode=>package.
        mr_search_input->set_value( |{ ms_search_input-package }| ).

      WHEN zif_dbbr_c_object_browser_mode=>query.
        mr_search_input->set_value( |{ ms_search_input-query }| ).

    ENDCASE.

    mr_input_dd->merge_document( ).

    mr_input_dd->display_document(
        reuse_control      = abap_true
        reuse_registration = abap_true
    ).
  ENDMETHOD.


  METHOD on_toolbar_button.
    CASE ev_fcode.

      WHEN zif_uitb_c_toolbar_functions=>collapse_all.
        DATA(lt_selected_nodes) = mr_tree_model->get_selections( )->get_selected_nodes( ).
        LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lr_node>).
          mr_tree_model->get_nodes( )->collapse_node(
              iv_node_key       = <lr_node>->mv_node_key
              if_collapse_subtree = abap_true
          ).
        ENDLOOP.

      WHEN c_functions-search.
        NEW zcl_dbbr_obj_brws_search_sc( )->show( ).

      WHEN c_functions-show_help.
        zcl_dbbr_help_repository=>show_help( zcl_dbbr_help_repository=>c_help_id-object_search ).

      WHEN zif_uitb_c_toolbar_functions=>expand_all.
        lt_selected_nodes = mr_tree_model->get_selections( )->get_selected_nodes( ).
        LOOP AT lt_selected_nodes ASSIGNING <lr_node>.
          mr_tree_model->get_nodes( )->expand_node(
              iv_node_key       = <lr_node>->mv_node_key
          ).
        ENDLOOP.

      WHEN c_functions-to_parent.
        show_superordinate_tree( ).

    ENDCASE.
  ENDMETHOD.


  METHOD show_superordinate_tree.
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).

    CHECK lines( lr_nodes->get_root_node_keys( ) ) = 1.

    DATA(lr_top_node) = lr_nodes->get_top_node( ).
    DATA(lr_s_node_map) = REF #( mt_node_map[ node_key = lr_top_node->mv_node_key ] ).

    IF lr_s_node_map->node_type = c_node_type-cds_view OR
       lr_s_node_map->node_type = c_node_type-dbtable OR
       lr_s_node_map->node_type = c_node_type-view.

      SELECT SINGLE developmentpackage
        FROM zdbbr_i_databaseentity( p_language = @sy-langu )
        WHERE entity = @lr_s_node_map->entity_id
      INTO @DATA(lv_package).

    ELSEIF lr_s_node_map->node_type = c_node_type-package.
      SELECT SINGLE parentcl
        FROM tdevc
        WHERE devclass = @lr_s_node_map->entity_id
      INTO @lv_package.
    ELSE.
      RETURN.
    ENDIF.

    IF lv_package IS INITIAL.
      MESSAGE s085(zdbbr_info).
      RETURN.
    ENDIF.

    zcl_dbbr_screen_helper=>show_progress( iv_progress = 1 iv_text = |{ 'Loading parent package...'(025) }| ).

    cl_package=>load_package( EXPORTING i_package_name = lv_package IMPORTING e_package = DATA(lr_package) ).

    create_single_package_tree(
        ir_package = lr_package
    ).

*.. Update object browser type and search field input
    mr_search_input->set_value( |{ lv_package }| ).
    mv_current_search_type = zif_dbbr_c_object_browser_mode=>package.
    mr_search_type_select->set_value( |{ zif_dbbr_c_object_browser_mode=>package }| ).

    mr_input_dd->merge_document( ).

    mr_input_dd->display_document(
        reuse_control      = abap_true
        reuse_registration = abap_true
    ).

  ENDMETHOD.


  METHOD trigger_new_search.
    DATA: lt_search_result TYPE zdbbr_entity_t,
          lv_found_lines   TYPE sy-tabix.

    CLEAR rf_success.
    zcl_dbbr_screen_helper=>show_progress( iv_progress = 1 iv_text     = 'Searching...' ).

    TRY.
        CASE mv_current_search_type.

*........ New CDS View Search
          WHEN zif_dbbr_c_object_browser_mode=>cds_view.
            lt_search_result = NEW zcl_dbbr_ob_cds_searcher( ir_query = mr_search_query )->zif_dbbr_object_searcher~search( ).
            lv_found_lines = lines( lt_search_result ).
            IF lv_found_lines = 0.
              MESSAGE s086(zdbbr_info).
              clear_tree( ).
            ELSEIF lv_found_lines = 1.
              create_single_cds_view_tree( EXPORTING ir_cds_view = zcl_dbbr_cds_view_factory=>read_cds_view( |{ lt_search_result[ 1 ]-entity_id }| ) ).
              MESSAGE s087(zdbbr_info) WITH 1.
            ELSEIF lv_found_lines > 1.
              IF lv_found_lines >= mr_search_query->mv_max_rows.
                MESSAGE s084(zdbbr_info) WITH mr_search_query->mv_max_rows.
              ELSE.
                MESSAGE s087(zdbbr_info) WITH lv_found_lines.
              ENDIF.
              create_multiple_cds_view_tree( EXPORTING it_cds_view_header = lt_search_result ).
            ENDIF.

*........ New Package search
          WHEN zif_dbbr_c_object_browser_mode=>package.
            IF mr_search_query->mv_search_string CS '*'.
              DATA(lt_packages) = zcl_dbbr_package_factory=>find_packages( |{ mr_search_query->mv_search_string }| ).
              IF lines( lt_packages ) = 50.
                MESSAGE s084(zdbbr_info) WITH 50.
              ENDIF.
              create_multiple_package_tree( lt_packages ).
            ELSE.
              create_single_package_tree( zcl_dbbr_package_factory=>get_package( |{ mr_search_query->mv_search_string }| ) ).
            ENDIF.

*........ New Database Table/View search
          WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
            lt_search_result = NEW zcl_dbbr_ob_dbtab_searcher( ir_query = mr_search_query )->zif_dbbr_object_searcher~search( ).
            lv_found_lines = lines( lt_search_result ).
            IF lv_found_lines = 0.
              MESSAGE s086(zdbbr_info).
              clear_tree( ).
            ELSEIF lv_found_lines = 1.
              create_single_db_tab_tree( lt_search_result[ 1 ] ).
              MESSAGE s087(zdbbr_info) WITH 1.
            ELSEIF lv_found_lines > 1.
              IF lv_found_lines >= mr_search_query->mv_max_rows.
                MESSAGE s084(zdbbr_info) WITH mr_search_query->mv_max_rows.
              ELSE.
                MESSAGE s087(zdbbr_info) WITH lv_found_lines.
              ENDIF.
              create_multiple_db_tab_tree( it_table_definition = lt_search_result ).
            ENDIF.

*........ New Query search
          WHEN zif_dbbr_c_object_browser_mode=>query.
            NEW zcl_dbbr_query_factory( )->find_queries( EXPORTING iv_query_name = |{ mr_search_query->mv_search_string }| IMPORTING et_queries = DATA(lt_queries) ).
            IF lines( lt_queries ) = 50.
              MESSAGE s084(zdbbr_info) WITH 50.
            ENDIF.
            create_query_tree( EXPORTING it_queries = lt_queries ).
        ENDCASE.

        update_toolbar( ).
        rf_success = abap_true.
      CATCH zcx_dbbr_application_exc INTO DATA(lx_appl_error).
        lx_appl_error->zif_dbbr_exception_message~print( iv_display_type = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD update_toolbar.
    DATA(lr_nodes) = mr_tree_model->get_nodes( ).

    DATA(lf_to_parent_enabled) = abap_true.
    IF lines( lr_nodes->get_root_node_keys( ) ) <> 1.
      lf_to_parent_enabled = abap_false.
    ELSE.
*... disable also if a query is displayed
      DATA(lr_top_node) = lr_nodes->get_top_node( ).
      TRY.
          IF mt_node_map[ node_key = lr_top_node->mv_node_key ]-node_type = c_node_type-query.
            lf_to_parent_enabled = abap_false.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

    DATA(lr_toolbar) = mr_tree_model->get_toolbar( ).
    lr_toolbar->set_button_state( iv_function = c_functions-to_parent
                                  if_enabled  = lf_to_parent_enabled ).
    lr_toolbar->refresh_ui( ).
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.
    DATA(ls_result) = mr_tree_model->get_search( )->find( ).
    IF ls_result IS NOT INITIAL.
      mr_tree_model->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    DATA(ls_result) = mr_tree_model->get_search( )->find_next( ).
    IF ls_result IS NOT INITIAL.
      mr_tree_model->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    mr_tree_model->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    rf_has_focus = mr_tree_model->zif_uitb_gui_control~has_focus( ).
  ENDMETHOD.

ENDCLASS.
