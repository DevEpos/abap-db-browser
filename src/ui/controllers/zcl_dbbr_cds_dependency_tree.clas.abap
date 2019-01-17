"! <p class="shorttext synchronized" lang="en">Dependency Tree for CDS View</p>
CLASS zcl_dbbr_cds_dependency_tree DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_view.
    INTERFACES zif_dbbr_c_object_browser .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        iv_cds_view TYPE zdbbr_cds_view_name.

    ALIASES show
      FOR zif_uitb_view~show.
    ALIASES c_node_type
      FOR zif_dbbr_c_object_browser~c_tree_node_type .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_functions,
        open_with_adt              TYPE ui_func VALUE 'ADTJUMP',
        open_in_new_window         TYPE ui_func VALUE 'OPENINDBBRSNEWWIN',
        exec_with_dbbrs            TYPE ui_func VALUE 'EXECWIHTDBBRS',
        exec_with_dbbrs_new_window TYPE ui_func VALUE 'EXECWIHTDBBRSNEW',
      END OF c_functions .

    TYPES:
      BEGIN OF ty_node_map,
        node_key    TYPE tm_nodekey,
        entity_id   TYPE zdbbr_entity_id,
        entity_type TYPE zdbbr_entity_type,
        sql_name    TYPE tabname,
      END OF ty_node_map .
    CONSTANTS c_relation_col TYPE tv_itmname VALUE 'RELATION' ##NO_TEXT.
    CONSTANTS c_entity_name_col TYPE tv_itmname VALUE 'DBOBJECT' ##NO_TEXT.
    CONSTANTS c_object_type_col TYPE tv_itmname VALUE 'OBJTYPE' ##NO_TEXT.
    CONSTANTS c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT.
    CONSTANTS c_fill_text_func TYPE sy-ucomm VALUE 'FILLTXT' ##no_text.

    DATA mr_tree TYPE REF TO zcl_uitb_column_tree_model .
    DATA mr_view TYPE REF TO zif_uitb_template_prog .
    DATA mv_view_name TYPE zdbbr_cds_view_name .
    DATA mt_node_map TYPE STANDARD TABLE OF ty_node_map WITH KEY node_key.
    DATA ms_dependencies TYPE cl_ddls_dependency_visitor=>ty_s_dependency_graph_node.
    DATA ms_metrics_map TYPE cl_dd_ddl_meta_num_collector=>entity2number_map.

    "! <p class="shorttext synchronized" lang="en">Add new node to the tree</p>
    "!
    METHODS add_node
      IMPORTING
        is_node        TYPE cl_ddls_dependency_visitor=>ty_s_dependency_graph_node
        iv_parent_node TYPE tm_nodekey OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Create tree model</p>
    "!
    METHODS create_tree .
    "! <p class="shorttext synchronized" lang="en">Fill Toolbar buttons</p>
    METHODS fill_toolbar .

    "! <p class="shorttext synchronized" lang="en">Fetches CDS dependencies</p>
    "!
    METHODS fetch_dependencies
      RETURNING
        VALUE(rf_dependencies_read) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Retrieve Text for constant</p>
    "!
    METHODS get_relation_text
      IMPORTING
        iv_constant    TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Get text for object type</p>
    "!
    METHODS get_object_type_text
      IMPORTING
        iv_constant    TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Get icon for node type</p>
    "!
    METHODS get_icon_for_node
      IMPORTING
        iv_type        TYPE string
      RETURNING
        VALUE(rv_icon) TYPE tv_image.
    METHODS fill_entity_texts.
    METHODS show_complexity_metrics.
    "! <p class="shorttext synchronized" lang="en">Handler for PAI Event</p>
    "!
    METHODS on_pai
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          !er_callback
          !ev_function_id .
    "! <p class="shorttext synchronized" lang="en">Handler for PBO event</p>
    "!
    METHODS on_pbo
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          !er_callback .

    "! <p class="shorttext synchronized" lang="en">Handler for toolbar button event</p>
    "!
    METHODS on_toolbar_button
          FOR EVENT function_selected OF zif_uitb_toolbar_events
      IMPORTING
          ev_fcode.
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
ENDCLASS.



CLASS zcl_dbbr_cds_dependency_tree IMPLEMENTATION.


  METHOD constructor.
    mv_view_name = iv_cds_view.

    mr_view = zcl_uitb_templt_prog_callback=>create_template_program( |{ 'SQL Dependency Tree for'(002) } { mv_view_name }| ).


    mr_view->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_f7
        iv_text        = |{ 'Show Complexity Metrics'(032) }|
        iv_icon        = icon_calculation
        iv_icon_text   = |{ 'Metrics'(033) }|
    ).

    SET HANDLER:
      on_pai FOR mr_view,
      on_pbo FOR mr_view.
  ENDMETHOD.


  METHOD create_tree.
    mr_tree = NEW zcl_uitb_column_tree_model(
      ir_parent           = mr_view->get_container( )
      is_hierarchy_header = VALUE #(
        heading = 'SQL Name'(001)
        width   = 110
      )
*      if_item_selection   =
      if_with_toolbar     = abap_true
      if_auto_node_key    = abap_true
      iv_selection_mode   = cl_tree_model=>node_sel_mode_multiple
    ).

    SET HANDLER:
      on_node_context_menu_request FOR mr_tree->get_events( ),
      on_node_context_menu_select FOR mr_tree->get_events( ),
      on_node_double_click FOR mr_tree->get_events( ).

*.. Create further columns
    DATA(lr_columns) = mr_tree->get_columns( ).
    lr_columns->add_hierarchy_column( c_hierarchy_node2 ).
    lr_columns->add_column(
        iv_colname        = c_relation_col
        iv_width          = 20
        iv_header_text    = |{ 'SQL Relation'(034) }|
    ).
    lr_columns->add_column(
        iv_colname        = c_entity_name_col
        iv_width          = 40
        iv_header_text    = |{ 'Entity Name'(035) }|
    ).
    lr_columns->add_column(
        iv_colname        = c_object_type_col
        iv_width          = 30
        iv_header_text    = |{ 'Object Type'(036) }|
    ).

    fill_toolbar( ).
    zcl_uitb_screen_util=>show_progress( iv_progress = 50 iv_text = |{ 'Building Dependency Tree...'(003) }| ).
    add_node(
      is_node = ms_dependencies
    ).

    mr_tree->get_nodes( )->expand_root_nodes( ).

    mr_tree->create_tree_control( ).
    mr_tree->get_nodes( )->set_first_root_node_as_top( ).
  ENDMETHOD.


  METHOD fill_toolbar.
    DATA(lr_toolbar) = mr_tree->get_toolbar( ).
    SET HANDLER:
      on_toolbar_button FOR lr_toolbar.

    lr_toolbar->add_expander_buttons( ).
  ENDMETHOD.

  METHOD fetch_dependencies.
    rf_dependencies_read = abap_true.

    zcl_uitb_screen_util=>show_progress( iv_progress = 1 iv_text = |{ 'Analyzing CDS Dependencies...'(004) }| ).
    ms_dependencies = zcl_dbbr_cds_dep_analyzer=>analyze_dependency( iv_cds_view_name = mv_view_name ).
    IF ms_dependencies IS INITIAL.
      rf_dependencies_read = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD add_node.
    DATA: lv_entity_type TYPE zdbbr_entity_type.

    DATA(lr_nodes) = mr_tree->get_nodes( ).
    DATA(lr_node) = lr_nodes->add_node(
        iv_relative_node_key = iv_parent_node
        iv_image             = get_icon_for_node( is_node-type )
        iv_expanded_image    = get_icon_for_node( is_node-type )
        it_item_table        = VALUE #(
          ( item_name = mr_tree->c_hierarchy_column
            class     = cl_item_tree_model=>item_class_text
            text      = |{ is_node-name }| )
          ( item_name = c_hierarchy_node2
            class     = cl_item_tree_model=>item_class_text
            style     = zif_uitb_c_ctm_style=>inverted_gray )
          ( item_name = c_entity_name_col
            class     = cl_item_tree_model=>item_class_text
            text      = |{ is_node-user_defined_entity_name }| )
          ( item_name = c_relation_col
            class     = cl_item_tree_model=>item_class_text
            text      = get_relation_text( is_node-relation ) )
          ( item_name = c_object_type_col
            class     = cl_item_tree_model=>item_class_text
            text      = get_object_type_text( is_node-type ) )
        )
    ).


    IF is_node-type = cl_ddls_dependency_visitor=>co_node_type-cds_view OR
       is_node-type = cl_ddls_dependency_visitor=>co_node_type-cds_table_function.
      lv_entity_type = zif_dbbr_c_entity_type=>cds_view.
    ELSEIF is_node-type = cl_ddls_dependency_visitor=>co_node_type-table.
      lv_entity_type = zif_dbbr_c_entity_type=>table.
    ELSEIF is_node-type = cl_ddls_dependency_visitor=>co_node_type-view.
      lv_entity_type = zif_dbbr_c_entity_type=>view.
    ENDIF.

    IF lv_entity_type IS NOT INITIAL.
      mt_node_map = VALUE #( BASE mt_node_map
        ( node_key    = lr_node->mv_node_key
          entity_id   = COND #( WHEN is_node-entity_name IS NOT INITIAL THEN is_node-entity_name ELSE is_node-name )
          entity_type = lv_entity_type
          sql_name    = is_node-name )
      ).
    ENDIF.


    IF is_node-children IS BOUND.
      LOOP AT CAST cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes( is_node-children )->* ASSIGNING FIELD-SYMBOL(<ls_child_node>).
        add_node(
            is_node        = <ls_child_node>
            iv_parent_node = lr_node->mv_node_key
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD on_pai.

    CASE ev_function_id.

      WHEN c_fill_text_func.
        fill_entity_texts( ).

      WHEN zif_uitb_template_prog=>c_func_f7.
        show_complexity_metrics( ).

      WHEN zif_uitb_template_prog=>c_func_find.
        DATA(ls_result) = mr_tree->get_search( )->find( ).

      WHEN zif_uitb_template_prog=>c_func_find_more.
        ls_result = mr_tree->get_search( )->find_next( ).

      WHEN zif_uitb_template_prog=>c_func_page_top.
        mr_tree->scroll_to( cl_tree_model=>scroll_home ).

      WHEN zif_uitb_template_prog=>c_func_page_up.
        mr_tree->scroll_to( cl_tree_model=>scroll_up_page ).

      WHEN zif_uitb_template_prog=>c_func_page_down.
        mr_tree->scroll_to( cl_tree_model=>scroll_down_page ).

      WHEN zif_uitb_template_prog=>c_func_page_bottom.
        mr_tree->scroll_to( cl_tree_model=>scroll_end ).

    ENDCASE.

    IF ls_result IS NOT INITIAL.
      mr_tree->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD on_pbo.
    er_callback->deactivate_function( zif_uitb_template_prog=>c_func_save ).

    IF er_callback->is_first_screen_call( ).
      create_tree( ).

      zcl_uitb_screen_util=>set_function_code( c_fill_text_func ).
    ENDIF.
  ENDMETHOD.

  METHOD on_toolbar_button.

    CASE ev_fcode.

      WHEN zif_uitb_c_toolbar_functions=>collapse_all.
        mr_tree->get_nodes( )->collapse_selected_nodes( ).

      WHEN zif_uitb_c_toolbar_functions=>expand_all.
        mr_tree->get_nodes( )->expand_selected_nodes( ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    CHECK fetch_dependencies( ).

    mr_view->show( ).
  ENDMETHOD.

  METHOD get_icon_for_node.
    rv_icon = SWITCH #(
      iv_type
      WHEN cl_ddls_dependency_visitor=>co_node_type-table THEN zif_dbbr_c_icon=>database_table
      WHEN cl_ddls_dependency_visitor=>co_node_type-cds_view THEN zif_dbbr_c_icon=>cds_view
      WHEN cl_ddls_dependency_visitor=>co_node_type-cds_db_view THEN zif_dbbr_c_icon=>database_view
      WHEN cl_ddls_dependency_visitor=>co_node_type-view THEN zif_dbbr_c_icon=>database_view
      WHEN cl_ddls_dependency_visitor=>co_node_type-cds_table_function THEN zif_dbbr_c_icon=>table_settings
      WHEN cl_ddls_dependency_visitor=>co_node_type-external_view THEN zif_dbbr_c_icon=>external_source
      WHEN cl_ddls_dependency_visitor=>co_node_type-select THEN ''
      WHEN cl_ddls_dependency_visitor=>co_node_type-union THEN zif_dbbr_c_icon=>union
      WHEN cl_ddls_dependency_visitor=>co_node_type-union_all THEN zif_dbbr_c_icon=>union
      WHEN cl_ddls_dependency_visitor=>co_node_type-result THEN zif_dbbr_c_icon=>sql_result
    ).
  ENDMETHOD.


  METHOD get_relation_text.
    CASE iv_constant.
      WHEN cl_ddls_dependency_visitor=>co_relation_type-from.
        rv_text = |{ 'From'(014) }|.
      WHEN cl_ddls_dependency_visitor=>co_relation_type-inner_join.
        rv_text = |{ 'Inner Join'(015) }|.
      WHEN cl_ddls_dependency_visitor=>co_relation_type-left_outer_join.
        rv_text = |{ 'Left Outer Join'(016) }|.
      WHEN cl_ddls_dependency_visitor=>co_relation_type-right_outer_join.
        rv_text = |{ 'Right Outer Join'(017) }|.
      WHEN cl_ddls_dependency_visitor=>co_relation_type-select.
        rv_text = |{ 'Select'(011) }|.
      WHEN cl_ddls_dependency_visitor=>co_relation_type-union.
        rv_text = |{ 'Union'(012) }|.
      WHEN cl_ddls_dependency_visitor=>co_relation_type-union_all.
        rv_text = |{ 'Union All'(013) }|.
    ENDCASE.
  ENDMETHOD.

  METHOD get_object_type_text.
    CASE iv_constant.

      WHEN cl_ddls_dependency_visitor=>co_node_type-table.
        rv_text = |{ 'Database Table (TABL)'(005) }|.
      WHEN cl_ddls_dependency_visitor=>co_node_type-cds_view.
        rv_text = |{ 'CDS View (STOB)'(006) }|.
      WHEN cl_ddls_dependency_visitor=>co_node_type-cds_db_view.
        rv_text = |{ 'DDL View (VIEW)'(007) }|.
      WHEN cl_ddls_dependency_visitor=>co_node_type-view.
        rv_text = |{ 'View (VIEW)'(008) }|.
      WHEN cl_ddls_dependency_visitor=>co_node_type-cds_table_function.
        rv_text = |{ 'CDS Table Function'(009) }|.
      WHEN cl_ddls_dependency_visitor=>co_node_type-external_view.
        rv_text = |{ 'External View'(010) }|.

    ENDCASE.
  ENDMETHOD.


  METHOD fill_entity_texts.
    DATA(lv_language) = zcl_dbbr_appl_util=>get_description_language( ).

    zcl_uitb_screen_util=>show_progress( iv_progress = 75 iv_text = |{ 'Loading Descriptions...'(037) }| ).

    SELECT entity, description
      FROM zdbbr_i_databaseentity( p_language = @lv_language )
      FOR ALL ENTRIES IN @mt_node_map
      WHERE entity = @mt_node_map-entity_id
    INTO TABLE @DATA(lt_texts).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_nodes) = mr_tree->get_nodes( ).

    LOOP AT mt_node_map ASSIGNING FIELD-SYMBOL(<ls_node_map>).
      ASSIGN lt_texts[ entity = <ls_node_map>-entity_id ] TO FIELD-SYMBOL(<ls_text>).
      CHECK sy-subrc = 0.

      DATA(lr_node) = lr_nodes->get_node( <ls_node_map>-node_key ).
      lr_node->get_item( c_hierarchy_node2 )->set_text( |{ <ls_text>-description }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD show_complexity_metrics.
    IF ms_metrics_map IS INITIAL.
      zcl_uitb_screen_util=>show_progress( iv_progress = 1 iv_text = |{ 'Calculating Metrics...'(038) }| ).

      DATA(lr_metrics_calculator) = NEW cl_dd_ddl_meta_num_collector(
        descend = abap_true
        bitmask = cl_dd_ddl_meta_num_collector=>c_check_mask-incl_associations
      ).
      lr_metrics_calculator->visitddlsource( iv_dsname = to_upper( mv_view_name ) ).
      ms_metrics_map = lr_metrics_calculator->getnumbermap( ).
    ENDIF.

    ASSIGN ms_metrics_map[ entity = to_upper( mv_view_name ) ]-numbers TO FIELD-SYMBOL(<ls_metrics>).
    CHECK sy-subrc = 0.

    zcl_uitb_popup_help_viewer=>create(
      iv_title        = |{ 'Complexity Metrics for'(018) } { mv_view_name }|
      iv_style        = |td:first-child \{ width: 200px; \} td:nth-child(2) \{ color: Gray; width: 50px; text-align: right; \}|
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
  ENDMETHOD.

  METHOD on_node_context_menu_select.
    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    CHECK sy-subrc = 0.
    CASE ev_fcode.

      WHEN c_functions-open_with_adt.

        TRY.
            zcl_dbbr_adt_util=>jump_adt(
                iv_obj_name     = |{ <ls_node_map>-entity_id }|
                iv_obj_type     = SWITCH #(
                    <ls_node_map>-entity_type
                    WHEN zif_dbbr_c_entity_type=>cds_view THEN 'DDLS'
                    WHEN zif_dbbr_c_entity_type=>table    THEN 'TABD'
                    WHEN zif_dbbr_c_entity_type=>view     THEN 'VIEW'
                )
            ).
          CATCH zcx_dbbr_adt_error INTO DATA(lx_adt_error).
          CATCH zcx_dbbr_data_read_error INTO DATA(lx_data_read_error).
        ENDTRY.

      WHEN c_functions-open_in_new_window.
        CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_OPEN_JUMP'
          EXPORTING
            iv_entity_id       = <ls_node_map>-entity_id
            iv_entity_type     = <ls_node_map>-entity_type
            if_load_parameters = abap_true.

      WHEN c_functions-exec_with_dbbrs_new_window.
        CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_EXEC_JUMP'
          EXPORTING
            iv_entity_id       = <ls_node_map>-entity_id
            iv_entity_type     = <ls_node_map>-entity_type
            if_skip_selscreen  = abap_true
            if_load_parameters = abap_true.

      WHEN c_functions-exec_with_dbbrs.
        DATA(lr_variant_starter) = zcl_dbbr_variant_starter_fac=>create_variant_starter(
            iv_variant_id        = zif_dbbr_global_consts=>c_dummy_variant
            iv_entity_type       = <ls_node_map>-entity_type
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

    mr_view->leave_program( ).

    zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
        iv_entity_id   = lr_s_node_map->entity_id
        iv_entity_type = lr_s_node_map->entity_type
    ).
  ENDMETHOD.

ENDCLASS.
