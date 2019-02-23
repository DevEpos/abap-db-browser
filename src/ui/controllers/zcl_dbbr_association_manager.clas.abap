CLASS zcl_dbbr_association_manager DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_query_dnd_flavor TYPE cndd_flavor VALUE 'query'.
    CONSTANTS c_table_dnd_flavor TYPE cndd_flavor VALUE 'TABLE'.
    CONSTANTS c_queries_root TYPE string VALUE 'queryS' ##NO_TEXT.
    CONSTANTS c_tables_root TYPE string VALUE 'TABLES' ##NO_TEXT.

    DATA mr_template_prog TYPE REF TO zif_uitb_template_prog .
    DATA mt_query_info TYPE zdbbr_query_info_itab.
    data mv_counter type i.
    DATA: mr_main_split              TYPE REF TO cl_gui_splitter_container,
          mr_association_tree_model  TYPE REF TO zcl_uitb_column_tree_model,
          mr_db_table_tree           TYPE REF TO zcl_uitb_column_tree_model,
          mr_query_alv              TYPE REF TO zcl_uitb_alv.

    METHODS on_before_output
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          er_callback.
    METHODS do_before_first_screen_call .
    METHODS create_db_table_search
      IMPORTING
        ir_container TYPE REF TO cl_gui_container.
    METHODS create_association_tree
      IMPORTING
        ir_container TYPE REF TO cl_gui_container.
    METHODS create_query_alv
      IMPORTING
        ir_container TYPE REF TO cl_gui_container.
    METHODS fill_association_tree.


    METHODS handle_packages
      IMPORTING
        iv_parent_key   TYPE tm_nodekey
        ir_package_list TYPE REF TO zif_uitb_list
        ir_nodes        TYPE REF TO zcl_uitb_ctm_nodes.
    METHODS add_new_table.
    METHODS on_exit
          FOR EVENT exit OF zif_uitb_view_callback
      IMPORTING
          er_callback.
    METHODS on_user_command
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          er_callback
          ev_function_id.

    METHODS on_toolbar_function
          FOR EVENT function_selected OF zif_uitb_toolbar_events
      IMPORTING
          ev_fcode.
    METHODS on_context_menu_request
          FOR EVENT node_context_menu_request OF zcl_uitb_ctm_events
      IMPORTING
          er_menu
          ev_node_key.
    METHODS on_context_menu_select
          FOR EVENT node_context_menu_select OF zcl_uitb_ctm_events
      IMPORTING
          ev_fcode
          ev_node_key.
    METHODS on_db_tree_nd_dbl_click
          FOR EVENT node_double_click OF zcl_uitb_ctm_events
      IMPORTING
          ev_node_key
          sender.
ENDCLASS.



CLASS ZCL_DBBR_ASSOCIATION_MANAGER IMPLEMENTATION.


  METHOD add_new_table.
    DATA(lv_table) = CONV tabname( zcl_dbbr_appl_util=>popup_get_value(
      is_field = VALUE #( tabname = 'ZDBBR_BROWSER_MODE_DATA' fieldname = 'TAB_NAME'  field_obl = abap_true fieldtext = 'Table' )
      iv_title  = 'Enter name of Table' )
    ).
    IF lv_table IS NOT INITIAL.
      TRY.
          zcl_dbbr_dictionary_helper=>validate_table_name( iv_table_name = lv_table ).
        CATCH zcx_dbbr_validation_exception INTO DATA(lr_valid_exc).
          lr_valid_exc->zif_dbbr_exception_message~print( ).
          return.
      ENDTRY.
    ENDIF.

    add 1 to mv_counter.
    data(lv_new_node) = |ENTRY_{ mv_counter }|.
    " add the table as new node to the association tree
    data(lr_nodes) = mr_association_tree_model->get_nodes( ).
    lr_nodes->add_node(
        iv_node_key          = lv_new_node
        iv_relative_node_key = c_tables_root
        if_folder            = abap_true
        iv_image             = |{ icon_database_table }|
*        iv_expanded_image    =
*        ir_user_object       =
*        if_items_incomplete  =
        it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                text      = lv_table )
              ( item_name = 'DESC'
                class     = cl_column_tree_model=>item_class_text
                text      = space )
            )
    ).
*      CATCH zcx_uitb_tree_error.    "


  ENDMETHOD.


  METHOD constructor.
    mr_template_prog = zcl_uitb_templt_prog_callback=>create_template_program( iv_title = text-tit ).

    SET HANDLER:
      on_exit FOR mr_template_prog,
      on_user_command FOR mr_template_prog,
      on_before_output FOR mr_template_prog.
  ENDMETHOD.


  METHOD create_association_tree.

    " create the tree model for the association tree
    mr_association_tree_model = NEW zcl_uitb_column_tree_model(
        ir_parent           = ir_container
        is_hierarchy_header = VALUE treemhhdr(
            heading = 'Table / query'(007)
            width   = 70
        )
        if_with_toolbar     = abap_true
    ).

    SET HANDLER:
       on_context_menu_request FOR mr_association_tree_model->get_events( ),
       on_context_menu_select FOR mr_association_tree_model->get_events( ).

    DATA(lr_toolbar) = mr_association_tree_model->get_toolbar( ).
    SET HANDLER: on_toolbar_function FOR lr_toolbar.

    lr_toolbar->add_button(
        iv_fcode     = 'AT_SEARCH'
        iv_icon      = icon_search
        iv_quickinfo = 'Find'(006)
    ).
    lr_toolbar->add_button(
        iv_fcode     = 'AT_SEARCHF'
        iv_icon      = icon_search_next
        iv_quickinfo = 'Find more'(005)
    ).


    TRY.
        mr_association_tree_model->get_columns( )->add_column(
            iv_colname           = 'DESC'
            iv_width             = 60
            iv_header_text       = 'Description'(004)
        ).

        mr_association_tree_model->get_nodes( )->add_node(
            iv_node_key          = c_tables_root
            if_folder            = abap_true
            iv_image             = |{ icon_table_settings }|
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                text      = 'Tables'(003) )
              ( item_name = 'DESC'
                class     = cl_column_tree_model=>item_class_text
                text      = 'Tables with associations'(002) )
            )
        ).
        mr_association_tree_model->get_nodes( )->add_node(
            iv_node_key          = c_queries_root
            if_folder            = abap_true
            iv_image             = 'ICON_PROTOCOL'
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                text      = 'querys' )
              ( item_name = 'DESC'
                class     = cl_column_tree_model=>item_class_text
                text      = 'querys with associations'(001) )
            )
        ).

*        mr_association_tree_model->add_dnd_behavior(
*            iv_flavor            = c_query_dnd_flavor
*            if_droptarget        = abap_true
*        ).

*        mr_association_tree_model->add_dnd_behavior(
*            iv_flavor            = c_table_dnd_flavor
*            if_droptarget        = abap_true
*        ).
      CATCH zcx_uitb_tree_error.    "
    ENDTRY.

    mr_association_tree_model->create_tree_control( ).

  ENDMETHOD.


  METHOD create_db_table_search.
  ENDMETHOD.


  METHOD create_query_alv.
    mr_query_alv = zcl_uitb_alv=>create_alv(
        ir_container = ir_container
        ir_data      = REF #( mt_query_info )
        if_editable  = abap_false
    ).

    TRY.
        DATA(lr_functions) = mr_query_alv->get_functions( ).

        lr_functions->set_all( abap_false ).
        lr_functions->set_function( zif_uitb_c_alv_functions=>sort_asc ).
        lr_functions->set_function( zif_uitb_c_alv_functions=>sort_desc ).
        lr_functions->set_function( zif_uitb_c_alv_functions=>filter ).
        lr_functions->set_function( zif_uitb_c_alv_functions=>filter_delete ).


        DATA(lr_columns) = mr_query_alv->get_columns( ).

        lr_columns->get_column( 'query_ID' )->set_visible( abap_false ).
        lr_columns->get_column( 'HAS_SORT_FIELDS' )->set_visible( abap_false ).
        lr_columns->get_column( 'HAS_OUTPUT_FIELDS' )->set_visible( abap_false ).
        lr_columns->get_column( 'HAS_JUMP_FIELDS' )->set_visible( abap_false ).
        lr_columns->get_column( 'IS_GLOBAL' )->set_visible( abap_false ).
        lr_columns->get_column( 'REF_JOIN_ID' )->set_visible( abap_false ).
        lr_columns->get_column( 'FORMULA' )->set_visible( abap_false ).

        mr_query_alv->display( ).
      CATCH zcx_uitb_alv_error.    "
    ENDTRY.
  ENDMETHOD.


  METHOD do_before_first_screen_call.
    " create screen layout
    mr_main_split = NEW cl_gui_splitter_container(
        parent  = mr_template_prog->get_container( )
        columns = 2
        rows    = 1
    ).
    mr_main_split->set_column_width( id = 1 width = 35 ).

    DATA(lr_left_col) = mr_main_split->get_container(
        row    = 1
        column = 1
    ).

    DATA(lr_right_col) = mr_main_split->get_container(
        row    = 1
        column = 2
    ).

    create_association_tree( lr_left_col ).
    create_db_table_search( lr_right_col ).

    fill_association_tree( ).

  ENDMETHOD.


  METHOD fill_association_tree.

  ENDMETHOD.


  METHOD handle_packages.
    CHECK ir_package_list->size( ) > 0.

    DATA(lr_package_iterator) = ir_package_list->get_iterator( ).

    WHILE lr_package_iterator->has_next( ).
      DATA(lr_package) = CAST zcl_dbbr_package( lr_package_iterator->get_next( ) ).

      ir_nodes->add_node(
          iv_node_key          = CONV #( |{ lr_package->mv_package }_P| )
          iv_relative_node_key = iv_parent_key
          if_folder            = abap_true
          iv_image             = |{ icon_package_standard }|
          it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                text      = lr_package->mv_package )
              ( item_name = 'DESC'
                class     = cl_column_tree_model=>item_class_text
                text      = lr_package->mv_package_description )
            )
      ).
    ENDWHILE.
  ENDMETHOD.


  METHOD on_before_output.
  ENDMETHOD.


  METHOD on_context_menu_request.
    er_menu->add_function(
        fcode = 'ADD_NEW'
        text  = 'New Entry'(008)
    ).
  ENDMETHOD.


  METHOD on_context_menu_select.
    CASE ev_fcode.

      WHEN 'ADD_NEW'.
        add_new_table( ).
    ENDCASE.
  ENDMETHOD.


  METHOD on_db_tree_nd_dbl_click.
    DATA(lr_nodes) = mr_db_table_tree->get_nodes( ).
    lr_nodes->toggle_node( ev_node_key ).
  ENDMETHOD.


  METHOD on_exit.
    mr_main_split->free( ).

    CLEAR mr_main_split.
  ENDMETHOD.


  METHOD on_toolbar_function.
    CASE ev_fcode.

      WHEN 'AT_SEARCH'.
        mr_association_tree_model->zif_uitb_content_searcher~search( ).

      WHEN 'AT_SEARCHF'.
        mr_association_tree_model->zif_uitb_content_searcher~search_next( ).

    ENDCASE.
  ENDMETHOD.


  METHOD on_user_command.
    CASE ev_function_id.

      WHEN zif_uitb_template_prog=>c_search.
      WHEN zif_uitb_template_prog=>c_search_more.

    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    do_before_first_screen_call( ).

    mr_template_prog->zif_uitb_view~show( ).
  ENDMETHOD.
ENDCLASS.
