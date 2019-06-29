CLASS zcl_dbbr_object_history_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_content_searcher .
    INTERFACES zif_uitb_gui_view.
    INTERFACES zif_uitb_gui_control .

    ALIASES focus
      FOR zif_uitb_gui_control~focus .
    ALIASES has_focus
      FOR zif_uitb_gui_control~has_focus .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        io_parent_container TYPE REF TO cl_gui_container
        io_parent_view      TYPE REF TO zif_uitb_gui_composite_view.

  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mo_control
      FOR zif_uitb_gui_control~mr_control .

    TYPES:
      BEGIN OF ty_fav_info,
        type         TYPE zdbbr_favmenu_type,
        favorite     TYPE tabname,
        favorite_raw TYPE zdbbr_entity_id_raw,
        description  TYPE ddtext,
      END OF ty_fav_info .
    TYPES:
      tt_fav_info TYPE STANDARD TABLE OF ty_fav_info WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_node_map,
        node_key    TYPE tm_nodekey,
        entity_id   TYPE zdbbr_entity_id,
        entity_type TYPE zdbbr_entity_type,
        is_variant  TYPE abap_bool,
      END OF ty_node_map .
    TYPES:
      BEGIN OF mty_node_data.
        INCLUDE TYPE treemsnod.
    TYPES: items TYPE treemcitab.
    TYPES: END OF mty_node_data .

    TYPES:
      BEGIN OF ty_s_variant_data,
        variant_id  TYPE zdbbr_variant_id,
        entity_id   TYPE zdbbr_entity_id,
        entity_type TYPE zdbbr_entity_type,
      END OF ty_s_variant_data.

    CONSTANTS:
      BEGIN OF c_context_codes,
        delete_list                TYPE sy-ucomm VALUE 'DELETE_ALL',
        delete_entry               TYPE sy-ucomm VALUE 'DELETE',
        show_in_object_list        TYPE sy-ucomm VALUE 'SHOWINOBJLIST',
        start_with_default_variant TYPE sy-ucomm VALUE 'RUNDEFAULTVAR',
        refresh                    TYPE sy-ucomm VALUE 'REFRESH',
        full_history_flag          TYPE sy-ucomm VALUE 'FULLHISTFLAG',
      END OF c_context_codes .
    CONSTANTS c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT.
    DATA mf_global_fav_mode TYPE boolean .
    DATA mo_favmenu_f TYPE REF TO zcl_dbbr_favmenu_factory .
    DATA mo_parent_container TYPE REF TO cl_gui_container .
    DATA mo_query_f TYPE REF TO zcl_dbbr_query_factory .
    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model .
    DATA mf_full_history TYPE abap_bool.
    DATA mt_node_map TYPE SORTED TABLE OF ty_node_map WITH UNIQUE KEY node_key .
    DATA mo_parent_view TYPE REF TO zif_uitb_gui_composite_view.

    "! <p class="shorttext synchronized" lang="en">Builds history tree</p>
    "!
    METHODS build_tree .
    "! <p class="shorttext synchronized" lang="en">Clears all nodes from the tree</p>
    "!
    METHODS clear_nodes
      IMPORTING
        !if_delete_from_db TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Creates the tree model and control</p>
    "!
    METHODS create_tree .
    "! <p class="shorttext synchronized" lang="en">Delete selected nodes</p>
    "!
    METHODS delete_selected_nodes .
    "! <p class="shorttext synchronized" lang="en">Fill toolbar with buttons</p>
    "!
    METHODS fill_toolbar .
    "! <p class="shorttext synchronized" lang="en">Handle function</p>
    "!
    METHODS handle_function
      IMPORTING
        !iv_function TYPE ui_func
        iv_node_key  TYPE tm_nodekey OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Loads history nodes</p>
    "!
    METHODS load_nodes .
    "! <p class="shorttext synchronized" lang="en">Handler for when children are to be loaded lazily</p>
    "!
    METHODS on_expand_no_children
          FOR EVENT expand_no_children OF zif_uitb_tree_model_events
      IMPORTING
          !ev_node_key .
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
    "! <p class="shorttext synchronized" lang="en">Handler for pressed toolbar button</p>
    "!
    METHODS on_toolbar_button
          FOR EVENT function_selected OF zif_uitb_toolbar_events
      IMPORTING
          !ev_fcode .
    "! <p class="shorttext synchronized" lang="en">Start node with default variant</p>
    "!
    METHODS start_with_default_variant
      IMPORTING
        iv_node_key TYPE tm_nodekey.
    "! <p class="shorttext synchronized" lang="en">Shows history entry in object browser</p>
    "!
    METHODS show_in_object_browser
      IMPORTING
        iv_node_key TYPE tm_nodekey.
ENDCLASS.



CLASS zcl_dbbr_object_history_tree IMPLEMENTATION.


  METHOD build_tree.

    clear_nodes( if_delete_from_db = abap_false ).

*.. fill tree with saved nodes
    load_nodes( ).

    focus( ).
  ENDMETHOD.


  METHOD clear_nodes.

    IF if_delete_from_db = abap_true AND
       zcl_dbbr_appl_util=>popup_to_confirm(
         iv_title                 = |{ 'Delete all?'(008) }|
         iv_query                 = |{ 'Are you sure you want to delete your full history?'(009) }|
         iv_icon_type             = 'ICON_QUESTION'
       ) <> '1'.
      RETURN.
    ENDIF.

    CLEAR mt_node_map.
    mo_tree->get_nodes( )->delete_all_nodes( ).

    IF if_delete_from_db = abap_true.
      mo_favmenu_f->clear_most_used_list( ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mo_parent_container = io_parent_container.
    mo_parent_view = io_parent_view.
    mo_favmenu_f = NEW #( ).

    create_tree( ).
    build_tree( ).
  ENDMETHOD.


  METHOD create_tree.
    DATA: lt_events TYPE cntl_simple_events.

    CHECK mo_tree IS INITIAL.

    mo_tree = NEW zcl_uitb_column_tree_model(
        ir_parent           = mo_parent_container
        if_auto_node_key    = abap_true
        is_hierarchy_header = VALUE #(
          heading = |{ 'Object Name'(002) }|
        )
        if_with_toolbar     = abap_true
        iv_selection_mode   = cl_tree_model=>node_sel_mode_multiple
    ).

    mo_tree->get_columns( )->add_hierarchy_column( c_hierarchy_node2 ).

    fill_toolbar( ).

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


  METHOD delete_selected_nodes.
    DATA: lt_mostused_to_delete TYPE zif_dbbr_global_types=>tt_mostused_k,
          lt_nodes_to_delete    TYPE treemnotab.

    DATA(lt_selected_nodes) = mo_tree->get_selections( )->get_selected_nodes( ).

    LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lo_node>).
      ASSIGN mt_node_map[ node_key = <lo_node>->mv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
      CHECK: sy-subrc = 0,
             <ls_node_map>-is_variant = abap_false.

      data(lr_user_data) = <lo_node>->get_user_data( ).

      TRY.
          DATA(lr_most_used) = CAST zdbbr_mostused( lr_user_data ).
          lt_mostused_to_delete = VALUE #(
            BASE lt_mostused_to_delete
            ( username        = sy-uname
              type            = lr_Most_used->type
              most_used_entry = lr_most_used->most_used_entry )
          ).
          lt_nodes_to_delete = VALUE #( BASE lt_nodes_to_delete ( <lo_node>->mv_node_key ) ).
          DELETE mt_node_map WHERE node_key = <lo_node>->mv_node_key.
        CATCH cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    mo_tree->get_nodes( )->delete_nodes( lt_nodes_to_delete ).
    mo_favmenu_f->delete_most_used_multiple( lt_mostused_to_delete ).
  ENDMETHOD.


  METHOD fill_toolbar.
    DATA(lo_toolbar) = mo_tree->get_toolbar( ).

    lo_toolbar->add_search_buttons( ).
    lo_toolbar->add_separator( ).
    lo_toolbar->add_button(
        iv_fcode     = c_context_codes-delete_list
        iv_icon      = icon_delete
        iv_text      = |{ 'Delete all'(001) }|
    ).
    lo_toolbar->add_button(
        iv_fcode     = c_context_codes-refresh
        iv_icon      = icon_refresh
        iv_text      = |{ 'Refresh'(003) }|
    ).
    lo_toolbar->add_button(
        iv_fcode     = c_context_codes-full_history_flag
        iv_icon      = icon_wd_radio_button_empty
        iv_butn_type = cntb_btype_button
        iv_text      = |{ 'Full History'(006) }|
        iv_quickinfo = |{ 'Toggle full history'(007) }|
    ).

*.. register event handler
    SET HANDLER on_toolbar_button FOR lo_toolbar.
  ENDMETHOD.


  METHOD handle_function.
    CASE iv_function.

      WHEN c_context_codes-delete_entry.
        delete_selected_nodes( ).

      WHEN c_context_codes-delete_list.
        clear_nodes( if_delete_from_db = abap_true ).

      WHEN c_context_codes-full_history_flag.
        mf_full_history = xsdbool( mf_full_history = abap_false ).
        mo_tree->get_toolbar( )->set_button_icon(
            iv_function = c_context_codes-full_history_flag
            iv_icon = COND #( WHEN mf_full_history = abap_true THEN icon_wd_radio_button ELSE icon_wd_radio_button_empty )
        ).
        mo_tree->get_toolbar( )->refresh_ui( ).
        load_nodes( ).

      WHEN c_context_codes-refresh.
        load_nodes( ).

      WHEN c_context_codes-show_in_object_list.
        show_in_object_browser( iv_node_key ).

      WHEN c_context_codes-start_with_default_variant.
        start_with_default_variant( iv_node_key ).

      WHEN zif_uitb_c_toolbar_functions=>find.
        zif_uitb_content_searcher~search( ).

      WHEN zif_uitb_c_toolbar_functions=>find_more.
        zif_uitb_content_searcher~search_next( ).

    ENDCASE.
  ENDMETHOD.


  METHOD load_nodes.
    DATA: lv_icon  TYPE tv_image,
          lt_items TYPE treemcitab.

    clear_nodes( if_delete_from_db = abap_false ).

    LOOP AT mo_favmenu_f->get_most_used_favorites( if_all = mf_full_history ) ASSIGNING FIELD-SYMBOL(<ls_most_used>).

      lv_icon = zcl_dbbr_tree_helper=>get_tree_node_icon( <ls_most_used>-type ).

      lt_items = VALUE #(
        ( item_name  = zcl_uitb_column_tree_model=>c_hierarchy_column
          class      = cl_item_tree_model=>item_class_text
          font       = cl_item_tree_model=>item_font_prop
          text       = <ls_most_used>-most_used_entry_raw )
      ).

      IF <ls_most_used>-text IS NOT INITIAL.
        lt_items = VALUE #( BASE lt_items
          ( item_name = c_hierarchy_node2
            style     = zif_uitb_c_ctm_style=>inverted_gray
            font      = cl_item_tree_model=>item_font_prop
            class     = cl_item_tree_model=>item_class_text
            text      = <ls_most_used>-text )
        ).
      ENDIF.


      DATA(lo_history_node) = mo_tree->get_nodes( )->add_node(
          if_folder            = <ls_most_used>-has_variants
          if_expander          = <ls_most_used>-has_variants
          iv_image             = lv_icon
          iv_expanded_image    = lv_icon
          ir_user_data         = NEW zdbbr_mostused( <ls_most_used> )
          it_item_table        = lt_items
      ).

*.... Fill node map for history entry
      mt_node_map = VALUE #( BASE mt_node_map
       ( node_key     = lo_history_node->mv_node_key
         entity_id    = <ls_most_used>-most_used_entry
         entity_type  = <ls_most_used>-type
       )
      ).
    ENDLOOP.

  ENDMETHOD.


  METHOD on_expand_no_children.
    DATA(lo_node) = mo_tree->get_nodes( )->get_node( ev_node_key ).
    data(lr_most_used) = cast zdbbr_mostused( lo_node->get_user_data( ) ).

    DATA(lt_variants) = zcl_dbbr_variant_factory=>find_variant_infos_for_type(
        iv_entity_id   = lr_most_used->most_used_entry
        iv_entity_type = lr_most_used->type
    ).

    LOOP AT lt_variants ASSIGNING FIELD-SYMBOL(<ls_variant>).
      <ls_variant>-entity_type = lr_most_used->type.

      IF <ls_variant>-description IS NOT INITIAL.
        DATA(lt_items_for_description) = VALUE treemcitab(
           ( item_name = c_hierarchy_node2
             style     = zif_uitb_c_ctm_style=>inverted_gray
             font      = cl_item_tree_model=>item_font_prop
             class     = cl_item_tree_model=>item_class_text
             text      = <ls_variant>-description )
         ).
      ENDIF.

      DATA(lo_variant_node) = mo_tree->get_nodes( )->add_node(
          iv_relative_node_key       = ev_node_key
          iv_relationship            = cl_list_tree_model=>relat_last_child
          iv_image                   = CONV #( icon_alv_variants )
          ir_user_data               = new ty_s_variant_data(
              variant_id  = <ls_variant>-variant_id
              entity_id   = lr_most_used->most_used_entry
              entity_type = <ls_variant>-entity_type
          )
          it_item_table              = VALUE #(
            ( item_name  = mo_tree->c_hierarchy_column
              class      = cl_item_tree_model=>item_class_text
              font       = cl_item_tree_model=>item_font_prop
              text       = <ls_variant>-variant_name )
            ( LINES OF lt_items_for_description )

          )
      ).

      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key    = lo_variant_node->mv_node_key
          entity_id   = lr_most_used->most_used_entry
          entity_type = <ls_variant>-entity_type
          is_variant  = abap_true )
      ).

      CLEAR lt_items_for_description.

    ENDLOOP.

    mo_tree->get_nodes( )->expand_node( ev_node_key ).
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    DATA(lt_selected_nodes) = mo_tree->get_selections( )->get_selected_nodes( ).

    er_menu->add_function(
        fcode = c_context_codes-delete_entry
        text  = |{ 'Delete Entry'(004) }|
    ).
    IF lines( lt_selected_nodes ) = 1.
      er_menu->add_separator( ).
*.... create new function for running the selected table/script and build_tree the results
      er_menu->add_function(
        fcode = c_context_codes-start_with_default_variant
        text  = |{ 'Execute with Default Settings'(005) }|
      ).
*.... create function for opening favorite in object list
      er_menu->add_function(
        fcode = c_context_codes-show_in_object_list
        text  = |{ 'Show in Object Browser'(010) }|
      ).

    ENDIF.
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    handle_function( iv_function = ev_fcode iv_node_key = ev_node_key ).
  ENDMETHOD.


  METHOD on_node_double_click.
    DATA(lo_node) = mo_tree->get_nodes( )->get_node( ev_node_key ).
    DATA(lr_user_data) = lo_node->get_user_data( ).
    IF lr_user_data IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF <ls_node_map>-is_variant = abap_true.
      DATA(lr_variant_info) = cast ty_s_variant_data( lr_user_data ).
      zcl_dbbr_selscr_nav_events=>raise_variant_entry_chosen(
          iv_entity_id    = lr_variant_info->entity_id
          iv_entity_type  = lr_variant_info->entity_type
          iv_variant_id   = lr_variant_info->variant_id
      ).
    ELSE.
      data(lr_most_used_entry) = cast zdbbr_Mostused( lr_user_data ).
      zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
          iv_entity_id   = lr_most_used_entry->most_used_entry
          iv_entity_type = lr_most_used_entry->type
      ).
    ENDIF.
  ENDMETHOD.


  METHOD on_node_enter_key.
    IF ev_key = cl_list_tree_model=>key_enter.
      on_node_double_click( ev_node_key = ev_node_key ).
    ELSEIF ev_key = cl_list_tree_model=>key_delete.
      on_node_context_menu_select(
          ev_fcode    = c_context_codes-delete_entry
          ev_node_key = ev_node_key
      ).
    ENDIF.
  ENDMETHOD.


  METHOD on_toolbar_button.
    handle_function( ev_fcode ).
  ENDMETHOD.

  METHOD show_in_object_browser.
    ASSIGN mt_node_map[ node_key = iv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    CHECK sy-subrc = 0.

    IF mo_parent_view IS BOUND.
      mo_parent_view->execute_command( NEW zcl_uitb_gui_simple_command(
        iv_function = zcl_dbbr_object_navigator=>c_command_id-show_object_list
        ir_params   = NEW zdbbr_entity( entity_id     = <ls_node_map>-entity_id
                                        entity_type   = <ls_node_map>-entity_type   ) )
      ).
    ENDIF.

  ENDMETHOD.


  METHOD start_with_default_variant.
    DATA: lv_entity_id   TYPE zdbbr_entity_id,
          lv_variant     TYPE zdbbr_variant_id VALUE zif_dbbr_global_consts=>c_dummy_variant,
          lv_entity_type TYPE zdbbr_entity_type.

    DATA(lo_node) = mo_tree->get_nodes( )->get_node( iv_node_key ).
    IF lo_node IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mt_node_map[ node_key = lo_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF <ls_node_map>-is_variant = abap_true.
      DATA(lr_variant_info) = cast ty_s_variant_data( lo_node->get_user_data( ) ).
      lv_entity_id = lr_variant_info->entity_id.
      lv_entity_type = lr_variant_info->entity_type.
      lv_variant = lr_variant_info->variant_id.
    ELSE.
      data(lr_most_used_entry) = cast zdbbr_Mostused( lo_node->get_user_data( ) ).
      lv_entity_id = lr_most_used_entry->most_used_entry.
      lv_entity_type = lr_most_used_entry->type.
    ENDIF.

    zcl_dbbr_selscr_nav_events=>raise_variant_entry_chosen(
        iv_entity_id    = lv_entity_id
        iv_entity_type  = lv_entity_type
        iv_variant_id   = lv_variant
        if_go_to_result = abap_true
    ).
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.
    DATA(ls_find_result) = mo_tree->get_search( )->find( ).

    IF ls_find_result IS NOT INITIAL.
      mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    DATA(ls_find_result) = mo_tree->get_search( )->find_next( ).

    IF ls_find_result IS NOT INITIAL.
      mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    mo_tree->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    rf_has_focus = mo_tree->zif_uitb_gui_control~has_focus( ).
  ENDMETHOD.
ENDCLASS.
