CLASS zcl_dbbr_object_history_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_content_searcher .
    INTERFACES zif_uitb_gui_control .

    ALIASES focus
      FOR zif_uitb_gui_control~focus .
    ALIASES has_focus
      FOR zif_uitb_gui_control~has_focus .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    "! @parameter ir_parent_container | <p class="shorttext synchronized" lang="en"></p>
    METHODS constructor
      IMPORTING
        ir_parent_container TYPE REF TO cl_gui_container.

  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mr_control
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

    CONSTANTS:
      BEGIN OF c_context_codes,
        delete_list                TYPE sy-ucomm VALUE 'DELETE_ALL',
        delete_entry               TYPE sy-ucomm VALUE 'DELETE',
        start_with_default_variant TYPE sy-ucomm VALUE 'RUNDEFAULTVAR',
        refresh                    TYPE sy-ucomm VALUE 'REFRESH',
        full_history_flag          TYPE sy-ucomm VALUE 'FULLHISTFLAG',
      END OF c_context_codes .
    CONSTANTS c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT.
    DATA mf_global_fav_mode TYPE boolean .
    DATA mr_favmenu_f TYPE REF TO zcl_dbbr_favmenu_factory .
    DATA mr_parent_container TYPE REF TO cl_gui_container .
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory .
    DATA mr_tree_model TYPE REF TO zcl_uitb_column_tree_model .
    DATA mr_variant_f TYPE REF TO zcl_dbbr_variant_factory .
    DATA mf_full_history TYPE abap_bool.
    DATA mt_node_map TYPE SORTED TABLE OF ty_node_map WITH UNIQUE KEY node_key .

    "! <p class="shorttext synchronized" lang="en">Builds history tree</p>
    "!
    METHODS build_tree .
    "! <p class="shorttext synchronized" lang="en">Clears all nodes from the tree</p>
    "!
    "! @parameter if_delete_from_db | <p class="shorttext synchronized" lang="en"></p>
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
    "! @parameter iv_function | <p class="shorttext synchronized" lang="en"></p>
    METHODS handle_function
      IMPORTING
        !iv_function TYPE ui_func .
    "! <p class="shorttext synchronized" lang="en">Loads history nodes</p>
    "!
    METHODS load_nodes .
    "! <p class="shorttext synchronized" lang="en">Handler for when children are to be loaded lazily</p>
    "! @parameter ev_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_expand_no_children
          FOR EVENT expand_no_children OF zif_uitb_tree_model_events
      IMPORTING
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for requesting a context menu for a node</p>
    "! @parameter er_menu | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ev_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_node_context_menu_request
          FOR EVENT node_context_menu_request OF zif_uitb_tree_model_events
      IMPORTING
          !er_menu
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for when the context menu entry was chosen</p>
    "! @parameter ev_fcode | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ev_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_node_context_menu_select
          FOR EVENT node_context_menu_select OF zif_uitb_tree_model_events
      IMPORTING
          !ev_fcode
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for double click on node</p>
    "! @parameter ev_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_node_double_click
          FOR EVENT node_double_click OF zif_uitb_tree_model_events
      IMPORTING
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for ENTER key press on node</p>
    "! @parameter ev_key | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ev_node_key | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_node_enter_key
          FOR EVENT node_keypress OF zif_uitb_tree_model_events
      IMPORTING
          !ev_key
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for pressed toolbar button</p>
    "! @parameter ev_fcode | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_toolbar_button
          FOR EVENT function_selected OF zif_uitb_toolbar_events
      IMPORTING
          !ev_fcode .
    "! <p class="shorttext synchronized" lang="en">Start node with default variant</p>
    "!
    METHODS start_with_default_variant .
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
    mr_tree_model->get_nodes( )->delete_all_nodes( ).

    IF if_delete_from_db = abap_true.
      mr_favmenu_f->clear_most_used_list( ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mr_parent_container = ir_parent_container.
    mr_favmenu_f = NEW #( ).
    mr_variant_f = NEW #( ).

    create_tree( ).
    build_tree( ).
  ENDMETHOD.


  METHOD create_tree.
    DATA: lt_events TYPE cntl_simple_events.

    CHECK mr_tree_model IS INITIAL.

    mr_tree_model = NEW zcl_uitb_column_tree_model(
        ir_parent           = mr_parent_container
        if_auto_node_key    = abap_true
        is_hierarchy_header = VALUE #(
          heading = |{ 'Object Name'(002) }|
        )
        if_with_toolbar     = abap_true
        iv_selection_mode   = cl_tree_model=>node_sel_mode_multiple
    ).

    mr_tree_model->get_columns( )->add_hierarchy_column( c_hierarchy_node2 ).

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


  METHOD delete_selected_nodes.
    DATA: lt_mostused_to_delete TYPE zif_dbbr_global_types=>tt_mostused_k,
          lt_nodes_to_delete    TYPE treemnotab.

    DATA(lt_selected_nodes) = mr_tree_model->get_selections( )->get_selected_nodes( ).

    LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lr_node>).
      ASSIGN mt_node_map[ node_key = <lr_node>->mv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
      CHECK: sy-subrc = 0,
             <ls_node_map>-is_variant = abap_false.

      DATA(lr_user_object) = <lr_node>->get_user_object( ).

      TRY.
          DATA(ls_mostused) = CAST zcl_dbbr_favmenu_entry( lr_user_object )->get_favmenu_data( ).
          lt_mostused_to_delete = VALUE #(
            BASE lt_mostused_to_delete
            ( username        = sy-uname
              type            = ls_mostused-favtype
              most_used_entry = ls_mostused-fav_entry )
          ).
          lt_nodes_to_delete = VALUE #( BASE lt_nodes_to_delete ( <lr_node>->mv_node_key ) ).
          DELETE mt_node_map WHERE node_key = <lr_node>->mv_node_key.
        CATCH cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    mr_tree_model->get_nodes( )->delete_nodes( lt_nodes_to_delete ).
    mr_favmenu_f->delete_most_used_multiple( lt_mostused_to_delete ).
  ENDMETHOD.


  METHOD fill_toolbar.
    DATA(lr_toolbar) = mr_tree_model->get_toolbar( ).

    lr_toolbar->add_search_buttons( ).
    lr_toolbar->add_separator( ).
    lr_toolbar->add_button(
        iv_fcode     = c_context_codes-delete_list
        iv_icon      = icon_delete
        iv_text      = |{ 'Delete all'(001) }|
    ).
    lr_toolbar->add_button(
        iv_fcode     = c_context_codes-refresh
        iv_icon      = icon_refresh
        iv_text      = |{ 'Refresh'(003) }|
    ).
    lr_toolbar->add_button(
        iv_fcode     = c_context_codes-full_history_flag
        iv_icon      = icon_wd_radio_button_empty
        iv_butn_type = cntb_btype_button
        iv_text      = |{ 'Full History'(006) }|
        iv_quickinfo = |{ 'Toggle full history'(007) }|
    ).

*.. register event handler
    SET HANDLER on_toolbar_button FOR lr_toolbar.
  ENDMETHOD.


  METHOD handle_function.
    CASE iv_function.

      WHEN c_context_codes-delete_entry.
        delete_selected_nodes( ).

      WHEN c_context_codes-delete_list.
        clear_nodes( if_delete_from_db = abap_true ).

      WHEN c_context_codes-full_history_flag.
        mf_full_history = xsdbool( mf_full_history = abap_false ).
        mr_tree_model->get_toolbar( )->set_button_icon(
            iv_function = c_context_codes-full_history_flag
            iv_icon = COND #( WHEN mf_full_history = abap_true THEN icon_wd_radio_button ELSE icon_wd_radio_button_empty )
        ).
        mr_tree_model->get_toolbar( )->refresh_ui( ).
        load_nodes( ).

      WHEN c_context_codes-refresh.
        load_nodes( ).

      WHEN c_context_codes-start_with_default_variant.
        start_with_default_variant( ).

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

    LOOP AT mr_favmenu_f->get_most_used_favorites( if_all = mf_full_history ) ASSIGNING FIELD-SYMBOL(<ls_most_used>).

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
            class      = cl_item_tree_model=>item_class_text
            text       = <ls_most_used>-text )
        ).
      ENDIF.


      DATA(lr_history_node) = mr_tree_model->get_nodes( )->add_node(
          if_folder            = <ls_most_used>-has_variants
          if_expander          = <ls_most_used>-has_variants
          iv_image             = lv_icon
          iv_expanded_image    = lv_icon
          " add reference of favorite to node
          ir_user_object       = NEW zcl_dbbr_favmenu_entry(
              VALUE #( uname         = <ls_most_used>-username
                       favtype       = <ls_most_used>-type
                       fav_entry     = <ls_most_used>-most_used_entry
                       fav_entry_raw = <ls_most_used>-most_used_entry_raw )
          )
          it_item_table        = lt_items
      ).

*.... Fill node map for history entry
      mt_node_map = VALUE #( BASE mt_node_map
       ( node_key     = lr_history_node->mv_node_key
         entity_id    = <ls_most_used>-most_used_entry
         entity_type  = <ls_most_used>-type
       )
      ).
    ENDLOOP.

  ENDMETHOD.


  METHOD on_expand_no_children.
    DATA(lr_node) = mr_tree_model->get_nodes( )->get_node( ev_node_key ).
    DATA(lr_user_object) = lr_node->get_user_object( ).
    DATA(lr_favmenu_object) = CAST zcl_dbbr_favmenu_entry( lr_user_object ).

    DATA(ls_favmenu_data) = lr_favmenu_object->get_favmenu_data( ).

    DATA(lt_variants) = mr_variant_f->find_variant_infos_for_type(
        iv_entity_id   = ls_favmenu_data-fav_entry
        iv_entity_type = ls_favmenu_data-favtype
    ).

    LOOP AT lt_variants ASSIGNING FIELD-SYMBOL(<ls_variant>).
      <ls_variant>-entity_type = ls_favmenu_data-favtype.

      IF <ls_variant>-description IS NOT INITIAL.
        DATA(lt_items_for_description) = VALUE treemcitab(
           ( item_name = c_hierarchy_node2
             style     = zif_uitb_c_ctm_style=>inverted_gray
             class      = cl_item_tree_model=>item_class_text
             text       = <ls_variant>-description )
         ).
      ENDIF.

      DATA(lr_variant_node) = mr_tree_model->get_nodes( )->add_node(
          iv_relative_node_key       = ev_node_key
          iv_relationship            = cl_list_tree_model=>relat_last_child
          iv_image                   = CONV #( icon_alv_variants )
          ir_user_object             = NEW zcl_dbbr_variant( <ls_variant> )
          it_item_table              = VALUE #(
            ( item_name  = mr_tree_model->c_hierarchy_column
              class      = cl_item_tree_model=>item_class_text
              font       = cl_item_tree_model=>item_font_prop
              text       = <ls_variant>-variant_name )
            ( LINES OF lt_items_for_description )

          )
      ).

      mt_node_map = VALUE #(
        BASE mt_node_map
        ( node_key    = lr_variant_node->mv_node_key
          entity_id   = <ls_variant>-entity_id
          entity_type = <ls_variant>-entity_type
          is_variant  = abap_true )
      ).

      CLEAR lt_items_for_description.

    ENDLOOP.

    mr_tree_model->get_nodes( )->expand_node( ev_node_key ).
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    DATA(lt_selected_nodes) = mr_tree_model->get_selections( )->get_selected_nodes( ).

    er_menu->add_function(
        fcode = c_context_codes-delete_entry
        text  = |{ 'Delete Entry'(004) }|
    ).
    IF lines( lt_selected_nodes ) = 1.
*.... create new function for running the selected table/script and build_tree the results
      er_menu->add_function(
        fcode = c_context_codes-start_with_default_variant
        text  = |{ 'Execute with Default Settings'(005) }|
      ).
    ENDIF.
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    handle_function( ev_fcode ).
  ENDMETHOD.


  METHOD on_node_double_click.
    DATA(lr_node) = mr_tree_model->get_nodes( )->get_node( ev_node_key ).
    DATA(lr_user_object) = lr_node->get_user_object( ).
    IF lr_user_object IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF <ls_node_map>-is_variant = abap_true.
      DATA(lr_variant_fav_entry) = CAST zcl_dbbr_variant( lr_user_object ).
      DATA(ls_variant_info) = lr_variant_fav_entry->get_variant_info( ).
      zcl_dbbr_selscr_nav_events=>raise_variant_entry_chosen(
          iv_entity_id    = ls_variant_info-entity_id
          iv_entity_type  = ls_variant_info-entity_type
          iv_variant_id   = ls_variant_info-variant_id
      ).
    ELSE.
      DATA(lr_fav_entry) = CAST zcl_dbbr_favmenu_entry( lr_user_object ).
      zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
          iv_entity_id   = lr_fav_entry->get_favmenu_data( )-fav_entry
          iv_entity_type = lr_fav_entry->get_favmenu_data( )-favtype
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


  METHOD start_with_default_variant.
    DATA: lv_entity_id   TYPE zdbbr_entity_id,
          lv_variant     TYPE zdbbr_variant_id VALUE zif_dbbr_global_consts=>c_dummy_variant,
          lv_entity_type TYPE zdbbr_entity_type.

    DATA(lr_selected_node) = mr_tree_model->get_selections( )->get_selected_node( ).
    IF lr_selected_node IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mt_node_map[ node_key = lr_selected_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF <ls_node_map>-is_variant = abap_true.
      DATA(ls_variant) = CAST zcl_dbbr_variant( lr_selected_node->get_user_object( ) )->get_variant_info( ).
      lv_entity_id = ls_variant-entity_id.
      lv_entity_type = ls_variant-entity_type.
      lv_variant = ls_variant-variant_id.
    ELSE.
      DATA(ls_mostused) = CAST zcl_dbbr_favmenu_entry( lr_selected_node->get_user_object( ) )->get_favmenu_data( ).
      lv_entity_id = ls_mostused-fav_entry.
      lv_entity_type = ls_mostused-favtype.
    ENDIF.

    zcl_dbbr_selscr_nav_events=>raise_variant_entry_chosen(
        iv_entity_id    = lv_entity_id
        iv_entity_type  = lv_entity_type
        iv_variant_id   = lv_variant
        if_go_to_result = abap_true
    ).
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.
    DATA(ls_find_result) = mr_tree_model->get_search( )->find( ).

    IF ls_find_result IS NOT INITIAL.
      mr_tree_model->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    DATA(ls_find_result) = mr_tree_model->get_search( )->find_next( ).

    IF ls_find_result IS NOT INITIAL.
      mr_tree_model->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    mr_tree_model->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    rf_has_focus = mr_tree_model->zif_uitb_gui_control~has_focus( ).
  ENDMETHOD.
ENDCLASS.
