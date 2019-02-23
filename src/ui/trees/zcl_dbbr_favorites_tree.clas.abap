"! <p class="shorttext synchronized" lang="en">Favorites tree controller</p>
CLASS zcl_dbbr_favorites_tree DEFINITION
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
    ALIASES free
      FOR zif_uitb_gui_control~free.

    CONSTANTS:
      BEGIN OF c_context_codes,
        personal_favorites         TYPE sy-ucomm VALUE 'PRIV_FAV',
        global_favorites           TYPE sy-ucomm VALUE 'GLOBAL_FAV',
        collapse_node              TYPE sy-ucomm VALUE 'COLLAPSE',
        collapse_node_all          TYPE sy-ucomm VALUE 'COLLAPSE_ALL',
        expand_node                TYPE sy-ucomm VALUE 'EXPAND',
        expand_node_all            TYPE sy-ucomm VALUE 'EXPAND_ALL',
        insert_folder              TYPE sy-ucomm VALUE 'INS_FOLDER',
        show_in_object_browser     TYPE sy-ucomm VALUE 'SHOWINOBJBROWSER',
        edit_favorite              TYPE sy-ucomm VALUE 'EDIT_FAV',
        delete_favorite            TYPE sy-ucomm VALUE 'DELETE_FAV',
        insert_favorite            TYPE sy-ucomm VALUE 'INS_FAV',
        insert_current             TYPE sy-ucomm VALUE 'INS_CURRENT',
        move_up                    TYPE sy-ucomm VALUE 'MOVE_UP',
        move_down                  TYPE sy-ucomm VALUE 'MOVE_DOWN',
        search                     TYPE sy-ucomm VALUE 'SEARCH',
        search_more                TYPE sy-ucomm VALUE 'SEARCH_MORE',
        clear_list                 TYPE sy-ucomm VALUE 'CLEAR_LIST',
        delete_most_used_entry     TYPE sy-ucomm VALUE 'CLEAR_ENTRY',
        start_with_default_variant TYPE sy-ucomm VALUE 'RUNDEFAULTVAR',
        load_full_history          TYPE sy-ucomm VALUE 'LOADFULLHIST',
        import_from_clipboard      TYPE sy-ucomm VALUE 'IMPFROMCLIP',
      END OF c_context_codes .

    "! <p class="shorttext synchronized" lang="en">Build/Re Build the favorite tree</p>
    METHODS build_tree
      IMPORTING
        !if_global TYPE boolean
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Collapse the current node</p>
    METHODS collapse_node
      IMPORTING
        !if_collapse_all TYPE boolean OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !io_parent     TYPE REF TO cl_gui_container OPTIONAL
        io_parent_view TYPE REF TO zif_uitb_gui_composite_view.
    "! <p class="shorttext synchronized" lang="en">Create new favorite</p>
    METHODS create_new_favorite
      IMPORTING
        !iv_fav_type    TYPE zdbbr_favmenu_type OPTIONAL
        !iv_favorite    TYPE tabname OPTIONAL
        iv_favorite_raw TYPE zdbbr_entity_id_raw OPTIONAL
        !iv_description TYPE ddtext OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Create new folder</p>
    METHODS create_new_fav_folder .
    "! <p class="shorttext synchronized" lang="en">Creates the tree</p>
    METHODS create_tree .
    "! <p class="shorttext synchronized" lang="en">Edits the current selected favorite</p>
    METHODS edit_favorite .
    "! <p class="shorttext synchronized" lang="en">Expands the current node</p>
    METHODS expand_node
      IMPORTING
        !if_expand_all TYPE boolean OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Retrieves the currently selected node</p>
    METHODS get_selected_favorite
      RETURNING
        VALUE(rr_node) TYPE REF TO zcl_uitb_ctm_node .

    "! <p class="shorttext synchronized" lang="en">Moves the current entry one position down</p>
    METHODS move_favorite_down .
    "! <p class="shorttext synchronized" lang="en">Moves the current entry one position up</p>
    METHODS move_favorite_up .
    "! <p class="shorttext synchronized" lang="en">Shows the tree</p>
    METHODS show
      IMPORTING
        !if_global TYPE abap_bool OPTIONAL
      RAISING
        zcx_uitb_tree_error .
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
      END OF ty_fav_info.
    TYPES: tt_fav_info TYPE STANDARD TABLE OF ty_fav_info WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_node_map,
        node_key    TYPE tm_nodekey,
        entity_id   TYPE zdbbr_entity_id,
        entity_type TYPE zdbbr_entity_type,
      END OF ty_node_map .
    TYPES:
      BEGIN OF mty_node_data.
        INCLUDE TYPE treemsnod.
    TYPES: items TYPE treemcitab.
    TYPES: END OF mty_node_data .

    CONSTANTS c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##no_text.
    CONSTANTS c_descr_column TYPE tv_itmname VALUE 'DESCR' ##NO_TEXT.
    CONSTANTS c_top_node TYPE tm_nodekey VALUE '00001' ##NO_TEXT.
    DATA mt_node_map TYPE SORTED TABLE OF ty_node_map WITH UNIQUE KEY node_key .
    DATA mv_last_var_node_key TYPE tm_nodekey VALUE '50000' ##NO_TEXT.
    DATA mo_tree_model TYPE REF TO zcl_uitb_column_tree_model .
    DATA mf_global_fav_mode TYPE boolean .
    DATA mo_favmenu_f TYPE REF TO zcl_dbbr_favmenu_factory .
    DATA mo_tree_dnd_behaviour TYPE REF TO cl_dragdrop .
    DATA mo_variant_f TYPE REF TO zcl_dbbr_variant_factory .
    DATA mo_query_f TYPE REF TO zcl_dbbr_query_factory .
    DATA mo_parent_container TYPE REF TO cl_gui_container .
    DATA mo_parent_view TYPE REF TO zif_uitb_gui_composite_view.

    "! <p class="shorttext synchronized" lang="en">Creates multiple favorite entries from table entries</p>
    METHODS create_multiple_faventries
      IMPORTING
        it_favinfo               TYPE tt_fav_info
        !io_selected_node        TYPE REF TO zcl_uitb_ctm_node
        !is_parent_favmenu       TYPE zdbbr_favmenu
        !iv_parent_relationship  TYPE i
      RETURNING
        VALUE(rv_first_new_node) TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Deletes the current favorite</p>
    METHODS delete_favorite_node
      IMPORTING
        !io_selected_node          TYPE REF TO zcl_uitb_ctm_node
      RETURNING
        VALUE(rf_deletion_success) TYPE boolean .

    "! <p class="shorttext synchronized" lang="en">Deletes several selected favorite entries</p>
    METHODS delete_selected_nodes .
    "! <p class="shorttext synchronized" lang="en">Checks if the favorite exists in the node</p>
    METHODS favorite_exists_in_node
      IMPORTING
        !io_node             TYPE REF TO zcl_uitb_ctm_node
      RETURNING
        VALUE(rf_fav_exists) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Fill the toolbar with functions</p>
    METHODS fill_toolbar .
    "! <p class="shorttext synchronized" lang="en">Gets favorite menu data for node</p>
    METHODS get_node_favmenu_data
      IMPORTING
        !io_node          TYPE REF TO zcl_uitb_ctm_node
      RETURNING
        VALUE(rs_favmenu) TYPE zdbbr_favmenu .
    "! <p class="shorttext synchronized" lang="en">Get the selection mode</p>
    METHODS get_selection_mode
      RETURNING
        VALUE(result) TYPE i .
    "! <p class="shorttext synchronized" lang="en">handles a ui function code</p>
    "!
    METHODS handle_gui_function
      IMPORTING
        iv_code     TYPE sy-ucomm
        iv_node_key TYPE tm_nodekey OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Loads all favorite nodes into the tree</p>
    METHODS load_favorite_nodes
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Handler for when drop completed</p>
    METHODS on_drop_complete
          FOR EVENT drop_complete OF zif_uitb_tree_model_events
      IMPORTING
          !er_drag_drop_object
          !ev_item_name
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for deleted entity -&gt; remove invalid nodes</p>
    METHODS on_entity_deleted
          FOR EVENT entity_deleted OF zcl_dbbr_selscreen_util
      IMPORTING
          !ev_entity_id
          !ev_entity_type .
    "! <p class="shorttext synchronized" lang="en">Handler for when children are to be loaded lazily</p>
    METHODS on_expand_no_children
          FOR EVENT expand_no_children OF zif_uitb_tree_model_events
      IMPORTING
          !ev_node_key .
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
    "! <p class="shorttext synchronized" lang="en">Handler for pressed toolbar button</p>
    METHODS on_toolbar_button
          FOR EVENT function_selected OF zif_uitb_toolbar_events
      IMPORTING
          !ev_fcode .
    "! <p class="shorttext synchronized" lang="en">Handler for Tree drag was started</p>
    METHODS on_tree_drag
          FOR EVENT drag OF zif_uitb_tree_model_events
      IMPORTING
          !er_drag_drop_object
          !ev_item_name
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler fro Multiple Tree drag was started</p>
    METHODS on_tree_drag_multiple
          FOR EVENT drag_multiple OF zif_uitb_tree_model_events
      IMPORTING
          !er_drag_drop_object
          !et_node_key_table
          !ev_item_name .
    "! <p class="shorttext synchronized" lang="en">Handler for a single drop</p>
    METHODS on_tree_drop
          FOR EVENT drop OF zif_uitb_tree_model_events
      IMPORTING
          !er_drag_drop_object
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for multiple drops completed</p>
    METHODS on_tree_drop_complete_multiple
          FOR EVENT drop_complete_multiple OF zif_uitb_tree_model_events
      IMPORTING
          !er_drag_drop_object
          !et_node_key_table
          !ev_item_name .
    "! <p class="shorttext synchronized" lang="en">Sets favorite menu data for node</p>
    METHODS set_node_favmenu_data
      IMPORTING
        !io_node    TYPE REF TO zcl_uitb_ctm_node
        !is_favmenu TYPE zdbbr_favmenu .
    "! <p class="shorttext synchronized" lang="en">Checks if node should be displayed as folder</p>
    METHODS should_display_as_folder
      IMPORTING
        !iv_fav_type     TYPE zdbbr_favmenu_type
        !if_has_variants TYPE abap_bool
      RETURNING
        VALUE(result)    TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Delete favorite</p>
    "!
    "! @parameter io_node | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_update_selection | <p class="shorttext synchronized" lang="en"></p>
    METHODS delete_favorite
      IMPORTING
        io_node             TYPE REF TO zcl_uitb_ctm_node OPTIONAL
        if_update_selection TYPE abap_bool DEFAULT abap_true.
    "! <p class="shorttext synchronized" lang="en">Start entity behind favorite with default variant</p>
    METHODS start_with_default_variant .
    "! <p class="shorttext synchronized" lang="en">Updates the menulevel of the child nodes of a node</p>
    METHODS update_menulevel_of_children
      IMPORTING
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Updates the sort order of the child nodes of a node</p>
    METHODS update_sortorder_of_children
      IMPORTING
        iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Import favorites from Clipboard</p>
    "!
    "! <p class="shorttext synchronized" lang="en">Import favorites from Clipboard</p>
    METHODS import_from_clipboard
      IMPORTING
        iv_node_key TYPE tm_nodekey OPTIONAL.
    METHODS show_in_object_list
      IMPORTING
        iv_node_key TYPE tm_nodekey.
ENDCLASS.



CLASS zcl_dbbr_favorites_tree IMPLEMENTATION.


  METHOD build_tree.

    mf_global_fav_mode = if_global.

    DATA(lo_nodes) = mo_tree_model->get_nodes( ).
    lo_nodes->delete_all_nodes( ).

    DATA(lv_node_text) = COND string( WHEN if_global = abap_true THEN
                                   'Favorites (Global)'
                                 ELSE
                                   |Favorites for { sy-uname }| ).

*.. create dummy favmenu entry data for top node
    DATA(ls_favmenu_data) = VALUE zdbbr_favmenu(
        uname      = COND #( WHEN mf_global_fav_mode = abap_false THEN sy-uname )
        favtype    = zif_dbbr_c_favmenu_type=>folder
        object_id  = 1
        menu_level = 0
    ).

    DATA(lo_root_dnd_behavior) = NEW cl_dragdrop( ).

    lo_root_dnd_behavior->add(
        flavor          = 'Node'
        dragsrc         = abap_false
        droptarget      = abap_true
        effect          = cl_dragdrop=>move
    ).
    lo_root_dnd_behavior->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

*.. create top node for fields
    lo_nodes->add_node(
      iv_node_key              = c_top_node
      if_folder                = abap_true
      ir_user_object           = NEW zcl_dbbr_favmenu_entry( ls_favmenu_data )
      iv_drag_drop_id          = lv_dnd_handle
      it_item_table            = VALUE #(
        ( item_name  = zcl_uitb_column_tree_model=>c_hierarchy_column
          class      = cl_list_tree_model=>item_class_text
          font       = cl_list_tree_model=>item_font_prop
          text       = lv_node_text
        )
      )
    ).

    CLEAR mt_node_map.

*.. fill tree with saved nodes
    load_favorite_nodes( ).

    lo_nodes->expand_node( c_top_node ).

    mo_tree_model->get_selections( )->select_nodes( VALUE #( ( c_top_node ) ) ).

    focus( ).
  ENDMETHOD.


  METHOD collapse_node.
    DATA(lt_selected_node) = mo_tree_model->get_selections( )->get_selected_nodes( ).

    CHECK: lt_selected_node IS NOT INITIAL,
           lines( lt_selected_node ) = 1.

    DATA(lo_selected_node) = lt_selected_node[ 1 ].

    IF lo_selected_node IS INITIAL OR NOT lo_selected_node->is_folder( ).
      RETURN.
    ENDIF.

    " expand the node
    mo_tree_model->get_nodes( )->collapse_node(
      iv_node_key         = lo_selected_node->mv_node_key
      if_collapse_subtree = if_collapse_all
    ).
  ENDMETHOD.


  METHOD constructor.
    mo_parent_view = io_parent_view.
    mo_favmenu_f = NEW zcl_dbbr_favmenu_factory( ).
    mo_query_f = NEW #( ).
    mo_variant_f = NEW #( ).
    mo_parent_container = io_parent.

    SET HANDLER:
       on_entity_deleted.
  ENDMETHOD.


  METHOD create_multiple_faventries.
    DATA: lt_favmenu_new TYPE zdbbr_favmenu_itab.

    mo_tree_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

    DATA(lv_user) = COND #( WHEN mf_global_fav_mode = abap_false THEN sy-uname ).
    DATA(lv_relationship) = iv_parent_relationship.
    DATA(lv_relative_key) = io_selected_node->mv_node_key.
    DATA(lv_new_fav_id) = mo_favmenu_f->get_next_object_id( if_global = mf_global_fav_mode ).
    DATA(lv_parent_id) = is_parent_favmenu-object_id.
    DATA(lv_menu_level) = is_parent_favmenu-menu_level + 1.


    LOOP AT it_favinfo ASSIGNING FIELD-SYMBOL(<ls_tabinfo>).
      DATA(ls_favmenu_entry) = VALUE zdbbr_favmenu(
          uname         = lv_user
          favtype       = <ls_tabinfo>-type
          fav_entry     = <ls_tabinfo>-favorite
          fav_entry_raw = <ls_tabinfo>-favorite_raw
          parent_id     = is_parent_favmenu-object_id
          object_id     = lv_new_fav_id
          menu_level    = lv_menu_level
          text          = <ls_tabinfo>-description
      ).

      lt_favmenu_new = VALUE #( BASE lt_favmenu_new ( ls_favmenu_entry ) ).

*.... add node to node table
      DATA(lv_node_id) = |{ lv_new_fav_id ALPHA = IN }|.

      IF rv_first_new_node IS INITIAL.
        rv_first_new_node = lv_node_id.
      ENDIF.

      DATA(lv_node_image) = COND #(
        WHEN <ls_tabinfo>-type = zif_dbbr_c_favmenu_type=>cds_view THEN zif_dbbr_c_icon=>cds_view
        WHEN <ls_tabinfo>-type = zif_dbbr_c_favmenu_type=>table THEN zif_dbbr_c_icon=>database_table
      ).

      mo_tree_model->get_nodes( )->add_node(
          iv_node_key          = lv_node_id
          iv_relative_node_key = lv_relative_key
          iv_relationship      = lv_relationship
          iv_drag_drop_id      = lv_dnd_handle
          iv_image             = lv_node_image
          iv_expanded_image    = lv_node_image
          ir_user_object       = NEW zcl_dbbr_favmenu_entry( ls_favmenu_entry )
          it_item_table        = VALUE #(
             ( item_name  = mo_tree_model->c_hierarchy_column
               class      = cl_item_tree_model=>item_class_text
               font       = cl_item_tree_model=>item_font_prop
               text       = ls_favmenu_entry-fav_entry_raw
             )
             ( item_name = c_hierarchy_node2
               style     = zif_uitb_c_ctm_style=>inverted_gray
               font      = cl_item_tree_model=>item_font_prop
               class     = cl_item_tree_model=>item_class_text
               text      = ls_favmenu_entry-text
             )
          )
      ).

*.... fill new id values
      lv_relationship = cl_list_tree_model=>relat_next_sibling.
      lv_relative_key = lv_new_fav_id.
      lv_parent_id = lv_new_fav_id.
      lv_new_fav_id = lv_new_fav_id + 1.

    ENDLOOP.

    mo_favmenu_f->update_favorites( lt_favmenu_new ).
  ENDMETHOD.


  METHOD create_new_favorite.
    DATA: lv_node_description  TYPE ddtext,
          lv_favorite_name     TYPE tabname,
          lv_favorite_name_raw TYPE zdbbr_entity_id_raw,
          lv_favorite_type     TYPE zdbbr_favmenu_type.

    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    IF lo_selected_node IS INITIAL OR
       zcl_dbbr_tree_helper=>is_variant_node( lo_selected_node ).
      RETURN.
    ENDIF.

    IF NOT favorite_exists_in_node( lo_selected_node ).
      build_tree( mf_global_fav_mode ).
      MESSAGE TEXT-001 TYPE 'S'.
      RETURN.
    ENDIF.

    IF iv_favorite IS NOT INITIAL.
      lv_node_description = iv_description.
      lv_favorite_name = iv_favorite.
      lv_favorite_name_raw = iv_favorite_raw.
      lv_favorite_type = iv_fav_type.
    ELSE.
      DATA(lo_choose_tab_query_ctrl) = NEW zcl_dbbr_choose_object_ctrl(
              if_global_fav_mode = mf_global_fav_mode
              if_for_fav_menu    = abap_true
          ).
      lo_choose_tab_query_ctrl->zif_uitb_screen_controller~call_screen( ).

      IF NOT lo_choose_tab_query_ctrl->zif_uitb_screen_controller~was_not_cancelled( ).
        RETURN.
      ENDIF.

      lo_choose_tab_query_ctrl->get_chosen_entry(
        IMPORTING ev_entry     = lv_favorite_name
                  ev_type      = lv_favorite_type
      ).
    ENDIF.

    """ determine new relationship information for new favorite
    IF lo_selected_node->is_folder( ).
      DATA(lv_relationship) = cl_list_tree_model=>relat_first_child.
      DATA(lo_parent_favnode) = lo_selected_node.
    ELSE.
      lv_relationship = cl_list_tree_model=>relat_next_sibling.
      lo_parent_favnode = lo_selected_node->get_parent( ).
    ENDIF.

    DATA(ls_parent_favnode_data) = get_node_favmenu_data( lo_parent_favnode ).

    """ 1) get description for new node
    IF lv_node_description IS INITIAL OR iv_favorite_raw IS INITIAL.
      CASE lv_favorite_type.

        WHEN zif_dbbr_c_favmenu_type=>table OR
             zif_dbbr_c_favmenu_type=>view.
          lv_node_description = zcl_dbbr_dictionary_helper=>get_table_info( lv_favorite_name )-ddtext.
          lv_favorite_name_raw = lv_favorite_name.

        WHEN zif_dbbr_c_favmenu_type=>cds_view.
          DATA(ls_cds_header) = zcl_dbbr_cds_view_factory=>read_cds_view_header( lv_favorite_name ).
          lv_node_description = ls_cds_header-description.
          lv_favorite_name_raw = ls_cds_header-entityname_raw.

        WHEN zif_dbbr_c_favmenu_type=>query.
          DATA(lo_query_f) = NEW zcl_dbbr_query_factory( ).
          DATA(ls_query) = lo_query_f->get_query( iv_query_name = lv_favorite_name ).
          lv_node_description = ls_query-description.
          lv_favorite_name_raw = lv_favorite_name.

      ENDCASE.
    ENDIF.

    " get the current new max object id for the new favorite entry
    DATA(lv_new_fav_id) = mo_favmenu_f->get_next_object_id( if_global = mf_global_fav_mode ).
    DATA(ls_favmenu_entry_new) = VALUE zdbbr_favmenu(
        uname         = COND sysuname( WHEN mf_global_fav_mode = abap_false THEN sy-uname )
        favtype       = lv_favorite_type
        fav_entry     = lv_favorite_name
        fav_entry_raw = lv_favorite_name_raw
        object_id     = lv_new_fav_id
        parent_id     = ls_parent_favnode_data-object_id
        menu_level    = ls_parent_favnode_data-menu_level + 1
        text          = lv_node_description
    ).

    mo_favmenu_f->update_favorite( ls_favmenu_entry_new ).

    DATA(lv_new_node_key) = |{ lv_new_fav_id ALPHA = IN }|.

    " 3) add node as next child
    mo_tree_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

    mo_tree_model->get_nodes( )->add_node(
      iv_node_key                = lv_new_node_key
      iv_relative_node_key       = lo_selected_node->mv_node_key
      iv_relationship            = lv_relationship
      if_folder                = abap_false
      iv_image                   = zcl_dbbr_tree_helper=>get_tree_node_icon( lv_favorite_type )
      iv_drag_drop_id            = lv_dnd_handle
      ir_user_object             = NEW zcl_dbbr_favmenu_entry( ls_favmenu_entry_new )
      it_item_table              = VALUE #(
        ( item_name  = mo_tree_model->c_hierarchy_column
          class      = cl_item_tree_model=>item_class_text
          font       = cl_item_tree_model=>item_font_prop
          text       = lv_favorite_name_raw
        )
        ( item_name = c_hierarchy_node2
          style     = zif_uitb_c_ctm_style=>inverted_gray
          font      = cl_item_tree_model=>item_font_prop
          class     = cl_item_tree_model=>item_class_text
          text      = lv_node_description
        )
      )    " Items des Knotens
    ).

    mt_node_map = VALUE #( BASE mt_node_map
     ( node_key     = lv_new_node_key
       entity_id    = ls_favmenu_entry_new-fav_entry
       entity_type  = ls_favmenu_entry_new-favtype
     )
    ).

    " expand  select the newly created folder
    mo_tree_model->get_selections( )->select_nodes( VALUE #( ( lv_new_node_key ) ) ).

    " update sort order of all childs
    update_sortorder_of_children( lo_parent_favnode->mv_node_key ).

    focus( ).
  ENDMETHOD.


  METHOD create_new_fav_folder.
    DATA: lv_rcode(1),
          lt_fields TYPE TABLE OF sval.

    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    IF lo_selected_node IS INITIAL OR
       zcl_dbbr_tree_helper=>is_variant_node( lo_selected_node ).
      RETURN.
    ENDIF.


    IF NOT favorite_exists_in_node( lo_selected_node ).
      build_tree( mf_global_fav_mode ).
      MESSAGE TEXT-001 TYPE 'S'.
      RETURN.
    ENDIF.

    DATA(lv_folder_name) = zcl_dbbr_appl_util=>popup_get_value(
         is_field  = VALUE #( tabname = 'DD03T' fieldname = 'DDTEXT'  field_obl = abap_true fieldtext = 'Folder name' )
         iv_title  = 'Enter name of folder'
     ).

    IF lv_folder_name IS INITIAL.
      RETURN.
    ENDIF.

    """ determine new relationship information for new favorite
    IF lo_selected_node->is_folder( ).
      DATA(lv_relationship) = cl_list_tree_model=>relat_first_child.
      DATA(lo_parent_favnode) = lo_selected_node.
    ELSE.
      lv_relationship = cl_list_tree_model=>relat_next_sibling.
      lo_parent_favnode = lo_selected_node->get_parent( ).
    ENDIF.

    DATA(ls_parent_favnode_data) = get_node_favmenu_data( lo_parent_favnode ).


    " 2) create new fav entry for folder
    " get the current new max object id for the new favorite entry
    DATA(lv_new_fav_id) = mo_favmenu_f->get_next_object_id( if_global = mf_global_fav_mode ).
    DATA(ls_favmenu_entry_new) = VALUE zdbbr_favmenu(
        uname      = COND sysuname( WHEN mf_global_fav_mode = abap_false THEN sy-uname )
        favtype    = zif_dbbr_c_favmenu_type=>folder
        object_id  = lv_new_fav_id
        parent_id  = ls_parent_favnode_data-object_id
        menu_level = ls_parent_favnode_data-menu_level + 1
        text       = lv_folder_name
    ).

    mo_favmenu_f->update_favorite( ls_favmenu_entry_new ).

    DATA(lv_new_node_key) = |{ lv_new_fav_id ALPHA = IN }|.


    " 3) add node as next child
    mo_tree_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

    mo_tree_model->get_nodes( )->add_node(
        iv_node_key                = lv_new_node_key
        iv_relative_node_key       = lo_selected_node->mv_node_key
        iv_relationship            = lv_relationship
        if_folder                = abap_true
        iv_drag_drop_id            = lv_dnd_handle
        ir_user_object             = NEW zcl_dbbr_favmenu_entry( ls_favmenu_entry_new )
        it_item_table              = VALUE #(
        ( item_name  = mo_tree_model->c_hierarchy_column
          class      = cl_item_tree_model=>item_class_text
          font       = cl_item_tree_model=>item_font_prop
          text       = lv_folder_name

        )
      )
    ).

*.. create new node map entry
    mt_node_map = VALUE #( BASE mt_node_map
     ( node_key     = lv_new_node_key
       entity_id    = ls_favmenu_entry_new-fav_entry
       entity_type  = ls_favmenu_entry_new-favtype
     )
    ).

    " expand  select the newly created folder
    mo_tree_model->get_selections( )->select_nodes( VALUE #( ( lv_new_node_key ) ) ).

    update_sortorder_of_children( lo_parent_favnode->mv_node_key ).

    focus( ).
  ENDMETHOD.


  METHOD create_tree.
*&---------------------------------------------------------------------*
*& Description: Creates the favorite tree
*&---------------------------------------------------------------------*
    DATA: lt_events TYPE cntl_simple_events.

    CHECK mo_tree_model IS INITIAL.

    mo_tree_model = NEW zcl_uitb_column_tree_model(
        ir_parent           = mo_parent_container
        is_hierarchy_header = VALUE #(
          heading = 'Object Name'
        )
        if_with_toolbar     = abap_true
        iv_selection_mode   = get_selection_mode( )
    ).

    mo_tree_model->get_columns( )->add_hierarchy_column( c_hierarchy_node2 ).

*.. create drag-n-drop behaviour object
    mo_tree_dnd_behaviour = NEW #( ).

    mo_tree_dnd_behaviour->add(
        flavor          = 'Node'
        dragsrc         = abap_true
        droptarget      = abap_true
        effect          = cl_dragdrop=>move
    ).

    fill_toolbar( ).

    mo_tree_model->create_tree_control( ).

    DATA(lo_events) = mo_tree_model->get_events( ).

    lo_events->add_key_for_keypress( cl_tree_model=>key_delete ).
    lo_events->add_key_for_keypress( cl_tree_model=>key_insert ).

*.. set event handler methods to tree control
    SET HANDLER:
      on_tree_drag FOR lo_events,
      on_tree_drag_multiple FOR lo_events,
      on_tree_drop_complete_multiple FOR lo_events,
      on_tree_drop FOR lo_events,
      on_drop_complete FOR lo_events,
      on_node_context_menu_request FOR lo_events,
      on_node_context_menu_select FOR lo_events,
      on_node_double_click FOR lo_events,
      on_node_enter_key FOR lo_events,
      on_expand_no_children FOR lo_events.

  ENDMETHOD.


  METHOD delete_favorite.
    DATA: lv_answer                  TYPE char1,
          lo_selected_node           TYPE REF TO zcl_uitb_ctm_node,
          lv_new_node_after_deletion TYPE tm_nodekey.

    IF io_node IS BOUND.
      lo_selected_node = io_node.
    ELSE.
      DATA(lt_selected_nodes) = mo_tree_model->get_selections( )->get_selected_nodes( ).
      IF lines( lt_selected_nodes ) > 1.
        delete_selected_nodes( ).
        RETURN.
      ELSEIF lines( lt_selected_nodes ) = 1.
        lo_selected_node = lt_selected_nodes[ 1 ].
      ENDIF.
    ENDIF.

    IF lo_selected_node IS INITIAL OR
       lo_selected_node->mv_node_key = c_top_node OR
       zcl_dbbr_tree_helper=>is_variant_node( lo_selected_node ).
      RETURN.
    ENDIF.

    IF NOT favorite_exists_in_node( lo_selected_node ).
      build_tree( mf_global_fav_mode ).
      MESSAGE TEXT-001 TYPE 'S'.
      RETURN.
    ENDIF.

    IF lo_selected_node->has_children( ).
      IF zcl_dbbr_appl_util=>popup_to_confirm( iv_title     = 'Delete favorite folder'
                                               iv_query     = 'Should the folder and all its child nodes really be deleted?'
                                               iv_icon_type = 'ICON_QUESTION' ) <> '1'.
        RETURN.
      ENDIF.
    ENDIF.


*.. 1) delete favorites from ZDBBR_favmenu
    delete_favorite_node( lo_selected_node ).

*.. 2) determine new selection node
    DATA(lo_previous_node) = lo_selected_node->get_previous_sibling( ).
    IF lo_previous_node IS NOT INITIAL.
      lv_new_node_after_deletion = lo_previous_node->mv_node_key.
    ELSE.
*.... get the parent node
      lv_new_node_after_deletion = lo_selected_node->get_parent( )->mv_node_key.
    ENDIF.

*.. 3) delete the node and all subnodes
    mo_tree_model->get_nodes( )->delete_node( lo_selected_node->mv_node_key ).

    IF if_update_selection = abap_true.
*.... 4) select new node
      mo_tree_model->get_selections( )->select_nodes( VALUE #( ( lv_new_node_after_deletion ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD delete_favorite_node.
*.. 1) delete all sub nodes (recursively first
    IF io_selected_node->has_children( ).
      LOOP AT io_selected_node->get_children( ) ASSIGNING FIELD-SYMBOL(<lo_child>).
        delete_favorite_node( <lo_child> ).
      ENDLOOP.
    ENDIF.

*.. delete entry from node map
    DELETE mt_node_map WHERE node_key = io_selected_node->mv_node_key.

*.. 2) delete the node itself
    mo_favmenu_f->delete_favorite( get_node_favmenu_data( io_selected_node ) ).
  ENDMETHOD.


  METHOD delete_selected_nodes.
    DATA(lt_nodes) = mo_tree_model->get_selections( )->get_selected_nodes( ).
    DATA(lv_node_count) = lines( lt_nodes ).
    LOOP AT lt_nodes ASSIGNING FIELD-SYMBOL(<lo_node>).
      delete_favorite(
          io_node             = <lo_node>
          if_update_selection = xsdbool( sy-tabix = lv_node_count )
      ).
    ENDLOOP.

    MESSAGE |{ lv_node_count } Favorites have been deleted.| TYPE 'S'.
  ENDMETHOD.


  METHOD edit_favorite.
    DATA: lv_rcode(1),
          lt_fields TYPE TABLE OF sval.

    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    IF lo_selected_node IS INITIAL OR
       lo_selected_node->mv_node_key = c_top_node.
      RETURN.
    ENDIF.

    IF NOT favorite_exists_in_node( lo_selected_node ).
      build_tree( mf_global_fav_mode ).
      MESSAGE TEXT-001 TYPE 'S'.
      RETURN.
    ENDIF.

    DATA(ls_favmenu_data) = get_node_favmenu_data( lo_selected_node ).

    lt_fields = VALUE #( ( tabname = 'DD03T' fieldname = 'DDTEXT'  field_obl = abap_true fieldtext = 'Text' value = ls_favmenu_data-text ) ).

    IF zcl_uitb_appl_util=>popup_get_values(
         EXPORTING iv_title  = 'Edit Text'
         CHANGING  ct_fields = lt_fields   ).

      DATA(lv_new_favorite_text) = CONV string( lt_fields[ 1 ]-value ).
      IF lv_new_favorite_text = ls_favmenu_data-text.
        RETURN. " no changes needed
      ENDIF.
      ls_favmenu_data-text = lv_new_favorite_text.
    ELSE.
      RETURN.
    ENDIF.

    IF lo_selected_node->is_folder( ).
      lo_selected_node->get_item( mo_tree_model->c_hierarchy_column )->set_text( |{ lv_new_favorite_text }| ).
    ELSE.
      lo_selected_node->get_item( c_hierarchy_node2 )->set_text( |{ lv_new_favorite_text }| ).
    ENDIF.

    mo_favmenu_f->update_favorite( ls_favmenu_data ).
    lo_selected_node->set_user_object( NEW zcl_dbbr_favmenu_entry( ls_favmenu_data ) ).

    mo_tree_model->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD expand_node.
    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    IF lo_selected_node IS INITIAL OR NOT lo_selected_node->is_folder( ).
      RETURN.
    ENDIF.

*.. expand the node
    mo_tree_model->get_nodes( )->expand_node(
      iv_node_key       = lo_selected_node->mv_node_key
      if_expand_subtree = if_expand_all
    ).
  ENDMETHOD.


  METHOD favorite_exists_in_node.
    IF io_node->mv_node_key = c_top_node.
      rf_fav_exists = abap_true.
      RETURN.
    ENDIF.

    rf_fav_exists = mo_favmenu_f->favorite_exists(
      get_node_favmenu_data( io_node )
    ).
  ENDMETHOD.


  METHOD fill_toolbar.
    DATA(lo_toolbar) = mo_tree_model->get_toolbar( ).

    DATA(lt_buttons) = VALUE ttb_button(
      ( function  = c_context_codes-personal_favorites
        icon      = icon_system_user_menu
        quickinfo = 'Show User Favorites'
        butn_type = cntb_btype_button
        text      = space )
      ( function  = c_context_codes-global_favorites
        icon      = icon_system_sap_menu
        quickinfo = 'Show global Favorites'
        butn_type = cntb_btype_button
        text      = space )
      ( butn_type = cntb_btype_sep )
      ( function  = c_context_codes-expand_node_all
        icon      = icon_expand_all
        butn_type = cntb_btype_button
        quickinfo = 'Expand node completely' )
      ( function  = c_context_codes-collapse_node_all
        icon      = icon_collapse_all
        butn_type = cntb_btype_button
        quickinfo = 'Collapse node completely' )
      ( butn_type = cntb_btype_sep )
      ( function  = c_context_codes-search
        icon      = icon_search
        butn_type = cntb_btype_button
        quickinfo = 'Find' )
      ( function  = c_context_codes-search_more
        icon      = icon_search_next
        butn_type = cntb_btype_button
        quickinfo = 'Find next' )
      ( butn_type = cntb_btype_sep )
      ( function  = c_context_codes-insert_folder
        icon      = icon_open_folder
        quickinfo = 'Insert new Folder'
        butn_type = cntb_btype_button  )
      ( function  = c_context_codes-insert_favorite
        icon      = icon_insert_favorites
        quickinfo = 'Insert Table/query'
        butn_type = cntb_btype_button )
      ( function  = c_context_codes-delete_favorite
        icon      = icon_delete_favorites
        quickinfo = 'Delete Favorite'
        butn_type = cntb_btype_button  )
      ( butn_type = cntb_btype_sep     )
      ( function  = c_context_codes-move_up
        icon      = icon_next_value
        quickinfo = 'Move up'
        butn_type = cntb_btype_button )
      ( function  = c_context_codes-move_down
        icon      = icon_previous_value
        quickinfo = 'Move down'
        butn_type = cntb_btype_button )
    ).

    lo_toolbar->add_buttons( lt_buttons ).

*.. register event handler
    SET HANDLER on_toolbar_button FOR lo_toolbar.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~free.
*... clear all nodes
    IF mo_tree_model IS BOUND.
      mo_tree_model->get_nodes( )->delete_all_nodes( ).
    ENDIF.

    CLEAR mt_node_map.
  ENDMETHOD.


  METHOD get_node_favmenu_data.
    DATA(lo_user_object) = io_node->get_user_object( ).
    CHECK lo_user_object IS BOUND.

    TRY.
        rs_favmenu = CAST zcl_dbbr_favmenu_entry( lo_user_object )->get_favmenu_data( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.


  METHOD get_selected_favorite.
    rr_node = mo_tree_model->get_selections( )->get_selected_node( ).
  ENDMETHOD.


  METHOD get_selection_mode.
    result = cl_tree_model=>node_sel_mode_multiple.
  ENDMETHOD.

  METHOD handle_gui_function.

    CASE iv_code.
      WHEN c_context_codes-search.
        zif_uitb_content_searcher~search( ).

      WHEN c_context_codes-search_more.
        zif_uitb_content_searcher~search_next( ).

      WHEN c_context_codes-personal_favorites.
        build_tree( abap_false ).

      WHEN c_context_codes-global_favorites.
        build_tree( abap_true ).

      WHEN c_context_codes-move_up.
        move_favorite_up( ).

      WHEN c_context_codes-move_down.
        move_favorite_down( ).

      WHEN c_context_codes-collapse_node.
        collapse_node( ).

      WHEN c_context_codes-collapse_node_all.
        collapse_node( abap_true ).

      WHEN c_context_codes-expand_node.
        expand_node( ).

      WHEN c_context_codes-expand_node_all.
        expand_node( abap_true ).

      WHEN c_context_codes-insert_folder.
        create_new_fav_folder( ).

      WHEN c_context_codes-edit_favorite.
        edit_favorite( ).

      WHEN c_context_codes-delete_favorite.
        delete_favorite( ).

      WHEN c_context_codes-insert_favorite.
        create_new_favorite( ).

      WHEN c_context_codes-insert_current.
        zcl_dbbr_selscr_nav_events=>raise_favtree_event( NEW zcl_dbbr_favmenu_evt_handler( me ) ).

      WHEN c_context_codes-import_from_clipboard.
        import_from_clipboard( ).

      WHEN c_context_codes-start_with_default_variant.
        start_with_default_variant( ).

      WHEN c_context_codes-show_in_object_browser.
        show_in_object_list( iv_node_key ).
    ENDCASE.
  ENDMETHOD.


  METHOD import_from_clipboard.
    DATA: lv_clipboard_count TYPE sy-tabix.

*.. 1) validate of import is possible on current node
    DATA(lo_node_for_import) = COND #( WHEN iv_node_key IS NOT INITIAL THEN
                                        mo_tree_model->get_nodes( )->get_node( iv_node_key )
                                     ELSE
                                        mo_tree_model->get_selections( )->get_selected_node( ) ).

    IF lo_node_for_import IS INITIAL OR
       zcl_dbbr_tree_helper=>is_variant_node( lo_node_for_import ).
      RETURN.
    ENDIF.

    IF NOT favorite_exists_in_node( lo_node_for_import ).
      build_tree( mf_global_fav_mode ).
      MESSAGE TEXT-001 TYPE 'S'.
      RETURN.
    ENDIF.

*.. Try to retrieve data from clipboard
    DATA(lt_entities) = zcl_dbbr_dictionary_helper=>get_entities_from_clipboard( IMPORTING ev_clipboard_count = lv_clipboard_count ).
    IF lt_entities IS INITIAL.
      MESSAGE s082(zdbbr_info).
      RETURN.
    ENDIF.

*.. 6) evaluate current parent node
*.. determine relationship information for new favorite

    IF lo_node_for_import->is_folder( ).
      DATA(lv_relationship) = cl_list_tree_model=>relat_first_child.
      DATA(lo_parent_favnode) = lo_node_for_import.
    ELSE.
      lv_relationship = cl_list_tree_model=>relat_next_sibling.
      lo_parent_favnode = lo_node_for_import->get_parent( ).
    ENDIF.

    DATA(ls_parent_favnode_data) = get_node_favmenu_data( lo_parent_favnode ).

*.. 7) create node/favmenu entries for all db tables in package
    DATA(lv_first_new_node) = create_multiple_faventries(
        it_favinfo             = VALUE #(
          FOR entity IN lt_entities
          ( favorite     = entity-entity_id
            favorite_raw = entity-entity_id_raw
            type         = entity-entity_type
            description  = entity-description )
        )
        io_selected_node       = lo_node_for_import
        is_parent_favmenu      = ls_parent_favnode_data
        iv_parent_relationship = lv_relationship
    ).

*.. expand  select the newly created folder
    mo_tree_model->get_selections( )->select_nodes( VALUE #( ( lv_first_new_node ) ) ).

*.. update sort order of all children
    update_sortorder_of_children( lo_parent_favnode->mv_node_key ).

    mo_tree_model->zif_uitb_gui_control~focus( ).

    MESSAGE s083(zdbbr_info) WITH |{ lines( lt_entities ) NUMBER = USER }| |{ lv_clipboard_count NUMBER = USER }|.
  ENDMETHOD.


  METHOD load_favorite_nodes.
    DATA: lt_items TYPE treemcitab,
          lv_icon  TYPE tv_image.

    mo_tree_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

    mo_favmenu_f->get_favorites(
      EXPORTING
        if_global_favorites = mf_global_fav_mode
      IMPORTING
        et_favmenu_entries  = DATA(lt_favmenu_entries)
    ).

    LOOP AT lt_favmenu_entries ASSIGNING FIELD-SYMBOL(<ls_favmenu>)
      GROUP BY ( menu_level = <ls_favmenu>-menu_level )
      ASCENDING
      ASSIGNING FIELD-SYMBOL(<ls_favmenu_group>).

      DATA(lt_menu_entries) = VALUE zdbbr_favorite_menu_itab( FOR entry IN GROUP <ls_favmenu_group> ( entry ) ).

      SORT lt_menu_entries BY sort_order ASCENDING.

      LOOP AT lt_menu_entries ASSIGNING FIELD-SYMBOL(<ls_group_entry>).

        DATA(lf_is_folder) = should_display_as_folder(
           iv_fav_type     = <ls_group_entry>-favtype
           if_has_variants = <ls_group_entry>-has_variants
        ).

        lv_icon = zcl_dbbr_tree_helper=>get_tree_node_icon( <ls_group_entry>-favtype ).

        lt_items = VALUE #(
          ( item_name  = zcl_uitb_column_tree_model=>c_hierarchy_column
            class      = cl_item_tree_model=>item_class_text
            font       = cl_item_tree_model=>item_font_prop
            text       = COND #( WHEN <ls_group_entry>-favtype = zif_dbbr_c_favmenu_type=>folder THEN
                                   <ls_group_entry>-text
                                 ELSE
                                   <ls_group_entry>-fav_entry_raw ) )
        ).

        IF <ls_group_entry>-favtype <> zif_dbbr_c_favmenu_type=>folder.
          lt_items = VALUE #( BASE lt_items
            ( item_name = c_hierarchy_node2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = <ls_group_entry>-text )
          ).
        ENDIF.


        mo_tree_model->get_nodes( )->add_node(
            iv_node_key          = |{ <ls_group_entry>-object_id }|
            iv_relative_node_key = |{ <ls_group_entry>-parent_id }|
            iv_drag_drop_id      = lv_dnd_handle
            if_folder            = lf_is_folder
            if_expander          = lf_is_folder
            iv_image             = lv_icon
            iv_expanded_image    = lv_icon
            " add reference of favorite to node
            ir_user_object       = NEW zcl_dbbr_favmenu_entry( CONV #( <ls_group_entry> ) )
            it_item_table        = lt_items
        ).

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD move_favorite_down.
    DATA: lv_relatkey     TYPE tm_nodekey,
          lv_relationship TYPE i.

*.. 1) get the selected node
    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    IF lo_selected_node IS INITIAL OR
       lo_selected_node->mv_node_key = c_top_node OR
       zcl_dbbr_tree_helper=>is_variant_node( lo_selected_node ).
      RETURN.
    ENDIF.

    IF NOT favorite_exists_in_node( lo_selected_node ).
      build_tree( mf_global_fav_mode ).
      MESSAGE TEXT-001 TYPE 'S'.
      RETURN.
    ENDIF.


    DATA(lo_next_sibling) = lo_selected_node->get_next_sibling( ).
    IF lo_next_sibling IS INITIAL.
      RETURN.
    ENDIF.

    lv_relationship = cl_list_tree_model=>relat_next_sibling.
    lv_relatkey = lo_next_sibling->mv_node_key.

    lo_selected_node->move_node_to(
        iv_relative_node    = lv_relatkey
        iv_relationship     = lv_relationship
    ).

*.. update sort order of sub nodes of parent
    update_sortorder_of_children( lo_selected_node->get_parent( )->mv_node_key ).

    mo_tree_model->get_selections( )->select_nodes( VALUE #( ( lo_selected_node->mv_node_key ) ) ).
  ENDMETHOD.


  METHOD move_favorite_up.
*.. 1) get the selected node
    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    IF lo_selected_node IS INITIAL OR
       lo_selected_node->mv_node_key = c_top_node OR
       zcl_dbbr_tree_helper=>is_variant_node( lo_selected_node ).
      RETURN.
    ENDIF.

    IF NOT favorite_exists_in_node( lo_selected_node ).
      build_tree( mf_global_fav_mode ).
      MESSAGE TEXT-001 TYPE 'S'.
      RETURN.
    ENDIF.

    DATA(lo_previous_sibling) = lo_selected_node->get_previous_sibling( ).
    IF lo_previous_sibling IS INITIAL.
      RETURN.
    ENDIF.

    lo_selected_node->move_node_to(
      iv_relationship  = cl_tree_model=>relat_prev_sibling
      iv_relative_node = lo_previous_sibling->mv_node_key
    ).

*.. update sort order of sub nodes of parent
    update_sortorder_of_children( lo_selected_node->get_parent( )->mv_node_key ).

    mo_tree_model->get_selections( )->select_nodes( VALUE #( ( lo_selected_node->mv_node_key ) ) ).
  ENDMETHOD.


  METHOD on_drop_complete.
    IF er_drag_drop_object->state = cl_dragdropobject=>state_aborted.
      build_tree( mf_global_fav_mode ).
    ENDIF.
  ENDMETHOD.


  METHOD on_entity_deleted.
    DATA: lt_nodes TYPE treemnotab.

    LOOP AT mt_node_map ASSIGNING FIELD-SYMBOL(<ls_node_map>) WHERE entity_id   = ev_entity_id
                                                                AND entity_type = ev_entity_type.
      lt_nodes = VALUE #( BASE lt_nodes ( <ls_node_map>-node_key ) ).
    ENDLOOP.

    CHECK lt_nodes IS NOT INITIAL.

    mo_tree_model->get_nodes( )->delete_nodes( lt_nodes ).
  ENDMETHOD.


  METHOD on_expand_no_children.
    DATA(lo_node) = mo_tree_model->get_nodes( )->get_node( ev_node_key ).
    DATA(lo_user_object) = lo_node->get_user_object( ).
    DATA(lo_favmenu_object) = CAST zcl_dbbr_favmenu_entry( lo_user_object ).

    DATA(ls_favmenu_data) = lo_favmenu_object->get_favmenu_data( ).

    DATA(lt_variants) = mo_variant_f->find_variant_infos_for_type(
        iv_entity_id   = ls_favmenu_data-fav_entry
        iv_entity_type = ls_favmenu_data-favtype
    ).

    LOOP AT lt_variants ASSIGNING FIELD-SYMBOL(<ls_variant>).
      <ls_variant>-entity_type = ls_favmenu_data-favtype.

      IF <ls_variant>-description IS NOT INITIAL.
        DATA(lt_items_for_description) = VALUE treemcitab(
           ( item_name = c_hierarchy_node2
             style     = zif_uitb_c_ctm_style=>inverted_gray
             font      = cl_item_tree_model=>item_font_prop
             class     = cl_item_tree_model=>item_class_text
             text      = <ls_variant>-description )
         ).
      ENDIF.

      mo_tree_model->get_nodes( )->add_node(
          iv_node_key                = mv_last_var_node_key
          iv_relative_node_key       = ev_node_key
          iv_relationship            = cl_list_tree_model=>relat_last_child
          iv_image                   = CONV #( icon_alv_variants )
          ir_user_object             = NEW zcl_dbbr_variant( <ls_variant> )
          it_item_table              = VALUE #(
            ( item_name  = mo_tree_model->c_hierarchy_column
              class      = cl_item_tree_model=>item_class_text
              font       = cl_item_tree_model=>item_font_prop
              text       = <ls_variant>-variant_name )
            ( LINES OF lt_items_for_description )

          )
      ).

      CLEAR lt_items_for_description.

      ADD 1 TO mv_last_var_node_key.
    ENDLOOP.

    mo_tree_model->get_nodes( )->expand_node( ev_node_key ).
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    CHECK NOT zcl_dbbr_tree_helper=>is_variant_node( lo_selected_node ).

    DATA(lo_parent_node) = lo_selected_node->get_parent( ).
    DATA(lv_parent_node) = COND #( WHEN lo_parent_node IS BOUND THEN lo_parent_node->mv_node_key ).

    DATA(lt_expanded_nodes) = mo_tree_model->get_nodes( )->get_expanded_nodes( ).

    IF lo_selected_node->is_folder( ).
      IF line_exists( lt_expanded_nodes[ table_line = ev_node_key ] ).
        er_menu->add_function(
            fcode = c_context_codes-collapse_node
            text  = |{ 'Collapse folder'(002) }|
        ).
      ELSE.
        er_menu->add_function(
            fcode = c_context_codes-expand_node
            text  = |{ 'Expand folder'(003) }|
        ).
      ENDIF.

      er_menu->add_separator( ).
    ENDIF.

    er_menu->add_function(
        fcode = c_context_codes-insert_current
        text  = |{ 'Insert Current Entity'(008) }|
    ).
    er_menu->add_separator( ).
    er_menu->add_function(
        fcode = c_context_codes-insert_folder
        text  = |{ 'Insert Folder'(009) }|
    ).
    er_menu->add_function(
        fcode = c_context_codes-insert_favorite
        text  = |{ 'Insert Favorite'(010) }|
    ).
    IF ev_node_key <> c_top_node.
      er_menu->add_function(
          fcode = c_context_codes-edit_favorite
          text  = |{ 'Change Favorite'(011) }|
      ).
      er_menu->add_function(
          fcode = c_context_codes-delete_favorite
          text  = |{ 'Delete Favorite'(012) }|
      ).
    ENDIF.

    er_menu->add_separator( ).

*... create new action for importing db tables from package
    er_menu->add_function(
      fcode = c_context_codes-import_from_clipboard
      text  = |{ 'Import from Clipboard'(013) }|
    ).

    IF NOT lo_selected_node->is_folder( ).
      er_menu->add_separator( ).

*... Shows the given favorite in the Object browser
      er_menu->add_function(
        fcode = c_context_codes-show_in_object_browser
        text  = |{ 'Show in Object Browser'(014) }|
      ).

*... create new function for running the selected table/script and build_tree the results
      er_menu->add_function(
        fcode = c_context_codes-start_with_default_variant
        text  = |{ 'Execute with Default Settings'(007) }|
      ).

    ENDIF.

  ENDMETHOD.


  METHOD on_node_context_menu_select.
    handle_gui_function(
        iv_code     = ev_fcode
        iv_node_key = ev_node_key
    ).
  ENDMETHOD.


  METHOD on_node_double_click.
    DATA: lf_is_favorite TYPE abap_bool.

*.. Get the selected node from the tree
    DATA(lo_selected_node) = mo_tree_model->get_selections( )->get_selected_node( ).

    DATA(lo_user_object) = lo_selected_node->get_user_object( ).
    IF lo_selected_node->is_folder( ).
*.... Check whether the node is truly a folder or just a favorite with variants
      IF lo_user_object IS BOUND.
        DATA(lv_fav_type) = CAST zcl_dbbr_favmenu_entry( lo_user_object )->get_favmenu_data( )-favtype.
        IF lv_fav_type <> zif_dbbr_c_favmenu_type=>folder.
          lf_is_favorite = abap_true.
        ENDIF.
      ENDIF.
    ELSE.
      lf_is_favorite = abap_true.
    ENDIF.

    IF lf_is_favorite = abap_true.
      IF zcl_dbbr_tree_helper=>is_variant_node( lo_selected_node ).
        DATA(lo_variant_fav_entry) = CAST zcl_dbbr_variant( lo_user_object ).
        DATA(ls_variant_info) = lo_variant_fav_entry->get_variant_info( ).
        zcl_dbbr_selscr_nav_events=>raise_variant_entry_chosen(
            iv_entity_id    = ls_variant_info-entity_id
            iv_entity_type  = ls_variant_info-entity_type
            iv_variant_id   = ls_variant_info-variant_id
        ).
      ELSE.
        DATA(lo_fav_entry) = CAST zcl_dbbr_favmenu_entry( lo_user_object ).
        zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
            iv_entity_id   = lo_fav_entry->get_favmenu_data( )-fav_entry
            iv_entity_type = lo_fav_entry->get_favmenu_data( )-favtype
        ).
      ENDIF.
    ELSE.
*.... check if folder is expanded/collapsed
      IF lo_selected_node->is_expanded( ).
        collapse_node( ).
      ELSE.
        expand_node( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD on_node_enter_key.
    IF ev_key = cl_list_tree_model=>key_enter.
      on_node_double_click( ev_node_key = ev_node_key ).
    ELSEIF ev_key = cl_list_tree_model=>key_delete.
      on_node_context_menu_select(
          ev_fcode    = c_context_codes-delete_favorite
          ev_node_key = ev_node_key
      ).
    ELSEIF ev_key = cl_list_tree_model=>key_insert.
      import_from_clipboard( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_toolbar_button.
    handle_gui_function( ev_fcode ).
  ENDMETHOD.


  METHOD on_tree_drag.

*.. first check if the node to be dragged still exists
    IF NOT favorite_exists_in_node( mo_tree_model->get_nodes( )->get_node( ev_node_key ) ).
      er_drag_drop_object->abort( ).
      RETURN.
    ENDIF.

*.. create drag and drop object
    er_drag_drop_object->object = NEW zcl_dbbr_tree_dnd_object(
        VALUE #( ( ev_node_key ) )
    ).

  ENDMETHOD.


  METHOD on_tree_drag_multiple.
*.. create drag and drop object
    er_drag_drop_object->object = NEW zcl_dbbr_tree_dnd_object(
        et_node_key_table
    ).
  ENDMETHOD.


  METHOD on_tree_drop.
    DATA: lv_relationship TYPE i,
          lv_menulevel    TYPE zdbbr_favmenu-menu_level.

    DATA(lo_nodes) = mo_tree_model->get_nodes( ).
    DATA(lo_target_node) = lo_nodes->get_node( ev_node_key ).

*.. does the target still exist
    IF NOT favorite_exists_in_node( lo_target_node ).
      er_drag_drop_object->abort( ).
      RETURN.
    ENDIF.

    DATA(ls_favmenu_target) = get_node_favmenu_data( lo_target_node ).

    IF lo_target_node->is_folder( ).
      lv_relationship = cl_tree_model=>relat_first_child.
      lv_menulevel = ls_favmenu_target-menu_level + 1.
    ELSE.
      lv_relationship = cl_tree_model=>relat_next_sibling.
      lv_menulevel = ls_favmenu_target-menu_level.
*.... also read the parent of the target node
      DATA(lo_target_parent_node) = lo_nodes->get_node( lo_target_node->get_parent( )->mv_node_key ).
      DATA(ls_favmenu_target_parent) = get_node_favmenu_data( lo_target_parent_node ).
    ENDIF.

*.. get drag and drop object
    TRY.
        DATA(lo_dnd_object) = CAST zcl_dbbr_tree_dnd_object( er_drag_drop_object->object ).

        DATA(lv_source_node) = lo_dnd_object->get_next_node( ).
        DATA(lo_source_node) = lo_nodes->get_node( lv_source_node ).

        lo_source_node->move_node_to(
            iv_relative_node    = lo_target_node->mv_node_key
            iv_relationship     = lv_relationship
        ).
      CATCH cx_sy_move_cast_error.
        er_drag_drop_object->abort( ).
        RETURN.
    ENDTRY.

*.. drop was successful
    DATA(ls_favmenu_source) = get_node_favmenu_data( lo_source_node ).

*...-> check which attributes of the favmenu entry need to be adjusted
*.. 1) has the parent changed of the source
    IF NOT lo_target_node->is_folder( ) AND
       lo_source_node->get_parent( )->mv_node_key = lo_target_node->mv_node_key.
*.... only sort order must be changed
      update_sortorder_of_children( lo_target_node->mv_node_key ).

    ELSE.
*.... source gets new parent -> delete current favmenu entry and create new one
      mo_favmenu_f->delete_favorite( ls_favmenu_source ).
      ls_favmenu_source-parent_id = COND #( WHEN lo_target_node->is_folder( ) THEN
                                              ls_favmenu_target-object_id
                                            ELSE
                                              ls_favmenu_target_parent-object_id ).
      ls_favmenu_source-menu_level = lv_menulevel.
      mo_favmenu_f->update_favorite( ls_favmenu_source ).
      lo_source_node->set_user_object( NEW zcl_dbbr_favmenu_entry( ls_favmenu_source ) ).

*.... update menu levels of node
      update_menulevel_of_children( lv_source_node ).

*.... check which node's children need an updated sort order
      IF lo_target_node->is_folder( ).
        update_sortorder_of_children( lo_target_node->mv_node_key ).
      ELSE.
        update_sortorder_of_children( lo_target_parent_node->mv_node_key ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD on_tree_drop_complete_multiple.

  ENDMETHOD.


  METHOD set_node_favmenu_data.
    io_node->set_user_object( NEW zcl_dbbr_favmenu_entry( is_favmenu ) ).
  ENDMETHOD.


  METHOD should_display_as_folder.
    CLEAR result.

    IF iv_fav_type = zif_dbbr_c_favmenu_type=>folder OR
       if_has_variants = abap_true.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD show.
    create_tree( ).
    build_tree( if_global ).
  ENDMETHOD.


  METHOD show_in_object_list.
    DATA: lv_fav_id TYPE string.

    DATA(lo_node) = mo_tree_model->get_nodes( )->get_node( iv_node_key ).

    DATA(lo_user_object) = lo_node->get_user_object( ).
    CHECK lo_user_object IS NOT INITIAL.
    DATA(lo_fav_entry) = CAST zcl_dbbr_favmenu_entry( lo_user_object ).

    DATA(ls_favmenu_data) = lo_fav_entry->get_favmenu_data( ).

    IF mo_parent_view IS BOUND.
      mo_parent_view->execute_command( NEW zcl_uitb_gui_simple_command(
        iv_function = zcl_dbbr_object_navigator=>c_command_id-show_object_list
        ir_params   = NEW zdbbr_entity( entity_id     = ls_favmenu_data-fav_entry
                                        entity_type   = ls_favmenu_data-favtype   ) )
      ).
    ENDIF.
**    zcl_dbbr_selscr_nav_events=>raise_display_object_list(
**        iv_entity_id    = ls_favmenu_data-fav_entry
**        iv_entity_type  = ls_favmenu_data-favtype
**    ).
  ENDMETHOD.


  METHOD start_with_default_variant.
    DATA: lv_fav_id TYPE string.

    DATA(lo_selected_node) = get_selected_favorite( ).
    CHECK lo_selected_node IS NOT INITIAL.

    DATA(lo_user_object) = lo_selected_node->get_user_object( ).
    CHECK lo_user_object IS NOT INITIAL.
    DATA(lo_fav_entry) = CAST zcl_dbbr_favmenu_entry( lo_user_object ).

    DATA(ls_favmenu_data) = lo_fav_entry->get_favmenu_data( ).
    DATA(lf_query) = xsdbool( ls_favmenu_data-favtype = zif_dbbr_c_favmenu_type=>query ).

    lv_fav_id = ls_favmenu_data-fav_entry.

    zcl_dbbr_selscr_nav_events=>raise_variant_entry_chosen(
        iv_entity_id    = CONV #( lv_fav_id )
        iv_entity_type  = ls_favmenu_data-favtype
        iv_variant_id   = zif_dbbr_global_consts=>c_dummy_variant
        if_go_to_result = abap_true
    ).
  ENDMETHOD.


  METHOD update_menulevel_of_children.
    DATA(lo_node) = mo_tree_model->get_nodes( )->get_node( iv_node_key ).

    CHECK lo_node->is_folder( ).

*.. starting menulevel from node
    DATA(ls_favmenu_data) = get_node_favmenu_data( lo_node ).
    DATA(lv_menulevel) = ls_favmenu_data-menu_level + 1.

    LOOP AT lo_node->get_children( ) ASSIGNING FIELD-SYMBOL(<lo_child>).
      ls_favmenu_data = get_node_favmenu_data( <lo_child> ).
      ls_favmenu_data-menu_level = lv_menulevel.
      mo_favmenu_f->update_favorite( ls_favmenu_data ).
      <lo_child>->set_user_object( NEW zcl_dbbr_favmenu_entry( ls_favmenu_data ) ).

*.... also update child nodes
      IF <lo_child>->is_folder( ).
        update_menulevel_of_children( <lo_child>->mv_node_key ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD update_sortorder_of_children.

    DATA(lo_node) = mo_tree_model->get_nodes( )->get_node( iv_node_key  = iv_node_key ).

    DATA(lv_sortorder) = 10.
    DATA(lo_favmenu_f) = NEW zcl_dbbr_favmenu_factory( ).

    LOOP AT lo_node->get_children( ) ASSIGNING FIELD-SYMBOL(<lo_child>).
      DATA(ls_favmenu_data) = get_node_favmenu_data( <lo_child> ).
      ls_favmenu_data-sort_order = lv_sortorder.

      lo_favmenu_f->update_favorite( ls_favmenu_data ).
      <lo_child>->set_user_object( NEW zcl_dbbr_favmenu_entry( ls_favmenu_data ) ).

      ADD 10 TO lv_sortorder.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.
    DATA(ls_find_result) = mo_tree_model->get_search( )->find( ).

    IF ls_find_result IS NOT INITIAL.
      mo_tree_model->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    DATA(ls_find_result) = mo_tree_model->get_search( )->find_next( ).

    IF ls_find_result IS NOT INITIAL.
      mo_tree_model->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    mo_tree_model->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    rf_has_focus = mo_tree_model->zif_uitb_gui_control~has_focus( ).
  ENDMETHOD.

ENDCLASS.
