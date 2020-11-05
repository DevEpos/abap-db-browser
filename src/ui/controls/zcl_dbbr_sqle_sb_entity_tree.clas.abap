"! <p class="shorttext synchronized" lang="en">Entity Tree in SQL Editor Sidebar</p>
CLASS zcl_dbbr_sqle_sb_entity_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_view.
    INTERFACES zif_uitb_gui_control.
    INTERFACES zif_uitb_content_searcher.

    METHODS constructor
      IMPORTING
        io_container TYPE REF TO cl_gui_container
        io_parent    TYPE REF TO zif_uitb_gui_composite_view.
    "! <p class="shorttext synchronized" lang="en">Loads entities into tree</p>
    "!
    METHODS load_entities
      IMPORTING
        it_entities TYPE zdbbr_tabname_range_itab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_function,
        insert_entity TYPE ui_func VALUE 'INSERT_ENTITY',
        clear_nodes   TYPE ui_func VALUE 'CLEAR_NODES',
        expand_all    TYPE ui_func VALUE 'EXPAND_ALL',
        collapse_all  TYPE ui_func VALUE 'COLLAPSE_ALL',
        find          TYPE ui_func VALUE 'FIND',
        find_more     TYPE ui_func VALUE 'FIND_MORE',
        filter        TYPE ui_func VALUE 'FILTER',
      END OF c_function,

      BEGIN OF c_node_type,
        cds_view     TYPE i VALUE 1,
        association  TYPE i VALUE 2,
        associations TYPE i VALUE 3,
        field        TYPE i VALUE 4,
        fields       TYPE i VALUE 5,
        parameter    TYPE i VALUE 6,
        parameters   TYPE i VALUE 7,
        table        TYPE i VALUE 8,
        view         TYPE i VALUE 9,
      END OF c_node_type,

      c_hierarchy_node2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT,
      c_hierarchy_node3 TYPE tv_itmname VALUE 'HIER3' ##NO_TEXT,
      c_text_class      TYPE i VALUE cl_item_tree_model=>item_class_text.

    TYPES:
      BEGIN OF ty_s_user_data,
        node_key            TYPE tm_nodekey,
        root_key            TYPE tm_nodekey,
        entity_id           TYPE zsat_entity_id,
        entity_type         TYPE zsat_entity_type,
        assoc_path          TYPE string,
        fieldname           TYPE string,
        from_part           TYPE string,
        param_default_value TYPE string,
        description         TYPE ddtext,
        node_type           TYPE i,
      END OF ty_s_user_data,
      ty_t_user_data_ref TYPE TABLE OF REF TO ty_s_user_data WITH EMPTY KEY,
      ty_t_node_type     TYPE TABLE OF i.

    DATA mo_container TYPE REF TO cl_gui_container.
    DATA mo_parent TYPE REF TO zif_uitb_gui_composite_view.
    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model.
    DATA mt_node_user_data TYPE SORTED TABLE OF ty_s_user_data WITH UNIQUE KEY node_key.
    DATA mt_entities TYPE HASHED TABLE OF tabname WITH UNIQUE KEY table_line.
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar.
    DATA mo_replace_dnd_behavior TYPE REF TO cl_dragdrop.
    DATA mo_insert_dnd_behavior TYPE REF TO cl_dragdrop.

    "! <p class="shorttext synchronized" lang="en">Creates the tree model and control</p>
    "!
    METHODS create_tree.

    "! <p class="shorttext synchronized" lang="en">Create tree node</p>
    METHODS create_node
      IMPORTING
        iv_parent_node     TYPE tm_nodekey OPTIONAL
        if_folder          TYPE abap_bool OPTIONAL
        if_expander        TYPE abap_bool OPTIONAL
        iv_hier1_item_text TYPE tm_itemtxt
        iv_handle          TYPE i OPTIONAL
        iv_image           TYPE tv_image OPTIONAL
        iv_expanded_image  TYPE tv_image OPTIONAL
        is_hier2_item      TYPE treemcitem OPTIONAL
        is_hier3_item      TYPE treemcitem OPTIONAL
        is_user_data       TYPE ty_s_user_data OPTIONAL
      RETURNING
        VALUE(ro_node)     TYPE REF TO zcl_uitb_ctm_node.
    METHODS add_entities_to_tree
      IMPORTING
        it_entities TYPE zcl_dbbr_db_entity_sel_dialog=>ty_t_db_entity.
    METHODS get_node_type_for_entity
      IMPORTING
        iv_entity_type      TYPE zsat_entity_type
      RETURNING
        VALUE(rv_node_type) TYPE i.
    METHODS get_node_image
      IMPORTING
        !iv_node_type   TYPE i OPTIONAL
        iv_entity_type  TYPE zsat_entity_type OPTIONAL
      RETURNING
        VALUE(rv_image) TYPE tv_image .
    METHODS load_entity
      IMPORTING
        io_node        TYPE REF TO zcl_uitb_ctm_node
        if_expand_node TYPE abap_bool OPTIONAL
        if_association TYPE abap_bool OPTIONAL
        ir_user_data   TYPE REF TO ty_s_user_data.
    METHODS fill_view_table_node
      IMPORTING
        io_node        TYPE REF TO zcl_uitb_ctm_node
        it_fields      TYPE zsat_dfies_itab
        if_expand_node TYPE abap_bool OPTIONAL
        if_association TYPE abap_bool OPTIONAL
        ir_user_data   TYPE REF TO ty_s_user_data.
    METHODS fill_cds_node
      IMPORTING
        io_node        TYPE REF TO zcl_uitb_ctm_node
        io_cds_view    TYPE REF TO zcl_sat_cds_view
        if_expand_node TYPE abap_bool OPTIONAL
        if_association TYPE abap_bool OPTIONAL
        ir_user_data   TYPE REF TO ty_s_user_data.
    METHODS collect_node_infos
      IMPORTING
        it_node_key              TYPE treemnotab
        if_allow_different_roots TYPE abap_bool
      EXPORTING
        et_user_data             TYPE ty_t_user_data_ref
        et_node_type             TYPE ty_t_node_type
        ef_different_roots       TYPE abap_bool.

    METHODS paste_select_clause
      IMPORTING
        io_node      TYPE REF TO zcl_uitb_ctm_node
        is_user_data TYPE zcl_dbbr_sqle_sb_entity_tree=>ty_s_user_data.
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
    "! <p class="shorttext synchronized" lang="en">Handler for node key press</p>
    "!
    METHODS on_node_key_press
        FOR EVENT node_keypress OF zif_uitb_tree_model_events
      IMPORTING
        ev_key
        ev_node_key.
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
        FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        fcode.

    METHODS on_tree_drag_multiple
        FOR EVENT drag_multiple OF zcl_uitb_ctm_events
      IMPORTING
        er_drag_drop_object
        et_node_key_table
        ev_item_name.
ENDCLASS.


CLASS zcl_dbbr_sqle_sb_entity_tree IMPLEMENTATION.

  METHOD constructor.
    mo_container = io_container.
    mo_parent = io_parent.
    create_tree( ).
  ENDMETHOD.

  METHOD load_entities.
    DATA: lt_db_entities TYPE zcl_dbbr_db_entity_sel_dialog=>ty_t_db_entity.

    CHECK it_entities IS NOT INITIAL.
    DATA(lt_entities) = it_entities.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      CHECK line_exists( mt_entities[ table_line = <ls_entity>-low ] ).
      DELETE lt_entities.
    ENDLOOP.

    CHECK lt_entities IS NOT INITIAL.

    SELECT
      FROM zsat_i_databaseentity
      FIELDS entityraw AS entity_id,
             description,
             type
      WHERE entity IN @lt_entities
    INTO CORRESPONDING FIELDS OF TABLE @lt_db_entities.

    IF sy-subrc = 0.
      add_entities_to_tree( lt_db_entities ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    CHECK mo_tree IS BOUND.
    mo_tree->zif_uitb_gui_control~focus( ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    CHECK mo_tree IS BOUND.
    rf_has_focus = mo_tree->has_focus( ).
  ENDMETHOD.

  METHOD zif_uitb_content_searcher~search.
    DATA(ls_find_result) = mo_tree->get_search( )->find( ).

    IF ls_find_result IS NOT INITIAL.
      mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
      mo_tree->zif_uitb_gui_control~focus( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    DATA(ls_find_result) = mo_tree->get_search( )->find_next( ).

    IF ls_find_result IS NOT INITIAL.
      mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_find_result-node_key ) ) ).
      mo_tree->zif_uitb_gui_control~focus( ).
    ENDIF.
  ENDMETHOD.

  METHOD create_tree.
    zcl_uitb_gui_helper=>create_control_toolbar(
      EXPORTING
        io_parent       = mo_container
        it_button       = VALUE #(
          ( function  = c_function-collapse_all
            icon      = zif_dbbr_c_icon=>collapse
            quickinfo = 'Collapse all nodes' )
          ( butn_type = cntb_btype_sep  )
          ( function  = c_function-insert_entity
            icon      = icon_insert_relation
            text      = |{ 'Entity' }|
            quickinfo = |{ 'Insert Database Entity' }| )
          ( function  = c_function-clear_nodes
            icon      = icon_delete
            quickinfo = |{ 'Remove all entities' }| )
          ( butn_type = cntb_btype_sep  )
          ( function  = c_function-find
            icon      = icon_search
            quickinfo = |{ 'Find' }| )
          ( function  = c_function-find_more
            icon      = icon_search_next
            quickinfo = |{ 'Find next' }| )
        )
      IMPORTING
        eo_toolbar      = mo_toolbar
        eo_client       = DATA(lo_client)
    ).
    mo_tree = NEW #(
      ir_parent           = lo_client
      is_hierarchy_header = VALUE #(
        heading = 'Object Name'
        width   = 60
      )
      if_auto_node_key    = abap_true
      iv_selection_mode   = zcl_uitb_column_tree_model=>c_multiple_selection
    ).

    mo_tree->get_columns( )->add_hierarchy_column( iv_colname = c_hierarchy_node2 ).
    mo_tree->get_columns( )->add_hierarchy_column( iv_colname = c_hierarchy_node3 ).

    mo_tree->create_tree_control( ).

    DATA(lo_events) = mo_tree->get_events( ).

    lo_events->add_key_for_keypress( cl_tree_model=>key_copy ).

*.. set event handler methods to tree control
    SET HANDLER:
      on_node_context_menu_request FOR lo_events,
      on_node_context_menu_select FOR lo_events,
      on_node_double_click FOR lo_events,
      on_node_key_press FOR lo_events,
*      on_node_enter_key FOR lo_events,
      on_expand_no_children FOR lo_events,
      on_tree_drag_multiple FOR lo_events,
      on_toolbar_button FOR mo_toolbar.

    mo_replace_dnd_behavior = NEW cl_dragdrop( ).
    mo_replace_dnd_behavior->add(
        flavor          = zcl_uitb_gui_code_editor=>c_dnd_flavor-replace
        dragsrc         = abap_true
        droptarget      = abap_false
        effect          = cl_dragdrop=>copy
    ).
    mo_insert_dnd_behavior = NEW cl_dragdrop( ).
    mo_insert_dnd_behavior->add(
        flavor          = zcl_uitb_gui_code_editor=>c_dnd_flavor-insert
        dragsrc         = abap_true
        droptarget      = abap_false
        effect          = cl_dragdrop=>copy
    ).
  ENDMETHOD.

  METHOD on_expand_no_children.

    DATA(lo_node) = mo_tree->get_nodes( )->get_node( ev_node_key ).
    CHECK lo_node IS BOUND.

    ASSIGN mt_node_user_data[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_user_data>).
    CHECK <ls_user_data> IS ASSIGNED.

    CASE <ls_user_data>-node_type.

      WHEN c_node_type-cds_view OR
           c_node_type-table OR
           c_node_type-view.

        load_entity(
          io_node        = lo_node
          ir_user_data   = REF #( <ls_user_data> )
        ).
        mo_tree->get_nodes( )->expand_node( iv_node_key = lo_node->mv_node_key ).

      WHEN c_node_type-association.
        load_entity(
           io_node        = lo_node
           if_association = abap_true
           ir_user_data   = REF #( <ls_user_data> )
        ).
        mo_tree->get_nodes( )->expand_node( iv_node_key = lo_node->mv_node_key ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_node_context_menu_request.

  ENDMETHOD.

  METHOD on_node_context_menu_select.
    LOOP AT mt_entities INTO DATA(lv_entity).
    ENDLOOP.
  ENDMETHOD.

  METHOD on_node_key_press.
    DATA: lv_rc                TYPE i,
          lt_clipboard         TYPE TABLE OF char256,
          lo_node              TYPE REF TO zcl_uitb_ctm_node,
          lv_clipboard_content TYPE string,
          lt_nodes             TYPE zcl_uitb_ctm_node=>ty_t_ctm_node.

    CASE ev_key.

      WHEN cl_tree_model=>key_copy.
        IF ev_node_key IS NOT INITIAL.
          lo_node = mo_tree->get_nodes( )->get_node( ev_node_key ).
          CHECK lo_node IS BOUND.
          lt_nodes = VALUE #( ( lo_node ) ).
        ELSE.
          lt_nodes = mo_tree->get_selections( )->get_selected_nodes( ).
        ENDIF.

        DATA(lv_node_count) = lines( lt_nodes ).
        LOOP AT lt_nodes INTO lo_node.
          DATA(lv_sep) = COND #( WHEN sy-tabix < lv_node_count THEN ',' ELSE '' ).

          ASSIGN mt_node_user_data[ node_key = lo_node->mv_node_key ] TO FIELD-SYMBOL(<ls_user_data>).
          CHECK sy-subrc = 0.

          CASE <ls_user_data>-node_type.

            WHEN c_node_type-parameter.
              lv_clipboard_content = |{ <ls_user_data>-fieldname } = '{ <ls_user_data>-param_default_value }'{ lv_sep }|.

            WHEN c_node_type-field.
              lv_clipboard_content = |{ <ls_user_data>-fieldname }{ lv_sep }|.

            WHEN OTHERS.
              lv_clipboard_content = |{ lo_node->get_item( iv_item_name = zcl_uitb_column_tree_model=>c_hierarchy_column )->get_text( ) }{ lv_sep }|.
          ENDCASE.
          lt_clipboard = VALUE #( BASE lt_clipboard ( CONV #( lv_clipboard_content ) ) ).
        ENDLOOP.

        cl_gui_frontend_services=>clipboard_export(
          IMPORTING data = lt_clipboard
          CHANGING  rc   = lv_rc
        ).
        IF sy-subrc <> 0.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD on_node_double_click.
    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(lo_node) = lo_nodes->get_node( ev_node_key ).
    CHECK lo_node IS BOUND.

    ASSIGN mt_node_user_data[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_user_data>).
    CHECK sy-subrc = 0.

    CASE <ls_user_data>-node_type.

      WHEN c_node_type-cds_view OR
           c_node_type-table OR
           c_node_type-view.

        IF NOT lo_node->has_children( ).
          load_entity(
             io_node        = lo_node
             ir_user_data   = REF #( <ls_user_data> )
          ).
        ENDIF.

        paste_select_clause(
          io_node      = lo_node
          is_user_data = <ls_user_data>
        ).

      WHEN c_node_type-associations OR
           c_node_type-association OR
           c_node_type-parameters OR
           c_node_type-fields.

        IF lo_node->is_expanded( ).
          lo_nodes->collapse_node( lo_node->mv_node_key ).
        ELSE.
          lo_nodes->expand_node( lo_node->mv_node_key ).
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD on_node_enter_key.

  ENDMETHOD.

  METHOD on_toolbar_button.
    CASE fcode.

      WHEN c_function-insert_entity.
        add_entities_to_tree( zcl_dbbr_db_entity_sel_dialog=>get_db_entities( if_multi_select = abap_true ) ).

      WHEN c_function-clear_nodes.
        mo_tree->get_nodes( )->delete_all_nodes( ).
        mo_tree->update_view( ).
        CLEAR: mt_entities,
               mt_node_user_data.

      WHEN c_function-collapse_all.
        mo_tree->get_nodes( )->collapse_all_nodes( ).

      WHEN c_function-find.
        mo_tree->zif_uitb_content_searcher~search( ).
      WHEN c_function-find_more.
        mo_tree->zif_uitb_content_searcher~search_next( ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_tree_drag_multiple.
    DATA: lt_content TYPE TABLE OF string,
          lv_content TYPE string,
          lv_sep     TYPE string.

    collect_node_infos(
      EXPORTING it_node_key              = et_node_key_table
                if_allow_different_roots = abap_false
      IMPORTING et_user_data             = DATA(lt_nodes_info)
                ef_different_roots       = DATA(lf_different_roots)
                et_node_type             = DATA(lt_node_type)
    ).

    IF lf_different_roots = abap_true.
      er_drag_drop_object->abort( ).
      MESSAGE |Drag-n-Drop from different Roots is not allowed| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_node_count) = lines( lt_nodes_info ).

    LOOP AT lt_nodes_info ASSIGNING FIELD-SYMBOL(<lr_node>).
      lv_sep = COND #( WHEN sy-tabix < lv_node_count THEN ',' ).
      lv_content = <lr_node>->fieldname.
      IF <lr_node>->node_type = c_node_type-parameter.
        lv_content = |{ lv_content } = '{ <lr_node>->param_default_value }'|.

      ENDIF.
      lt_content = VALUE #( BASE lt_content ( |{ lv_content }{ lv_sep }| ) ).
    ENDLOOP.

    IF lt_content IS INITIAL.
      er_drag_drop_object->abort( ).
    ELSE.
      CONCATENATE LINES OF lt_content INTO lv_content SEPARATED BY cl_abap_char_utilities=>cr_lf.
      DATA(lo_dnd_object) = NEW zcl_uitb_gui_editor_dnd_object(
        iv_text          = lv_content
        if_adjust_spaces = abap_true
        if_is_long_text  = abap_true
      ).
      er_drag_drop_object->object = lo_dnd_object.
    ENDIF.

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
            iv_drag_drop_id      = iv_handle
        ).
        IF is_user_data IS NOT INITIAL.
          DATA(ls_user_data) = is_user_data.
          ls_user_data-node_key = ro_node->mv_node_key.
          INSERT ls_user_data INTO TABLE mt_node_user_data.
        ENDIF.
      CATCH zcx_uitb_tree_error INTO DATA(lx_error).
        lx_error->zif_uitb_exception_message~print(
            iv_msg_type = 'X'
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD add_entities_to_tree.
    DATA: lv_added_count TYPE i.

    CHECK it_entities IS NOT INITIAL.

    mo_replace_dnd_behavior->get_handle( IMPORTING handle = DATA(lv_handle) ).

    LOOP AT it_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      DATA(lv_entity) = CONV tabname( to_upper( <ls_entity>-entity_id ) ).
      CHECK NOT line_exists( mt_entities[ table_line = lv_entity ] ).

      mt_entities = VALUE #( BASE mt_entities ( lv_entity ) ).

      create_node(
          if_expander        = abap_true
          if_folder          = abap_true
          iv_hier1_item_text = |{ <ls_entity>-entity_id }|
          iv_image           = get_node_image( iv_entity_type = <ls_entity>-type )
          is_hier2_item      = VALUE #( style = zif_uitb_c_ctm_style=>inverted_gray text = <ls_entity>-description )
          is_user_data       = VALUE #(
              entity_id   = <ls_entity>-entity_id
              description = <ls_entity>-description
              node_type   = get_node_type_for_entity( <ls_entity>-type )
          )
          iv_handle          = lv_handle
      ).
      ADD 1 TO lv_added_count.
    ENDLOOP.

    MESSAGE |Loaded { lv_added_count } entities| TYPE 'S'.
  ENDMETHOD.

  METHOD get_node_image.
    IF iv_entity_type IS NOT INITIAL.
      rv_image = SWITCH #( iv_entity_type
        WHEN zif_sat_c_entity_type=>table    THEN zif_dbbr_c_icon=>database_table
        WHEN zif_sat_c_entity_type=>view     THEN zif_dbbr_c_icon=>database_view
        WHEN zif_sat_c_entity_type=>cds_view THEN zif_dbbr_c_icon=>cds_view
      ).
    ELSEIF iv_node_type IS NOT INITIAL.
      rv_image = SWITCH #( iv_node_type
        WHEN c_node_type-table    THEN zif_dbbr_c_icon=>database_table
        WHEN c_node_type-view     THEN zif_dbbr_c_icon=>database_view
        WHEN c_node_type-cds_view THEN zif_dbbr_c_icon=>cds_view
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_node_type_for_entity.
    rv_node_type = SWITCH #( iv_entity_type
      WHEN zif_sat_c_entity_type=>table    THEN c_node_type-table
      WHEN zif_sat_c_entity_type=>view     THEN c_node_type-view
      WHEN zif_sat_c_entity_type=>cds_view THEN c_node_type-cds_view
    ).
  ENDMETHOD.

  METHOD load_entity.
    DATA: lt_fields          TYPE dfies_table,
          lv_assoc_node_type TYPE i.

    CASE ir_user_data->node_type.

      WHEN c_node_type-table OR
           c_node_type-view.

        zcl_sat_ddic_repo_access=>get_table_field_infos(
          EXPORTING iv_tablename    = ir_user_data->entity_id
          IMPORTING et_table_fields = lt_fields
        ).
        fill_view_table_node(
          io_node        = io_node
          it_fields      = lt_fields
          if_expand_node = if_expand_node
          if_association = if_association
          ir_user_data   = ir_user_data
        ).

      WHEN c_node_type-cds_view.
        TRY.
            DATA(lo_cds_view) = zcl_sat_cds_view_factory=>read_cds_view( iv_cds_view = to_upper( ir_user_data->entity_id ) ).
          CATCH zcx_sat_data_read_error INTO DATA(lx_data_read).
            lx_data_read->zif_sat_exception_message~print( ).
*.......... Delete the entity if for some reason it no longer exists
            mo_tree->get_nodes( )->delete_node( io_node->mv_node_key ).
            RETURN.
        ENDTRY.
        fill_cds_node(
          io_node        = io_node
          io_cds_view    = lo_cds_view
          if_expand_node = if_expand_node
          if_association = if_association
          ir_user_data   = ir_user_data
        ).

      WHEN c_node_type-association.
        CASE ir_user_data->entity_type.
          WHEN zif_sat_c_entity_type=>table.
            lv_assoc_node_type = c_node_type-table.

          WHEN zif_sat_c_entity_type=>view.
            lv_assoc_node_type = c_node_type-view.

          WHEN zif_sat_c_entity_type=>cds_view.
            lv_assoc_node_type = c_node_type-cds_view.

        ENDCASE.
        CHECK lv_assoc_node_type IS NOT INITIAL.
        DATA(ls_user_data) = ir_user_data->*.
        ls_user_data-node_type = lv_assoc_node_type.

        load_entity(
           io_node        = io_node
           if_expand_node = if_expand_node
           if_association = if_association
           ir_user_data   = REF #( ls_user_data )
        ).
    ENDCASE.
  ENDMETHOD.

  METHOD fill_cds_node.
    DATA: lv_spaces TYPE string VALUE `       `.

    mo_insert_dnd_behavior->get_handle( IMPORTING handle = DATA(lv_handle) ).

    ir_user_data->from_part = |{ ir_user_data->entity_id }|.

    DATA(lv_root_key) = COND #( WHEN if_association = abap_true THEN ir_user_data->root_key ELSE io_node->mv_node_key ).
    DATA(lv_field_prefix) = COND #( WHEN ir_user_data->assoc_path IS NOT INITIAL THEN ir_user_data->assoc_path && '-' ).

    IF if_association = abap_false AND io_cds_view->has_parameters( ).
      DATA(lo_params_node) = create_node(
        iv_parent_node     = io_node->mv_node_key
        if_folder          = abap_true
        if_expander        = abap_true
        iv_hier1_item_text = 'Parameters'
        iv_handle          = lv_handle
        is_user_data       = VALUE #(
           root_key    = lv_root_key
           entity_id   = io_cds_view->get_header( )-entityname
           node_type   = c_node_type-parameters
        )
      ).
      DATA(lt_params) = io_cds_view->get_parameters( if_exclude_system_params = abap_true ).
      IF lt_params IS NOT INITIAL.
        ir_user_data->from_part = ir_user_data->from_part && |( |.
        DO strlen( ir_user_data->from_part ) TIMES.
          lv_spaces = | { lv_spaces }|.
        ENDDO.
      ENDIF.

      LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<ls_param>).
        DATA(lv_sep) = COND #( WHEN sy-tabix < lines( lt_params ) THEN `,` && cl_abap_char_utilities=>newline && lv_spaces ).
        ir_user_data->from_part = ir_user_data->from_part &&
                                 |{ <ls_param>-parametername_raw } = '{ <ls_param>-default_value }'| && lv_sep.
        create_node(
          iv_parent_node     = lo_params_node->mv_node_key
          iv_image           = |{ icon_parameter_import }|
          iv_hier1_item_text = |{ <ls_param>-parametername_raw }|
          iv_handle          = lv_handle
          is_user_data       = VALUE #(
             root_key            = lv_root_key
             entity_id           = io_cds_view->get_header( )-entityname
             fieldname           = <ls_param>-parametername_raw
             node_type           = c_node_type-parameter
             param_default_value = <ls_param>-default_value
          )
        ).
      ENDLOOP.
      IF sy-subrc = 0.
        ir_user_data->from_part = ir_user_data->from_part && | )|.
      ENDIF.
    ENDIF.

    DATA(lo_fields_node) = create_node(
      iv_parent_node     = io_node->mv_node_key
      if_folder          = abap_true
      if_expander        = abap_true
      iv_hier1_item_text = 'Fields'
      iv_handle          = lv_handle
      is_user_data       = VALUE #(
         root_key    = lv_root_key
         entity_id   = io_cds_view->get_header( )-entityname
         node_type   = c_node_type-fields
      )
    ).

    zcl_sat_ddic_repo_access=>get_table_field_infos(
      EXPORTING iv_tablename    = io_cds_view->get_header( )-entityname
      IMPORTING et_table_fields = DATA(lt_fields)
    ).
    LOOP AT io_cds_view->get_columns( ) ASSIGNING FIELD-SYMBOL(<ls_column>).
      ASSIGN lt_fields[ fieldname = <ls_column>-fieldname ] TO FIELD-SYMBOL(<ls_field>).
      CHECK sy-subrc = 0.
      create_node(
        iv_parent_node     = lo_fields_node->mv_node_key
        iv_hier1_item_text = |{ <ls_column>-fieldname_raw }|
        is_hier2_item      = VALUE #(
          text  = <ls_field>-fieldtext
          style = zif_uitb_c_ctm_style=>inverted_gray
        )
        iv_image           = COND #( WHEN <ls_column>-keyflag = abap_true THEN |{ icon_foreign_key }| )
        iv_handle          = lv_handle
        is_user_data       = VALUE #(
           root_key    = lv_root_key
           entity_id   = io_cds_view->get_header( )-entityname
           fieldname   = lv_field_prefix && <ls_column>-fieldname_raw
           description = <ls_field>-fieldtext
           node_type   = c_node_type-field
        )
      ).
    ENDLOOP.

    IF io_cds_view->has_associations( ).
      DATA(lo_assocs_node) = create_node(
          iv_parent_node     = io_node->mv_node_key
          if_folder          = abap_true
          if_expander        = abap_true
          iv_hier1_item_text = 'Associations'
          is_user_data       = VALUE #(
             root_key    = lv_root_key
             entity_id   = io_cds_view->get_header( )-entityname
             node_type   = c_node_type-associations
          )
        ).
      LOOP AT io_cds_view->get_associations( ) ASSIGNING FIELD-SYMBOL(<ls_assoc>).
        create_node(
          iv_parent_node     = lo_assocs_node->mv_node_key
          if_folder          = abap_true
          if_expander        = abap_true
          iv_hier1_item_text = |{ <ls_assoc>-raw_name }|
          is_hier2_item      = VALUE #(
            text  = <ls_assoc>-ddtext
            style = zif_uitb_c_ctm_style=>inverted_gray
          )
          iv_image           = get_node_image( iv_entity_type = <ls_assoc>-entity_type )
          is_user_data       = VALUE #(
             root_key    = lv_root_key
             entity_id   = <ls_assoc>-ref_cds_view
             assoc_path  = ir_user_data->assoc_path && '\' && <ls_assoc>-raw_name
             entity_type = <ls_assoc>-entity_type
             description = <ls_assoc>-ddtext
             node_type   = c_node_type-association
          )
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD fill_view_table_node.
    mo_insert_dnd_behavior->get_handle( IMPORTING handle = DATA(lv_handle) ).

    ir_user_data->from_part = |{ ir_user_data->entity_id }|.
    DATA(lv_root_key) = COND #( WHEN if_association = abap_true THEN ir_user_data->root_key ELSE io_node->mv_node_key ).
    DATA(lv_field_prefix) = COND #( WHEN ir_user_data->assoc_path IS NOT INITIAL THEN ir_user_data->assoc_path && '-' ).

    DATA(lo_fields_node) = create_node(
      iv_parent_node     = io_node->mv_node_key
      if_folder          = abap_true
      if_expander        = abap_true
      iv_handle          = lv_handle
      iv_hier1_item_text = 'Fields'
      is_user_data       = VALUE #(
         root_key    = lv_root_key
         entity_id   = ir_user_data->entity_id
         node_type   = c_node_type-fields
      )
    ).

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      create_node(
        iv_parent_node     = lo_fields_node->mv_node_key
        iv_hier1_item_text = |{ <ls_field>-fieldname }|
        is_hier2_item      = VALUE #(
          text  = <ls_field>-fieldtext
          style = zif_uitb_c_ctm_style=>inverted_gray
        )
        iv_image           = COND #( WHEN <ls_field>-keyflag = abap_true THEN |{ icon_foreign_key }| )
        iv_handle          = lv_handle
        is_user_data       = VALUE #(
           root_key    = lv_root_key
           entity_id   = ir_user_data->entity_id
           fieldname   = lv_field_prefix && to_lower( <ls_field>-fieldname )
           description = <ls_field>-fieldtext
           node_type   = c_node_type-field
        )
      ).
    ENDLOOP.
  ENDMETHOD.



  METHOD collect_node_infos.
    TYPES:
      BEGIN OF lty_s_root,
        root_key TYPE tm_nodekey,
      END OF lty_s_root.

    DATA: lt_root           TYPE HASHED TABLE OF tm_nodekey WITH UNIQUE KEY table_line,
          lt_node_key_range TYPE RANGE OF tm_nodekey,
          lt_root_key       TYPE SORTED TABLE OF lty_s_root WITH NON-UNIQUE KEY root_key,
          lt_node_type      TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.

    DATA(lo_nodes) = mo_tree->get_nodes( ).

    lt_node_key_range = VALUE #( FOR <lv_key> IN it_node_key ( sign = 'I' option = 'EQ' low = <lv_key> ) ).

    LOOP AT it_node_key INTO DATA(lv_node_key).
      DATA(lr_user_data) = REF #( mt_node_user_data[ node_key = lv_node_key ] OPTIONAL ).
      CHECK lr_user_data IS BOUND.

      INSERT lr_user_data->root_key INTO TABLE lt_root.
      IF lines( lt_root ) > 1.
        ef_different_roots = abap_true.
        IF if_allow_different_roots = abap_false.
          CLEAR et_user_data.
          RETURN.
        ENDIF.
      ENDIF.


      IF lr_user_data->node_type = c_node_type-fields OR
         lr_user_data->node_type = c_node_type-parameters.

        IF lr_user_data->node_type = c_node_type-fields.
          INSERT c_node_type-field INTO TABLE lt_node_type.
        ELSEIF lr_user_data->node_type = c_node_type-parameters.
          INSERT c_node_type-parameter INTO TABLE lt_node_type.
        ENDIF.

        DATA(lo_node) = lo_nodes->get_node( lv_node_key ).
        LOOP AT lo_node->get_children( ) INTO DATA(lo_child).
          lr_user_data = REF #( mt_node_user_data[ node_key = lo_child->mv_node_key ] OPTIONAL ).
          CHECK lr_user_data IS BOUND.
          et_user_data = VALUE #( BASE et_user_data ( lr_user_data ) ).
        ENDLOOP.
        CONTINUE.
      ENDIF.
      INSERT lr_user_data->node_type INTO TABLE lt_node_type.

      et_user_data = VALUE #( BASE et_user_data ( lr_user_data ) ).
    ENDLOOP.

    et_node_type = lt_node_type.

  ENDMETHOD.

  METHOD paste_select_clause.
    DATA: lt_select TYPE string_table.

    DATA(lr_select) = NEW string( ).

    lt_select = VALUE #(
      ( |SELECT| )
      ( |  FROM { is_user_data-from_part }| )
      ( |  FIELDS *| )
    ).
    CONCATENATE LINES OF lt_select INTO lr_select->* SEPARATED BY cl_abap_char_utilities=>newline.
*    DATA(lt_children) = io_node->get_children( ).

    mo_parent->execute_command( NEW zcl_uitb_gui_simple_command(
      iv_function = zcl_uitb_gui_code_editor=>c_command_ids-replace_content
      ir_params   = lr_select )
    ).
  ENDMETHOD.

ENDCLASS.
