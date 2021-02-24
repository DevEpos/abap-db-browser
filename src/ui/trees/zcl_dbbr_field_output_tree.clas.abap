CLASS zcl_dbbr_field_output_tree DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_page_scroller .
    INTERFACES zif_uitb_content_searcher .

    TYPES mty_node_move_type TYPE int1 .

    CONSTANTS:
      BEGIN OF c_node_move_types,
        up           TYPE mty_node_move_type VALUE 1,
        down         TYPE mty_node_move_type VALUE 2,
        top          TYPE mty_node_move_type VALUE 3,
        bottom       TYPE mty_node_move_type VALUE 4,
        to_specified TYPE mty_node_move_type VALUE 5,
      END OF c_node_move_types .

    EVENTS tree_data_updated .

    METHODS constructor
      IMPORTING
        io_fields             TYPE REF TO zcl_dbbr_tabfield_list
        io_original_fields    TYPE REF TO zcl_dbbr_tabfield_list
        !if_field_aggregation TYPE boolean OPTIONAL
        !if_single_table_mode TYPE boolean OPTIONAL
        !iv_mode              TYPE zdbbr_field_chooser_mode
        !iv_entity_type       TYPE zsat_entity_type
        !iv_current_table     TYPE tabname OPTIONAL .
    METHODS fill_tree
      IMPORTING
        !if_update_data TYPE boolean OPTIONAL .
    METHODS reset_update_ability .
    METHODS create_tree .
    METHODS sort_fields_in_ddic_order .
    METHODS delete_selected_nodes .
    METHODS free .
    METHODS update_current_table
      IMPORTING
        !iv_tabname TYPE tabname .
    METHODS move_selected_nodes
      IMPORTING
        !iv_move_type      TYPE mty_node_move_type
        !it_selected_nodes TYPE treemnotab OPTIONAL
        !iv_target_node    TYPE tm_nodekey OPTIONAL .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF mty_node_data.
        INCLUDE TYPE treemsnod.
        TYPES: items TYPE treemcitab.
    TYPES: END OF mty_node_data .
    TYPES:
      mtt_node_data TYPE TABLE OF mty_node_data .

    CONSTANTS c_output_field_container TYPE dynfnam VALUE 'OUTPUT_CONTAINER' ##NO_TEXT.
    CONSTANTS c_top_node TYPE tm_nodekey VALUE 'TOP' ##NO_TEXT.
    CONSTANTS c_toolbar_container TYPE dynfnam VALUE 'OUTPUT_TOOLBAR' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_column_names,
        hierarchy_column   TYPE tv_itmname VALUE 'HIERARCHY',
        key_column         TYPE tv_itmname VALUE 'KEYCOL',
        description_column TYPE tv_itmname VALUE 'DESCRCOL',
        sorted_column      TYPE tv_itmname VALUE 'SORTCOL',
      END OF c_column_names .
    CONSTANTS:
      BEGIN OF c_fcode,
        cut_nodes          TYPE ui_func VALUE 'CUT_NODES' ##NO_TEXT,
        insert_nodes       TYPE ui_func VALUE 'INSERT_NODES' ##NO_TEXT,
        discard_nodes      TYPE ui_func VALUE 'DISCARD_NODES' ##NO_TEXT,
        del_nodes          TYPE ui_func VALUE 'DEL_NODES' ##NO_TEXT,
        insert_text_fields TYPE ui_func VALUE 'INS_TEXT_FIELDS' ##NO_TEXT,
        insert_form_fields TYPE ui_func VALUE 'INS_FORM_FIELDS' ##NO_TEXT,
      END OF c_fcode.
    DATA mo_tree_model TYPE REF TO cl_column_tree_model .
    DATA mo_container TYPE REF TO cl_gui_custom_container .
    DATA mo_toolbar_container TYPE REF TO cl_gui_custom_container .
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar .
    DATA mo_output_tree_container TYPE REF TO cl_gui_custom_container .
    DATA mo_tree_dnd_behaviour TYPE REF TO cl_dragdrop .
    DATA mo_fields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_original_fields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mf_update_off TYPE boolean .
    DATA mv_mode TYPE zdbbr_field_chooser_mode .
    DATA mv_field_count TYPE sy-tabix .
    DATA mf_field_aggregation TYPE boolean .
    DATA mf_single_table_mode TYPE boolean .
    DATA mv_current_table TYPE tabname .
    DATA mt_node_data_buffer TYPE mtt_node_data .
    DATA mv_entity_type TYPE zsat_entity_type .

    METHODS create_sortorder_from_checked .
    METHODS create_sort_order_from_visible .
    METHODS create_table_field_node
      IMPORTING
        VALUE(io_tabfield_info) TYPE REF TO zdbbr_tabfield_info_ui
        !iv_dnd_handle          TYPE i OPTIONAL .
    METHODS create_nodes_from_tabfields
      IMPORTING
        !iv_dnd_handle TYPE i .
    METHODS get_node_image
      IMPORTING
        !if_is_key       TYPE boolean OPTIONAL
        !if_is_formula   TYPE boolean OPTIONAL
        !if_is_textfield TYPE boolean OPTIONAL
      RETURNING
        VALUE(rv_image)  TYPE tv_image .
  PRIVATE SECTION.

    METHODS create_nodes .


    METHODS sort_fields_in_user_order .

    METHODS create_toolbar .
    METHODS move_nodes
      IMPORTING
        !it_selected_nodes    TYPE treemnotab OPTIONAL
        !iv_target_node       TYPE tm_nodekey OPTIONAL
        !iv_move_type         TYPE mty_node_move_type
      EXPORTING
        !ef_no_nodes_selected TYPE boolean .
    METHODS determine_previous_node
      IMPORTING
        !it_node_key       TYPE treemnotab
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey .
    METHODS determine_next_node
      IMPORTING
        !it_node_key       TYPE treemnotab
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey .
    METHODS cut_selected_nodes
      IMPORTING
        it_selected_nodes TYPE treemnotab.
    METHODS discard_selected_nodes.
    METHODS insert_nodes_from_buffer
      IMPORTING
        iv_insert_at_node TYPE tm_nodekey.
    METHODS create_missing_text_fld_nodes.
    METHODS create_missing_form_fld_nodes.
    METHODS on_tree_drag
        FOR EVENT drag OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !node_key .
    METHODS on_tree_drop
        FOR EVENT drop OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !node_key
        !sender .
    METHODS on_tree_drag_multiple
        FOR EVENT drag_multiple OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !item_name
        !node_key_table .
    METHODS on_toolbar_button_clicked
        FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        !fcode .
    METHODS on_node_context_menu_request
        FOR EVENT node_context_menu_request OF cl_column_tree_model
      IMPORTING
        !menu
        !node_key .
    METHODS on_node_context_menu_select
        FOR EVENT node_context_menu_select OF cl_column_tree_model
      IMPORTING
        !fcode
        !node_key .
ENDCLASS.



CLASS zcl_dbbr_field_output_tree IMPLEMENTATION.


  METHOD constructor.

    mo_fields = io_fields.
    mo_original_fields = io_original_fields.
    mo_fields->build_complete_fieldnames( ).
    mv_field_count = mo_fields->get_field_count( ).
    mf_field_aggregation = if_field_aggregation.
    mf_single_table_mode = if_single_table_mode.
    mv_mode = iv_mode.
    mv_current_table = iv_current_table.
    mv_entity_type = iv_entity_type.

  ENDMETHOD.


  METHOD create_missing_text_fld_nodes.
    DATA(lo_iterator) = mo_fields->zif_uitb_data_ref_list~get_iterator(
        iv_where = 'OUTPUT_ACTIVE = abap_false AND IS_TEXT_FIELD = abap_true'
    ).

    mo_tree_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle)  ).

    WHILE lo_iterator->has_next( ).
      DATA(lr_element) = CAST zdbbr_tabfield_info_ui( lo_iterator->get_next( ) ).

      lr_element->output_active = abap_true.

      create_table_field_node(
          io_tabfield_info = lr_element
          iv_dnd_handle    = lv_dnd_handle
      ).

    ENDWHILE.
  ENDMETHOD.

  METHOD create_missing_form_fld_nodes.
    DATA: lo_iterator TYPE REF TO zif_uitb_data_ref_iterator,
          lr_element  TYPE REF TO zdbbr_tabfield_info_ui.

    IF mf_field_aggregation = abap_true.
      lo_iterator = mo_original_fields->zif_uitb_data_ref_list~get_iterator(
          iv_where = 'IS_FORMULA_FIELD = abap_true'
      ).
    ELSE.
      lo_iterator = mo_fields->zif_uitb_data_ref_list~get_iterator(
          iv_where = 'OUTPUT_ACTIVE = abap_false AND IS_FORMULA_FIELD = abap_true'
      ).
    ENDIF.

    mo_tree_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle)  ).

    WHILE lo_iterator->has_next( ).
      lr_element = CAST zdbbr_tabfield_info_ui( lo_iterator->get_next( ) ).

      IF mf_field_aggregation = abap_true.
        " check if field already exists in aggregation list
        TRY.
            mo_fields->get_field_ref( iv_tabname_alias = lr_element->tabname_alias
                                      iv_fieldname     = lr_element->fieldname ).
            CONTINUE.
          CATCH cx_sy_itab_line_not_found.
            mo_fields->add( lr_element ).
        ENDTRY.
      ENDIF.

      lr_element->output_active = abap_true.

      create_table_field_node(
          io_tabfield_info = lr_element
          iv_dnd_handle    = lv_dnd_handle
      ).

    ENDWHILE.
  ENDMETHOD.


  METHOD create_nodes.
*... create drag-n-drop behaviour object
    mo_tree_dnd_behaviour = NEW #( ).

    mo_tree_dnd_behaviour->add(
        flavor          = 'Node'
        dragsrc         = abap_true
        droptarget      = abap_true
        effect          = cl_dragdrop=>move
    ).

*...get handle of drag n drop control
    mo_tree_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle)  ).

*... create top node for fields
    mo_tree_model->add_node(
      node_key                = c_top_node
      isfolder                = abap_true
      style                   = cl_list_tree_model=>style_emphasized_c
      item_table              = VALUE treemcitab(
        ( item_name  = c_column_names-hierarchy_column
          class      = cl_list_tree_model=>item_class_text
          font       = cl_list_tree_model=>item_font_prop
          text       = COND #( WHEN mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-output THEN
                                 TEXT-009
                               WHEN mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-selection THEN
                                 TEXT-010 )
        )
      )
    ).

    create_nodes_from_tabfields( lv_dnd_handle ).

    mo_tree_model->expand_root_nodes( ).

  ENDMETHOD.


  METHOD create_nodes_from_tabfields.
    mo_fields->initialize_iterator( if_for_active = abap_true ).

    WHILE mo_fields->has_more_lines( ).
      create_table_field_node( io_tabfield_info = mo_fields->get_next_entry( )
                               iv_dnd_handle    = iv_dnd_handle ).
    ENDWHILE.

  ENDMETHOD.


  METHOD create_sortorder_from_checked.

*&---------------------------------------------------------------------*
*& Description: Creates user output/selection order from current checked fields
*&---------------------------------------------------------------------*
    " first sort fields in ddic order
    mo_fields->sort_in_ddic_order( ).
    mo_fields->create_order_from_active( ).

  ENDMETHOD.


  METHOD create_sort_order_from_visible.

    mo_tree_model->node_get_children( EXPORTING node_key               = c_top_node
                                      IMPORTING node_key_table = DATA(lt_child_nodes) ).

    LOOP AT lt_child_nodes ASSIGNING FIELD-SYMBOL(<lv_node>).
      DATA(lv_index) = sy-tabix.

      mo_tree_model->node_get_user_object( EXPORTING node_key    = <lv_node>
                                           IMPORTING user_object = DATA(lr_user_object) ).

      CAST zcl_dbbr_tabfield( lr_user_object )->set_custom_order( CONV #( lv_index ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create_table_field_node.

    FIELD-SYMBOLS: <ls_tabfield_info> TYPE zdbbr_tabfield_info_ui.
    ASSIGN io_tabfield_info->* TO <ls_tabfield_info>.

    DATA(lv_prefix) = COND string( WHEN <ls_tabfield_info>-alias IS NOT INITIAL THEN
                                     <ls_tabfield_info>-alias && '~' ).

    DATA(lv_node_key) = COND tm_nodekey(
      WHEN <ls_tabfield_info>-is_text_field = abap_true THEN
        <ls_tabfield_info>-sql_fieldname
      ELSE
        lv_prefix && <ls_tabfield_info>-fieldname_raw
    ).

    DATA(lt_items) = VALUE treemcitab(
        ( item_name  = c_column_names-hierarchy_column
          class      = cl_list_tree_model=>item_class_text
          font       = cl_list_tree_model=>item_font_prop
          text       = lv_node_key
        )
        ( item_name  = c_column_names-key_column
          class      = cl_list_tree_model=>item_class_text
          font       = cl_list_tree_model=>item_font_prop
          text       = <ls_tabfield_info>-is_key
        )
    ).

    " add sorted column symbol
    IF <ls_tabfield_info>-sort_active = abap_true.
      lt_items = VALUE #(
        BASE lt_items
        ( item_name  = c_column_names-sorted_column
          class      = cl_list_tree_model=>item_class_text
          font       = cl_list_tree_model=>item_font_prop
          t_image    = SWITCH #(
             <ls_tabfield_info>-sort_direction
             WHEN zif_dbbr_c_global=>c_sort_direction-ascending  THEN icon_sort_up
             WHEN zif_dbbr_c_global=>c_sort_direction-descending THEN icon_sort_down
          )
        )
      ).
    ENDIF.

    lt_items = VALUE #(
       BASE lt_items
       ( item_name  = c_column_names-description_column
         class      = cl_list_tree_model=>item_class_text
         font       = cl_list_tree_model=>item_font_prop
         text       = <ls_tabfield_info>-field_ddtext
       )
    ).

    mo_tree_model->add_node(
      node_key                = lv_node_key
      relative_node_key       = c_top_node
      relationship            = cl_list_tree_model=>relat_last_child
      image                   = get_node_image( if_is_key       = <ls_tabfield_info>-is_key
                                                if_is_formula   = <ls_tabfield_info>-is_formula_field
                                                if_is_textfield = <ls_tabfield_info>-is_text_field )
      isfolder                = abap_false
      drag_drop_id            = iv_dnd_handle
      user_object             = NEW zcl_dbbr_tabfield( ir_table_field_info = io_tabfield_info
                                                        iv_mode             = mv_mode           )
      item_table              = lt_items
    ).

  ENDMETHOD.


  METHOD create_toolbar.

    DATA: lt_events TYPE cntl_simple_events.
    " create toolbar
    mo_toolbar_container = NEW cl_gui_custom_container( container_name = c_toolbar_container ).
    mo_toolbar = NEW cl_gui_toolbar( parent = mo_toolbar_container ).

    IF mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-output.
      mo_toolbar->add_button(
          fcode            = c_fcode-insert_text_fields    " fcode associated to a button
          icon             = icon_insert_row    " icon name defined like '@0a@'
          butn_type        = cntb_btype_button
          text             = 'Text Fields'
          quickinfo        = 'Add Missing Text Fields'
      ).

      mo_toolbar->add_button(
          fcode            = c_fcode-insert_form_fields    " fcode associated to a button
          icon             = icon_biw_formula    " icon name defined like '@0a@'
          butn_type        = cntb_btype_button
          text             = 'Formula Fields'
          quickinfo        = 'Add Missing Formula'
      ).
    ENDIF.

    lt_events = VALUE #(
        ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
    ).

    mo_toolbar->set_registered_events( lt_events ).

    SET HANDLER on_toolbar_button_clicked FOR mo_toolbar.

  ENDMETHOD.


  METHOD create_tree.

    DATA: lt_events TYPE cntl_simple_events.

    CHECK mo_container IS INITIAL.

    mo_container = NEW cl_gui_custom_container(
        container_name              = c_output_field_container
    ).
    mo_tree_model = NEW cl_column_tree_model(
        node_selection_mode  = cl_gui_list_tree=>node_sel_mode_multiple
        item_selection       = abap_false
        hierarchy_header     = VALUE treemhhdr(
            heading = 'Feld'
            width   = 60
        )
        hierarchy_column_name = c_column_names-hierarchy_column
    ).

    mo_tree_model->add_column(
        name           = c_column_names-key_column
        width          = 7
        alignment      = cl_column_tree_model=>align_center
        header_text    = 'Key Field'
        header_tooltip = 'Key Field'
    ).

    mo_tree_model->add_column(
        name           = c_column_names-sorted_column
        width          = 7
        alignment      = cl_column_tree_model=>align_center
        header_text    = 'Sorted'
        header_tooltip = 'Sort Column'
    ).

    mo_tree_model->add_column(
        name        = c_column_names-description_column
        width       = 50
        header_text = 'Description'
    ).

    " create tree control from model
    mo_tree_model->create_tree_control( parent = mo_container ).

    create_toolbar( ).

    lt_events = VALUE #(
       ( eventid = cl_column_tree_model=>eventid_node_context_menu_req appl_event = abap_true )
    ).

    mo_tree_model->set_registered_events( lt_events ).

    """ set event handler methods to tree control
    SET HANDLER:
      on_tree_drag FOR mo_tree_model,
      on_tree_drag_multiple FOR mo_tree_model,
      on_tree_drop FOR mo_tree_model,
      on_node_context_menu_request FOR mo_tree_model,
      on_node_context_menu_select FOR mo_tree_model.


  ENDMETHOD.


  METHOD cut_selected_nodes.
    " collect complete node data
    DATA: lv_relatkey        TYPE tm_nodekey,
          lv_relatship       TYPE i,
          lf_first_iteration TYPE boolean VALUE abap_true.

    LOOP AT it_selected_nodes ASSIGNING FIELD-SYMBOL(<lv_node>).

      mo_tree_model->node_get_items(
        EXPORTING node_key   = <lv_node>
        IMPORTING item_table = DATA(lt_items)
      ).

      mo_tree_model->node_get_properties(
        EXPORTING node_key   = <lv_node>
        IMPORTING properties = DATA(ls_node_properties)
      ).

      DATA(ls_tabfield_info) = CAST zcl_dbbr_tabfield( ls_node_properties-userobject )->get_tabfield_info( ).

      " cache node with item data
      APPEND VALUE mty_node_data(
          node_key   = <lv_node>
          relatkey   = lv_relatkey
          relatship  = lv_relatship
          n_image    = COND #( WHEN ls_tabfield_info-is_formula_field = abap_true THEN
                                 icon_biw_formula
                               WHEN ls_tabfield_info-is_key = abap_true THEN
                                 icon_foreign_key
                               WHEN ls_tabfield_info-is_text_field = abap_true THEN
                                 icon_text_ina
                               ELSE
                                 zif_dbbr_c_icon=>no_icon )
          style      = ls_node_properties-style
          dragdropid = ls_node_properties-dragdropid
          userobject = ls_node_properties-userobject
          items      = lt_items
      ) TO mt_node_data_buffer.

      " update relative node key and relationship for following nodes
      IF lf_first_iteration = abap_true.
        lv_relatkey = <lv_node>.
        lv_relatship = cl_column_tree_model=>relat_next_sibling.
        CLEAR lf_first_iteration.
      ENDIF.
    ENDLOOP.

    " delete all nodes
    mo_tree_model->delete_nodes( it_selected_nodes ).
  ENDMETHOD.


  METHOD delete_selected_nodes.

*&---------------------------------------------------------------------*
*& Description: Delete all the selected nodes
*&---------------------------------------------------------------------*

    mo_tree_model->get_selected_nodes( IMPORTING node_key_table = DATA(lt_selected_nodes) ).

    DELETE lt_selected_nodes WHERE table_line = c_top_node.
    IF lt_selected_nodes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lv_node>).
      " reset user object behind node
      mo_tree_model->node_get_user_object( EXPORTING node_key    = <lv_node>
                                           IMPORTING user_object = DATA(lr_user_object) ).

      DATA(lo_tabfield) = CAST zcl_dbbr_tabfield( lr_user_object ).
      IF mf_field_aggregation = abap_true.
        IF lo_tabfield->get_tabfield_info( )-is_text_field = abap_false.
          MESSAGE |{ 'Only Text Fields can be deleted if Aggregation is active' }| TYPE 'E'.
          RETURN.
        ENDIF.
      ENDIF.

      lo_tabfield->set_custom_active( abap_false ).

      " delete the node
      mo_tree_model->delete_node( node_key = <lv_node> ).
    ENDLOOP.

    RAISE EVENT tree_data_updated.


  ENDMETHOD.


  METHOD determine_next_node.

    " get last node in passed table
    DATA(lv_node) = it_node_key[ 1 ].

    mo_tree_model->node_get_next_sibling(
      EXPORTING
        node_key         = it_node_key[ lines( it_node_key ) ]
      IMPORTING
        sibling_node_key = rv_node_key
    ).


  ENDMETHOD.


  METHOD determine_previous_node.

    " get first node in passed table
    DATA(lv_node) = it_node_key[ 1 ].

    mo_tree_model->node_get_prev_sibling(
      EXPORTING
        node_key         = it_node_key[ 1 ]
      IMPORTING
        sibling_node_key = rv_node_key
    ).


  ENDMETHOD.


  METHOD discard_selected_nodes.
    LOOP AT mt_node_data_buffer ASSIGNING FIELD-SYMBOL(<ls_node_data>) WHERE userobject IS BOUND.
      CHECK <ls_node_data>-userobject IS BOUND.

      CAST zcl_dbbr_tabfield( <ls_node_data>-userobject )->set_custom_active( abap_false ).
    ENDLOOP.

    CLEAR mt_node_data_buffer.

    RAISE EVENT tree_data_updated.
  ENDMETHOD.


  METHOD fill_tree.

    CHECK mf_update_off = abap_false.

    create_tree( ).

*... default order will be the custom user order
*... check if output fields need to be sorted
    IF mo_fields->custom_order_exists( ).
      mo_fields->sort_in_custom_order( ).
    ELSE.
      mo_fields->sort_in_ddic_order( ).
    ENDIF.

*... delete old nodes
    mo_tree_model->delete_all_nodes( ).
*... create nodes for model
    create_nodes( ).
    mo_tree_model->update_view( ).

    mf_update_off = abap_true.

  ENDMETHOD.


  METHOD free.

    IF mo_container IS BOUND.
      mo_container->free( ).
    ENDIF.
    IF mo_toolbar_container IS BOUND.
      mo_toolbar_container->free( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_node_image.
    IF if_is_formula = abap_false AND
       if_is_key = abap_false AND
       if_is_textfield = abap_false.
      rv_image = zif_dbbr_c_icon=>no_icon.
      RETURN.
    ENDIF.

    IF if_is_key = abap_true.
      rv_image = icon_foreign_key.
    ELSEIF if_is_formula = abap_true.
      rv_image = icon_biw_formula.
    ELSEIF if_is_textfield = abap_true.
      rv_image = icon_text_ina.
    ENDIF.
  ENDMETHOD.


  METHOD insert_nodes_from_buffer.
    DATA: lv_relatship TYPE i.

    " first get the node info
    CHECK iv_insert_at_node IS NOT INITIAL.

    IF iv_insert_at_node = c_top_node.
      lv_relatship = cl_list_tree_model=>relat_first_child.
    ELSE.
      lv_relatship = cl_list_tree_model=>relat_next_sibling.
    ENDIF.

    " insert cached nodes at top node
    LOOP AT mt_node_data_buffer ASSIGNING FIELD-SYMBOL(<ls_node_data>).
      mo_tree_model->add_node(
        node_key          = <ls_node_data>-node_key
        relative_node_key = COND #( WHEN sy-tabix = 1 THEN iv_insert_at_node ELSE <ls_node_data>-relatkey )
        relationship      = COND #( WHEN sy-tabix = 1 THEN lv_relatship ELSE <ls_node_data>-relatship )
        isfolder          = abap_false
        style             = <ls_node_data>-style
        image             = <ls_node_data>-n_image
        drag_drop_id      = CONV #( <ls_node_data>-dragdropid )
        user_object       = <ls_node_data>-userobject
        item_table        = <ls_node_data>-items
      ).
    ENDLOOP.

    CLEAR mt_node_data_buffer.

    create_sort_order_from_visible( ).
    RAISE EVENT tree_data_updated.
  ENDMETHOD.


  METHOD move_nodes.

*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/11/27
*&---------------------------------------------------------------------*
*& Description: Moves the selected nodes according to mode
*&---------------------------------------------------------------------*

    DATA: lt_node_data TYPE mtt_node_data,
          lv_relatship TYPE i,
          lv_relatkey  TYPE tm_nodekey.

    " get selected nodes
    IF it_selected_nodes IS NOT INITIAL.
      DATA(lt_sel_nodes) = it_selected_nodes.
    ELSE.
      mo_tree_model->get_selected_nodes( IMPORTING node_key_table = lt_sel_nodes ).
    ENDIF.

    IF lt_sel_nodes IS INITIAL.
      ef_no_nodes_selected = abap_true.
      RETURN.
    ENDIF.

    CASE iv_move_type.
      WHEN c_node_move_types-top.
        lv_relatship = cl_column_tree_model=>relat_first_child.
        lv_relatkey = c_top_node.

      WHEN c_node_move_types-bottom.
        lv_relatship = cl_column_tree_model=>relat_last_child.
        lv_relatkey = c_top_node.

      WHEN c_node_move_types-up.
        lv_relatkey = determine_previous_node( lt_sel_nodes ).
        lv_relatship = cl_column_tree_model=>relat_prev_sibling.
        IF lv_relatkey IS INITIAL.
          " insert nodes at the top
          lv_relatkey = c_top_node.
          lv_relatship = cl_column_tree_model=>relat_first_child.
        ENDIF.

      WHEN c_node_move_types-down.
        lv_relatkey = determine_next_node( lt_sel_nodes ).
        lv_relatship = cl_column_tree_model=>relat_next_sibling.
        IF lv_relatkey IS INITIAL.
          " insert node(s) at the bottom
          lv_relatship = cl_column_tree_model=>relat_last_child.
          lv_relatkey = c_top_node.
        ENDIF.

      WHEN c_node_move_types-to_specified.
        IF iv_target_node IS INITIAL.
          ef_no_nodes_selected = abap_true.
          RETURN.
        ENDIF.
        lv_relatship = cl_column_tree_model=>relat_prev_sibling.
        lv_relatkey = iv_target_node.
    ENDCASE.

    " collect complete node data
    LOOP AT lt_sel_nodes ASSIGNING FIELD-SYMBOL(<lv_node>).
      mo_tree_model->node_get_items(
        EXPORTING node_key   = <lv_node>
        IMPORTING item_table = DATA(lt_items)
      ).

      mo_tree_model->node_get_properties(
        EXPORTING node_key   = <lv_node>
        IMPORTING properties = DATA(ls_node_properties)
      ).

      DATA(ls_tabfield_info) = CAST zcl_dbbr_tabfield( ls_node_properties-userobject )->get_tabfield_info( ).

      " cache node with item data
      APPEND VALUE mty_node_data(
          node_key   = <lv_node>
          relatkey   = lv_relatkey
          relatship  = lv_relatship
          n_image    = COND #( WHEN ls_tabfield_info-is_formula_field = abap_true THEN
                                 icon_biw_formula
                               WHEN ls_tabfield_info-is_key = abap_true THEN
                                 icon_foreign_key
                               WHEN ls_tabfield_info-is_text_field = abap_true THEN
                                 icon_text_ina
                               ELSE
                                 zif_dbbr_c_icon=>no_icon )
          style      = ls_node_properties-style
          dragdropid = ls_node_properties-dragdropid
          userobject = ls_node_properties-userobject
          items      = lt_items
      ) TO lt_node_data.

      " update relative node key and relationship for following nodes
      lv_relatkey = <lv_node>.
      lv_relatship = cl_column_tree_model=>relat_next_sibling.
    ENDLOOP.

    " delete all nodes
    mo_tree_model->delete_nodes( lt_sel_nodes ).

    " insert cached nodes at top node
    LOOP AT lt_node_data ASSIGNING FIELD-SYMBOL(<ls_node_data>).
      mo_tree_model->add_node(
        node_key          = <ls_node_data>-node_key
        relative_node_key = <ls_node_data>-relatkey
        relationship      = <ls_node_data>-relatship
        isfolder          = abap_false
        style             = <ls_node_data>-style
        image             = <ls_node_data>-n_image
        drag_drop_id      = CONV #( <ls_node_data>-dragdropid )
        user_object       = <ls_node_data>-userobject
        item_table        = <ls_node_data>-items
      ).
    ENDLOOP.

    " select moved nodes
    mo_tree_model->unselect_all( ).
    mo_tree_model->select_nodes( lt_sel_nodes ).

  ENDMETHOD.


  METHOD move_selected_nodes.

    move_nodes( EXPORTING iv_move_type         = iv_move_type
                          iv_target_node       = iv_target_node
                IMPORTING ef_no_nodes_selected = DATA(lf_no_nodes_selected) ).

    IF lf_no_nodes_selected = abap_false.
      create_sort_order_from_visible( ).
      RAISE EVENT tree_data_updated.
    ENDIF.

  ENDMETHOD.


  METHOD on_node_context_menu_request.

    IF mf_field_aggregation = abap_false.

      IF node_key <> c_top_node.
        menu->add_function(
            fcode = c_fcode-cut_nodes
            text  = 'Cut selected Fields'
        ).
      ENDIF.
      IF mt_node_data_buffer IS NOT INITIAL.
        menu->add_function(
            fcode = c_fcode-insert_nodes
            text  = 'Insert Fields from Buffer'
        ).
        menu->add_function(
            fcode = c_fcode-discard_nodes
            text  = 'Discard Cut Fields'
        ).
      ENDIF.

      IF node_key <> c_top_node.
        menu->add_separator( ).

        menu->add_function(
            fcode = c_fcode-del_nodes
            text  = 'Delete selected Fields'
        ).
      ENDIF.

    ELSE.
      mo_tree_model->get_selected_nodes( IMPORTING node_key_table = DATA(lt_selected) ).
      IF lines( lt_selected ) = 1 AND lt_selected[ 1 ] <> c_top_node.
        mo_tree_model->node_get_user_object(
          EXPORTING node_key    = lt_selected[ 1 ]
          IMPORTING user_object = DATA(lr_user_object)
        ).

        DATA(lr_tabfield_user_object) = CAST zcl_dbbr_tabfield( lr_user_object ).
        IF lr_tabfield_user_object->get_tabfield_info( )-is_text_field = abap_true.
          menu->add_function(
              fcode = c_fcode-del_nodes
              text  = 'Delete selected Fields'
          ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD on_node_context_menu_select.

    CASE fcode.
      WHEN c_fcode-cut_nodes.
        " get selected nodes
        mo_tree_model->get_selected_nodes( IMPORTING node_key_table = DATA(lt_selected_nodes) ).
        cut_selected_nodes( it_selected_nodes = lt_selected_nodes ).

      WHEN c_fcode-discard_nodes.
        discard_selected_nodes( ).

      WHEN c_fcode-insert_nodes.
        insert_nodes_from_buffer( iv_insert_at_node = node_key ).

      WHEN c_fcode-del_nodes.
        delete_selected_nodes( ).

      WHEN c_fcode-insert_text_fields.
        create_missing_text_fld_nodes( ).
    ENDCASE.

  ENDMETHOD.


  METHOD on_toolbar_button_clicked.

    CASE fcode.
      WHEN zcl_dbbr_tabfield_manager=>c_fcode-delete_selected.
        delete_selected_nodes( ).

      WHEN zcl_dbbr_tabfield_manager=>c_fcode-sort_in_ddic.
        sort_fields_in_ddic_order( ).

      WHEN c_fcode-insert_text_fields.
        create_missing_text_fld_nodes( ).

      WHEN c_fcode-insert_form_fields.
        create_missing_form_fld_nodes( ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_tree_drag.

    """ create drag and drop object
    drag_drop_object->object = NEW zcl_dbbr_tree_dnd_object(
        VALUE #( ( node_key ) )
    ).

  ENDMETHOD.


  METHOD on_tree_drag_multiple.

    drag_drop_object->object = NEW zcl_dbbr_tree_dnd_object(
        node_key_table
    ).

  ENDMETHOD.


  METHOD on_tree_drop.

    DATA(lv_current_node) = node_key.
    """ get drag and drop object
    TRY.
        DATA(lr_dnd_object) = CAST zcl_dbbr_tree_dnd_object( drag_drop_object->object ).
        DATA(lv_relationship) = cl_list_tree_model=>relat_prev_sibling.

        WHILE lr_dnd_object->has_more_keys( ).
          DATA(lv_next_node) = lr_dnd_object->get_next_node( ).

          sender->move_node(
            EXPORTING
              node_key                = lv_next_node
              relative_node_key       = lv_current_node
              relationship            = lv_relationship
            EXCEPTIONS
              control_not_existing    = 1
              control_dead            = 2
              failed                  = 3
              cntl_system_error       = 4
              node_not_found          = 5
              move_error              = 6
              relative_node_not_found = 7
              illegal_relationship    = 8
              OTHERS                  = 9
          ).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          lv_current_node = lv_next_node.
          lv_relationship = cl_list_tree_model=>relat_next_sibling.
        ENDWHILE.
      CATCH cx_sy_move_cast_error.
        drag_drop_object->abort( ).
        RETURN.
    ENDTRY.

    """ recreate sort order
    create_sort_order_from_visible( ).

  ENDMETHOD.


  METHOD reset_update_ability.

    CLEAR mf_update_off.

  ENDMETHOD.


  METHOD sort_fields_in_ddic_order.

    create_sortorder_from_checked( ).
    " delete old nodes
    mo_tree_model->delete_all_nodes( ).
    """ create nodes for model
    create_nodes( ).

  ENDMETHOD.


  METHOD sort_fields_in_user_order.

    mo_fields->sort_in_custom_order( ).

  ENDMETHOD.


  METHOD update_current_table.

    mv_current_table = iv_tabname.

  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.

    CHECK mo_tree_model IS NOT INITIAL.

    mo_tree_model->find(
        IMPORTING result_item_key_table = DATA(lt_result_item)
                  result_type           = DATA(lv_result_type)
    ).

    IF lv_result_type <> 0 AND lt_result_item IS NOT INITIAL.
      mo_tree_model->unselect_all( ).
      mo_tree_model->select_nodes( VALUE #( ( lt_result_item[ 1 ]-node_key ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.

    mo_tree_model->find_next(
        IMPORTING result_item_key_table    = DATA(lt_result_item)
                  result_expander_node_key = DATA(lt_result_expander_node_key)
                  result_type              = DATA(lv_result_type)
    ).

    IF lv_result_type <> 0 AND lt_result_item IS NOT INITIAL.
      mo_tree_model->unselect_all( ).
      mo_tree_model->select_nodes( VALUE #( ( lt_result_item[ 1 ]-node_key ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_bottom.

    mo_tree_model->scroll( cl_column_tree_model=>scroll_end ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_down.

    mo_tree_model->scroll( cl_column_tree_model=>scroll_down_page ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_top.

    mo_tree_model->scroll( cl_column_tree_model=>scroll_home ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_up.

    mo_tree_model->scroll( cl_column_tree_model=>scroll_up_page ).

  ENDMETHOD.

ENDCLASS.
