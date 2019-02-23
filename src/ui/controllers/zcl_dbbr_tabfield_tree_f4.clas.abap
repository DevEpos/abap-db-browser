CLASS zcl_dbbr_tabfield_tree_f4 DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_uitb_gui_modal_dialog
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: c_hier_col2 TYPE tv_itmname VALUE 'HIER2' ##NO_TEXT.
    CONSTANTS: c_hier_col3 TYPE tv_itmname VALUE 'HIER3' ##NO_TEXT.
    CONSTANTS: c_hier_col4 TYPE tv_itmname VALUE 'HIER4' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !iv_screen_title     TYPE string
        !io_tree_node_filler TYPE REF TO zif_dbbr_tree_node_filler .
    METHODS display_value_help
      EXPORTING
        !ev_chosen_field           TYPE fieldname
        !ev_chosen_table           TYPE tabname
        ev_chosen_table_alias      TYPE zdbbr_entity_alias
        ev_chosen_field_with_alias TYPE zdbbr_fieldname_with_alias .
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_functions,
        find         TYPE ui_func VALUE zif_uitb_c_gui_screen=>c_functions-search,
        find_more    TYPE ui_func VALUE zif_uitb_c_gui_screen=>c_functions-search_more,
        expand_all   TYPE ui_func VALUE 'EXPANDALL',
        collapse_all TYPE ui_func VALUE 'COLLAPSEALL',
      END OF c_functions.

    DATA mv_chosen_field TYPE fieldname .
    DATA mv_chosen_field_with_alias TYPE zdbbr_fieldname_with_alias .
    DATA mv_chosen_table TYPE tabname .
    DATA mo_tree_node_filler TYPE REF TO zif_dbbr_tree_node_filler .
    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model .
    DATA mv_top_node TYPE tm_nodekey .
    DATA mt_node_map TYPE zif_dbbr_tree_node_filler=>tt_node_map .
    DATA mv_chosen_table_alias TYPE zdbbr_entity_alias.

    METHODS create_tree
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    METHODS handle_selected_node
      IMPORTING
        !iv_node_key TYPE tm_nodekey .
    METHODS on_node_double_click
          FOR EVENT node_double_click OF zcl_uitb_ctm_events
      IMPORTING
          !ev_node_key .
    METHODS on_node_key_press
          FOR EVENT node_keypress OF zcl_uitb_ctm_events
      IMPORTING
          !ev_node_key
          !ev_key .

    METHODS set_selected_values
      IMPORTING
        !is_node_map TYPE zif_dbbr_tree_node_filler=>ty_node_map .
ENDCLASS.



CLASS zcl_dbbr_tabfield_tree_f4 IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_screen_title ).
    mo_tree_node_filler = io_tree_node_filler.
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    DATA: lf_search_function TYPE abap_bool.

    CASE io_command->mv_function.

      WHEN c_functions-find.
        DATA(ls_result) = mo_tree->get_search( )->find( ).
        lf_search_function = abap_true.

      WHEN c_functions-find_more.
        ls_result = mo_tree->get_search( )->find_next( ).
        lf_search_function = abap_true.

      WHEN c_functions-expand_all.
        mo_tree->get_nodes( )->expand_root_nodes( ).

      WHEN c_functions-collapse_all.
        mo_tree->get_nodes( )->collapse_all_nodes( ).

    ENDCASE.

    IF lf_search_function = abap_true.
      IF ls_result IS NOT INITIAL.
        mo_tree->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
      ELSE.
        MESSAGE |No node was found| TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD do_before_dynpro_output.

    io_callback->deactivate_function( zif_uitb_c_gui_screen=>c_functions-save ).
  ENDMETHOD.

  METHOD create_content.
    create_control_toolbar(
      EXPORTING
        io_parent    = io_container
        it_button    = VALUE #(
          ( function  = c_functions-expand_all
            quickinfo = 'Expand All'
            icon      = icon_expand_all )
          ( function  = c_functions-collapse_all
            quickinfo = 'Collapse All'
            icon      = icon_collapse_all )
          ( butn_type = cntb_btype_sep )
          ( function  = zif_uitb_c_gui_screen=>c_functions-search
            icon      = icon_search
            quickinfo = 'Find' )
          ( function  = zif_uitb_c_gui_screen=>c_functions-search_more
            icon      = icon_search_next
            quickinfo = 'Find next' )
        )
      IMPORTING
        eo_toolbar   = DATA(lo_toolbar)
        eo_client    = DATA(lo_container)
    ).
    create_tree( lo_container ).

*... Fill the tree through the node filler instance
    mt_node_map = mo_tree_node_filler->fill_node_item_tables( mo_tree->get_nodes( ) ).

    mo_tree->get_nodes( )->collapse_all_nodes( ).
    ASSIGN mt_node_map[ node_type = zif_dbbr_tree_node_filler=>c_node_type-matched_fields ] TO FIELD-SYMBOL(<ls_matched_fields_node>).
    IF sy-subrc = 0.
      mo_tree->get_nodes( )->expand_node( <ls_matched_fields_node>-node_key ).
    ELSE.
      mo_tree->get_nodes( )->expand_root_nodes( ).
    ENDIF.
    mo_tree->get_nodes( )->set_first_root_node_as_top( ).
    mo_tree->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD create_tree.
    mo_tree = NEW zcl_uitb_column_tree_model(
        ir_parent           = io_container
        is_hierarchy_header = VALUE treemhhdr(
           heading = 'Table / Field'
*           width   = 60
        )
    ).
    mo_tree->get_columns( )->add_hierarchy_column( c_hier_col2 ).
    mo_tree->get_columns( )->add_hierarchy_column( c_hier_col3 ).
    mo_tree->get_columns( )->add_hierarchy_column( c_hier_col4 ).
*    mo_tree->get_columns( )->add_column(
*        iv_colname          = 'DESC'
*        iv_width            = 60
*        iv_header_text      = 'Description'
*    ).

    DATA(lr_events) = mo_tree->get_events( ).
    SET HANDLER:
      on_node_double_click FOR lr_events,
      on_node_key_press FOR lr_events.

*.. Create the control
    mo_tree->create_tree_control( ).
  ENDMETHOD.


  METHOD display_value_help.
    CLEAR: mo_tree,
           mt_node_map,
           mv_chosen_field_with_alias,
           mv_chosen_field,
           mv_chosen_table,
           mv_chosen_table_alias.

    show(
        iv_top    = 2
        iv_left   = 10
        iv_width  = 90
        iv_height = 28
    ).

    " transfer chosen values
    ev_chosen_field = mv_chosen_field.
    ev_chosen_field_with_alias = mv_chosen_field_with_alias.
    ev_chosen_table = mv_chosen_table.
    ev_chosen_table_alias = mv_chosen_table_alias.
  ENDMETHOD.


  METHOD handle_selected_node.
    CHECK iv_node_key IS NOT INITIAL.
    ASSIGN mt_node_map[ node_key = iv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).

    IF sy-subrc = 0.
      set_selected_values( <ls_node_map> ).
*.... leave the screen
      leave_screen( ).
    ELSE.
*.... Check if the selected node was a folder
      DATA(lr_selected_node) = mo_tree->get_nodes( )->get_node( iv_node_key ).
      IF lr_selected_node->is_folder( ).
        lr_selected_node->toggle( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD on_node_double_click.
    handle_selected_node( ev_node_key ).
  ENDMETHOD.


  METHOD on_node_key_press.
    handle_selected_node( ev_node_key ).
  ENDMETHOD.


  METHOD set_selected_values.
    mv_chosen_field_with_alias =  is_node_map-alias_fieldname.
    mv_chosen_field = is_node_map-fieldname.
    mv_chosen_table = is_node_map-tabname.
    mv_chosen_table_alias = is_node_map-alias.
  ENDMETHOD.

ENDCLASS.
