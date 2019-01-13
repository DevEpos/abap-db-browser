CLASS zcl_dbbr_tabfield_tree_f4 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view .

    METHODS constructor
      IMPORTING
        !iv_screen_title     TYPE string
        !ir_tree_node_filler TYPE REF TO zif_dbbr_tree_node_filler .
    METHODS display_value_help
      EXPORTING
        !ev_chosen_field           TYPE fieldname
        !ev_chosen_table           TYPE tabname
        ev_chosen_field_with_alias TYPE zdbbr_fieldname_with_alias .
  PROTECTED SECTION.
private section.

  data MR_TMPLT_PROG type ref to ZIF_UITB_TEMPLATE_PROG .
  data MV_CHOSEN_FIELD type FIELDNAME .
  data MV_CHOSEN_FIELD_WITH_ALIAS type ZDBBR_FIELDNAME_WITH_ALIAS .
  data MV_CHOSEN_TABLE type TABNAME .
  data MR_TREE_NODE_FILLER type ref to ZIF_DBBR_TREE_NODE_FILLER .
  data MR_F4_TREE type ref to ZCL_UITB_COLUMN_TREE_MODEL .
  data MV_TOP_NODE type TM_NODEKEY .
  data MT_NODE_MAP type ZIF_DBBR_TREE_NODE_FILLER=>TT_NODE_MAP .

  methods CREATE_TREE .
  methods DO_ON_FIRST_CALL .
  methods HANDLE_SELECTED_NODE
    importing
      !IV_NODE_KEY type TM_NODEKEY .
  methods ON_NODE_DOUBLE_CLICK
    for event NODE_DOUBLE_CLICK of ZCL_UITB_CTM_EVENTS
    importing
      !EV_NODE_KEY .
  methods ON_NODE_KEY_PRESS
    for event NODE_KEYPRESS of ZCL_UITB_CTM_EVENTS
    importing
      !EV_NODE_KEY
      !EV_KEY .
  methods ON_PAI
    for event USER_COMMAND of ZIF_UITB_VIEW_CALLBACK
    importing
      !EV_FUNCTION_ID
      !ER_CALLBACK .
  methods ON_PBO
    for event BEFORE_OUTPUT of ZIF_UITB_VIEW_CALLBACK
    importing
      !ER_CALLBACK .
  methods ON_TOOLBAR_FUNCTION
    for event FUNCTION_SELECTED of ZIF_UITB_TOOLBAR_EVENTS
    importing
      !EV_FCODE .
  methods SET_SELECTED_VALUES
    importing
      !IS_NODE_MAP type ZIF_DBBR_TREE_NODE_FILLER=>TY_NODE_MAP .
ENDCLASS.



CLASS ZCL_DBBR_TABFIELD_TREE_F4 IMPLEMENTATION.


  METHOD constructor.
    mr_tmplt_prog = zcl_uitb_templt_prog_callback=>create_template_program( |{ iv_screen_title }|  ).
    mr_tree_node_filler = ir_tree_node_filler.

    SET HANDLER:
      on_pai FOR mr_tmplt_prog,
      on_pbo FOR mr_tmplt_prog.
  ENDMETHOD.


  METHOD create_tree.
    mr_f4_tree = NEW zcl_uitb_column_tree_model(
        ir_parent           = mr_tmplt_prog->get_container( )
        is_hierarchy_header = VALUE treemhhdr(
           heading = 'Table / Field'
           width   = 60
        )
        if_with_toolbar     = abap_true
    ).
    mr_f4_tree->get_columns( )->add_column(
        iv_colname          = 'DESC'
        iv_width            = 60
        iv_header_text      = 'Description'
    ).

    DATA(lr_toolbar) = mr_f4_tree->get_toolbar( ).
    lr_toolbar->add_button(
        iv_fcode     = 'EXPANDALL'
        iv_icon      = icon_expand_all
        iv_quickinfo = 'Expand All nodes'
    ).
    lr_toolbar->add_button(
        iv_fcode     = 'COLLAPSEALL'
        iv_icon      = icon_collapse_all
        iv_quickinfo = 'Collapse All nodes'
    ).
    lr_toolbar->add_separator( ).
    lr_toolbar->add_button(
        iv_fcode     = zif_uitb_template_prog=>c_func_find
        iv_icon      = icon_search
        iv_quickinfo = 'Find'
    ).
    lr_toolbar->add_button(
        iv_fcode     = zif_uitb_template_prog=>c_func_find_more
        iv_icon      = icon_search_next
        iv_quickinfo = 'Find Next'
    ).

    DATA(lr_events) = mr_f4_tree->get_events( ).
    SET HANDLER:
      on_node_double_click FOR lr_events,
      on_node_key_press FOR lr_events,
      on_toolbar_function FOR lr_toolbar.

*.. Create the control
    mr_f4_tree->create_tree_control( ).
  ENDMETHOD.


  METHOD display_value_help.
    CLEAR: mr_f4_tree,
       mt_node_map,
       mv_chosen_field_with_alias,
       mv_chosen_field,
       mv_chosen_table.

    me->zif_uitb_view~show( ).

    " transfer chosen values
    ev_chosen_field = mv_chosen_field.
    ev_chosen_field_with_alias = mv_chosen_field_with_alias.
    ev_chosen_table = mv_chosen_table.
  ENDMETHOD.


  METHOD do_on_first_call.
    create_tree( ).
*... Fill the tree through the node filler instance
    mt_node_map = mr_tree_node_filler->fill_node_item_tables( mr_f4_tree->get_nodes( ) ).
*... Expand root nodes
    mr_f4_tree->get_nodes( )->expand_root_nodes( ).
    mr_f4_tree->get_nodes( )->set_first_root_node_as_top( ).
    mr_f4_tree->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD handle_selected_node.
    CHECK iv_node_key IS NOT INITIAL.
    ASSIGN mt_node_map[ node_key = iv_node_key ] TO FIELD-SYMBOL(<ls_node_map>).

    IF sy-subrc = 0.
      set_selected_values( <ls_node_map> ).
*.... leave the screen
      mr_tmplt_prog->leave_program( ).
    ELSE.
*.... Check if the selected node was a folder
      DATA(lr_selected_node) = mr_f4_tree->get_nodes( )->get_node( iv_node_key ).
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


  METHOD on_pai.

    CASE ev_function_id.
      WHEN zif_uitb_template_prog=>c_func_ok.
        TRY.
            DATA(lt_selections) = mr_f4_tree->get_selections( )->get_selected_nodes( ).
            DATA(lr_selected_node) = lt_selections[ 1 ].
            handle_selected_node( lr_selected_node->mv_node_key ).
          CATCH zcx_uitb_tree_error
                cx_sy_itab_line_not_found.
        ENDTRY.

      WHEN zif_uitb_template_prog=>c_func_cancel.
        CLEAR: mv_chosen_field, mv_chosen_table, mv_chosen_field_with_alias.

      WHEN OTHERS.
        on_toolbar_function( ev_function_id ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_pbo.
    IF er_callback->is_first_screen_call( ).
      do_on_first_call( ).
    ENDIF.

    er_callback->deactivate_function( zif_uitb_template_prog=>c_save ).
  ENDMETHOD.


  METHOD on_toolbar_function.
    DATA: lf_search_function TYPE abap_bool.

    CASE ev_fcode.

      WHEN zif_uitb_template_prog=>c_func_find.
        DATA(ls_result) = mr_f4_tree->get_search( )->find( ).
        lf_search_function = abap_true.

      WHEN zif_uitb_template_prog=>c_func_find_more.
        ls_result = mr_f4_tree->get_search( )->find_next( ).
        lf_search_function = abap_true.

      WHEN 'EXPANDALL'.
        mr_f4_tree->get_nodes( )->expand_root_nodes( ).

      WHEN 'COLLAPSEALL'.
        mr_f4_tree->get_nodes( )->collapse_all_nodes( ).
    ENDCASE.

    IF lf_search_function = abap_true.
      IF ls_result IS NOT INITIAL.
        mr_f4_tree->get_selections( )->select_nodes( VALUE #( ( ls_result-node_key ) ) ).
      ELSE.
        MESSAGE |No node was found| TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_selected_values.
    mv_chosen_field_with_alias =  is_node_map-alias_fieldname.
    mv_chosen_field = is_node_map-fieldname.
    mv_chosen_table = is_node_map-tabname.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    mr_tmplt_prog->show(
        iv_start_column = 10
        iv_start_line   = 2
        iv_end_column   = 100
        iv_end_line     = 30
    ).
  ENDMETHOD.
ENDCLASS.
