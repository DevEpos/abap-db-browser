class ZCL_DBBR_FIELD_SELECT_TREE definition
  public
  create public .

public section.

  interfaces ZIF_UITB_PAGE_SCROLLER .
  interfaces ZIF_UITB_CONTENT_SEARCHER .

  events TREE_DATA_UPDATED .

  methods CONSTRUCTOR
    importing
      value(IR_FIELDS) type ref to ZCL_DBBR_TABFIELD_LIST
      !IV_MODE type ZDBBR_FIELD_CHOOSER_MODE
      !IF_SINGLE_TABLE_MODE type BOOLEAN optional
      !IV_CURRENT_TABLE type TABNAME optional
      !IV_ENTITY_TYPE type ZDBBR_ENTITY_TYPE .
  methods CREATE_TREE .
  methods SELECT_ALL_FIELDS
    importing
      !IV_NODE_KEY type TM_NODEKEY optional .
  methods DESELECT_ALL_FIELDS
    importing
      !IV_NODE_KEY type TM_NODEKEY optional .
  methods SELECT_KEY_FIELDS
    importing
      !IV_NODE_KEY type TM_NODEKEY optional .
  methods REFRESH_FROM_MODEL .
  methods UPDATE_CURRENT_TABLE
    importing
      !IV_TABNAME type TABNAME .
  methods FREE .
  methods UPDATE_NODES .
  methods SELECT_ALL_TEXT_FIELDS .
  methods DESELECT_ALL_TEXT_FIELDS .
  PROTECTED SECTION.
private section.

  constants MC_FORMULA_NODE type TABNAME value 'Z_FORMS' ##NO_TEXT.
  constants MC_FIELD_PROPS_CONTAINER type DYNFNAM value 'FIELD_CONTAINER' ##NO_TEXT.
  constants MC_TOOLBAR_CONTAINER type DYNFNAM value 'TOOLBAR' ##NO_TEXT.
  constants:
    BEGIN OF mc_select_buttons,
        select_all        TYPE tv_itmname VALUE '2' ##no_text,
        select_key_fields TYPE tv_itmname VALUE '3' ##no_text,
        deselect_fields   TYPE tv_itmname VALUE '4' ##no_text,
      END OF mc_select_buttons .
  constants:
    BEGIN OF mc_column_names,
        alias_column       TYPE tv_itmname VALUE 'HIERCOL1',
        field_check_column TYPE tv_itmname VALUE 'HIERCOL1',
        sel_all_column     TYPE tv_itmname VALUE 'HIERCOL2',
        fieldname_column   TYPE tv_itmname VALUE 'HIERCOL2',
        sel_key_column     TYPE tv_itmname VALUE 'HIERCOL3',
        desel_all_column   TYPE tv_itmname VALUE 'HIERCOL4',
        tablename_column   TYPE tv_itmname VALUE 'HIERCOL5',
        key_column         TYPE tv_itmname VALUE 'KEYCOL',
        text_field_column  TYPE tv_itmname VALUE 'TEXTFLDCOL',
        description_column TYPE tv_itmname VALUE 'DESCRCOL',
      END OF mc_column_names .
  data MR_TREE_MODEL type ref to CL_COLUMN_TREE_MODEL .
  data MR_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data MR_TOOLBAR_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data MR_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MR_FIELDS type ref to ZCL_DBBR_TABFIELD_LIST .
  data MV_MODE type ZDBBR_FIELD_CHOOSER_MODE .
  data MT_TOP_NODES type TREEMNOTAB .
  data MV_FIELD_COUNT type SY-TABIX .
  data MV_CURRENT_TABLE type TABNAME .
  data MF_SINGLE_TABLE_MODE type BOOLEAN .
  data MR_TREE type ref to CL_GUI_CONTROL .
  data MV_ENTITY_TYPE type ZDBBR_ENTITY_TYPE .

  methods CREATE_NODES .
  methods CREATE_TABLE_NODE
    importing
      !IV_TABLENAME type TABNAME
      !IV_ALIAS type ZDBBR_TABLE_ALIAS
      !IV_TABLE_TEXT type DDTEXT
      !IF_EXPANDER type ABAP_BOOL optional .
  methods CREATE_TABLE_FIELD_NODE
    importing
      value(IR_TABFIELD_INFO) type ref to ZDBBR_TABFIELD_INFO_UI .
  methods SELECT_FIELDS_INTERNAL
    importing
      !IV_NODE_KEY type TM_NODEKEY optional
      !IF_SELECT type BOOLEAN default ABAP_TRUE
      !IF_KEY_SELECT type BOOLEAN optional
      !IF_TEXT_SELECT type BOOLEAN optional .
  methods SELECT_FIELD_INTERNAL
    importing
      !IV_NODE_KEY type TM_NODEKEY optional
      !IF_SELECT type BOOLEAN default ABAP_TRUE
      !IF_CUSTOM_ORDER_EXISTS type BOOLEAN
      !IF_KEY_SELECT type BOOLEAN optional
      !IF_TEXT_SELECT type BOOLEAN optional .
  methods CREATE_COLUMNS .
  methods CREATE_TOOLBAR .
  methods ON_CHECKBOX_CHANGE
    for event CHECKBOX_CHANGE of CL_COLUMN_TREE_MODEL
    importing
      !CHECKED
      !ITEM_NAME
      !NODE_KEY .
  methods ON_BUTTON_CLICK
    for event BUTTON_CLICK of CL_COLUMN_TREE_MODEL
    importing
      !ITEM_NAME
      !NODE_KEY .
  methods ON_NODE_CONTEXT_MENU_REQUEST
    for event NODE_CONTEXT_MENU_REQUEST of CL_COLUMN_TREE_MODEL
    importing
      !MENU
      !NODE_KEY .
  methods ON_NODE_CONTEXT_MENU_SELECT
    for event NODE_CONTEXT_MENU_SELECT of CL_COLUMN_TREE_MODEL
    importing
      !FCODE
      !NODE_KEY .
  methods ON_TOOLBAR_BUTTON_CLICKED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
ENDCLASS.



CLASS ZCL_DBBR_FIELD_SELECT_TREE IMPLEMENTATION.


  METHOD constructor.
    mr_fields = ir_fields.
    mv_field_count = mr_fields->get_field_count( ).
    mv_current_table = iv_current_table.
    mf_single_table_mode = if_single_table_mode.
    mv_mode = iv_mode.
    mv_entity_type  = iv_entity_type.
  ENDMETHOD.


  METHOD create_columns.

*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/11/21
*&---------------------------------------------------------------------*
*& Description: Creates columns for tree model
*&---------------------------------------------------------------------*
    " create additional hierarchy columns for buttons, etc.
    mr_tree_model->add_hierarchy_column( name = mc_column_names-sel_all_column ).
    mr_tree_model->add_hierarchy_column( name = mc_column_names-sel_key_column ).
    mr_tree_model->add_hierarchy_column( name = mc_column_names-desel_all_column ).
    mr_tree_model->add_hierarchy_column( name = mc_column_names-tablename_column ).

    " create other columns

    mr_tree_model->add_column(
        name           = mc_column_names-key_column
        width          = 7
        alignment      = cl_column_tree_model=>align_center
        header_text    = 'Key Field'
        header_tooltip = 'Key Field'
    ).

    IF mv_mode = zif_DBBR_global_consts=>gc_field_chooser_modes-output.
      mr_tree_model->add_column(
          name           = mc_column_names-text_field_column
          width          = 7
          alignment      = cl_column_tree_model=>align_center
          header_text    = 'Text Field'
          header_tooltip = 'Affiliated Text Field'
      ).
    ENDIF.

    mr_tree_model->add_column(
        name        = mc_column_names-description_column
        width       = 50
        header_text = 'Description'
    ).


  ENDMETHOD.


  METHOD create_nodes.
*& Description: Creates nodes and items for the tree model
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lt_fields> TYPE zdbbr_tabfield_info_ui_itab.

    mr_fields->sort_in_ddic_order( ).
    DATA(lr_fields_ref) = mr_fields->get_fields_ref( ).

    DATA(lt_table_list) = mr_fields->get_table_list( ).


    LOOP AT lt_table_list ASSIGNING FIELD-SYMBOL(<ls_table>) WHERE no_output = abap_false
                                                               and tabname <> zif_dbbr_global_consts=>c_parameter_dummy_table
                                                               and fields_are_loaded = abap_true.
      IF <ls_table>-tabname = zif_dbbr_global_consts=>gc_formula_dummy_table.
        create_table_node(
            iv_tablename  = <ls_table>-tabname_alias
            iv_alias      = <ls_table>-alias
            iv_table_text = 'Formula Fields'(017)
        ).
      ELSE.
        create_table_node(
            iv_tablename  = <ls_table>-tabname_alias
            iv_alias      = <ls_table>-alias
            iv_table_text = <ls_table>-description
            if_expander   = xsdbool( <ls_table>-fields_are_loaded = abap_false )
        ).
      ENDIF.

*... create nodes / items for table fields
      LOOP AT lr_fields_ref->* ASSIGNING FIELD-SYMBOL(<ls_tabfield>) WHERE tabname_alias = <ls_table>-tabname_alias
                                                                       AND is_text_field = abap_false.
        create_table_field_node( REF #( <ls_tabfield> ) ).
      ENDLOOP.
    ENDLOOP.

*... now sort the field data as one table according to selection/output flag and order
    mr_fields->sort_in_custom_order( ).

  ENDMETHOD.


  METHOD create_table_field_node.
    DATA: lr_text_field_info TYPE REF TO zdbbr_tabfield_info_ui,
          lv_node_key        TYPE tm_nodekey.

*    IF mv_entity_type = zif_dbbr_c_entity_type=>cds_view.
*      lv_node_key = ir_tabfield_info->fieldname_raw.
*    ELSE.
      lv_node_key = COND #( WHEN ir_tabfield_info->alias IS NOT INITIAL THEN
                              ir_tabfield_info->alias && '~' && ir_tabfield_info->fieldname_raw
                            ELSE
                              |{ ir_tabfield_info->fieldname_raw }| ).
*    ENDIF.



    " create item table of node
    DATA(lt_item_table) = VALUE treemcitab(
      ( item_name  = mc_column_names-field_check_column
        class      = cl_list_tree_model=>item_class_checkbox
        editable   = abap_true
        chosen     = SWITCH #(
          mv_mode
          WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-output THEN
            ir_tabfield_info->output_active
          WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-selection THEN
             ir_tabfield_info->selection_active
        )
      )
      ( item_name  = mc_column_names-fieldname_column
        font       = cl_list_tree_model=>item_font_prop
        class      = cl_list_tree_model=>item_class_text
        text       = lv_node_key
      )
      ( item_name  = mc_column_names-key_column
        font       = cl_list_tree_model=>item_font_prop
        class      = cl_list_tree_model=>item_class_text
        text       = ir_tabfield_info->is_key
      )
    ).

    " check if item for additional text field has to be added
    IF mv_mode = zif_dbbr_global_consts=>gc_field_chooser_modes-output AND
       ir_tabfield_info->has_text_field = abap_true.

      APPEND VALUE #(
        item_name  = mc_column_names-text_field_column
        class      = cl_list_tree_model=>item_class_checkbox
        chosen     = mr_fields->active_field_exists( iv_tabname       = ir_tabfield_info->tabname_alias
                                                     iv_fieldname     = ir_tabfield_info->fieldname
                                                     if_is_text_field = abap_true )
        editable   = abap_true
      ) TO lt_item_table.
    ENDIF.

    APPEND VALUE #(
      item_name  = mc_column_names-description_column
      class      = cl_list_tree_model=>item_class_text
      font       = cl_list_tree_model=>item_font_prop
      text       = ir_tabfield_info->field_ddtext
    ) TO lt_item_table.

    mr_tree_model->add_node(
      EXPORTING
        node_key          = lv_node_key
        relative_node_key = CONV #( COND #( WHEN ir_tabfield_info->tabname_alias IS NOT INITIAL THEN
                                              ir_tabfield_info->tabname_alias
                                            ELSE
                                              mc_formula_node ) )
        relationship      = cl_list_tree_model=>relat_last_child
        image             = zif_dbbr_c_icon=>no_icon
        isfolder          = abap_false
        user_object       = NEW zcl_dbbr_tabfield( ir_table_field_info = ir_tabfield_info
*                                                    ir_text_field_info  = lr_text_field_info
                                                    iv_mode             = mv_mode           )
        style             = COND #( WHEN ir_tabfield_info->is_key = abap_true THEN
                                      cl_list_tree_model=>style_emphasized_b        )
        item_table        = lt_item_table
      EXCEPTIONS
        node_key_exists   = 1
    ).
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD create_table_node.

    mr_tree_model->add_node(
      EXPORTING
        node_key                = CONV #( iv_tablename )
        relationship            = cl_list_tree_model=>relat_last_child
        isfolder                = abap_true
        expander                = if_expander
        style                   = COND #( WHEN iv_tablename = mc_formula_node THEN
                                            cl_list_tree_model=>style_emphasized_b
                                          ELSE
                                            cl_list_tree_model=>style_emphasized_c )
        item_table              = VALUE treemcitab(
          ( item_name  = mc_column_names-alias_column
            class      = cl_list_tree_model=>item_class_text
            style      = COND #( WHEN iv_tablename = mc_formula_node THEN
                                   cl_list_tree_model=>style_emphasized_b
                                 ELSE
                                   cl_list_tree_model=>style_emphasized_c )
            text       = iv_alias
            font       = cl_list_tree_model=>item_font_prop
          )
          ( item_name  = mc_column_names-sel_all_column
            class      = cl_list_tree_model=>item_class_button
            t_image    = zif_dbbr_c_icon=>select_all
          )
          ( item_name  = mc_column_names-sel_key_column
            class      = cl_list_tree_model=>item_class_button
            t_image    = zif_dbbr_c_icon=>select_block
          )
          ( item_name  = mc_column_names-desel_all_column
            class      = cl_list_tree_model=>item_class_button
            t_image    = zif_dbbr_c_icon=>deselect_all
          )
          ( item_name  = mc_column_names-tablename_column
            class      = cl_list_tree_model=>item_class_text
            font       = cl_list_tree_model=>item_font_prop
            text       = iv_tablename
          )
          ( item_name  = mc_column_names-description_column
            class      = cl_list_tree_model=>item_class_text
            font       = cl_gui_list_tree=>item_font_prop
            text       = iv_table_text
          )
        )
    ).
    APPEND iv_tablename TO mt_top_nodes.

  ENDMETHOD.


  METHOD create_toolbar.

    DATA: lt_events TYPE cntl_simple_events.
    " create toolbar
    mr_toolbar_container = NEW cl_gui_custom_container( container_name = mc_toolbar_container ).
    mr_toolbar = NEW cl_gui_toolbar(
        parent             = mr_toolbar_container
    ).

    DATA(lt_buttons) = VALUE ttb_button(
      ( function  = 'EXPAND_ALL'
        icon      = zif_dbbr_c_icon=>expand
        butn_type = cntb_btype_button )
      ( function  = 'COLLAPSE_ALL'
        icon      = zif_dbbr_c_icon=>collapse
        butn_type = cntb_btype_button )
    ).

    mr_toolbar->add_button_group( lt_buttons ).

    lt_events = VALUE #(
        ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
    ).

    mr_toolbar->set_registered_events( lt_events ).

    SET HANDLER on_toolbar_button_clicked FOR mr_toolbar.

  ENDMETHOD.


  METHOD create_tree.

    DATA: lt_events TYPE cntl_simple_events.

    CHECK mr_container IS INITIAL.

    mr_container = NEW cl_gui_custom_container(
        container_name              = mc_field_props_container
    ).
    mr_tree_model = NEW cl_column_tree_model(
        node_selection_mode         = cl_column_tree_model=>node_sel_mode_multiple
        item_selection              = abap_true
        hierarchy_header            = VALUE treemhhdr(
            heading = 'Table / Field'
            width   = 75
        )
        hierarchy_column_name       = mc_column_names-alias_column
    ).

    create_toolbar( ).

    " create tree control from model
    mr_tree_model->create_tree_control( EXPORTING parent  = mr_container
                                        IMPORTING control = mr_tree ).

    lt_events = VALUE #(
       ( eventid = cl_column_tree_model=>eventid_checkbox_change       appl_event = abap_true )
       ( eventid = cl_column_tree_model=>eventid_button_click          appl_event = abap_true )
       ( eventid = cl_column_tree_model=>eventid_node_context_menu_req appl_event = abap_true )
    ).

    mr_tree_model->set_registered_events( lt_events ).

    SET HANDLER:
       on_checkbox_change FOR mr_tree_model,
       on_button_click FOR mr_tree_model,
       on_node_context_menu_request FOR mr_tree_model,
       on_node_context_menu_select FOR mr_tree_model.

    create_columns( ).

    update_nodes( ).

  ENDMETHOD.


  METHOD deselect_all_fields.

    select_fields_internal(
        if_select     = abap_false
    ).

  ENDMETHOD.


  METHOD deselect_all_text_fields.
    select_fields_internal(
        if_select      = abap_false
        if_text_select = abap_true
    ).
  ENDMETHOD.


  METHOD free.

    IF mr_container IS BOUND.
      mr_container->free( ).
    ENDIF.
    IF mr_toolbar_container IS BOUND.
      mr_toolbar_container->free( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_button_click.

    CASE item_name.
      WHEN mc_column_names-sel_all_column.
        select_fields_internal(
            iv_node_key   = node_key
        ).
      WHEN mc_column_names-sel_key_column.
        select_fields_internal(
            iv_node_key   = node_key
            if_key_select = abap_true
        ).
      WHEN mc_column_names-desel_all_column.
        select_fields_internal(
            iv_node_key   = node_key
            if_select     = abap_false
        ).
    ENDCASE.

    RAISE EVENT tree_data_updated.

  ENDMETHOD.


  METHOD on_checkbox_change.
    DATA: lv_new_order TYPE numc3.

    " retrieve table field for checked node
    mr_tree_model->node_get_parent( EXPORTING node_key        = node_key
                                    IMPORTING parent_node_key = DATA(lv_parent_node) ).

    mr_tree_model->node_get_user_object( EXPORTING node_key    = node_key
                                         IMPORTING user_object = DATA(lr_user_object) ).

    """ determine new order according to 'checked' value
    DATA(lr_tabfield) = CAST zcl_DBBR_tabfield( lr_user_object ).

    lv_new_order = 0.
    DATA(ls_tabfield_info) = lr_tabfield->get_tabfield_info( ).

    IF mr_fields->custom_order_exists( ).
      IF ls_tabfield_info-output_order <> 0.
        lv_new_order = ls_tabfield_info-output_order.
      ELSE.
        lv_new_order = ls_tabfield_info-ddic_order.
      ENDIF.
    ENDIF.

    IF item_name = mc_column_names-field_check_column.
      lr_tabfield->set_custom_active( checked ).
      lr_tabfield->set_custom_order( lv_new_order ).
    ELSEIF item_name = mc_column_names-text_field_column.
      mr_fields->set_all_text_fields(
          iv_tabname      = ls_tabfield_info-tabname_alias
          iv_fieldname    = ls_tabfield_info-fieldname
          if_active       = checked
          iv_output_order = lv_new_order
      ).
    ENDIF.

    RAISE EVENT tree_data_updated.


  ENDMETHOD.


  METHOD on_node_context_menu_request.

    mr_tree_model->node_get_parent( EXPORTING node_key = node_key
                                    IMPORTING parent_node_key = DATA(lv_parent_node) ).
    IF lv_parent_node IS INITIAL.
      RETURN.
    ENDIF.

    " select / deselect selected nodes
    menu->add_function(
        fcode = 'SELECT'
        text  = CONV #( text-007 )
    ).
    menu->add_function(
        fcode = 'DESELECT'
        text  = CONV #( text-008 )
    ).

  ENDMETHOD.


  METHOD on_node_context_menu_select.

    " get selected nodes
    mr_tree_model->get_selected_nodes( IMPORTING node_key_table = DATA(lt_selected_nodes) ).

    LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lv_node>).
      CASE fcode.
        WHEN 'SELECT'.
          select_field_internal(
            iv_node_key            = <lv_node>
            if_custom_order_exists = mr_fields->custom_order_exists( )
          ).
        WHEN 'DESELECT'.
          select_field_internal(
            iv_node_key             = <lv_node>
            if_custom_order_exists  = mr_fields->custom_order_exists( )
            if_select               = abap_false
          ).
      ENDCASE.
    ENDLOOP.

    " raise event to detect data change
    RAISE EVENT tree_data_updated.

  ENDMETHOD.


  METHOD on_toolbar_button_clicked.

    CASE fcode.
      WHEN 'EXPAND_ALL'.
        mr_tree_model->expand_root_nodes( ).

      WHEN 'COLLAPSE_ALL'.
        mr_tree_model->collapse_all_nodes( ).

      WHEN zcl_DBBR_tabfield_manager=>mc_functions-select_fields.
        select_all_fields( ).

      WHEN zcl_DBBR_tabfield_manager=>mc_functions-select_key_fields.
        select_key_fields( ).

      WHEN zcl_DBBR_tabfield_manager=>mc_functions-deselect_fields.
        deselect_all_fields( ).

    ENDCASE.

  ENDMETHOD.


  METHOD refresh_from_model.
    DATA: lf_checked TYPE boolean.

    " 1) select all fields in the internal model
    LOOP AT mt_top_nodes ASSIGNING FIELD-SYMBOL(<lv_top_node>).
      """ get child nodes of table field nodes
      mr_tree_model->node_get_children(
        EXPORTING node_key       = <lv_top_node>
        IMPORTING node_key_table = DATA(lt_children) ).

      LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<lv_child_node>).
        mr_tree_model->node_get_user_object(
          EXPORTING node_key       = <lv_child_node>
          IMPORTING user_object    = DATA(lr_user_object) ).

        " get user object information
        DATA(ls_tabfield_info) = CAST zcl_DBBR_tabfield( lr_user_object )->get_tabfield_info( ).
        CASE mv_mode.
          WHEN zif_DBBR_global_consts=>gc_field_chooser_modes-output.
            lf_checked = ls_tabfield_info-output_active.
            " update text select column
            IF ls_tabfield_info-has_text_field = abap_true.
              DATA(lr_textfield_ref) = mr_fields->get_field_ref(
                 iv_tabname_alias = ls_tabfield_info-tabname_alias
                 iv_fieldname     = ls_tabfield_info-fieldname
                 if_is_text_field = abap_true
              ).
              mr_tree_model->item_set_chosen(
                node_key  = <lv_child_node>
                item_name = mc_column_names-text_field_column
                chosen    = lr_textfield_ref->output_active
              ).
            ENDIF.
          WHEN zif_DBBR_global_consts=>gc_field_chooser_modes-selection.
            lf_checked = ls_tabfield_info-selection_active.
        ENDCASE.

        mr_tree_model->item_set_chosen(
          node_key             = <lv_child_node>
          item_name            = mc_column_names-field_check_column
          chosen               = lf_checked
        ).

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_all_fields.

    select_fields_internal( ).

  ENDMETHOD.


  METHOD select_all_text_fields.
    select_fields_internal(
        if_select      = abap_true
        if_text_select = abap_true
    ).
  ENDMETHOD.


  METHOD select_fields_internal.

    DATA: lf_checked TYPE boolean.

    IF iv_node_key IS NOT INITIAL.
      DATA(lt_nodes) = VALUE treemnotab( ( iv_node_key ) ).
    ELSE.
      lt_nodes = mt_top_nodes.
      " are there any selected nodes
      mr_tree_model->get_selected_nodes( IMPORTING node_key_table = DATA(lt_selected_nodes) ).
      IF if_key_select = abap_false AND lt_selected_nodes IS NOT INITIAL.
        LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lv_node>).
          CHECK NOT line_exists( mt_top_nodes[ table_line = <lv_node> ] ).
          select_field_internal(
              iv_node_key            = <lv_node>
              if_select              = if_select
              if_custom_order_exists = mr_fields->custom_order_exists( )
              if_text_select         = if_text_select
              if_key_select          = if_key_select
          ).
        ENDLOOP.
        RAISE EVENT tree_data_updated.
        RETURN.
      ENDIF.

    ENDIF.

    " select all fields in the internal model
    LOOP AT lt_nodes ASSIGNING FIELD-SYMBOL(<lv_top_node>).
      """ get child nodes of table field nodes
      mr_tree_model->node_get_children(
        EXPORTING node_key       = <lv_top_node>
        IMPORTING node_key_table = DATA(lt_children) ).

      LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<lv_child_node>).
        select_field_internal(
            iv_node_key            = <lv_child_node>
            if_select              = if_select
            if_custom_order_exists = mr_fields->custom_order_exists( )
            if_text_select         = if_text_select
            if_key_select          = if_key_select
        ).
      ENDLOOP.
    ENDLOOP.

    RAISE EVENT tree_data_updated.


  ENDMETHOD.


  METHOD select_field_internal.

*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/11/17
*&---------------------------------------------------------------------*
*& Description: Selects/Deselects single table field
*&---------------------------------------------------------------------*
    DATA: lv_new_order TYPE numc3.

    mr_tree_model->node_get_user_object(
      EXPORTING node_key       = iv_node_key
      IMPORTING user_object    = DATA(lr_user_object) ).

    DATA(lr_tabfield) = CAST zcl_DBBR_tabfield( lr_user_object ).
    DATA(ls_tabfield_info) = lr_tabfield->get_tabfield_info( ).

    IF if_text_select = abap_true AND
       ls_tabfield_info-has_text_field = abap_false.
      RETURN.
    ENDIF.

    DATA(lf_checked) = if_select.

    IF if_key_select = abap_true.
      lf_checked = ls_tabfield_info-is_key.
    ENDIF.

    mr_tree_model->item_set_chosen(
      node_key             = iv_node_key
      item_name            = COND #( WHEN if_text_select = abap_true THEN
                                        mc_column_names-text_field_column
                                     ELSE
                                        mc_column_names-field_check_column )
      chosen               = lf_checked
    ).

    lv_new_order = 0.
    IF if_custom_order_exists = abap_true.
      IF ls_tabfield_info-output_order <> 0.
        lv_new_order = ls_tabfield_info-output_order.
      ELSE.
        lv_new_order = ls_tabfield_info-ddic_order.
      ENDIF.
    ENDIF.

    IF if_text_select = abap_false.
      lr_tabfield->set_custom_active( lf_checked ).
      lr_tabfield->set_custom_order( lv_new_order ).
    ELSE.
      mr_fields->set_all_text_fields(
          iv_tabname      = ls_tabfield_info-tabname_alias
          iv_fieldname    = ls_tabfield_info-fieldname
          if_active       = lf_checked
          iv_output_order = lv_new_order
      ).
    ENDIF.

  ENDMETHOD.


  METHOD select_key_fields.

    select_fields_internal(
        if_key_select = abap_true
    ).

  ENDMETHOD.


  METHOD update_current_table.

    mv_current_table = iv_tabname.

  ENDMETHOD.


  METHOD update_nodes.

    CLEAR mt_top_nodes.
    mr_tree_model->delete_all_nodes( ).
    """ create nodes for model
    create_nodes( ).

    """ expand top nodes
    mr_tree_model->expand_root_nodes( ).

    """ select first top node
    mr_tree_model->scroll( cl_list_tree_model=>scroll_home ).

  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.

    CHECK mr_tree_model IS NOT INITIAL.

    mr_tree_model->find(
        IMPORTING result_item_key_table = DATA(lt_result_item)
                  result_type           = DATA(lv_result_type)
    ).

    IF lv_result_type <> 0 AND lt_result_item IS NOT INITIAL.
      mr_tree_model->select_item(
        node_key          = lt_result_item[ 1 ]-node_key
        item_name         = lt_result_item[ 1 ]-item_name
      ).

      cl_gui_control=>set_focus( mr_tree ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.

    CHECK mr_tree_model IS NOT INITIAL.

    mr_tree_model->find_next(
        IMPORTING result_item_key_table    = DATA(lt_result_item)
                  result_expander_node_key = DATA(lt_result_expander_node_key)
                  result_type              = DATA(lv_result_type)
    ).

    IF lv_result_type <> 0 AND lt_result_item IS NOT INITIAL.
      mr_tree_model->select_item(
        node_key          = lt_result_item[ 1 ]-node_key
        item_name         = lt_result_item[ 1 ]-item_name
      ).
      cl_gui_control=>set_focus( mr_tree ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_bottom.

    mr_tree_model->scroll( cl_column_tree_model=>scroll_end ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_down.

    mr_tree_model->scroll( cl_column_tree_model=>scroll_down_page ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_top.

    mr_tree_model->scroll( cl_column_tree_model=>scroll_home ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_up.

    mr_tree_model->scroll( cl_column_tree_model=>scroll_up_page ).

  ENDMETHOD.
ENDCLASS.
