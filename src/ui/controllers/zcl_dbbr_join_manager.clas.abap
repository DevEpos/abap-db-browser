"! <p class="shorttext synchronized" lang="en">Manager for Join definitions</p>
CLASS zcl_dbbr_join_manager DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_c_selection_condition .

    "! <p class="shorttext synchronized" lang="en">CLASS_CONSTRUCTOR</p>
    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        iv_primary_entity     TYPE zdbbr_entity_id
        iv_primary_entity_raw TYPE zdbbr_entity_id_raw
        iv_entity_type        TYPE zdbbr_entity_type DEFAULT zif_dbbr_c_entity_type=>table
        !ir_join_ref          TYPE REF TO zdbbr_join_def .
    "! <p class="shorttext synchronized" lang="en">Signals to the caller if the join was updated</p>
    METHODS was_updated
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
    METHODS handle_exit_request
        REDEFINITION.
  PRIVATE SECTION.

    ALIASES and
      FOR zif_dbbr_c_selection_condition~and .
    ALIASES or
      FOR zif_dbbr_c_selection_condition~or .

    DATA mo_join TYPE REF TO lcl_join .
    "! <p class="shorttext synchronized" lang="en">Tree for storing join definition</p>
    DATA mo_join_tree TYPE REF TO zcl_uitb_column_tree_model .

    CONSTANTS:
      BEGIN OF c_functions,
        focus_on_tree         TYPE ui_func VALUE 'FOCUS',
        transfer_data         TYPE ui_func VALUE 'TRANSFER',
        show_help             TYPE ui_func VALUE 'HELP',
        change_prim_tab_alias TYPE ui_func VALUE 'PRIMALIASCHANGE',
        show_sql              TYPE ui_func VALUE 'SHOWSQL',
        collapse_all_nodes    TYPE ui_func VALUE 'COLLAPSE_ALL',
        expand_all_nodes      TYPE ui_func VALUE 'EXPAND_ALL',
        add_table             TYPE ui_func VALUE 'NEW_ADD_TABLE',
        delete_table          TYPE ui_func VALUE 'DELETE_TABLE',
        delete_all_tables     TYPE ui_func VALUE 'DELETE_ALL_TABLES',
        add_and_condition     TYPE ui_func VALUE 'NEW_AND_COND',
        add_or_condition      TYPE ui_func VALUE 'NEW_OR_COND',
        edit_condition        TYPE ui_func VALUE 'EDIT_CONDITION',
        delete_condition      TYPE ui_func VALUE 'DELETE_CONDITION',
      END OF c_functions .

    "! <p class="shorttext synchronized" lang="en">Definition of a Join</p>
    DATA mr_join_def TYPE REF TO zdbbr_join_def .

    "! <p class="shorttext synchronized" lang="en">Tree Model: Table of Node Keys</p>
    DATA mt_table_nodes TYPE treemnotab .
    CONSTANTS c_tab_node_suffix TYPE string VALUE '-TAB' ##NO_TEXT.
    CONSTANTS c_field_cond_node_suffix TYPE string VALUE '-FIELD' ##NO_TEXT.
    CONSTANTS c_filter_cond_node_suffix TYPE string VALUE '-SELECT' ##NO_TEXT.
    "! <p class="shorttext synchronized" lang="en">Tree Model: Node Key</p>
    DATA mv_current_key_count TYPE i .
    DATA:
      mt_node_map TYPE STANDARD TABLE OF lty_s_node_map WITH KEY node_key .
    "! <p class="shorttext synchronized" lang="en">Tree Model: Node Key</p>
    DATA ms_new_cond_relat_nodemap TYPE lty_s_node_map .
    DATA mv_new_cond_relationship TYPE i .
    "! <p class="shorttext synchronized" lang="en">Node of a column tree model</p>
    DATA mo_relat_node TYPE REF TO zcl_uitb_ctm_node .
    DATA mf_init_mode TYPE abap_bool .
    DATA mf_no_query_on_deletion TYPE abap_bool VALUE abap_true ##NO_TEXT.
    DATA mv_evt_filt_mode_node_type TYPE lty_node_type .
    DATA mf_was_updated TYPE abap_bool .
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar.
    "! <p class="shorttext synchronized" lang="en">Description of Domain Fixed Values</p>
    CLASS-DATA st_join_type_fixvals TYPE ddfixvalues .

    "! <p class="shorttext synchronized" lang="en">Add new AND condition to join table</p>
    METHODS add_and_condition .
    "! <p class="shorttext synchronized" lang="en">Adds new join table to the tree</p>
    METHODS add_new_join_table .
    "! <p class="shorttext synchronized" lang="en">Add new OR condition to join table</p>
    METHODS add_or_condition .
    "! <p class="shorttext synchronized" lang="en">Changes values of given field condition</p>
    METHODS change_field_condition
      IMPORTING
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Changes values of given filter condition</p>
    METHODS change_filter_condition
      IMPORTING
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Changes values of a given join table</p>
    METHODS change_join_table
      IMPORTING
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Change node content</p>
    METHODS change_node .
    "! <p class="shorttext synchronized" lang="en">Create columns for join tree</p>
    METHODS create_columns
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Create node for field condition</p>
    METHODS create_field_condition_node
      IMPORTING
        !is_field_cond     TYPE zdbbr_joinfld
        !if_from_event     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Create node for filter condition</p>
    METHODS create_filter_condition_node
      IMPORTING
        !is_filter_condition TYPE zdbbr_joinfil
        !iv_node_type        TYPE lty_node_type DEFAULT c_node_type-table_filter
        !if_from_event       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_node_key)   TYPE tm_nodekey .

    "! <p class="shorttext synchronized" lang="en">Create node for join table</p>
    METHODS create_join_table_node
      IMPORTING
        !is_join_table     TYPE zdbbr_join_table_ui
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Create OR Group Node</p>
    METHODS create_or_group_node
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey .



    "! <p class="shorttext synchronized" lang="en">Create Tree to hold tables/conditions</p>
    METHODS create_tree
      IMPORTING
        io_container TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Deletes all conditions (filter or field)</p>
    METHODS delete_all_conditions
      IMPORTING
        !io_node       TYPE REF TO zcl_uitb_ctm_node
        !io_join_table TYPE REF TO lcl_join_table
        !is_node_map   TYPE lty_s_node_map
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Delete Condition</p>
    METHODS delete_condition .
    "! <p class="shorttext synchronized" lang="en">Deletes a field condition</p>
    METHODS delete_field_condition
      IMPORTING
        !io_node       TYPE REF TO zcl_uitb_ctm_node
        !io_join_table TYPE REF TO lcl_join_table
        !is_node_map   TYPE lty_s_node_map
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Deletes a filter condition</p>
    METHODS delete_filter_condition
      IMPORTING
        !io_node       TYPE REF TO zcl_uitb_ctm_node
        !io_join_table TYPE REF TO lcl_join_table
        !is_node_map   TYPE lty_s_node_map
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Deletes the passed node and select next best node</p>
    METHODS delete_node_and_select_next
      IMPORTING
        !io_node           TYPE REF TO zcl_uitb_ctm_node
        !is_node_map       TYPE lty_s_node_map
        !if_stop_recursion TYPE abap_bool OPTIONAL
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Deletes the selected OR-Group</p>
    METHODS delete_or_group
      IMPORTING
        !io_node       TYPE REF TO zcl_uitb_ctm_node
        !io_join_table TYPE REF TO lcl_join_table
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Delete selected table</p>
    METHODS delete_table
      IMPORTING
        !if_all TYPE abap_bool OPTIONAL .

    "! <p class="shorttext synchronized" lang="en">Fill tree with existing join definition</p>
    METHODS fill_tree_with_join .
    "! <p class="shorttext synchronized" lang="en">Fill text content for value items</p>
    METHODS fill_value_item_content
      IMPORTING
        !io_node             TYPE REF TO zcl_uitb_ctm_node
        !io_filter_condition TYPE REF TO lcl_join_filter .
    "! <p class="shorttext synchronized" lang="en">Get next free node key</p>
    METHODS get_next_key
      RETURNING
        VALUE(result) TYPE tm_nodekey .


    "! <p class="shorttext synchronized" lang="en">Handler for when join field condition was created</p>
    METHODS on_created_field_condition
          FOR EVENT created_field_condition OF zcl_dbbr_edit_join_cond_view
      IMPORTING
          !es_field .
    "! <p class="shorttext synchronized" lang="en">Handler for when join filter condition was created</p>
    METHODS on_created_filter_condition
          FOR EVENT created_filter_condition OF zcl_dbbr_edit_join_cond_view
      IMPORTING
          !es_filter .
    "! <p class="shorttext synchronized" lang="en">Handler for when join table was created</p>
    METHODS on_created_join_table
          FOR EVENT created OF zcl_dbbr_edit_join_table_ctrl
      IMPORTING
          !es_join_table .
    "! <p class="shorttext synchronized" lang="en">Handler for when join requests node deletion</p>
    METHODS on_delete_request_from_join
          FOR EVENT request_deletion OF lif_tree_node_events
      IMPORTING
          !ev_node_key .

    "! <p class="shorttext synchronized" lang="en">Handler for node context menu request</p>
    METHODS on_node_ctx_menu_request
          FOR EVENT node_context_menu_request OF zcl_uitb_ctm_events
      IMPORTING
          !ev_node_key
          !er_menu .
    "! <p class="shorttext synchronized" lang="en">Handler for menu node selection</p>
    METHODS on_node_ctx_menu_select
          FOR EVENT node_context_menu_select OF zcl_uitb_ctm_events
      IMPORTING
          !ev_node_key
          !ev_fcode .
    "! <p class="shorttext synchronized" lang="en">Handler for tree node double click</p>
    METHODS on_node_double_click
          FOR EVENT node_double_click OF zcl_uitb_ctm_events
      IMPORTING
          !ev_node_key .
    "! <p class="shorttext synchronized" lang="en">Handler for key press on node</p>
    METHODS on_node_keypress
          FOR EVENT node_keypress OF zcl_uitb_ctm_events
      IMPORTING
          !ev_node_key
          !ev_key .


    "! <p class="shorttext synchronized" lang="en">Test the join definition</p>
    "!
    METHODS test_join .
    "! <p class="shorttext synchronized" lang="en">Transfers join from tree to structure</p>
    "!
    METHODS transfer_join .
    "! <p class="shorttext synchronized" lang="en">Updates the OR Group node key for this filter node</p>
    "!
    METHODS update_or_group_of_filter
      IMPORTING
        !io_filter_node   TYPE REF TO zcl_uitb_ctm_node
        !iv_or_group_node TYPE tm_nodekey OPTIONAL
        !iv_node_type     TYPE lty_node_type
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Do some further validations for the join</p>
    "!
    METHODS validate_join .
    "! <p class="shorttext synchronized" lang="en">Changes the alias of the primary table</p>
    "!
    METHODS change_primary_table_alias.
    "! <p class="shorttext synchronized" lang="en">Update alias of join table and all dependent entities</p>
    "!
    METHODS update_table_alias
      IMPORTING
        iv_old_alias TYPE zdbbr_entity_alias
        iv_new_alias TYPE zdbbr_entity_alias.
    "! <p class="shorttext synchronized" lang="en">Updates node/item values in the table</p>
    "!
    METHODS update_node_values
      IMPORTING
        it_nodes_to_update TYPE lty_t_node_to_update.
ENDCLASS.



CLASS zcl_dbbr_join_manager IMPLEMENTATION.
  METHOD class_constructor.
    DATA(lo_join_type_descr) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_data( VALUE zdbbr_jointype( ) )
    ).

    st_join_type_fixvals = lo_join_type_descr->get_ddic_fixed_values(
        p_langu = 'E'
    ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( iv_title = |{ 'DB Browser - Joins for'(020) } { iv_primary_entity_raw }| ).
    zcl_dbbr_entity_alias_util=>initialize_aliases( ).

    mr_join_def = ir_join_ref.

    mo_join = NEW #(
        VALUE #(
            entity_id        = iv_primary_entity
            entity_id_raw    = iv_primary_entity_raw
            entity_type      = iv_entity_type
            entity_alias     = COND #( WHEN mr_join_def->primary_table_alias IS INITIAL THEN
                                         zcl_dbbr_entity_alias_util=>create_entity_alias( mr_join_def->primary_table )
                                       ELSE
                                         mr_join_def->primary_table_alias )
        )
    ).
  ENDMETHOD.

  METHOD was_updated.
    result = mf_was_updated.
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    TRY.
        CASE io_command->mv_function.

          WHEN c_functions-change_prim_tab_alias.
            change_primary_table_alias( ).

          WHEN c_functions-transfer_data.
            validate_join( ).
            transfer_join( ).
            leave_screen( ).

          WHEN c_functions-show_sql.
            test_join( ).

          WHEN c_functions-show_help.
            zcl_dbbr_help_repository=>show_help( zcl_dbbr_help_repository=>c_help_id-join_manager ).

          WHEN c_functions-add_table.
            add_new_join_table( ).

          WHEN c_functions-delete_table.
            delete_table( ).

          WHEN c_functions-delete_all_tables.
            delete_table( if_all = abap_true ).

          WHEN c_functions-expand_all_nodes.
            mo_join_tree->get_nodes( )->expand_root_nodes( ).

          WHEN c_functions-collapse_all_nodes.
            mo_join_tree->get_nodes( )->collapse_all_nodes( ).

          WHEN c_functions-add_and_condition.
            add_and_condition( ).

          WHEN c_functions-add_or_condition.
            add_or_condition( ).

          WHEN c_functions-edit_condition.
            change_node( ).

          WHEN c_functions-delete_condition.
            delete_condition( ).

          WHEN c_functions-focus_on_tree.
            mo_join_tree->zif_uitb_gui_control~focus( ).
        ENDCASE.
      CATCH zcx_dbbr_validation_exception INTO DATA(lx_validation_error).
        lx_validation_error->print_message(
            iv_msg_type  = 'I'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_content.
    create_control_toolbar(
      EXPORTING
        io_parent    = io_container
        it_button    = VALUE #(
          ( function  = c_functions-transfer_data
            icon      = icon_okay
            quickinfo = |{ 'Update Join Definition'(009) }|
          )
          ( function  = c_functions-show_help
            icon      = icon_information
            quickinfo = |{ 'Show Application Help'(021) }|
          )
          ( function  = c_functions-show_sql
            quickinfo = |{ 'Show SQL FROM'(017) }|
            icon      = icon_display_text
            text      = |{ 'Show SQL'(019) }|
          )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-expand_all_nodes
            icon      = icon_expand_all
            quickinfo = 'Expand all nodes'(007)
          )
          ( function = c_functions-collapse_all_nodes
            icon      = icon_collapse_all
            quickinfo = 'Collapse all'(008)
          )
          ( butn_type = cntb_btype_sep )
          ( function  = c_functions-change_prim_tab_alias
            icon      = icon_change
            text      = |{ 'Prim. Table Alias'(022) }|
            quickinfo = |{ 'Change Alias of Primary Table'(023) }| )
          ( butn_type = cntb_btype_sep )
          ( function = c_functions-add_table
            icon      = icon_create
            text      = 'Join Table'(013)
          )
          ( function = c_functions-delete_table
            icon      = icon_delete
            text      = 'Join Table'(013)
            butn_type = cntb_btype_dropdown
          )
          ( butn_type = cntb_btype_sep )
          ( function = c_functions-add_and_condition
            icon      = icon_insert_relation
            text      = |{ 'AND' }|
            quickinfo = |{ 'Create AND Condition'(024) }|
          )
          ( function = c_functions-add_or_condition
            icon      = icon_tree
            text      = |{ 'OR' }|
            quickinfo = |{ 'Create OR Condition'(025) }|
          )
          ( function = c_functions-edit_condition
            icon      = icon_change
            text      = 'Condition'(026)
          )
          ( function = c_functions-delete_condition
            icon      = icon_delete
            text      = 'Condition'(026)
          )
        )
      IMPORTING
        eo_toolbar   = mo_toolbar
        eo_client    = DATA(lo_tree_container)
    ).
    mo_toolbar->set_static_ctxmenu(
        fcode = c_functions-delete_table
        ctxmenu = zcl_uitb_gui_helper=>create_menu(
            VALUE #( ( function = c_functions-delete_all_tables
                       icon      = icon_delete
                       text      = 'All Join Tables'(015)
                     )
            )
        )
    ).
    create_tree( lo_tree_container ).
    fill_tree_with_join( ).

*.. set focus on tree
    mo_join_tree->zif_uitb_gui_control~focus( ).
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->set_title( |{ 'DB Browser - Joins for'(020) } { mo_join->mv_primary_entity } ({ to_lower( mo_join->mv_primary_entity_alias ) })| ).
    io_callback->deactivate_function( zif_uitb_c_gui_screen=>c_functions-save ).
    io_callback->map_fkey_functions( VALUE #(
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_f1
        text            = |{ 'Focus on Tree' }|
        mapped_function = c_functions-focus_on_tree )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-f8
        mapped_function = c_functions-transfer_data
        text            = |{ 'Update Join Definition'(009) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f8
        mapped_function = c_functions-show_sql
        text            = |{ 'Show SQL FROM'(017) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-f9
        mapped_function = c_functions-show_help
        text            = |{ 'Help'(027) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-f5
        mapped_function = c_functions-add_table
        text            = |{ 'New Join Table'(028) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-f6
        mapped_function = c_functions-add_and_condition
        text            = |{ 'New AND Condition'(029) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-f7
        mapped_function = c_functions-add_or_condition
        text            = |{ 'New OR Condition'(030) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f5
        mapped_function = c_functions-change_prim_tab_alias
        text            = |{ 'Change Alias of Primary Table'(023) }| ) )
    ).
  ENDMETHOD.

  METHOD handle_exit_request.
*... 1) check if there are changes in the join definition
    IF mo_join->has_changes( ).
*.... query user if he really wants to leave screen regardless of possible
*.... data loss
      IF zcl_dbbr_appl_util=>popup_to_confirm(
              iv_title                 = 'Leave'
              iv_query                 = |There are unsaved changes in the Join definion. | &&
                                         |Are you sure you want to leave the Screen?|
              iv_icon_type             = 'ICON_MESSAGE_QUESTION'
          ) <> '1'.
        io_callback->cancel_exit( ).
      ELSE.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD add_and_condition.
    DATA: lv_condition_view_mode TYPE i,
          lv_source_table        TYPE tabname.

    TRY .
*...... Get selected node
        DATA(lt_nodes) = mo_join_tree->get_selections( )->get_selected_nodes( ).
        CHECK lines( lt_nodes ) = 1.
        DATA(lo_selected_node) = lt_nodes[ 1 ].

*...... Retrieve information about node from map
        ASSIGN mt_node_map[ node_key = lo_selected_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node>).
        CHECK sy-subrc = 0.

        CASE <ls_node>-node_type.

          WHEN c_node_type-table_field OR
               c_node_type-table_fields.
            lv_condition_view_mode = zcl_dbbr_edit_join_cond_view=>c_field_mode.
*.......... get join table reference to determine possible virtual join mode for condition view
            DATA(lo_join_table) = mo_join->get_entity( <ls_node>-alias ).
            lv_source_table = <ls_node>-tabname.
            DATA(lf_virtual_join) = lo_join_table->get_tab_info( )-is_virtual.
*.......... do not allow the current join table as a possible target table in the first release
            DATA(lf_exclude_current_table) = abap_true.

          WHEN c_node_type-table_filters OR
               c_node_type-table_filter OR
               c_node_type-or_filter_group.
            lv_condition_view_mode = zcl_dbbr_edit_join_cond_view=>c_value_mode.

          WHEN OTHERS.
            RETURN.
        ENDCASE.

*...... check node type again for determining the correct node relationship to the
*...... currently selected node
        CASE <ls_node>-node_type.
          WHEN c_node_type-table_field OR
               c_node_type-table_filter OR
               c_node_type-or_filter_group.
            mv_new_cond_relationship = cl_tree_model=>relat_next_sibling.

          WHEN OTHERS.
            mv_new_cond_relationship = cl_tree_model=>relat_last_child.
        ENDCASE.

*...... Get target tables for condition view
        DATA(lt_entities) = mo_join->get_possible_entities_for_f4( iv_entity_alias = <ls_node>-alias ).
        IF lf_exclude_current_table = abap_true.
          DELETE lt_entities WHERE entity_alias = <ls_node>-alias.
        ENDIF.

        ms_new_cond_relat_nodemap = <ls_node>.
        mo_relat_node = mo_join_tree->get_nodes( )->get_node( <ls_node>-node_key ).

*...... Create new AND condition
        DATA(lo_edit_join_cond_view) = NEW zcl_dbbr_edit_join_cond_view(
          iv_mode               = lv_condition_view_mode
          is_source_entity      = mo_join->get_entity( iv_alias = <ls_node>-alias )->get_tab_info( )
          it_target_entity_list = lt_entities
          iv_source_table       = lv_source_table
          if_allow_offset       = lf_virtual_join
        ).

*...... Set the node type to 'AND' for successive create calls from view
        mv_evt_filt_mode_node_type = c_node_type-table_filter.

        SET HANDLER:
           on_created_filter_condition FOR lo_edit_join_cond_view,
           on_created_field_condition FOR lo_edit_join_cond_view.

        lo_edit_join_cond_view->show( ).

        CHECK lo_edit_join_cond_view->was_not_cancelled( ).

*...... Retrieve the updated information from the view
        lo_edit_join_cond_view->get_updated_condition(
          IMPORTING
            es_field_condition  = DATA(ls_field_cond)
            es_filter_condition = DATA(ls_filter_cond)
        ).

*...... create the appropriate node
        IF lv_condition_view_mode = zcl_dbbr_edit_join_cond_view=>c_field_mode.
          create_field_condition_node(
              is_field_cond     = ls_field_cond
          ).
        ELSE.
          create_filter_condition_node(
              is_filter_condition = ls_filter_cond
          ).
        ENDIF.
      CATCH zcx_uitb_tree_error.

    ENDTRY.

  ENDMETHOD.


  METHOD add_new_join_table.
    DATA(lo_edit_join_table_controller) = NEW zcl_dbbr_edit_join_table_ctrl( ).

    SET HANDLER: on_created_join_table FOR lo_edit_join_table_controller.
    lo_edit_join_table_controller->zif_uitb_screen_controller~call_screen( ).

    CHECK lo_edit_join_table_controller->zif_uitb_screen_controller~was_not_cancelled( ).

    create_join_table_node( lo_edit_join_table_controller->get_updated_join_table( ) ).
  ENDMETHOD.


  METHOD add_or_condition.
    TRY .
*...... Get selected node
        DATA(lt_nodes) = mo_join_tree->get_selections( )->get_selected_nodes( ).
        CHECK lines( lt_nodes ) = 1.
        DATA(lo_selected_node) = lt_nodes[ 1 ].

*...... Retrieve information about node from map
        ASSIGN mt_node_map[ node_key = lo_selected_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node>).
        CHECK sy-subrc = 0.

*...... Check if adding or OR condition is allowed and determine some stuff
*...... for the new node
        CASE <ls_node>-node_type.

          WHEN c_node_type-or_filter_group.
            mv_new_cond_relationship = cl_tree_model=>relat_last_child.

          WHEN c_node_type-table_filter OR
               c_node_type-table_filter_or.
            mv_new_cond_relationship = cl_tree_model=>relat_next_sibling.

          WHEN OTHERS.
            RETURN.
        ENDCASE.

        ms_new_cond_relat_nodemap = <ls_node>.
        mo_relat_node = lo_selected_node.

*...... Create new OR condition
        DATA(lo_edit_join_cond_view) = NEW zcl_dbbr_edit_join_cond_view(
          iv_mode               = zcl_dbbr_edit_join_cond_view=>c_value_mode
          it_target_entity_list = mo_join->get_possible_entities_for_f4( <ls_node>-alias )
        ).

        SET HANDLER:
           on_created_filter_condition FOR lo_edit_join_cond_view.

*...... Set the node type to 'OR' for successive create calls from view
        mv_evt_filt_mode_node_type = c_node_type-table_filter_or.

        lo_edit_join_cond_view->show( ).

        CHECK lo_edit_join_cond_view->was_not_cancelled( ).

*...... Retrieve the updated information from the view
        lo_edit_join_cond_view->get_updated_condition(
          IMPORTING
            es_filter_condition = DATA(ls_filter_cond)
        ).

*...... Create the OR Group node if the target is and AND Filter Node
        IF <ls_node>-node_type = c_node_type-table_filter.
          create_or_group_node( ).
        ENDIF.

*...... Now create the OR filter condition
        create_filter_condition_node(
            is_filter_condition = ls_filter_cond
            iv_node_type        = c_node_type-table_filter_or
        ).
      CATCH zcx_uitb_tree_error.
    ENDTRY.
  ENDMETHOD.


  METHOD change_field_condition.
    TRY.
*...... fetch the node to get the information about the join field condition
        DATA(lo_node) = mo_join_tree->get_nodes( )->get_node( iv_node_key ).
        DATA(lo_join_field) = CAST lcl_join_field( lo_node->get_user_object( ) ).
        DATA(ls_join_field) = lo_join_field->get_field( ).

*...... get join table as well, to get virtual join information
        DATA(lo_join_table) = mo_join->get_entity( lo_join_field->mv_table_alias ).

*...... build target table list
        DATA(lt_entities) = mo_join->get_possible_entities_for_f4( lo_join_field->mv_table_alias ).
        DELETE lt_entities WHERE entity_alias = lo_join_field->mv_table_alias.

*...... change the join field information
        DATA(lo_edit_join_cond) = NEW zcl_dbbr_edit_join_cond_view(
            if_is_new             = abap_false
            if_allow_offset       = lo_join_table->get_tab_info( )-is_virtual
            is_source_entity      = lo_join_table->get_tab_info( )
            is_field_condition    = ls_join_field
            it_target_entity_list = lt_entities
        ).
        lo_edit_join_cond->show( ).

        CHECK lo_edit_join_cond->was_not_cancelled( ).
        lo_edit_join_cond->get_updated_condition( IMPORTING es_field_condition  = ls_join_field ).


*...... update the join field information
        lo_node->get_item( c_columns-field )->set_text( |{ ls_join_field-field }| ).
        lo_node->get_item( c_columns-comparator1 )->set_text( |{ ls_join_field-operator }| ).
        lo_node->get_item( c_columns-value )->set_text( |{ ls_join_field-ref_table_alias }-{ ls_join_field-ref_field }| ).

        CHECK lo_join_field->update_field( ls_join_field ).
        MESSAGE s088(zdbbr_info).
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
        lx_tree_error->print_message( iv_msg_type = 'I' ).
    ENDTRY.
  ENDMETHOD.


  METHOD change_filter_condition.
    TRY.
*...... fetch the node to get the information about the join filter condition
        DATA(lo_node) = mo_join_tree->get_nodes( )->get_node( iv_node_key ).
        DATA(lo_join_filter) = CAST lcl_join_filter( lo_node->get_user_object( ) ).
        DATA(ls_join_filter) = lo_join_filter->get_filter( ).

*...... build target table list
        DATA(lt_entities) = mo_join->get_possible_entities_for_f4( lo_join_filter->mv_table_alias ).

*...... change the join field information
        DATA(lo_edit_join_cond) = NEW zcl_dbbr_edit_join_cond_view(
            if_is_new             = abap_false
            iv_mode               = zcl_dbbr_edit_join_cond_view=>c_value_mode
            is_filter_condition   = ls_join_filter
            it_target_entity_list = lt_entities
        ).
        lo_edit_join_cond->show( ).

        CHECK lo_edit_join_cond->was_not_cancelled( ).
        lo_edit_join_cond->get_updated_condition( IMPORTING es_filter_condition = ls_join_filter ).

        CHECK lo_join_filter->update_filter( ls_join_filter ).
        fill_value_item_content(
          io_node             = lo_node
          io_filter_condition = lo_join_filter
        ).

*...... update the join field information
        lo_node->get_item( c_columns-field )->set_text( |{ ls_join_filter-tabname_alias }-{ ls_join_filter-fieldname }| ).
        lo_node->get_item( c_columns-comparator1 )->set_text( |{ ls_join_filter-operator }| ).

        MESSAGE |Filter Condition was updated| TYPE 'S'.
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
        lx_tree_error->print_message( iv_msg_type = 'I' ).
    ENDTRY.
  ENDMETHOD.


  METHOD change_join_table.
    TRY.
*...... 1) fetch the node to get the information about the join table
        DATA(lo_node) = mo_join_tree->get_nodes( )->get_node( iv_node_key ).
        DATA(lo_join_table) = CAST lcl_join_table( lo_node->get_user_object( ) ).

*...... 2) change the join table
        DATA(ls_current_tab_info) = lo_join_table->get_tab_info( ).
        DATA(lo_edit_join_table_ctrl) = NEW zcl_dbbr_edit_join_table_ctrl(
            is_join_table = ls_current_tab_info
            if_is_new     = abap_false
        ).
        lo_edit_join_table_ctrl->zif_uitb_screen_controller~call_screen( ).

        CHECK lo_edit_join_table_ctrl->zif_uitb_screen_controller~was_not_cancelled( ).
        DATA(ls_updated_tab_info) = lo_edit_join_table_ctrl->get_updated_join_table( ).

        CHECK lo_join_table->set_tab_info( CORRESPONDING #( ls_updated_tab_info ) ).

        IF ls_updated_tab_info-add_table_alias <> ls_current_tab_info-add_table_alias.
          update_table_alias( EXPORTING iv_old_alias = ls_current_tab_info-add_table_alias
                                        iv_new_alias = ls_updated_tab_info-add_table_alias ).
        ENDIF.

*...... Update field conditions if virtual join was activated
        IF ls_updated_tab_info-is_virtual = abap_false AND
           ls_current_tab_info-is_virtual = abap_true.
*........ clear all offset information from associated field conditions
          lo_join_table->clear_offset_from_fields( ).
        ENDIF.

*...... update the join type
        lo_node->get_item( c_columns-value )->set_text(
          COND #( WHEN ls_updated_tab_info-is_virtual = abap_true THEN
                    |{ 'Virtual'(018) } |
                  ELSE
                    || ) &&
          st_join_type_fixvals[ low = ls_updated_tab_info-join_type ]-ddtext
        ).
        lo_node->get_item( c_columns-hier2 )->set_text( |({ to_lower( ls_updated_tab_info-add_table_alias ) })| ).

        MESSAGE |Join Table { ls_updated_tab_info-add_table } was updated| TYPE 'S'.
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
        lx_tree_error->print_message( iv_msg_type = 'I' ).
    ENDTRY.
  ENDMETHOD.


  METHOD change_node.
    TRY.

        DATA(lt_selected_nodes) = mo_join_tree->get_selections( )->get_selected_nodes( ).
        CHECK lines( lt_selected_nodes ) = 1.
        DATA(lo_selected_node) = lt_selected_nodes[ 1 ].

*...... retrieve information about node from map
        ASSIGN mt_node_map[ node_key = lo_selected_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node>).
        CHECK sy-subrc = 0.

*...... perform appropriate edit action according to node type
        CASE <ls_node>-node_type.

          WHEN c_node_type-table_filter OR
               c_node_type-table_filter_or.
            change_filter_condition( iv_node_key =  <ls_node>-node_key ).

          WHEN c_node_type-table_field.
            change_field_condition( iv_node_key =  <ls_node>-node_key ).

          WHEN c_node_type-table.
            change_join_table( iv_node_key = <ls_node>-node_key ).

        ENDCASE.

      CATCH zcx_uitb_tree_error.
    ENDTRY.
  ENDMETHOD.


  METHOD create_columns.
    DATA(lo_cols) = mo_join_tree->get_columns( ).
    lo_cols->add_hierarchy_column( iv_colname = c_columns-hier2 ).
    lo_cols->add_column(
        iv_colname           = c_columns-field
        iv_width             = 40
        iv_header_text       = 'Field / Description'(002)
    ).
    lo_cols->add_column(
        iv_colname           = c_columns-comparator1
        iv_width             = 15
        iv_alignment         = cl_item_tree_model=>align_center
        iv_header_text       = 'Comparator'(003)
    ).
    lo_cols->add_column(
        iv_colname           = c_columns-value
        iv_width             = 40
        iv_header_text       = 'Field2 / Value / Join Type'(004)
    ).
    lo_cols->add_column(
        iv_colname           = c_columns-comparator2
        iv_width             = 20
        iv_alignment         = cl_item_tree_model=>align_center
        iv_header_text       = 'Comparator 2'(005)
    ).
    lo_cols->add_column(
        iv_colname           = c_columns-value2
        iv_width             = 40
        iv_header_text       = 'Value2'(006)
    ).
  ENDMETHOD.


  METHOD create_field_condition_node.
    DATA(lo_new_field_cond) = NEW lcl_join_field(
        is_field    = is_field_cond
        iv_table    = ms_new_cond_relat_nodemap-tabname
        iv_alias    = ms_new_cond_relat_nodemap-alias
        iv_node_key = get_next_key( )
    ).

    DATA(lv_and_text) = |AND|.

    IF ms_new_cond_relat_nodemap-node_type = c_node_type-table_fields.
      IF mo_relat_node IS BOUND AND NOT mo_relat_node->has_children( ).
        CLEAR lv_and_text.
      ENDIF.
    ENDIF.

    TRY.
        DATA(lo_new_field_cond_node) = mo_join_tree->get_nodes( )->add_node(
            iv_node_key          = lo_new_field_cond->mv_node_key
            iv_relationship      = mv_new_cond_relationship
            iv_relative_node_key = ms_new_cond_relat_nodemap-node_key
            if_folder            = abap_false
            ir_user_object       = lo_new_field_cond
            iv_image             = |{ icon_io_predefined_value }|
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = lv_and_text )
              ( item_name = c_columns-field
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = is_field_cond-field )
              ( item_name = c_columns-comparator1
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = is_field_cond-operator )
              ( item_name = c_columns-value
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = |{ is_field_cond-ref_table_alias }-{ is_field_cond-ref_field }| )
            )
        ).

*...... Build the node map entry for the new table
        DATA(ls_field_node_map) = VALUE lty_s_node_map(
          node_key  = lo_new_field_cond->mv_node_key
          tabname   = ms_new_cond_relat_nodemap-tabname
          alias     = ms_new_cond_relat_nodemap-alias
          node_type = c_node_type-table_field
        ).
        mt_node_map = VALUE #( BASE mt_node_map ( ls_field_node_map ) ).

*...... expand all nodes of new table node
        IF mv_new_cond_relationship = cl_tree_model=>relat_last_child.
          mo_join_tree->get_nodes( )->expand_node(
              iv_node_key            = ms_new_cond_relat_nodemap-node_key
              if_expand_subtree      = abap_true
          ).
        ENDIF.

*...... add the new condition to the join table
        DATA(lo_join_table) = mo_join->get_entity( ms_new_cond_relat_nodemap-alias ).
        lo_join_table->add_field(
            io_field          = lo_new_field_cond
            io_field_node     = lo_new_field_cond_node
        ).

*...... update node map entry and relation ship for next new entry
        IF if_from_event = abap_true.
          ms_new_cond_relat_nodemap = ls_field_node_map.
          mv_new_cond_relationship = cl_tree_model=>relat_next_sibling.
          mo_relat_node = lo_new_field_cond_node.
        ELSE.
          CLEAR: mv_new_cond_relationship,
                 mo_relat_node,
                 ms_new_cond_relat_nodemap.
        ENDIF.

        rv_node_key = lo_new_field_cond->mv_node_key.

        IF mf_init_mode = abap_false.
          MESSAGE |Field Condition was created| TYPE 'S'.
          mo_join_tree->get_selections( )->select_nodes( VALUE #( ( rv_node_key ) ) ).
        ENDIF.
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
    ENDTRY.
  ENDMETHOD.


  METHOD create_filter_condition_node.
    DATA(lo_new_filter_cond) = NEW lcl_join_filter(
         is_filter   = is_filter_condition
         iv_table    = ms_new_cond_relat_nodemap-tabname
         iv_alias    = ms_new_cond_relat_nodemap-alias
         iv_node_key = get_next_key( )
     ).

*.. set AND/OR according to requested node type
    DATA(lv_and_or) = COND #( WHEN iv_node_type = c_node_type-table_filter THEN and ELSE or ).
    lo_new_filter_cond->set_and_or( lv_and_or ).

    IF mo_relat_node IS BOUND AND
*       mv_new_cond_relationship <> cl_tree_model=>relat_last_child AND
       mo_relat_node->is_folder( ) AND
       NOT mo_relat_node->has_children( ).
      CLEAR lv_and_or.
    ENDIF.

    TRY.
        DATA(lo_new_filter_cond_node) = mo_join_tree->get_nodes( )->add_node(
            iv_node_key          = lo_new_filter_cond->mv_node_key
            iv_relationship      = mv_new_cond_relationship
            iv_relative_node_key = ms_new_cond_relat_nodemap-node_key
            if_folder            = abap_false
            ir_user_object       = lo_new_filter_cond
            iv_image             = |{ icon_io_predefined_value }|
            it_item_table        = VALUE treemcitab(
               ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                 class     = cl_column_tree_model=>item_class_text
                 font      = cl_item_tree_model=>item_font_prop
                 text      = lv_and_or )
               ( item_name = c_columns-field
                 class     = cl_column_tree_model=>item_class_text
                 font      = cl_item_tree_model=>item_font_prop
                 text      = |{ is_filter_condition-tabname_alias }-{ is_filter_condition-fieldname }| )
               ( item_name = c_columns-comparator1
                 class     = cl_column_tree_model=>item_class_text
                 font      = cl_item_tree_model=>item_font_prop
                 text      = is_filter_condition-operator )
               ( item_name = c_columns-value
                 class     = cl_column_tree_model=>item_class_text
                 font      = cl_item_tree_model=>item_font_prop
                 text      = '' )
               ( item_name = c_columns-comparator2
                 class     = cl_column_tree_model=>item_class_text
                 font      = cl_item_tree_model=>item_font_prop
                 text      = ''  )
               ( item_name = c_columns-value2
                 class     = cl_column_tree_model=>item_class_text
                 font      = cl_item_tree_model=>item_font_prop
                 text      = '' )
            )
        ).

*...... update or group for filter ( if necessary )
        update_or_group_of_filter(
          io_filter_node = lo_new_filter_cond_node
          iv_node_type   = iv_node_type
        ).
        fill_value_item_content( io_node             = lo_new_filter_cond_node
                                 io_filter_condition = lo_new_filter_cond      ).

*...... Build the node map entry for the new filter condition
        DATA(ls_field_node_map) = VALUE lty_s_node_map(
          node_key  = lo_new_filter_cond->mv_node_key
          tabname   = ms_new_cond_relat_nodemap-tabname
          alias     = ms_new_cond_relat_nodemap-alias
          node_type = iv_node_type
        ).
        mt_node_map = VALUE #( BASE mt_node_map ( ls_field_node_map ) ).

*...... expand relative node where this filter condition node will be added to
        IF mv_new_cond_relationship = cl_tree_model=>relat_last_child.
          mo_join_tree->get_nodes( )->expand_node(
              iv_node_key            = ms_new_cond_relat_nodemap-node_key
              if_expand_subtree      = abap_true
          ).
        ENDIF.

*...... add the new condition to the join table
        DATA(lo_join_table) = mo_join->get_entity( ms_new_cond_relat_nodemap-alias ).
*        determine_predecessor_
        lo_join_table->add_filter(
            io_filter         = lo_new_filter_cond
            io_filter_node    = lo_new_filter_cond_node
        ).

*...... update node map entry and relation ship for next new entry
        IF if_from_event = abap_true.
          ms_new_cond_relat_nodemap = ls_field_node_map.
          mv_new_cond_relationship = cl_tree_model=>relat_next_sibling.
          mo_relat_node = lo_new_filter_cond_node.
        ELSE.
          CLEAR: mv_new_cond_relationship,
                 mo_relat_node,
                 ms_new_cond_relat_nodemap.
        ENDIF.

        rv_node_key = lo_new_filter_cond->mv_node_key.

        IF mf_init_mode = abap_false.
          MESSAGE |Filter Condition was created| TYPE 'S'.
          mo_join_tree->get_selections( )->select_nodes( VALUE #( ( rv_node_key ) ) ).
        ENDIF.
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
    ENDTRY.
  ENDMETHOD.

  METHOD create_join_table_node.
    DATA: lv_node_key TYPE tm_nodekey.

*... Create new join table entry
    DATA(lo_new_join_table) = NEW lcl_join_table(
        is_join_table = CORRESPONDING #( is_join_table )
        iv_node_key   = get_next_key( )
    ).

    SET HANDLER on_delete_request_from_join FOR lo_new_join_table.

    mo_join->add_table( lo_new_join_table ).

    TRY.
        mo_join_tree->get_nodes( )->add_node(
            iv_node_key          = lo_new_join_table->mv_node_key
            if_folder            = abap_true
            ir_user_object       = lo_new_join_table
            iv_image             = zcl_dbbr_tree_helper=>get_tree_node_icon( lo_new_join_table->mv_type )
            iv_expanded_image    = zcl_dbbr_tree_helper=>get_tree_node_icon( lo_new_join_table->mv_type )
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = |{ is_join_table-add_table_raw }| )
              ( item_name = c_columns-hier2
                class     = cl_column_tree_model=>item_class_text
                text      = |({ to_lower( is_join_table-add_table_alias ) })|
                style     = zif_uitb_c_ctm_style=>inverted_blue
                font      = cl_item_tree_model=>item_font_prop )
              ( item_name = c_columns-field
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = is_join_table-table_name )
              ( item_name = c_columns-value
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = COND #( WHEN is_join_table-is_virtual = abap_true THEN
                                      |{ 'Virtual'(018) } |
                                    ELSE
                                      || ) &&
                            st_join_type_fixvals[ low = is_join_table-join_type ]-ddtext
              )
            )
        ).
*...... Build the node map entry for the new table
        mt_node_map = VALUE #( BASE mt_node_map
          ( node_key  = lo_new_join_table->mv_node_key
            tabname   = is_join_table-add_table
            alias     = is_join_table-add_table_alias
            node_type = c_node_type-table            )
        ).
*...... Add sub nodes for field conditions
        lv_node_key = get_next_key( ).
        mo_join_tree->get_nodes( )->add_node(
            iv_node_key          = lv_node_key
            iv_relative_node_key = lo_new_join_table->mv_node_key
            if_folder            = abap_true
            iv_image             = |{ icon_ps_relationship }|
            iv_expanded_image    = |{ icon_ps_relationship }|
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = |{ 'Field Conditions'(011) }| )
            )
        ).
*...... Build the node map entry for the field conditions node of the table
        mt_node_map = VALUE #( BASE mt_node_map
          ( node_key  = lv_node_key
            tabname   = is_join_table-add_table
            alias     = is_join_table-add_table_alias
            node_type = c_node_type-table_fields       )
        ).
*...... Add sub node for filter conditions of table
        lv_node_key = get_next_key( ).
        mo_join_tree->get_nodes( )->add_node(
            iv_node_key          = lv_node_key
            iv_relative_node_key = lo_new_join_table->mv_node_key
            if_folder            = abap_true
            iv_image             = |{ icon_select_with_condition }|
            iv_expanded_image    = |{ icon_select_with_condition }|
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = |{ 'Filter Conditions'(012) }| )
            )
        ).
*...... Build the node map entry for the field conditions node of the table
        mt_node_map = VALUE #( BASE mt_node_map
          ( node_key  = lv_node_key
            tabname   = is_join_table-add_table
            alias     = is_join_table-add_table_alias
            node_type = c_node_type-table_filters  )
        ).
*...... expand all nodes of new table node
        mo_join_tree->get_nodes( )->expand_node(
            iv_node_key            = lo_new_join_table->mv_node_key
            if_expand_subtree      = abap_true
        ).

        rv_node_key = lo_new_join_table->mv_node_key.

        IF mf_init_mode = abap_false.
          MESSAGE |Join Table { is_join_table-add_table } was created| TYPE 'S'.
          mo_join_tree->get_selections( )->select_nodes( VALUE #( ( rv_node_key ) ) ).
        ENDIF.
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
    ENDTRY.
  ENDMETHOD.


  METHOD create_or_group_node.
    DATA(lv_and_text) = and.
    TRY .
        DATA(lf_first_child) = mo_relat_node->is_first_child( ).
*...... if relative node is first child of it's parent then the AND text is
*...... unecessary as the OR Group node will become the first child
        IF mv_new_cond_relationship = cl_tree_model=>relat_next_sibling AND
           lf_first_child = abap_true.
          CLEAR lv_and_text.
        ENDIF.

        rv_node_key = get_next_key( ).
        DATA(lo_or_group_node) = mo_join_tree->get_nodes( )->add_node(
            iv_node_key          = rv_node_key
            iv_relationship      = mv_new_cond_relationship
            iv_relative_node_key = ms_new_cond_relat_nodemap-node_key
            if_folder            = abap_true
            iv_image             = |{ icon_tree }|
            iv_expanded_image    = |{ icon_tree }|
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = lv_and_text )
            )
        ).
*...... Create node map entry for this node
        DATA(ls_or_group_nm) = VALUE lty_s_node_map(
          node_key  = rv_node_key
          node_type = c_node_type-or_filter_group
          tabname   = ms_new_cond_relat_nodemap-tabname
          alias     = ms_new_cond_relat_nodemap-alias
        ).
        mt_node_map = VALUE #( BASE mt_node_map ( ls_or_group_nm ) ).

*...... check if OR Group was created on top of Table filter, then
*...... the table filter has to be moved under the new or group
        IF ms_new_cond_relat_nodemap-node_type = c_node_type-table_filter.
          IF lf_first_child = abap_false.
            mo_relat_node->get_item( mo_join_tree->c_hierarchy_column )->set_text( '' ).
          ENDIF.

          mo_relat_node->move_node_to(
            iv_relative_node = rv_node_key
            iv_relationship  = cl_tree_model=>relat_first_child
          ).
          update_or_group_of_filter(
              io_filter_node      = mo_relat_node
              iv_or_group_node    = rv_node_key
              iv_node_type        = c_node_type-table_filter_or
          ).
*........ Now the former AND filter was transfered to an OR Filter, this has to be
*........ changed in the user object and the node map entry
          ASSIGN mt_node_map[ node_key = mo_relat_node->mv_node_key ] TO FIELD-SYMBOL(<ls_and_filter>).
          <ls_and_filter>-node_type = c_node_type-table_filter_or.
          CAST lcl_join_filter( mo_relat_node->get_user_object( ) )->set_and_or( or ).
          mo_relat_node->get_item( mo_join_tree->c_hierarchy_column )->set_text( '' ).
        ENDIF.

*........ Update relative node information
        ms_new_cond_relat_nodemap = ls_or_group_nm.
        mv_new_cond_relationship = cl_tree_model=>relat_last_child.
        mo_relat_node = lo_or_group_node.
      CATCH zcx_uitb_tree_error.
    ENDTRY.
  ENDMETHOD.


  METHOD create_tree.
*.... create the tree model for the association tree
    mo_join_tree = NEW #(
        ir_parent           = io_container
        is_hierarchy_header = VALUE treemhhdr(
            heading = 'Table/Conditions'(001)
            width   = 60
        )
    ).

    DATA(lo_events) = mo_join_tree->get_events( ).

    SET HANDLER:
      on_node_double_click      FOR lo_events,
      on_node_keypress          FOR lo_events,
      on_node_ctx_menu_request  FOR lo_events,
      on_node_ctx_menu_select   FOR lo_events.

    TRY.
        create_columns( ).

        lo_events->add_key_for_keypress( iv_key =  cl_tree_model=>key_delete ).

        mo_join_tree->create_tree_control( ).
        mo_join_tree->update_view( ).
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
        lx_tree_error->print_message( ).
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_all_conditions.
    DATA: lt_node_type_to_delete TYPE lty_t_node_type_range.

    DATA(lo_join_table) = mo_join->get_entity( is_node_map-alias ).

    CASE is_node_map-node_type.

      WHEN c_node_type-table_fields.

        IF io_node->has_children( ) AND
               zcl_dbbr_appl_util=>popup_to_confirm(
                  if_suppress_query        = mf_no_query_on_deletion
                  iv_title                 = 'Delete all?'
                  iv_query                 = |Are you sure you want to delete all Field Conditions | &&
                                             | for Table { is_node_map-tabname }?|
                  iv_icon_type             = 'ICON_MESSAGE_QUESTION'
              ) = '1'.
          lt_node_type_to_delete = VALUE #( ( sign = 'I' option = 'EQ' low = c_node_type-table_field ) ).
          lo_join_table->delete_fields( ).
        ELSE.
          RETURN.
        ENDIF.

      WHEN c_node_type-table_filters.
        IF io_node->has_children( ) AND
            zcl_dbbr_appl_util=>popup_to_confirm(
               if_suppress_query        = mf_no_query_on_deletion
               iv_title                 = 'Delete all?'
               iv_query                 = |Are you sure you want to delete all Filter Conditions | &&
                                          | for Table { is_node_map-tabname }?|
               iv_icon_type             = 'ICON_MESSAGE_QUESTION'
           ) = '1'.
          lo_join_table->delete_filters( ).
          lt_node_type_to_delete = VALUE #(
            ( sign = 'I' option = 'EQ' low = c_node_type-table_filter )
            ( sign = 'I' option = 'EQ' low = c_node_type-table_filter_or )
            ( sign = 'I' option = 'EQ' low = c_node_type-or_filter_group )
          ).
        ELSE.
          RETURN.
        ENDIF.
    ENDCASE.

    io_node->delete_children( ).

*.. Delete all child nodes and all node map entries for the given type
    DELETE mt_node_map WHERE alias     = is_node_map-alias
                         AND node_type IN lt_node_type_to_delete.
  ENDMETHOD.


  METHOD delete_condition.
    DATA: lv_condition_view_mode TYPE i.

    TRY .
*...... Get selected node
        DATA(lt_nodes) = mo_join_tree->get_selections( )->get_selected_nodes( ).
        CHECK lines( lt_nodes ) = 1.
        DATA(lo_selected_node) = lt_nodes[ 1 ].

*...... Retrieve information about node from map
        ASSIGN mt_node_map[ node_key = lo_selected_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node>).
        CHECK sy-subrc = 0.

*........ Get current owner join table for condition
        DATA(lo_join_table) = mo_join->get_entity( <ls_node>-alias ).

*...... popup to confirm deletion ???
*...... Delete the condition
        CASE <ls_node>-node_type.

          WHEN c_node_type-table_filter OR
               c_node_type-table_filter_or.
            lo_join_table->delete_filter( <ls_node>-node_key ).
            delete_filter_condition(
                io_node       = lo_selected_node
                io_join_table = lo_join_table
                is_node_map   = <ls_node>
            ).

          WHEN c_node_type-table_field.
            delete_field_condition(
                io_node       = lo_selected_node
                io_join_table = lo_join_table
                is_node_map   = <ls_node>
            ).

          WHEN c_node_type-table_fields OR
               c_node_type-table_filters.
            delete_all_conditions(
                io_node             = lo_selected_node
                io_join_table       = lo_join_table
                is_node_map         = <ls_node>
            ).

          WHEN c_node_type-or_filter_group.
            delete_or_group(
              io_node       = lo_selected_node
              io_join_table = lo_join_table
            ).

          WHEN OTHERS.
        ENDCASE.
      CATCH zcx_uitb_tree_error.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_field_condition.
    io_join_table->delete_field( is_node_map-node_key ).

*.. Reset the 'AND' text of the node if necessary
    DATA(lo_parent) = io_node->get_parent( ).
    DATA(lo_first_child) = lo_parent->get_first_child( ).
    IF io_node->mv_node_key = lo_first_child->mv_node_key AND
       lo_parent->get_nr_of_children( ) > 1.
      lo_first_child->get_next_sibling( )->get_item(
        zcl_uitb_column_tree_model=>c_hierarchy_column
      )->set_text( '' ).
    ENDIF.

    delete_node_and_select_next(
      io_node     = io_node
      is_node_map = is_node_map
    ).
  ENDMETHOD.


  METHOD delete_filter_condition.
    io_join_table->delete_filter( is_node_map-node_key ).

*.. Reset the 'OR/AND' text of the node if necessary
    DATA(lo_parent) = io_node->get_parent( ).
    DATA(lo_first_child) = lo_parent->get_first_child( ).
    IF io_node->mv_node_key = lo_first_child->mv_node_key AND
       lo_parent->get_nr_of_children( ) > 1.
      lo_first_child->get_next_sibling( )->get_item(
        zcl_uitb_column_tree_model=>c_hierarchy_column
      )->set_text( '' ).
    ENDIF.

    delete_node_and_select_next(
      io_node     = io_node
      is_node_map = is_node_map
    ).
  ENDMETHOD.


  METHOD delete_node_and_select_next.
    DATA(lf_relat_is_sibling) = abap_true.

*.. first get previous sibling to update selection after node was deleted
    DATA(lo_next_node) = io_node->get_next_relat_node( IMPORTING ef_is_sibling = lf_relat_is_sibling ).

*.. delete the node
    mo_join_tree->get_nodes( )->delete_node( is_node_map-node_key ).
*.. the node map entry also has to be deleted
    DELETE mt_node_map WHERE node_key = is_node_map-node_key.

*.. Sibling was found, so just select the sibling
    IF lf_relat_is_sibling = abap_false.
*.. Check if the new node is an OR group, then obviously all children of this
*.. have been deleted and the group itself has to be deleted as well
      DATA(ls_new_node_map) = mt_node_map[ node_key = lo_next_node->mv_node_key ].
      IF ls_new_node_map-node_type = c_node_type-or_filter_group AND
         if_stop_recursion = abap_false.
*.... Call the same method again to delete the or group as well
        delete_node_and_select_next(
            io_node           = lo_next_node
            is_node_map       = ls_new_node_map
            if_stop_recursion = abap_true
        ).
      ENDIF.
    ELSE.
*.... update selection
      mo_join_tree->get_selections( )->select_nodes( VALUE #( ( lo_next_node->mv_node_key ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_or_group.
    DATA: lt_node_type_to_delete TYPE lty_t_node_type_range.

    CHECK zcl_dbbr_appl_util=>popup_to_confirm(
        if_suppress_query        = mf_no_query_on_deletion
        iv_title                 = 'Delete OR Group?'
        iv_query                 = |Are you sure you want to delete the selected OR Group | &&
                                   | and all its child nodes?|
        iv_icon_type             = 'ICON_MESSAGE_QUESTION'
    ) = '1'.

    LOOP AT io_node->get_children( ) INTO DATA(lo_or_filter_node).
      io_join_table->delete_filter( lo_or_filter_node->mv_node_key ).
      DELETE mt_node_map WHERE node_key = lo_or_filter_node->mv_node_key.
    ENDLOOP.

    DELETE mt_node_map WHERE node_key = io_node->mv_node_key.

    io_node->delete_children( ).

    DATA(lo_filters_node) = io_node->get_parent( ).
    IF lo_filters_node->get_nr_of_children( ) = 2 AND
       io_node->is_first_child( ).
*.... set 'AND' text to '' as after this group is deleted the other one will
*.... be the only remaining filter node
      DATA(lt_children) = lo_filters_node->get_children( ).
      DATA(lo_2nd_child) = lt_children[ 2 ].
      lo_2nd_child->get_item( mo_join_tree->c_hierarchy_column )->set_text( '' ).
    ENDIF.

    DATA(lo_next_node) = io_node->get_next_relat_node( ).

*.. finally delete the node itself
    mo_join_tree->get_nodes( )->delete_node( io_node->mv_node_key ).

*.. Select the new node
    IF lo_next_node IS NOT INITIAL.
      mo_join_tree->get_selections( )->select_nodes( VALUE #( ( lo_next_node->mv_node_key ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_table.
    DATA: lv_condition_view_mode TYPE i.

    TRY .

        IF if_all = abap_true.
          CHECK mt_node_map IS NOT INITIAL.
          CHECK zcl_dbbr_appl_util=>popup_to_confirm(
             if_suppress_query  = mf_no_query_on_deletion
             iv_title           = |{ 'Delete all Join Tables?'(016) }|
             iv_query           = |Are you sure you want to delete all Join tables and all their | &&
                                  | field and filter conditions?|
             iv_icon_type       = 'ICON_MESSAGE_WARNING'
          ) = '1'.
          CLEAR: mt_node_map.
          mo_join->delete_all_tables( ).
          mo_join_tree->get_nodes( )->delete_all_nodes( ).
        ELSE.
*........ Get selected node
          DATA(lt_nodes) = mo_join_tree->get_selections( )->get_selected_nodes( ).
          CHECK lines( lt_nodes ) = 1.
          DATA(lo_selected_node) = lt_nodes[ 1 ].

*........ Retrieve information about node from map
          ASSIGN mt_node_map[ node_key = lo_selected_node->mv_node_key ] TO FIELD-SYMBOL(<ls_node>).
          CHECK sy-subrc = 0.

          CASE <ls_node>-node_type.

            WHEN c_node_type-table.
            WHEN OTHERS.
              RETURN.
          ENDCASE.
*........ popup to confirm deletion
          CHECK zcl_dbbr_appl_util=>popup_to_confirm(
             if_suppress_query = mf_no_query_on_deletion
             iv_title          = |{ 'Delete Join Table?'(014) }|
             iv_query          = |Should the Join Table { <ls_node>-tabname } really be deleted? | &&
                                 |All dependent filter and field conditions used in other Join Tables | &&
                                 |will be deleted as well!|
             iv_icon_type      = 'ICON_MESSAGE_WARNING'
          ) = '1'.
*........ Delete the table
          mo_join->delete_table( <ls_node>-tabname ).
*........ delete the node
          mo_join_tree->get_nodes( )->delete_node( <ls_node>-node_key ).
*........ the node map entry also has to be deleted
          DELETE mt_node_map WHERE node_key = <ls_node>-node_key.
        ENDIF.
      CATCH zcx_uitb_tree_error.
    ENDTRY.
  ENDMETHOD.

  METHOD fill_tree_with_join.
    FIELD-SYMBOLS: <ls_join_def> TYPE zdbbr_join_def.

    mf_init_mode = abap_true.

    ASSIGN mr_join_def->* TO <ls_join_def>.

*.. Register alias of primary table
    IF mo_join->mv_primary_entity_alias IS NOT INITIAL.
      zcl_dbbr_entity_alias_util=>add_entity_alias( mo_join->mv_primary_entity_alias ).
    ENDIF.

    LOOP AT <ls_join_def>-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>).
*.... Register existing aliases
      IF <ls_join_table>-add_table_alias IS NOT INITIAL.
        zcl_dbbr_entity_alias_util=>add_entity_alias( <ls_join_table>-add_table_alias ).
      ENDIF.

*.... Create node and objects for the join table
**... Fill description for entity
      TRY.
          DATA(ls_entity) = zcl_dbbr_dictionary_helper=>get_entity( <ls_join_table>-add_table ).
          <ls_join_table>-table_name = ls_entity-description.
          <ls_join_table>-add_table_raw = ls_entity-entity_id_raw.
        CATCH zcx_dbbr_data_read_error INTO DATA(lx_error).
          lx_error->zif_dbbr_exception_message~print( ).
      ENDTRY.

      DATA(lv_new_join_tab_key) = create_join_table_node( is_join_table = CORRESPONDING #( <ls_join_table> ) ).

*..... Create field conditions
*..... get node map entry for join tables fields node
      ASSIGN mt_node_map[ alias     = <ls_join_table>-add_table_alias
                          node_type = c_node_type-table_fields ] TO FIELD-SYMBOL(<ls_join_tab_fields_node>).

      mv_new_cond_relationship = cl_tree_model=>relat_last_child.
      ms_new_cond_relat_nodemap = <ls_join_tab_fields_node>.
      mo_relat_node = mo_join_tree->get_nodes( )->get_node( <ls_join_tab_fields_node>-node_key ).

      LOOP AT <ls_join_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_join_field>).

        create_field_condition_node(
            is_field_cond = <ls_join_field>
            if_from_event = abap_true
        ).
      ENDLOOP.

*..... Create filter conditions
*..... get node map entry for join table's filters node
      ASSIGN mt_node_map[ alias     = <ls_join_table>-add_table_alias
                          node_type = c_node_type-table_filters ] TO FIELD-SYMBOL(<ls_join_tab_filters_node>).

      DATA(lf_group_created) = abap_false.
      DATA: lv_node_type       TYPE lty_node_type,
            lv_previous_and_or TYPE vsconj.

      DATA(lo_join_tab_filters_node) = mo_join_tree->get_nodes( )->get_node( <ls_join_tab_filters_node>-node_key ).
      mv_new_cond_relationship = cl_tree_model=>relat_last_child.
      ms_new_cond_relat_nodemap = <ls_join_tab_filters_node>.
      mo_relat_node = lo_join_tab_filters_node.

      LOOP AT <ls_join_table>-filter_conditions ASSIGNING FIELD-SYMBOL(<ls_join_filter>).

        IF <ls_join_filter>-and_or = or.
          lv_node_type = c_node_type-table_filter_or.
          IF lf_group_created = abap_false.
*.......... reset filter to add OR Group as Last Child as last child
            mv_new_cond_relationship = cl_tree_model=>relat_last_child.
            ms_new_cond_relat_nodemap = <ls_join_tab_filters_node>.
            mo_relat_node = lo_join_tab_filters_node.

            create_or_group_node( ).
            lf_group_created = abap_true.
          ENDIF.
        ELSEIF <ls_join_filter>-and_or = and.
          IF lf_group_created = abap_true.
            lv_node_type = c_node_type-table_filter_or.
          ELSE.
*.......... reset filter to add AND as last child
            mv_new_cond_relationship = cl_tree_model=>relat_last_child.
            ms_new_cond_relat_nodemap = <ls_join_tab_filters_node>.
            mo_relat_node = lo_join_tab_filters_node.

            lv_node_type = c_node_type-table_filter.
          ENDIF.
          CLEAR lf_group_created.
        ELSE.
*......... this could only be the last filter entry of the table
          IF lv_previous_and_or IS NOT INITIAL.
            IF lv_previous_and_or = or.
              lv_node_type = c_node_type-table_filter_or.
            ELSE.
              lv_node_type = c_node_type-table_filter.
            ENDIF.
          ELSE.
            lv_node_type = c_node_type-table_filter.
          ENDIF.
        ENDIF.

        create_filter_condition_node(
            is_filter_condition = <ls_join_filter>
            if_from_event       = abap_true
            iv_node_type        = lv_node_type
        ).

        lv_previous_and_or = <ls_join_filter>-and_or.
      ENDLOOP.
    ENDLOOP.

    CLEAR mf_init_mode.
*.. clear all changes flag of the join
    mo_join->clear_changed_flag( ).
  ENDMETHOD.


  METHOD fill_value_item_content.
    DATA: lv_value1 TYPE zdbbr_value,
          lv_value2 TYPE zdbbr_value.

    DATA(ls_filter) = io_filter_condition->get_filter( ).

    TRY.

        DATA(lo_value1_item) = io_node->get_item( c_columns-value ).
        lo_value1_item->set_style( zif_uitb_c_ctm_style=>default ).

        DATA(lo_value2_item) = io_node->get_item( c_columns-value2 ).
        lo_value2_item->set_style( zif_uitb_c_ctm_style=>default ).

        DATA(lo_comparator2_item) = io_node->get_item( c_columns-comparator2 ).

        IF ls_filter-operator = zif_dbbr_c_operator=>between.
          lo_comparator2_item->set_text( 'and' ).
        ELSE.
          lo_comparator2_item->set_text( '' ).
        ENDIF.

        CASE ls_filter-value_type.

          WHEN zif_dbbr_c_join_cond_val_type=>typed_input.
            lv_value1 = ls_filter-value.

            IF ls_filter-operator = zif_dbbr_c_operator=>between.
              lv_value2 = ls_filter-value2.
            ENDIF.

*.......... Convert into display format for better readability
            zcl_dbbr_data_converter=>convert_selopt_to_disp_format(
              EXPORTING
                iv_tabname   = ls_filter-tabname
                iv_fieldname = ls_filter-fieldname
              CHANGING
                cv_value1    = lv_value1
                cv_value2    = lv_value2
            ).

            lo_value1_item->set_text( |'{ lv_value1 }'| ).
            IF ls_filter-operator = zif_dbbr_c_operator=>between.
              lo_value2_item->set_text( |'{ lv_value2 }'| ).
            ELSE.
              lo_value2_item->set_text( || ).
            ENDIF.

          WHEN zif_dbbr_c_join_cond_val_type=>system_value_input.
            lo_value2_item->set_text( '' ).
*.......... Display the current system value

            lo_value1_item->set_text(
              |{ ls_filter-value } | &&
              |({ zcl_dbbr_system_helper=>get_disp_val_for_system_va( |{ ls_filter-value }| ) })|
            ).
            lo_value1_item->set_style( zif_uitb_c_ctm_style=>light_yellow ).

        ENDCASE.

      CATCH zcx_uitb_tree_error.
    ENDTRY.
  ENDMETHOD.


  METHOD get_next_key.
    ADD 1 TO mv_current_key_count.
    result = |{ mv_current_key_count }|.
  ENDMETHOD.


  METHOD on_created_field_condition.
    CHECK es_field IS NOT INITIAL.

    create_field_condition_node(
      if_from_event = abap_true
      is_field_cond = es_field
    ).
  ENDMETHOD.


  METHOD on_created_filter_condition.
    CHECK es_filter IS NOT INITIAL.

    create_filter_condition_node(
      if_from_event       = abap_true
      is_filter_condition = es_filter
      iv_node_type        = mv_evt_filt_mode_node_type
    ).
  ENDMETHOD.


  METHOD on_created_join_table.
    CHECK es_join_table IS NOT INITIAL.

    create_join_table_node( es_join_table ).
  ENDMETHOD.


  METHOD on_delete_request_from_join.
    CHECK ev_node_key IS NOT INITIAL.

    TRY .
        mo_join_tree->get_nodes( )->delete_node( ev_node_key ).
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
        lx_tree_error->print_message(
            iv_msg_type        = 'I'
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD on_node_ctx_menu_request.
    DATA: BEGIN OF ls_flags,
            add_or      TYPE abap_bool,
            add_and     TYPE abap_bool,
            edit        TYPE abap_bool,
            edit_text   TYPE string VALUE 'Edit Condition',
            delete      TYPE abap_bool,
            delete_func TYPE ui_func VALUE c_functions-delete_condition,
            delete_text TYPE string VALUE 'Delete Condition',
          END OF ls_flags.

    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    CHECK sy-subrc = 0.

    DATA(lo_node) = mo_join_tree->get_nodes( )->get_node( <ls_node_map>-node_key ).

    CASE <ls_node_map>-node_type.

      WHEN c_node_type-or_filter_group.
        ls_flags-add_or = abap_true.
        ls_flags-add_and = abap_true.
        ls_flags-delete = abap_true.

      WHEN c_node_type-table_field.
        ls_flags-add_and = abap_true.
        ls_flags-add_or = abap_true.
        ls_flags-edit = abap_true.
        ls_flags-delete = abap_true.

      WHEN c_node_type-table_fields.
        ls_flags-add_and = abap_true.
        ls_flags-delete = lo_node->has_children( ).
        ls_flags-delete_text = 'Delete all Field Conditions'.

      WHEN c_node_type-table_filter_or.
        ls_flags-add_or = abap_true.
        ls_flags-edit = abap_true.
        ls_flags-delete = abap_true.

      WHEN c_node_type-table_filter.
        ls_flags-add_and = abap_true.
        ls_flags-add_or = abap_true.
        ls_flags-delete = abap_true.
        ls_flags-edit = abap_true.

      WHEN c_node_type-table_filters.
        ls_flags-add_and = abap_true.
        ls_flags-delete = lo_node->has_children( ).
        ls_flags-delete_text = 'Delete All Filter Condition'.

      WHEN c_node_type-table.
        ls_flags-delete = abap_true.
        ls_flags-delete_text = 'Delete Table'.
        ls_flags-delete_func = c_functions-delete_table.
        ls_flags-edit = abap_true.
        ls_flags-edit_text = 'Edit Table'.
      WHEN OTHERS.
    ENDCASE.


    IF ls_flags-edit = abap_true.
      er_menu->add_function(
          fcode = c_functions-edit_condition
          text  = |{ ls_flags-edit_text }|
      ).
      er_menu->add_separator( ).
    ENDIF.

    IF ls_flags-add_and = abap_true OR ls_flags-add_or = abap_true.
      IF ls_flags-add_and = abap_true.
        er_menu->add_function(
            fcode = c_functions-add_and_condition
            text  = 'Add AND Condition'
        ).
      ENDIF.
      IF ls_flags-add_or = abap_true.
        er_menu->add_function(
            fcode = c_functions-add_or_condition
            text  = 'Add OR Condition'
        ).
      ENDIF.
      er_menu->add_separator( ).
    ENDIF.

    IF ls_flags-delete = abap_true.
      er_menu->add_function(
          fcode = ls_flags-delete_func
          text  = |{ ls_flags-delete_text }|
      ).
    ENDIF.

  ENDMETHOD.


  METHOD on_node_ctx_menu_select.
    trigger_command( ev_fcode ).
  ENDMETHOD.


  METHOD on_node_double_click.
*.. Retrieve information about node from map
    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node>).
    CHECK sy-subrc = 0.

    CASE <ls_node>-node_type.

      WHEN c_node_type-table.
        change_join_table( ev_node_key ).

      WHEN c_node_type-table_field.
        change_field_condition( ev_node_key ).

      WHEN c_node_type-table_filter OR
           c_node_type-table_filter_or.
        change_filter_condition( ev_node_key ).

      WHEN c_node_type-table_filters OR
           c_node_type-table_fields OR
           c_node_type-or_filter_group.

        TRY.
            DATA(lo_node) = mo_join_tree->get_nodes( )->get_node( <ls_node>-node_key ).
            lo_node->toggle( ).
          CATCH zcx_uitb_tree_error.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.


  METHOD on_node_keypress.
    ASSIGN mt_node_map[ node_key = ev_node_key ] TO FIELD-SYMBOL(<ls_node_map>).
    CHECK sy-subrc = 0.

    IF ev_key = cl_tree_model=>key_delete.

      DATA(lv_function) = SWITCH #( <ls_node_map>-node_type
        WHEN c_node_type-table THEN c_functions-delete_table
        WHEN c_node_type-table_field OR
             c_node_type-table_fields OR
             c_node_type-table_filter OR
             c_node_type-table_filters OR
             c_node_type-table_filter_or OR
             c_node_type-or_filter_group THEN c_functions-delete_condition
      ).

      CHECK lv_function IS NOT INITIAL.

      trigger_command( lv_function ).
    ELSEIF ev_key = cl_tree_model=>key_enter.
      on_node_double_click( ev_node_key = ev_node_key ).
    ENDIF.
  ENDMETHOD.


  METHOD test_join.
    DATA(ls_join_def) = mo_join->to_structure( mo_join_tree ).

    DATA(lt_from_clause) = zcl_dbbr_join_helper=>build_from_clause_for_join_def(
      is_join_def = ls_join_def
    ).

    LOOP AT lt_from_clause ASSIGNING FIELD-SYMBOL(<lv_from>).
      IF sy-tabix = 1.
        <lv_from> = |FROM { <lv_from> }|.
      ELSE.
        <lv_from> = |     { <lv_from> }|.
      ENDIF.
    ENDLOOP.

    CONCATENATE LINES OF lt_from_clause INTO DATA(lv_string) SEPARATED BY cl_abap_char_utilities=>cr_lf.

    zcl_uitb_abap_code_viewer=>show_code(
        iv_title  = 'FROM SQL Clause for Join definition'
        it_code   = lt_from_clause
        iv_width  = 650
        iv_height = 400
    ).
  ENDMETHOD.


  METHOD transfer_join.
    mr_join_def->* = mo_join->to_structure( mo_join_tree ).
    mf_was_updated = abap_true.
  ENDMETHOD.


  METHOD update_or_group_of_filter.
    CHECK iv_node_type = c_node_type-table_filter OR
          iv_node_type = c_node_type-table_filter_or.

    DATA(lo_filter) = CAST lcl_join_filter( io_filter_node->get_user_object( ) ).

    IF iv_node_type = c_node_type-table_filter.
      lo_filter->set_or_group_node( '' ).
    ELSE.
*.... get the parent of the node to update the or group node key of the filter
      lo_filter->set_or_group_node(
        COND #( WHEN iv_or_group_node IS NOT INITIAL THEN
                  iv_or_group_node
                ELSE
                  io_filter_node->get_parent( )->mv_node_key )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD validate_join.
    mo_join->validate( ).
  ENDMETHOD.


  METHOD change_primary_table_alias.
    DATA(lv_new_alias) = zcl_dbbr_appl_util=>popup_get_value(
        is_field = VALUE #( tabname = 'ZDBBR_JOINT' fieldname = 'ADD_TABLE_ALIAS' fieldtext = |{ 'Alias' }| field_obl = abap_true value = mo_join->mv_primary_entity_alias )
        iv_title = |{ 'Change Alias of Primary Table'(023) }|
    ).

    IF lv_new_alias IS NOT INITIAL AND
       mo_join->mv_primary_entity_alias <> lv_new_alias.

*.... Check if this alias is free to use
      IF NOT zcl_dbbr_entity_alias_util=>add_entity_alias( |{ lv_new_alias }| ).
        MESSAGE |Alias { lv_new_alias } is already in use!| TYPE 'S' DISPLAY LIKE 'E'.
        return.
      ELSE.
        zcl_dbbr_entity_alias_util=>unregister_alias( |{ mo_join->mv_primary_entity_alias }| ).
      ENDIF.

      DATA(lt_nodes_to_update) = mo_join->update_alias( |{ lv_new_alias }| ).

      update_node_values( lt_nodes_to_update ).

      MESSAGE s089(zdbbr_info).

      cl_gui_cfw=>set_new_ok_code( space ).
    ENDIF.
  ENDMETHOD.


  METHOD update_table_alias.
    DATA: lt_nodes_to_update TYPE lty_t_node_to_update.

    LOOP AT mo_join->mt_tables REFERENCE INTO DATA(lo_join_table).
      lt_nodes_to_update = VALUE #(
          BASE lt_nodes_to_update
          ( LINES OF CORRESPONDING #(
               lo_join_table->tab_ref->update_alias(
                  iv_alias_old = iv_old_alias
                  iv_alias_new = iv_new_alias )
               )
          )
      ).
    ENDLOOP.

    update_node_values( lt_nodes_to_update ).
  ENDMETHOD.

  METHOD update_node_values.
    LOOP AT it_nodes_to_update ASSIGNING FIELD-SYMBOL(<ls_node_to_update>).
      TRY.
          DATA(lo_node) = mo_join_tree->get_nodes( )->get_node( <ls_node_to_update>-node_key ).
          DATA(lo_item) = lo_node->get_item( <ls_node_to_update>-item_key ).
          lo_item->set_text( |{ <ls_node_to_update>-new_value }| ).
        CATCH zcx_uitb_tree_error.
          "handle exception
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
