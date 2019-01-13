*&---------------------------------------------------------------------*
*&  Include           LZDBBRD01
*&--------------------------------------------------------------------*
**********************************************************************
**********************************************************************
" Exception classes
**********************************************************************

**********************************************************************

*CLASS lcl_selection_table DEFINITION
*CREATE PRIVATE.
*  PUBLIC SECTION.
*    EVENTS aggregation_attr_changed.
*    CLASS-METHODS:
*      get_instance
*        RETURNING
*          VALUE(rr_instance) TYPE REF TO lcl_selection_table.
*    METHODS:
*      change_attributes,
*      get_current_line
*        IMPORTING
*          if_reset_index  TYPE boolean DEFAULT abap_true
*        RETURNING
*          VALUE(rv_index) LIKE sy-tabix,
*      get_current_loopline
*        RETURNING
*          VALUE(rv_index) TYPE sy-tabix,
*      update_fields,
*      get_linecount,
*      display_lines,
*      delete_line,
*      delete_lines,
*      search_value
*        IMPORTING
*          iv_code                     LIKE sy-ucomm
*        RETURNING
*          VALUE(rf_search_successful) TYPE boolean,
*      scroll_to_table,
*      scroll_page_down,
*      scroll_page_up,
*      scroll_page_top,
*      scroll_page_end,
*      get_loop_lines
*        RETURNING
*          VALUE(rv_looplines) TYPE sy-loopc,
*      determine_loop_lines,
*      get_col_index
*        IMPORTING
*          iv_name         TYPE screen-name OPTIONAL
*          iv_group1       TYPE screen-group1 OPTIONAL
*          iv_group2       TYPE screen-group2 OPTIONAL
*          iv_group3       TYPE screen-group3 OPTIONAL
*          iv_group4       TYPE screen-group4 OPTIONAL
*        RETURNING
*          VALUE(rv_index) TYPE sy-index.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    DATA:
*      mv_linecount    LIKE sy-tabix,
*      mv_search       TYPE string,
*      mv_current_line LIKE sy-tabix,
*      mv_looplines    TYPE sy-loopc.
*    METHODS handle_tech_first_setting.
*    METHODS handle_column_optimization.
*    METHODS delete_line_values
*      CHANGING
*        cs_selfields TYPE ZDBBR_selfield.
*    CLASS-DATA:
*      sr_instance TYPE REF TO lcl_selection_table.
*ENDCLASS.
*
*CLASS lcl_data_browser_controller DEFINITION
*CREATE PUBLIC.
*  PUBLIC SECTION.
*    METHODS:
*      constructor
*        IMPORTING
*          if_query_mode TYPE boolean OPTIONAL,
*      handle_ucomm_0100,
*      handle_ucomm_0201,
*      handle_ucomm_0300,
*      call_f4_help
*        IMPORTING
*          if_for_low TYPE boolean,
*      call_table_f4
*        IMPORTING
*          iv_dynp_field_name TYPE devparname
*        CHANGING
*          cv_table           TYPE tabname,
*      perform_exit,
*      set_status_0100,
*      set_status_0300,
*      fill_table
*        IMPORTING
*          if_from_variant_read TYPE boolean OPTIONAL,
*      handle_option_selection,
*      show_multi_select,
*      reset_flags,
*      call_alv_variant_f4,
*      check_alv_variant,
*      call_selfield_f4_help
*        IMPORTING
*          if_low           TYPE boolean OPTIONAL
*          iv_selfield_name TYPE dynfnam
*          iv_current_line  LIKE sy-tabix
*        CHANGING
*          cs_selfield      TYPE ZDBBR_selfield,
*      set_status_0201,
*      check_edit_mode,
*      pbo_main,
*      get_table_to_alias_map
*        EXPORTING
*          et_table_to_alias_map TYPE ZDBBR_table_to_alias_map_itab,
*      init_query_fields,
*      load_query
*        IMPORTING
*          iv_query_name TYPE ZDBBR_query_name,
*      update_join_relations.
*  PROTECTED SECTION.
*
*  PRIVATE SECTION.
*    CONSTANTS:
*      BEGIN OF mc_function_codes,
*        display_query_editor         TYPE sy-ucomm VALUE 'DISP_SCRED',
*        show_option_dialog            TYPE sy-ucomm VALUE 'OPTION',
*        show_multi_select_dialog      TYPE sy-ucomm VALUE 'MORE',
*        search_table                  TYPE sy-ucomm VALUE 'SEARCH',
*        search_table_continue         TYPE sy-ucomm VALUE 'SEARCH_MOR',
*        delete_line_input             TYPE sy-ucomm VALUE 'DELETE',
*        select_additional_texts       TYPE sy-ucomm VALUE 'SEL_TEXT',
*        activate_tech_view            TYPE sy-ucomm VALUE 'TECHVIEW',
*        deactivate_tech_view          TYPE sy-ucomm VALUE 'NOTECHVIEW',
*        activate_optional_or_select   TYPE sy-ucomm VALUE 'OR_SEL_ON',
*        deactivate_optional_or_select TYPE sy-ucomm VALUE 'OR_SEL_OFF',
*        count_lines                   TYPE sy-ucomm VALUE 'LINE_COUNT',
*        execute_selection             TYPE sy-ucomm VALUE 'EXEC',
*        delete_joins                  TYPE sy-ucomm VALUE 'DEL_JOIN',
*        define_joins                  TYPE sy-ucomm VALUE 'SET_JOINS',
*        leave_screen                  TYPE sy-ucomm VALUE '&F03',
*        show_technical_settings       TYPE sy-ucomm VALUE 'SET_FLAGS',
*        save_variant                  TYPE sy-ucomm VALUE 'LT_SAVE',
*        get_variant                   TYPE sy-ucomm VALUE 'LT_GET',
*        delete_variant                TYPE sy-ucomm VALUE 'LT_DELETE',
*        delete_all_input              TYPE sy-ucomm VALUE 'DELETE_ALL',
*        scroll_to_table               TYPE sy-ucomm VALUE 'GO_TO_TAB',
*        check_edit_option             TYPE sy-ucomm VALUE 'CHECK_EDIT',
*        save_f4_at_field              TYPE sy-ucomm VALUE 'SAVE_F4',
*        delete_f4_from_field          TYPE sy-ucomm VALUE 'DELETE_F4',
*        manage_f4_helps               TYPE sy-ucomm VALUE 'MNG_F4',
*        control_output_fields         TYPE sy-ucomm VALUE 'CTRL_OUTP',
*        control_sort_fields           TYPE sy-ucomm VALUE 'CTRL_SORT',
*        control_sel_fields            TYPE sy-ucomm VALUE 'CTRL_SEL',
*        save_query                   TYPE sy-ucomm VALUE 'SAVESCRIPT',
*        multi_or_selection            TYPE sy-ucomm VALUE 'MULTI_OR',
*        show_formula_manager          TYPE sy-ucomm VALUE 'FF_MANAGER',
*        select_option_equal           TYPE sy-ucomm VALUE 'SEL_EQUAL',
*        select_option_not_equal       TYPE sy-ucomm VALUE 'SEL_NEQUAL',
*      END OF mc_function_codes.
*    DATA:
*      mt_selection_fields     TYPE ZDBBR_selfield_itab,
*      mt_multi_or_all         TYPE ZDBBR_or_seltab_itab,
*      mt_active_formula       TYPE ZDBBR_formula_data_ui_itab,
*      mv_current_function     TYPE sy-ucomm,
*      mt_table_to_alias_map   TYPE ZDBBR_table_to_alias_map_itab,
*      mf_join_tables_updated  TYPE boolean,
*      mf_table_was_changed    TYPE boolean,
*      mf_search_successful    TYPE boolean,
*      mf_join_is_active       TYPE boolean,
*      mr_custom_f4_map        TYPE REF TO ZCL_DBBR_custom_f4_map,
*      mr_selection_table      TYPE REF TO lcl_selection_table,
*      mf_params_read          TYPE boolean,
*      mf_group_fields_updated TYPE boolean
*      .
*
*    " attributes for data selection
*    DATA:
*      mt_add_texts          TYPE ZDBBR_additional_text_itab,
*      mt_tabfield_info      TYPE ZDBBR_tabfield_info_ui_itab,
*      mr_tabfield_list      TYPE REF TO ZCL_DBBR_tabfield_list,
*      mr_tabfield_aggr_list TYPE REF TO ZCL_DBBR_tabfield_list,
*      ms_query_info        TYPE ZDBBR_query_info,
*      mf_query_mode        TYPE boolean.
*
*    " attributes for data output
*    DATA:
*      mt_sort_alv TYPE lvc_t_sort,
*      mr_cursor   TYPE REF TO ZCL_DBBR_cursor.
*
*    METHODS:
*      set_cursor,
*      execute_selection
*        IMPORTING
*          if_count_lines_only TYPE boolean OPTIONAL,
*      delete_join_relations,
*      build_table_to_alias_map,
*      change_technical_settings,
*      save_variant,
*      read_variant,
*      delete_variant,
*      save_built_in_f4,
*      delete_f4_helps,
*      define_join_relations,
*      determine_optional_text_fields,
*      create_table_fields
*        IMPORTING
*          iv_tablename        TYPE tabname
*          if_add_table_header TYPE boolean OPTIONAL
*          if_mark_for_output  TYPE boolean OPTIONAL
*          if_selection_active TYPE boolean OPTIONAL
*          iv_selection_order  TYPE tabfdpos OPTIONAL,
*      create_table_field
*        IMPORTING
*          is_dfies           TYPE dfies
*        RETURNING
*          VALUE(rs_selfield) TYPE ZDBBR_selfield,
*      show_sel_query_editor,
*      choose_tabfields
*        IMPORTING
*          iv_mode TYPE ZDBBR_field_chooser_mode,
*      update_alias_names,
*      clear_alias_names,
*      choose_sort_fields,
*      field_aggregation_is_active
*        RETURNING
*          VALUE(rf_aggr_active) TYPE boolean,
*      update_aggrgtd_tabfield_list,
*      update_selection_mask,
*      update_multi_selection_mask,
*      create_table_header
*        IMPORTING
*          iv_tablename           TYPE tabname
*        RETURNING
*          VALUE(rs_table_header) TYPE ZDBBR_selfield,
*      save_query,
*      show_multi_or,
*      show_formula_manager,
*      set_select_option
*        IMPORTING
*          iv_current_line TYPE sy-tabix
*          iv_option       TYPE char2
*          iv_sign         TYPE char1.
*
*    """ event handler methods
*    METHODS:
*      on_aggr_attr_changed FOR EVENT aggregation_attr_changed
*         OF lcl_selection_table.
*ENDCLASS.
*
*CLASS lcl_join_controller DEFINITION
*CREATE PUBLIC.
*  PUBLIC SECTION.
*    INTERFACES ZIF_UITB_SCREEN_CONTROLLER.
*    CONSTANTS:
*      BEGIN OF mc_function_codes,
*        pick               TYPE sy-ucomm VALUE 'PICK',
*        add_join_table     TYPE sy-ucomm VALUE 'ADD_TABLE',
*        delete_table       TYPE sy-ucomm VALUE 'DEL_TABLE',
*        delete_all_tables  TYPE sy-ucomm VALUE 'DEL_TABLE_ALL',
*        add_new_field      TYPE sy-ucomm VALUE 'ADD_FIELD',
*        delete_field       TYPE sy-ucomm VALUE 'DEL_FIELD',
*        delete_all_fields  TYPE sy-ucomm VALUE 'DEL_FIELDS_ALL',
*        transfer_values    TYPE sy-ucomm VALUE 'TAKE_VAL',
*        save_definition    TYPE sy-ucomm VALUE 'SAVE',
*        display_definition TYPE sy-ucomm VALUE 'DISPLAY',
*        copy_definition    TYPE sy-ucomm VALUE 'COPY',
*        edit_definition    TYPE sy-ucomm VALUE 'EDIT',
*        delete_definition  TYPE sy-ucomm VALUE 'DELETE',
*        cancel_dialog      TYPE sy-ucomm VALUE '&F12',
*        quit               TYPE sy-ucomm VALUE '&F15',
*        leave_screen       TYPE sy-ucomm VALUE '&F03',
*        cancel             TYPE sy-ucomm VALUE 'CANCEL',
*        clear_all_values   TYPE sy-ucomm VALUE 'CLEAR_ALL',
*        create_definition  TYPE sy-ucomm VALUE 'CREATE',
*      END OF mc_function_codes.
*    CONSTANTS:
*      BEGIN OF mc_modes,
*        display TYPE char1 VALUE 'D',
*        edit    TYPE char1 VALUE 'E',
*        temp    TYPE char1 VALUE 'T',
*      END OF mc_modes.
*    METHODS:
*      constructor,
*      change_parameter_attributes,
*      define_joins
*        IMPORTING
*          is_join_def             TYPE ZDBBR_join_data
*        RETURNING
*          VALUE(rf_joins_updated) TYPE boolean,
*      display_value_f4,
*      join_definition_f4
*        IMPORTING
*          iv_join_key_dynfname TYPE dynfnam
*        CHANGING
*          cv_join_key          TYPE ZDBBR_join_key,
*      fill_join_table_names,
*      get_deleted_joins
*        EXPORTING
*          et_deleted_joins TYPE ZDBBR_join_k_itab.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    METHODS:
*      pick_join_table,
*      add_new_join_field
*        RETURNING
*          VALUE(rv_index) TYPE sy-tabix,
*      init
*        IMPORTING
*          is_join_def TYPE ZDBBR_join_data,
*      transfer_entered_values,
*      delete_all_join_tables,
*      save_current_add_fields,
*      load_join_definition
*        RETURNING
*          VALUE(rf_load_successful) TYPE boolean,
*      save_join_definition,
*      edit_join_definition,
*      display_join_definition,
*      delete_join_definition,
*      delete_selected_join_table,
*      validate_join_definition,
*      clear_all_values
*        IMPORTING
*          if_clear_global_join_def TYPE boolean OPTIONAL,
*      copy_join_definition,
*      assert_join_name
*        RETURNING
*          VALUE(rf_assert_success) TYPE boolean.
*    DATA:
*      mt_join_tables       TYPE ZDBBR_join_table_itab,
*      mt_join_table_fields TYPE ZDBBR_join_condition_itab,
*      mr_join_table        TYPE REF TO lcl_join_table,
*      mr_join_fields_table TYPE REF TO lcl_join_field_table,
*      mv_function_code     TYPE syst-ucomm,
*      mv_current_add_table TYPE tabname,
*      mf_joins_updated     TYPE boolean,
*      mf_set_cursor        TYPE boolean,
*      mv_cursor_field      TYPE dynfnam,
*      mv_cursor_line       TYPE sy-tabix,
*      mv_mode              TYPE char1,
*      mt_deleted_joins     TYPE ZDBBR_join_k_itab,
*      ms_join_def          TYPE ZDBBR_join_data_ui.
*ENDCLASS.
*
*CLASS lcl_join_field_f4_controller DEFINITION
*CREATE PRIVATE.
*  PUBLIC SECTION.
*    CLASS-METHODS:
*      get_instance
*        RETURNING VALUE(rr_instance) TYPE REF TO lcl_join_field_f4_controller.
*    METHODS:
*      handle_user_command,
*      set_status,
*      call_screen,
*      refresh_tree,
*      display_value_help
*        IMPORTING
*          it_join_tables     TYPE ZDBBR_SELOPT_itab
*          is_join_field_info TYPE dfies
*        EXPORTING
*          ev_chosen_field    TYPE fieldname
*          ev_chosen_table    TYPE tabname.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    METHODS:
*      init_tree_control,
*      fill_tree_node_and_items,
*      set_selected_values
*        IMPORTING
*          iv_node_key TYPE tv_nodekey,
*      " event handlers for tree control
*      handle_node_double_click
*      FOR EVENT node_double_click
*            OF cl_gui_list_tree
*        IMPORTING
*            node_key,
*      handle_node_key_press
*      FOR EVENT node_keypress
*            OF cl_gui_list_tree
*        IMPORTING
*            node_key
*            key,
*      handle_item_selection
*        IMPORTING
*          iv_node_key TYPE tv_nodekey .
*    CLASS-DATA: sr_instance TYPE REF TO lcl_join_field_f4_controller.
*    DATA: mr_join_field_tree      TYPE REF TO cl_gui_list_tree,
*          mr_join_field_container TYPE REF TO cl_gui_custom_container,
*          mt_join_tables_selopt   TYPE ZDBBR_SELOPT_itab,
*          " attributes for tree control
*          mt_node_table           TYPE treev_ntab,
*          mt_item_table           TYPE ZDBBR_TREEITEM_ITAB,
*          mt_node_keys            TYPE treev_nks,
*          " attributes for chosen value
*          mv_chosen_field         TYPE fieldname,
*          mv_chosen_table         TYPE tabname,
*          ms_join_field_info      TYPE dfies.
*    CONSTANTS: mc_tree_control TYPE string VALUE 'JOIN_FIELD_TREE',
*               mc_root_node    TYPE tv_nodekey VALUE 'ROOT' ##NO_TEXT.
*ENDCLASS.
*
*CLASS lcl_join_field_table DEFINITION
*CREATE PRIVATE.
*  PUBLIC SECTION.
*    INTERFACES ZIF_UITB_TABLE.
*    CONSTANTS:
*      BEGIN OF mc_table_field_names,
*        field     TYPE dynfnam VALUE 'GS_JOIN_TABLE_FIELD-FIELD',
*        value     TYPE dynfnam VALUE 'GS_JOIN_TABLE_FIELD-VALUE',
*        ref_table TYPE dynfnam VALUE 'GS_JOIN_TABLE_FIELD-REF_TABLE',
*        method    TYPE dynfnam VALUE 'GS_JOIN_TABLE_FIELD-METHOD',
*        add_table TYPE dynfnam VALUE 'GS_JOIN_TABLE_FIELD-ADD_TABLE',
*      END OF mc_table_field_names.
*    CLASS-METHODS:
*      get_instance
*        RETURNING VALUE(rr_instance) TYPE REF TO lcl_join_field_table.
*    METHODS:
*      add_line
*        IMPORTING
*          iv_join_table   TYPE tabname
*        RETURNING
*          VALUE(rv_index) TYPE sy-tabix,
*      set_mode
*        IMPORTING
*          iv_mode TYPE char1,
*      get_current_line
*        RETURNING
*          VALUE(rs_current_line) TYPE ZDBBR_joinc.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    CLASS-DATA: sr_instance TYPE REF TO lcl_join_field_table.
*    DATA: mv_current_line TYPE sy-tabix,
*          mv_mode         TYPE char1
*          .
*    METHODS validate_addtable_field.
*    METHODS validate_reference_values.
*
*ENDCLASS.
*
*CLASS lcl_join_table DEFINITION
*CREATE PRIVATE.
*  PUBLIC SECTION.
*    INTERFACES ZIF_UITB_TABLE.
*    CONSTANTS:
*      BEGIN OF mc_table_field_names,
*        add_table TYPE dynfnam VALUE 'GS_JOIN_TABLE-ADD_TABLE',
*      END OF mc_table_field_names.
*    CLASS-METHODS:
*      get_instance
*        RETURNING VALUE(rr_instance) TYPE REF TO lcl_join_table.
*    METHODS:
*      get_current_line
*        RETURNING VALUE(rs_current_line) TYPE ZDBBR_joint,
*      set_mode
*        IMPORTING
*          iv_mode TYPE char1.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    CLASS-DATA: sr_instance TYPE REF TO lcl_join_table.
*    DATA: mv_current_line TYPE sy-tabix,
*          mv_mode         TYPE char1.
*ENDCLASS.
*
*CLASS lcl_f4_manager_controller DEFINITION
*CREATE PRIVATE.
*  PUBLIC SECTION.
*    CLASS-METHODS:
*      get_instance
*        RETURNING
*          VALUE(rr_instance) TYPE REF TO lcl_f4_manager_controller.
*    METHODS:
*      manage_f4_helps
*        IMPORTING
*          iv_primary_table TYPE tabname.
*  PRIVATE SECTION.
*    CLASS-DATA: sr_instance TYPE REF TO lcl_f4_manager_controller.
*    DATA: mt_f4_overview   TYPE ZDBBR_f4_overview_itab,
*          mv_primary_table TYPE tabname,
*          mr_f4_alv        TYPE REF TO cl_salv_table.
*    METHODS:
*      display_data,
*      select_data,
*      on_user_command FOR EVENT added_function OF cl_salv_events
*        IMPORTING
*            e_salv_function.
*ENDCLASS.
*
*CLASS lcl_multi_select_controller DEFINITION.
*  PUBLIC SECTION.
*    INTERFACES ZIF_UITB_SCREEN_CONTROLLER.
*    METHODS:
*      constructor
*        IMPORTING
*          ir_custom_f4_map   TYPE REF TO ZCL_DBBR_custom_f4_map
*          ir_selection_table TYPE REF TO lcl_multi_selection_table,
*      determine_line_count,
*      call_f4_help
*        IMPORTING
*          if_for_low TYPE boolean
*          if_multi   TYPE boolean OPTIONAL,
*      should_data_be_transferred
*        RETURNING
*          VALUE(rf_transfer) TYPE boolean.
*  PRIVATE SECTION.
*    DATA: mr_custom_f4_map TYPE REF TO ZCL_DBBR_custom_f4_map,
*          mr_table         TYPE REF TO lcl_multi_selection_table,
*          mf_transfer      TYPE boolean.
*
*    METHODS call_selfield_f4_help_multi
*      IMPORTING
*        if_low       TYPE boolean
*        iv_tablename TYPE tabname
*        iv_fieldname TYPE fieldname
*      CHANGING
*        cv_low       TYPE se16n_value
*        cv_high      TYPE se16n_value.
*    METHODS take_entered_values.
*    METHODS import_from_clipboard.
*    METHODS call_selfield_f4_custom_multi
*      IMPORTING
*        iv_current_line LIKE sy-tabix
*        if_low          TYPE boolean.
*
*ENDCLASS.
*
*CLASS lcl_custom_f4_table DEFINITION
*CREATE PRIVATE.
*  PUBLIC SECTION.
*    CLASS-METHODS:
*      get_instance
*        RETURNING
*          VALUE(rr_instance) TYPE REF TO lcl_custom_f4_table.
*    METHODS:
*      change_attributes,
*      get_current_line
*        RETURNING
*          VALUE(rv_index) LIKE sy-tabix,
*      get_current_line_value
*        RETURNING
*          VALUE(rs_line) TYPE ZDBBR_F4_SEARCH,
*      update_fields,
*      determine_line_count,
*      move_to_top,
*      move_line_up
*        IMPORTING
*          if_to_top TYPE boolean OPTIONAL,
*      move_line_down
*        IMPORTING
*          if_to_bottom TYPE boolean OPTIONAL,
*      determine_selected_line,
*      get_selected_line
*        RETURNING
*          VALUE(rv_selected_line) LIKE sy-tabix,
*      set_looplines,
*      get_selected_line_in_loop
*        RETURNING
*          VALUE(rv_selected_line) LIKE sy-tabix
*        ,
*      delete_line.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    METHODS: constructor.
*    DATA:
*      mv_linecount     LIKE sy-tabix,
*      mv_selected_line LIKE sy-tabix,
*      mv_looplines     LIKE sy-loopc.
*    CLASS-DATA:
*      sr_instance TYPE REF TO lcl_custom_f4_table.
*
*ENDCLASS.
*
*CLASS lcl_custom_f4_controller DEFINITION
*CREATE PRIVATE.
*  PUBLIC SECTION.
*    CLASS-METHODS:
*      get_instance
*        RETURNING
*          VALUE(rr_instance) TYPE REF TO lcl_custom_f4_controller.
*    METHODS:
*      fill_search_fields
*        IMPORTING
*          ir_fields TYPE REF TO ZCL_DBBR_tabfield_list,
*      call_view
*        IMPORTING
*          ir_fields TYPE REF TO ZCL_DBBR_tabfield_list,
*      handle_user_command,
*      set_status,
*      modify_screen_output,
*      call_built_in_f4_view
*        IMPORTING
*          iv_current_table TYPE tabname
*          iv_current_field TYPE fieldname,
*      delete_f4_from_field
*        IMPORTING
*          ir_selfield_ref TYPE REF TO ZDBBR_selfield.
*  PRIVATE SECTION.
*    METHODS:
*      constructor,
*      save_search_help,
*      should_save
*        RETURNING
*          VALUE(rf_save) TYPE boolean,
*      save_to_db,
*      built_search_help_exists
*        RETURNING
*          VALUE(rf_exists) TYPE boolean.
*    CLASS-DATA:
*      sr_instance TYPE REF TO lcl_custom_f4_controller.
*    DATA:
*      mr_table                  TYPE REF TO lcl_custom_f4_table,
*      mf_delete_existing        TYPE boolean,
*      ms_se16join_f4_definition TYPE ZDBBR_f4_data,
*      mt_existing_f4            TYPE ZDBBR_f4_data_itab.
*ENDCLASS.
*
*CLASS lcl_save_query_controller DEFINITION.
*  PUBLIC SECTION.
*    INTERFACES ZIF_UITB_SCREEN_CONTROLLER.
*    METHODS constructor
*      IMPORTING
*        ir_tabfield_list TYPE REF TO ZCL_DBBR_tabfield_list
*        is_query_info   TYPE ZDBBR_query_info.
*    METHODS call_screen.
*  PRIVATE SECTION.
*    DATA:
*      mr_tabfield_list TYPE REF TO ZCL_DBBR_tabfield_list,
*      ms_query_info   TYPE ZDBBR_query_info.
*    METHODS save_query
*      IMPORTING
*        if_load_after_save TYPE boolean OPTIONAL.
*
*ENDCLASS.
*
*CLASS lcl_variant_controller DEFINITION.
*  PUBLIC SECTION.
*    INTERFACES ZIF_UITB_SCREEN_CONTROLLER.
*    CONSTANTS: BEGIN OF mc_modes,
*                 save   TYPE int1 VALUE 1,
*                 read   TYPE int1 VALUE 2,
*                 delete TYPE int1 VALUE 3,
*               END OF mc_modes.
*    METHODS:
*      constructor
*        IMPORTING
*          if_query_mode TYPE boolean
*          iv_mode        TYPE int1
*          is_query_info TYPE ZDBBR_query_info
*          is_join_def    TYPE ZDBBR_join_data
*          ir_tabfields   TYPE REF TO ZCL_DBBR_tabfield_list,
*      call_variant_f4,
*      leave_screen.
*  PRIVATE SECTION.
*    DATA: mf_query_mode TYPE boolean,
*          ms_query_info TYPE ZDBBR_query_info,
*          ms_join_def    TYPE ZDBBR_join_data,
*          mr_tabfields   TYPE REF TO ZCL_DBBR_tabfield_list,
*          mv_mode        TYPE int1.
*    METHODS:
*      add_special_variant_data
*        IMPORTING
*          iv_layout_data_type TYPE ZDBBR_variant_datatype
*          iv_layout_data_low  TYPE any OPTIONAL
*          iv_layout_data_high TYPE any OPTIONAL
*        CHANGING
*          cs_layout_data      TYPE ZDBBR_vardata
*          ct_layout_data      TYPE ZDBBR_vardata_itab
*          cv_line_counter     TYPE sy-tabix,
*      read_variant,
*      delete_variant,
*      create_variant,
*      add_value_variant_data
*        IMPORTING
*          is_selfield     TYPE ZDBBR_selfield
*        CHANGING
*          cs_layout_data  TYPE ZDBBR_vardata
*          ct_layout_data  TYPE ZDBBR_vardata_itab
*          cv_line_counter TYPE sy-tabix.
*
*ENDCLASS.
*
*CLASS lcl_multi_selection_table DEFINITION.
*  PUBLIC SECTION.
*    METHODS:
*      change_attributes,
*      get_current_line
*        RETURNING
*          VALUE(rv_index) LIKE sy-tabix,
*      update_fields,
*      determine_line_count,
*      display_lines,
*      delete_line,
*      delete_lines,
*      append_new_lines,
*      handle_option_selection,
*      set_template_line
*        IMPORTING
*          is_selfield TYPE ZDBBR_selfield.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    DATA:
*      mv_linecount     LIKE sy-tabix,
*      ms_template      TYPE ZDBBR_selfield,
*      mr_custom_f4_map TYPE REF TO ZCL_DBBR_custom_f4_map.
*ENDCLASS.
*
*
*CLASS lcl_start_controller DEFINITION.
*  PUBLIC SECTION.
*    INTERFACES: ZIF_UITB_SCREEN_CONTROLLER.
*    METHODS call_query_f4.
*
*  PRIVATE SECTION.
*    CONSTANTS:
*      BEGIN OF mc_function_codes,
*        select_mode         TYPE sy-ucomm VALUE 'SEL_MODE',
*        start               TYPE sy-ucomm VALUE 'START',
*        start_with_variant  TYPE sy-ucomm VALUE 'STARTWVAR',
*        show_query_catalog TYPE sy-ucomm VALUE 'OPENSCRCAT',
*      END OF mc_function_codes.
*    DATA: mv_function_code TYPE sy-ucomm.
*    METHODS show_query.
*    METHODS show_table.
*    METHODS start_data_browser.
*
*ENDCLASS.
*
*CLASS lcl_multi_or_controller DEFINITION CREATE PUBLIC.
*
*  PUBLIC SECTION.
*    INTERFACES ZIF_UITB_SCREEN_CONTROLLER.
*
*    METHODS:
*      constructor
*        IMPORTING
*          ir_multi_or_table TYPE REF TO lcl_multi_or_table
*          it_multi_or_all   TYPE ZDBBR_or_seltab_itab
*          ir_custom_f4_map  TYPE REF TO ZCL_DBBR_custom_f4_map,
*
*      call_selfield_value_help
*        IMPORTING
*          if_for_low TYPE boolean OPTIONAL,
*      get_multi_or_values
*        RETURNING
*          VALUE(rt_multi_or) TYPE ZDBBR_or_seltab_itab,
*      should_data_be_transferred
*        RETURNING
*          VALUE(rf_do_transfer) TYPE boolean.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    CONSTANTS:
*      BEGIN OF mc_functions,
*        search_for_field        TYPE sy-ucomm VALUE 'SEARCH',
*        search_further          TYPE sy-ucomm VALUE 'SEARCHFROM',
*        go_to_next_tuple        TYPE sy-ucomm VALUE 'NEXT',
*        go_to_previous_tuple    TYPE sy-ucomm VALUE 'PREV',
*        transfer_data           TYPE sy-ucomm VALUE 'TAKE',
*        delete_current_line     TYPE sy-ucomm VALUE 'DELETE',
*        delete_all              TYPE sy-ucomm VALUE 'DELETE_ALL',
*        go_to_next_criteria     TYPE sy-ucomm VALUE 'CRITNEXT',
*        go_to_previous_criteria TYPE sy-ucomm VALUE 'CRITPREV',
*        multi_selection         TYPE sy-ucomm VALUE 'MORE',
*        select_option_choice    TYPE sy-ucomm VALUE 'OPTION',
*      END OF mc_functions.
*    DATA: mr_multi_or_table    TYPE REF TO lcl_multi_or_table,
*          mt_multi_or_all      TYPE ZDBBR_or_seltab_itab,
*          mf_search_successful TYPE boolean,
*          mr_custom_f4_map     TYPE REF TO ZCL_DBBR_custom_f4_map,
*          mr_current_multi_or  TYPE REF TO ZDBBR_or_seltab,
*          mt_multi_selection   TYPE ZDBBR_selfield_itab,
*          mf_transfer_data     TYPE boolean,
*          mf_first_call        TYPE boolean.
*    METHODS:
*      get_tuple_data_for_tuple,
*      delete_all_entries,
*      delete_current_line,
*      go_to_next_tuple,
*      go_to_previous_tuple,
*      go_to_next_criteria,
*      go_to_previous_criteria,
*      show_option_dialog,
*      show_multi_selection_dialog,
*      store_current_tuple_data.
*ENDCLASS.
*
*CLASS lcl_multi_or_table DEFINITION CREATE PUBLIC.
*
*  PUBLIC SECTION.
*    INTERFACES: ZIF_UITB_TABLE,
*      ZIF_UITB_PAGE_SCROLLER.
*
*    METHODS:
*      set_multi_or_all
*        IMPORTING
*          ir_multi_or_all TYPE REF TO ZDBBR_or_seltab_itab,
*      set_multi_or_current_tuple
*        IMPORTING
*          ir_multi_or_single TYPE REF TO ZDBBR_or_seltab,
*      set_multi_or_multi_select
*        IMPORTING
*          ir_multi_or_multi TYPE REF TO ZDBBR_selfield_itab,
*      pbo,
*      search
*        IMPORTING
*          if_continue_previous_search TYPE boolean OPTIONAL
*        RETURNING
*          VALUE(rf_search_successful) TYPE boolean.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    DATA:
*      mv_linecount        TYPE sy-tabix,
*      mv_looplines        TYPE sy-loopc,
*      mv_current_line     TYPE sy-tabix,
*      mr_multi_or_all     TYPE REF TO ZDBBR_or_seltab_itab,
*      mr_multi_or_current TYPE REF TO ZDBBR_or_seltab,
*      mr_multi_or_multi   TYPE REF TO ZDBBR_selfield_itab,
*      mv_search_value     TYPE fieldname.
*    METHODS:
*      delete_line
*        CHANGING
*          cs_selfield TYPE ZDBBR_selfield,
*      query_for_search_criteria.
*
*ENDCLASS.
