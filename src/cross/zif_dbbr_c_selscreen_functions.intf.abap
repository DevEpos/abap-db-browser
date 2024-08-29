INTERFACE zif_dbbr_c_selscreen_functions
  PUBLIC.

  CONSTANTS show_edit_mode_info TYPE ui_func VALUE 'EDIT_MODE_INFO' ##NO_TEXT.
  CONSTANTS import_queries TYPE ui_func VALUE 'IMPORT_QUERIES' ##NO_TEXT.
  CONSTANTS to_next_criteria TYPE sy-ucomm VALUE 'TO_NEXT_CRITERIA' ##NO_TEXT.
  CONSTANTS to_previous_criteria TYPE sy-ucomm VALUE 'TO_PREVIOUS_CRITERIA' ##NO_TEXT.
  CONSTANTS load_default_variant TYPE sy-ucomm VALUE 'LOADDEFAULTVARIANT' ##NO_TEXT.
  CONSTANTS create_default_variant TYPE sy-ucomm VALUE 'CREATEDEFAULTVARIANT' ##NO_TEXT.
  CONSTANTS delete_default_variant TYPE sy-ucomm VALUE 'DELETEDEFAULTVARIANT' ##NO_TEXT.
  CONSTANTS object_browser_search TYPE sy-ucomm VALUE 'NEWOBJSRCH' ##NO_TEXT.
  CONSTANTS open_cds_query_in_aox TYPE sy-ucomm VALUE 'OPENQRYINAOX' ##NO_TEXT.
  CONSTANTS open_cds_query_in_qry_mon TYPE sy-ucomm VALUE 'OPENQRTINQRYMON' ##NO_TEXT.
  CONSTANTS open_in_design_studio TYPE sy-ucomm VALUE 'OPENQUERYINDESIGNSTUDIO' ##NO_TEXT.
  CONSTANTS edit_sql_query TYPE sy-ucomm VALUE 'EDITSQLQUERY' ##NO_TEXT.
  CONSTANTS create_sql_query TYPE sy-ucomm VALUE 'CREATESQLQUERY' ##NO_TEXT.
  CONSTANTS show_cds_dependency_tree TYPE sy-ucomm VALUE 'SHOWCDSDEPTREE' ##NO_TEXT.
  CONSTANTS display_db_browser_version TYPE sy-ucomm VALUE 'SHOWVERSIO' ##NO_TEXT.
  CONSTANTS goto_next_obj_navigator_view TYPE sy-ucomm VALUE 'NXTOBJNAVV' ##NO_TEXT.
  CONSTANTS activate_optional_or_select TYPE sy-ucomm VALUE 'OR_SEL_ON' ##NO_TEXT.
  CONSTANTS activate_tech_view TYPE sy-ucomm VALUE 'TECHVIEW' ##NO_TEXT.
  CONSTANTS change_entity_type TYPE sy-ucomm VALUE 'CHANGE_ENTITY_TYPE' ##NO_TEXT.
  CONSTANTS check_edit_option TYPE sy-ucomm VALUE 'CHECK_EDIT' ##NO_TEXT.
  CONSTANTS choose_cds_sub_entity TYPE sy-ucomm VALUE 'ENTITYFC02' ##NO_TEXT.
  CONSTANTS choose_different_entity TYPE sy-ucomm VALUE 'CHSEENTITY' ##NO_TEXT.
  CONSTANTS collapse_all_tables TYPE sy-ucomm VALUE 'COLLAPSE_ALL' ##NO_TEXT.
  CONSTANTS configure_parameter TYPE sy-ucomm VALUE 'CONFIG_PARAM' ##NO_TEXT.
  CONSTANTS control_calc_field_option TYPE sy-ucomm VALUE 'CALCFIELD' ##NO_TEXT.
  CONSTANTS control_output_fields TYPE sy-ucomm VALUE 'CTRL_OUTP' ##NO_TEXT.
  CONSTANTS control_sel_fields TYPE sy-ucomm VALUE 'CTRL_SEL' ##NO_TEXT.
  CONSTANTS control_sort_fields TYPE sy-ucomm VALUE 'CTRL_SORT' ##NO_TEXT.
  CONSTANTS copy_query TYPE sy-ucomm VALUE 'ENTITYFC02' ##NO_TEXT.
  CONSTANTS count_lines TYPE sy-ucomm VALUE 'LINE_COUNT' ##NO_TEXT.
  CONSTANTS cross_reference_table TYPE sy-ucomm VALUE 'ENTITYFC02' ##NO_TEXT.
  CONSTANTS deactiv_optl_or_select TYPE sy-ucomm VALUE 'OR_SEL_OFF' ##NO_TEXT.
  CONSTANTS define_joins TYPE sy-ucomm VALUE 'SET_JOINS' ##NO_TEXT.
  CONSTANTS define_sub_queries TYPE sy-ucomm VALUE 'DEFSUBQURY' ##NO_TEXT.
  CONSTANTS delete_db_content TYPE sy-ucomm VALUE 'DELDBCONT' ##NO_TEXT.
  CONSTANTS select_group_by_all TYPE sy-ucomm VALUE 'SELECT_GROUP_BY_ALL' ##NO_TEXT.
  CONSTANTS unselect_group_by_all TYPE sy-ucomm VALUE 'UNSELECT_GROUP_BY_ALL' ##NO_TEXT.
  CONSTANTS reset_input_fields TYPE sy-ucomm VALUE 'RESETENTTY' ##NO_TEXT.
  CONSTANTS delete_aggregations TYPE sy-ucomm VALUE 'DEL_AGGREG' ##NO_TEXT.
  CONSTANTS delete_all_input TYPE sy-ucomm VALUE 'DELETE_ALL' ##NO_TEXT.
  CONSTANTS delete_all_or_tuple TYPE sy-ucomm VALUE 'DEL_ORTUPL' ##NO_TEXT.
  CONSTANTS delete_all_criteria TYPE sy-ucomm VALUE 'DELALLCRIT' ##NO_TEXT.
  CONSTANTS delete_f4_from_field TYPE sy-ucomm VALUE 'DELETE_F4' ##NO_TEXT.
  CONSTANTS delete_joins TYPE sy-ucomm VALUE 'DEL_JOIN' ##NO_TEXT.
  CONSTANTS delete_line_input TYPE sy-ucomm VALUE 'DELETE' ##NO_TEXT.
  CONSTANTS delete_mode TYPE sy-ucomm VALUE 'DEL_MODE' ##NO_TEXT.
  CONSTANTS delete_query TYPE sy-ucomm VALUE 'ENTITYFC04' ##NO_TEXT.
  CONSTANTS delete_variant TYPE sy-ucomm VALUE 'LT_DELETE' ##NO_TEXT.
  CONSTANTS display_entity_navigator TYPE sy-ucomm VALUE 'NAVIGATOR' ##NO_TEXT.
  CONSTANTS display_object_list TYPE sy-ucomm VALUE 'DISPOBJLST' ##NO_TEXT.
  CONSTANTS edit_alternative_coltexts TYPE sy-ucomm VALUE 'EDITCOLTXT' ##NO_TEXT.
  CONSTANTS edit_jump_fields TYPE sy-ucomm VALUE 'ENTITYFC01' ##NO_TEXT.
  CONSTANTS execute_selection TYPE sy-ucomm VALUE 'EXEC' ##NO_TEXT.
  CONSTANTS exec_selection_without_grp TYPE sy-ucomm VALUE 'EXECNOGRP' ##NO_TEXT.
  CONSTANTS expand_all_tables TYPE sy-ucomm VALUE 'EXPAND_ALL' ##NO_TEXT.
  CONSTANTS expand_collapse_table_fields TYPE sy-ucomm VALUE 'EXPAND' ##NO_TEXT.
  CONSTANTS export_favorites TYPE sy-ucomm VALUE 'EXPORTFAV' ##NO_TEXT.
  CONSTANTS export_query TYPE sy-ucomm VALUE 'ENTITYFC03' ##NO_TEXT.
  CONSTANTS get_variant TYPE sy-ucomm VALUE 'LT_GET' ##NO_TEXT.
  CONSTANTS go_to_ddic_view_of_cds TYPE sy-ucomm VALUE 'ENTITYFC04' ##NO_TEXT.
  CONSTANTS go_to_next_table TYPE sy-ucomm VALUE 'NEXTTAB' ##NO_TEXT.
  CONSTANTS go_to_previous_table TYPE sy-ucomm VALUE 'PREVTAB' ##NO_TEXT.
  CONSTANTS import_favorites TYPE sy-ucomm VALUE 'IMPORTFAV' ##NO_TEXT.
  CONSTANTS leave_screen TYPE sy-ucomm VALUE '&F03' ##NO_TEXT.
  CONSTANTS maintain_value_helps TYPE sy-ucomm VALUE 'MAINTF4' ##NO_TEXT.
  CONSTANTS manage_associations TYPE sy-ucomm VALUE 'ASSOCMGMT' ##NO_TEXT.
  CONSTANTS multi_or_selection TYPE sy-ucomm VALUE 'MULTI_OR' ##NO_TEXT.
  CONSTANTS navigate_back TYPE sy-ucomm VALUE 'NAVBACK' ##NO_TEXT.
  CONSTANTS navigate_forward TYPE sy-ucomm VALUE 'NAVFORWARD' ##NO_TEXT.
  CONSTANTS navigate_to_table_def TYPE sy-ucomm VALUE 'ENTITYFC01' ##NO_TEXT.
  CONSTANTS open_cds_view_with_adt TYPE sy-ucomm VALUE 'ENTITYFC03' ##NO_TEXT.
  CONSTANTS open_specific_extended_search TYPE sy-ucomm VALUE 'EXT_SEARCH_BY_TYPE' ##NO_TEXT.
  CONSTANTS pick_navigation TYPE sy-ucomm VALUE 'PICK' ##NO_TEXT.
  CONSTANTS assign_built_in_f4_at_field TYPE sy-ucomm VALUE 'ASSGNBIF4' ##NO_TEXT.
  CONSTANTS assign_custom_f4_at_field TYPE sy-ucomm VALUE 'ASSGNCF4' ##NO_TEXT.
  CONSTANTS save_query TYPE sy-ucomm VALUE 'SAVESCRIPT' ##NO_TEXT.
  CONSTANTS save_variant TYPE sy-ucomm VALUE 'LT_SAVE' ##NO_TEXT.
  CONSTANTS search_table TYPE sy-ucomm VALUE 'SEARCH' ##NO_TEXT.
  CONSTANTS search_table_continue TYPE sy-ucomm VALUE 'SEARCH_MOR' ##NO_TEXT.
  CONSTANTS select_additional_texts TYPE sy-ucomm VALUE 'DEFADDTEXT' ##NO_TEXT.
  CONSTANTS select_option_equal TYPE sy-ucomm VALUE 'SEL_EQUAL' ##NO_TEXT.
  CONSTANTS select_option_not_equal TYPE sy-ucomm VALUE 'SEL_NEQUAL' ##NO_TEXT.
  CONSTANTS set_field_lowercase TYPE sy-ucomm VALUE 'LOWERCASE' ##NO_TEXT.
  CONSTANTS set_focus_to_1st_selfield TYPE sy-ucomm VALUE 'FOCUS1STFL' ##NO_TEXT.
  CONSTANTS set_focus_to_navigator TYPE sy-ucomm VALUE 'FOCONNAV' ##NO_TEXT.
  CONSTANTS show_ddls_source TYPE sy-ucomm VALUE 'ENTITYFC01' ##NO_TEXT.
  CONSTANTS show_formula_manager TYPE sy-ucomm VALUE 'FF_MANAGER' ##NO_TEXT.
  CONSTANTS show_multi_select_dialog TYPE sy-ucomm VALUE 'MORE' ##NO_TEXT.
  CONSTANTS show_navigation_history TYPE sy-ucomm VALUE 'NAVHISTORY' ##NO_TEXT.
  CONSTANTS show_option_dialog TYPE sy-ucomm VALUE 'OPTION' ##NO_TEXT.
  CONSTANTS show_technical_settings TYPE sy-ucomm VALUE 'SET_FLAGS' ##NO_TEXT.
ENDINTERFACE.
