INTERFACE zif_dbbr_main_report_var_ids
  PUBLIC .


  "! Type - ZDBBR_button
  CONSTANTS c_bt_expand TYPE string VALUE 'EXPAND' ##NO_TEXT.
  "! Type - ZDBBR_query_name
  CONSTANTS c_p_query_name TYPE string VALUE 'P_QUERYN' ##NO_TEXT.
  "! Type - ddtext
  CONSTANTS c_p_query_description TYPE string VALUE 'P_QRYDEC' ##NO_TEXT.
  "! Type - syst_ucomm
  CONSTANTS c_ok_code TYPE string VALUE 'OK_CODE' ##NO_TEXT.
  "! Type - cxtab_control
  CONSTANTS c_addtexts_tc TYPE string VALUE 'ADDTEXTS_TC' ##NO_TEXT.
  "! Type - ZDBBR_button
  CONSTANTS c_bt_filter_fields TYPE string VALUE 'FILTER_FIELDS' ##NO_TEXT.
  "! Type - ZDBBR_addtext_ui
  CONSTANTS c_s_add_text TYPE string VALUE 'GS_ADD_TEXT' ##NO_TEXT.
  "! Type - ZDBBR_browser_mode_data
  CONSTANTS c_s_browser_mode TYPE string VALUE 'GS_BROWSER_MODE' ##NO_TEXT.
  "! Type - ZDBBR_ENTITY_INFO_SIMPLE
  CONSTANTS c_s_entity_info TYPE string VALUE 'GS_ENTITY_INFO' ##NO_TEXT.
  "! Type - ZDBBR_browser_mode_data
  CONSTANTS c_s_entity TYPE string VALUE 'GS_ENTITY' ##NO_TEXT.
  "! Type - ZDBBR_build_in_f4_ui_data
  CONSTANTS c_s_built_in_f4 TYPE string VALUE 'GS_BUILT_IN_F4' ##NO_TEXT.
  "! Type - ZDBBR_custom_searchhelp_ui
  CONSTANTS c_s_custom_search_help TYPE string VALUE 'GS_CUSTOM_SEARCH_HELP' ##NO_TEXT.
  "! Type - ZDBBR_global_data
  CONSTANTS c_s_data TYPE string VALUE 'GS_DATA' ##NO_TEXT.
  "! Type - ZDBBR_USRSETTNG
  CONSTANTS c_s_settings TYPE string VALUE 'GS_USER_SETTINGS' ##NO_TEXT.
  "! Type - lvc_s_layo
  CONSTANTS c_s_layout TYPE string VALUE 'GS_LAYOUT' ##NO_TEXT.
  "! Type - ZDBBR_selfield
  CONSTANTS c_s_multi_or TYPE string VALUE 'GS_MULTI_OR' ##NO_TEXT.
  "! Type - ZDBBR_selfield
  CONSTANTS c_s_multi_select TYPE string VALUE 'GS_MULTI_SELECT' ##NO_TEXT.
  "! Type - ZDBBR_button
  CONSTANTS c_bt_option_template TYPE string VALUE 'OPTION_TEMPLATE' ##NO_TEXT.
  "! Type - se16n_option
  CONSTANTS c_v_option_template TYPE string VALUE 'GV_OPTION_TEMPLATE' ##NO_TEXT.
  "! Type - slis_print_alv
  CONSTANTS c_s_print TYPE string VALUE 'GS_PRINT' ##NO_TEXT.
  "! Type - ZDBBR_selfield
  CONSTANTS c_s_selfields TYPE string VALUE 'GS_SELFIELDS' ##NO_TEXT.
  "! Type - smp_dyntxt
  CONSTANTS c_s_sel_text_gui_text TYPE string VALUE 'GS_SEL_TEXT_GUI_TEXT' ##NO_TEXT.
  "! Type - disvariant
  CONSTANTS c_s_variant TYPE string VALUE 'GS_VARIANT' ##NO_TEXT.
  "! Type - ZDBBR_selfield_itab
  CONSTANTS c_t_multi_or TYPE string VALUE 'GT_MULTI_OR' ##NO_TEXT.
  "! Type - ZDBBR_selfield_itab
  CONSTANTS c_t_multi_select TYPE string VALUE 'GT_MULTI_SELECT' ##NO_TEXT.
  "! Type - ZDBBR_selfield_itab
  CONSTANTS c_t_selection_fields TYPE string VALUE 'GT_SELECTION_FIELDS' ##NO_TEXT.
  "! Type - ZDBBR_selfield_itab
  CONSTANTS c_t_selection_fields_multi TYPE string VALUE 'GT_SELECTION_FIELDS_MULTI' ##NO_TEXT.
  "! Type - ZDBBR_selopt_control_itab
  CONSTANTS c_t_sel_init TYPE string VALUE 'GT_SEL_INIT' ##NO_TEXT.
  "! Type - syst_ucomm
  CONSTANTS c_v_active_tab TYPE string VALUE 'GV_ACTIVE_TAB' ##NO_TEXT.
  "! Type - ZDBBR_button
  CONSTANTS c_v_add_text_icon TYPE string VALUE 'GV_ADD_TEXT_ICON' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_v_fieldname TYPE string VALUE 'GV_FIELDNAME' ##NO_TEXT.
  CONSTANTS c_p_cds_view_name TYPE string VALUE 'P_CDSNAM' ##NO_TEXT.
  "! Type - scrtext_m
  CONSTANTS c_v_field_descr TYPE string VALUE 'GV_FIELD_DESCR' ##NO_TEXT.
  "! Type - syst_tabix
  CONSTANTS c_v_linecount TYPE string VALUE 'GV_LINECOUNT' ##NO_TEXT.
  "! Type - syst_dynnr
  CONSTANTS c_v_main_screen_0100 TYPE string VALUE 'GV_MAIN_SCREEN_0100' ##NO_TEXT.
  "! Type - syst_tabix
  CONSTANTS c_v_multi_select_lines TYPE string VALUE 'GV_MULTI_SELECT_LINES' ##NO_TEXT.
  "! Type - tswpos
  CONSTANTS c_v_or_tuple_number TYPE string VALUE 'GV_OR_TUPLE_NUMBER' ##NO_TEXT.
  "! Type - ddtext
  CONSTANTS c_v_selmask_entity_text TYPE string VALUE 'GV_SELMASK_ENTITY_TEXT' ##NO_TEXT.
  CONSTANTS c_v_selmask_entity_type TYPE string VALUE 'GV_ENTITY_TYPE' ##NO_TEXT.
  CONSTANTS c_v_selmask_entity_name TYPE string VALUE 'GV_ENTITY_NAME' ##NO_TEXT.
  "! Type - syst_dynnr
  CONSTANTS c_v_tree_screen TYPE string VALUE 'GV_TREE_SCREEN' ##NO_TEXT.
  "! Type - smp_dyntxt
  CONSTANTS c_multi_or_icon TYPE string VALUE 'MULTI_OR_ICON' ##NO_TEXT.
  "! Type - cxtab_control
  CONSTANTS c_multi_or_tc TYPE string VALUE 'MULTI_OR_TC' ##NO_TEXT.
  "! Type - cxtab_control
  CONSTANTS c_multi_tc TYPE string VALUE 'MULTI_TC' ##NO_TEXT.
  "! Type - char26
  CONSTANTS c_selection_field_count_txt TYPE string VALUE 'GV_SELFIELD_COUNT' ##NO_TEXT.
  "! Type - ZDBBR_button
  CONSTANTS c_option TYPE string VALUE 'OPTION' ##NO_TEXT.
  "! Type - ZDBBR_button
  CONSTANTS c_push TYPE string VALUE 'PUSH' ##NO_TEXT.
  "! Type - ddtext
  CONSTANTS c_p_scrdec TYPE string VALUE 'P_SCRDEC' ##NO_TEXT.
  "! Type - ZDBBR_query_name
  CONSTANTS c_p_scrnam TYPE string VALUE 'P_SCRNAM' ##NO_TEXT.
  "! Type - tabname16
  CONSTANTS c_p_tab TYPE string VALUE 'P_TAB' ##NO_TEXT.
  "! Type - ZDBBR_variant_name
  CONSTANTS c_p_varnam TYPE string VALUE 'P_VARNAM' ##NO_TEXT.
  "! Type - ddtext
  CONSTANTS c_p_vartxt TYPE string VALUE 'P_VARTXT' ##NO_TEXT.
  "! Type - boolean
  CONSTANTS c_p_xfield TYPE string VALUE 'P_XFIELD' ##NO_TEXT.
  "! Type - boolean
  CONSTANTS c_p_xsort TYPE string VALUE 'P_XSORT' ##NO_TEXT.
  "! Type - abap_bool
  CONSTANTS c_p_has_defined_column_widths TYPE string VALUE 'P_XCOLWD' ##NO_TEXT.
  "! Type - abap_bool
  CONSTANTS c_p_has_selection_criteria TYPE string VALUE 'P_XFILTV' ##NO_TEXT.
  "! Type - tabname
  CONSTANTS c_p_idtab TYPE string VALUE 'P_IDTAB' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_idfld TYPE string VALUE 'P_IDFLD' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_idfld2 TYPE string VALUE 'P_IDFLD2' ##NO_TEXT.
  "! Type - rollname
  CONSTANTS c_p_iddtel TYPE string VALUE 'P_IDDTEL' ##NO_TEXT.
  "! Type - sap_bool
  CONSTANTS c_rb_ttdtel TYPE string VALUE 'P_TTDTEL' ##NO_TEXT.
  "! Type - sap_bool
  CONSTANTS c_rb_ttfld TYPE string VALUE 'P_TTFLD' ##NO_TEXT.
  "! Type - tabname
  CONSTANTS c_p_txttab TYPE string VALUE 'P_TXTTAB' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_txtfld TYPE string VALUE 'P_TXTFLD' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_lngfld TYPE string VALUE 'P_LNGFLD' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_keyfld TYPE string VALUE 'P_KEYFLD' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_keyfld2 TYPE string VALUE 'P_KEYFD2' ##NO_TEXT.
  "! Type - cxtab_control
  CONSTANTS c_selfields_tc TYPE string VALUE 'SELFIELDS_TC' ##NO_TEXT.
  "! Type - SYST_TITLE
  CONSTANTS c_svar_t TYPE string VALUE 'SVAR_T' ##NO_TEXT.
  "! Type - CXTAB_TABSTRIP
  CONSTANTS c_tree_tab TYPE string VALUE 'TREE_TAB' ##NO_TEXT.
  CONSTANTS c_t_cds_view_variant TYPE string VALUE 'CVAR_T' ##NO_TEXT.
  "! Type - SYST_TITLE
  CONSTANTS c_tvar_t TYPE string VALUE 'TVAR_T' ##NO_TEXT.
  "! Type - ZDBBR_altcoltext_data_itab
  CONSTANTS c_t_altcoltext TYPE string VALUE 'GT_ALTCOLTEXT' ##NO_TEXT.
  "! Type - ZDBBR_altcoltext_data
  CONSTANTS c_s_altcoltext TYPE string VALUE 'GS_ALTCOLTEXT' ##NO_TEXT.
  "! Type - cxtab_control
  CONSTANTS c_altcoltext_tc TYPE string VALUE 'ALTCOLTEXT_TC' ##NO_TEXT.
  "! Type - boolean
  CONSTANTS c_p_xglob TYPE string VALUE 'P_XGLOB' ##NO_TEXT.
  "! Type - ZDBBR_query_name
  CONSTANTS c_p_scrnt TYPE string VALUE 'P_SCRNT' ##NO_TEXT.
  "! Type - ddtext
  CONSTANTS c_p_scrdtg TYPE string VALUE 'P_SCRDTG' ##NO_TEXT.
  "! Type - boolean
  CONSTANTS c_p_xglobt TYPE string VALUE 'P_XGLOBT' ##NO_TEXT.
  "! Type - boolean
  CONSTANTS c_p_copyv TYPE string VALUE 'P_COPYV' ##NO_TEXT.
  "! Type - ZDBBR_addtext_cond_operation
  CONSTANTS c_p_condo TYPE string VALUE 'P_CONDO' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_condf TYPE string VALUE 'P_CONDF' ##NO_TEXT.
  "! Type - ZDBBR_addtext_cond_val
  CONSTANTS c_p_condv TYPE string VALUE 'P_CONDV' ##NO_TEXT.
  "! Type - smp_dyntxt
  CONSTANTS c_s_save_function TYPE string VALUE 'SAVE_DYN' ##NO_TEXT.
  "! Type - smp_dyntxt
  CONSTANTS c_s_save_and_stay_function TYPE string VALUE 'SAVENEW_DYN' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_function1 TYPE string VALUE 'ENTITYFC01' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_function2 TYPE string VALUE 'ENTITYFC02' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_function3 TYPE string VALUE 'ENTITYFC03' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_function4 TYPE string VALUE 'ENTITYFC04' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_function5 TYPE string VALUE 'ENTITYFC05' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_function11 TYPE string VALUE 'ENTITYFC11' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_function12 TYPE string VALUE 'ENTITYFC12' ##NO_TEXT.
  CONSTANTS c_s_custom_entity_menu TYPE string VALUE 'ENTITY_TOP_MENU' ##NO_TEXT.
  "! Type - ZDBBR_button
  CONSTANTS c_subquery TYPE string VALUE 'SUBQUERY' ##NO_TEXT.
  CONSTANTS c_r_tabfield_manager TYPE string VALUE 'GR_TABFIELD_MANAGER' ##NO_TEXT.
  CONSTANTS c_r_choose_entity_controller TYPE string VALUE 'GR_CHOOSE_ENTITY_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_f4_screen_controller TYPE string VALUE 'GR_F4_SCREEN_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_multi_select_controller TYPE string VALUE 'GR_MULTI_SELECT_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_multi_select_table TYPE string VALUE 'GR_MULTI_SELECT_TABLE' ##NO_TEXT.
  CONSTANTS c_r_selscreen_controller TYPE string VALUE 'GR_SELSCREEN_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_selscreen_table TYPE string VALUE 'GR_SELSCREEN_TABLE' ##NO_TEXT.
  CONSTANTS c_r_sort_controller TYPE string VALUE 'GR_SORT_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_save_query_controller TYPE string VALUE 'GR_SAVE_QUERY_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_save_sql_query_controller TYPE string VALUE 'GR_SAVE_SQL_QUERY_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_variant_controller TYPE string VALUE 'GR_VARIANT_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_multi_or_table TYPE string VALUE 'GR_MULTI_OR_TABLE' ##NO_TEXT.
  CONSTANTS c_r_multi_or_controller TYPE string VALUE 'GR_MULTI_OR_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_addtextfield_controller TYPE string VALUE 'GR_ADDTEXTFIELD_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_copy_query_controller TYPE string VALUE 'GR_COPY_QUERY_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_choose_query_controller TYPE string VALUE 'GR_CHOOSE_QUERY_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_altcoltext_controller TYPE string VALUE 'GR_ALTCOLTEXT_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_altcoltext_table TYPE string VALUE 'GR_ALTCOLTEXT_TABLE' ##NO_TEXT.
ENDINTERFACE.
