class ZCL_DBBR_SELSCREEN_DATA definition
  public
  final
  create public .

public section.

  data MR_S_TABLEVIEW type ref to CXTAB_CONTROL read-only .
  data MR_S_CURRENT_LINE type ref to ZDBBR_SELFIELD read-only .
  data MR_T_TABLE_DATA type ref to ZDBBR_SELFIELD_ITAB read-only .
  data MR_V_PUSH_ICON type ref to ZDBBR_BUTTON read-only .
  data MR_V_MULTI_OR_ICON type ref to SMP_DYNTXT read-only .
  data MR_S_OLD_JOIN_DEF type ref to ZDBBR_JOIN_DEF read-only .
  data MR_V_OPTION_ICON type ref to ZDBBR_BUTTON read-only .
  data MR_S_GLOBAL_DATA type ref to ZDBBR_GLOBAL_DATA read-only .
  data MR_T_SEL_INIT_TABLE type ref to ZDBBR_SELOPT_CONTROL_ITAB read-only .
  data MR_T_SELFIELDS_MULTI type ref to ZDBBR_SELFIELD_ITAB read-only .
  data MR_V_SELTEXT_GUI type ref to SMP_DYNTXT read-only .
  data MR_S_BROWSER_MODE type ref to ZDBBR_BROWSER_MODE_DATA read-only .
  data MR_V_SELMASK_ENTITY_TEXT type ref to DDTEXT read-only .
  data MR_V_SELMASK_ENTITY_TYPE type ref to CHAR20 read-only .
  data MR_V_SELMASK_ENTITY_NAME type ref to TABNAME read-only .
  data MR_V_VARIANT_DESCRIPTION type ref to DDTEXT read-only .
  data MR_V_VARIANT_NAME type ref to ZDBBR_VARIANT_NAME read-only .
  data MR_V_SELTABLE_COUNTER_TEXT type ref to CHAR26 read-only .
  data MR_CUSTOM_F4_MAP type ref to ZCL_DBBR_CUSTOM_F4_MAP read-only .
  data MR_SELECTION_TABLE type ref to ZCL_DBBR_SELSCREEN_TABLE read-only .
  data MR_S_ENTITY_INFO type ref to ZDBBR_ENTITY_INFO_SIMPLE read-only .
  data MR_T_TABLE_TO_ALIAS_MAP type ref to ZDBBR_TABLE_TO_ALIAS_MAP_ITAB read-only .
    " attributes for data selection
  data MR_TABFIELD_LIST type ref to ZCL_DBBR_TABFIELD_LIST read-only .
  data MR_TABFIELD_AGGR_LIST type ref to ZCL_DBBR_TABFIELD_LIST read-only .
  data MR_S_SETTINGS type ref to ZDBBR_SELSCREEN_SETTINGS read-only .
  data MR_S_query_INFO type ref to ZDBBR_query_INFO read-only .
  data MR_S_TOP_CUSTOM_MENU type ref to SMP_DYNTXT read-only .
  data MR_S_ENTITY_FUNCTION1 type ref to SMP_DYNTXT read-only .
  data MR_S_ENTITY_FUNCTION2 type ref to SMP_DYNTXT read-only .
  data MR_S_ENTITY_FUNCTION3 type ref to SMP_DYNTXT read-only .
  data MR_S_ENTITY_FUNCTION4 type ref to SMP_DYNTXT read-only .
  data MR_S_ENTITY_FUNCTION5 type ref to SMP_DYNTXT read-only .
  data MR_S_JOIN_DEF type ref to ZDBBR_JOIN_DEF read-only .
  data MR_V_INTERVAL_ON_OFF type ref to ZDBBR_BUTTON read-only .
  data mr_f_from_central_search type ref to abap_bool read-only.

  methods STORE_OLD_JOIN .
  methods GET_MODE
    returning
      value(RESULT) type ZDBBR_SELSCREEN_MODE .
  methods SET_MODE
    importing
      !VALUE type ZDBBR_SELSCREEN_MODE .
  methods GET_CUSTOM_F4_MAP
    returning
      value(RESULT) type ref to ZCL_DBBR_CUSTOM_F4_MAP .
  methods GET_FORMULA
    returning
      value(RESULT) type ref to ZCL_DBBR_FORMULA .
  methods SET_FORMULA
    importing
      !VALUE type ref to ZCL_DBBR_FORMULA .
  methods GET_MULTI_OR_ALL
    returning
      value(RESULT) type ref to ZDBBR_OR_SELTAB_ITAB .
  methods IS_JOIN_ACTIVE
    returning
      value(RESULT) type BOOLEAN .
  methods SET_JOIN_ACTIVE
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods IS_query_MODE
    returning
      value(RESULT) type BOOLEAN .
  methods IS_CDS_MODE
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_TABLE_MODE
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_TABFIELD_AGGR_LIST
    importing
      !VALUE type ref to ZCL_DBBR_TABFIELD_LIST .
  methods SET_TABFIELD_LIST
    importing
      !VALUE type ref to ZCL_DBBR_TABFIELD_LIST .
  methods IS_MULTI_TABLE_MODE
    returning
      value(RESULT) type ABAP_BOOL .
  methods CONSTRUCTOR
    importing
      !IR_SELECTION_TABLE type ref to ZCL_DBBR_SELSCREEN_TABLE .
  methods CLEAR_CUSTOM_FUNCTIONS .
  methods CLEAR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_multi_or_all TYPE zdbbr_or_seltab_itab .
    DATA mf_join_is_active TYPE abap_bool .
    DATA mf_group_fields_updated TYPE abap_bool .
    DATA mr_formula TYPE REF TO zcl_dbbr_formula .
    DATA mv_mode TYPE zdbbr_selscreen_mode .
ENDCLASS.



CLASS ZCL_DBBR_SELSCREEN_DATA IMPLEMENTATION.


  METHOD clear.
    CLEAR: mr_s_global_data->primary_table,
           mr_s_global_data->alv_variant,
           mr_s_global_data->alv_varianttext,
           mr_s_global_data->join_key,
           mr_s_global_data->query_name,
           mr_s_current_line->*,
           mr_t_table_data->*,
           mr_v_push_icon->*,
           mr_v_multi_or_icon->*,
           mr_s_old_join_def->*,
           mr_v_option_icon->*,
           mr_t_selfields_multi->*,
           mr_v_seltext_gui->*,
           mr_s_browser_mode->*,
           mr_v_selmask_entity_text->*,
           mr_v_selmask_entity_name->*,
           mr_v_variant_description->*,
           mr_v_variant_name->*,
           mr_v_seltable_counter_text->*,
           mr_t_table_to_alias_map->*,
           mr_s_query_info->*,
           mr_s_join_def->*.

    mr_custom_f4_map->clear( ).
    mr_selection_table->clear( ).
    mr_tabfield_list->clear( ).
    mr_tabfield_aggr_list->clear( ).
  ENDMETHOD.


  METHOD clear_custom_functions.
    CLEAR: mr_s_entity_function1->*,
           mr_s_entity_function2->*,
           mr_s_entity_function3->*,
           mr_s_entity_function4->*,
           mr_s_entity_function5->*.
  ENDMETHOD.


  METHOD constructor.
    mr_selection_table = ir_selection_table.

    DEFINE read_cached_field.
      &1 = cast #( lr_data_cache->get_data_ref( &2 ) ).
    END-OF-DEFINITION.

    " init global data references from cache
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    read_cached_field:
       mr_s_tableview             zif_dbbr_main_report_var_ids=>c_selfields_tc,
       mr_s_current_line          zif_dbbr_main_report_var_ids=>c_s_selfields,
       mr_t_table_data            zif_dbbr_main_report_var_ids=>c_t_selection_fields,
       mr_v_push_icon             zif_dbbr_main_report_var_ids=>c_push,
       mr_v_multi_or_icon         zif_dbbr_main_report_var_ids=>c_multi_or_icon,
       mr_v_option_icon           zif_dbbr_main_report_var_ids=>c_option,
       mr_s_global_data           zif_dbbr_main_report_var_ids=>c_s_data,
       mr_t_sel_init_table        zif_dbbr_main_report_var_ids=>c_t_sel_init,
       mr_t_selfields_multi       zif_dbbr_main_report_var_ids=>c_t_selection_fields_multi,
       mr_v_seltext_gui           zif_dbbr_main_report_var_ids=>c_s_sel_text_gui_text,
       mr_s_browser_mode          zif_dbbr_main_report_var_ids=>c_s_browser_mode,
       mr_v_selmask_entity_text   zif_dbbr_main_report_var_ids=>c_v_selmask_entity_text,
       mr_v_selmask_entity_type   zif_dbbr_main_report_var_ids=>c_v_selmask_entity_type,
       mr_v_selmask_entity_name   zif_dbbr_main_report_var_ids=>c_v_selmask_entity_name,
       mr_v_variant_description   zif_dbbr_main_report_var_ids=>c_p_vartxt,
       mr_s_entity_info           zif_dbbr_main_report_var_ids=>c_s_entity_info,
       mr_v_variant_name          zif_dbbr_main_report_var_ids=>c_p_varnam,
       mr_v_seltable_counter_text zif_dbbr_main_report_var_ids=>c_selection_field_count_txt,
       mr_s_top_custom_menu       zif_dbbr_main_report_var_ids=>c_s_custom_entity_menu,
       mr_s_entity_function1      zif_dbbr_main_report_var_ids=>c_s_custom_entity_function1,
       mr_s_entity_function2      zif_dbbr_main_report_var_ids=>c_s_custom_entity_function2,
       mr_s_entity_function3      zif_dbbr_main_report_var_ids=>c_s_custom_entity_function3,
       mr_s_entity_function4      zif_dbbr_main_report_var_ids=>c_s_custom_entity_function4,
       mr_s_entity_function5      zif_dbbr_main_report_var_ids=>c_s_custom_entity_function5.

    mr_s_settings = NEW #( ).

    "" register for events of selection table
    mr_tabfield_list = NEW #( ).
    mr_tabfield_aggr_list = NEW #( ).
    mr_custom_f4_map = NEW #( ).
    mr_s_query_info = NEW #( ).
    mr_s_join_def = NEW #( ).
    mr_s_old_join_def = NEW #( ).
    mr_f_from_central_search = new #( ).

    mr_t_table_to_alias_map = NEW zdbbr_table_to_alias_map_itab( ).
  ENDMETHOD.


  METHOD get_custom_f4_map.
    result = mr_custom_f4_map.
  ENDMETHOD.


  METHOD get_formula.
    result = mr_formula.
  ENDMETHOD.


  METHOD get_mode.
    result = mv_mode.
  ENDMETHOD.


  METHOD get_multi_or_all.
    result = REF #( mt_multi_or_all ).
  ENDMETHOD.


  METHOD is_cds_mode.
    result = xsdbool( mv_mode = zif_dbbr_c_selscreen_mode=>cds_view ).
  ENDMETHOD.


  METHOD is_join_active.
    result = mf_join_is_active.
  ENDMETHOD.


  METHOD is_multi_table_mode.
    result = mr_tabfield_list->has_multiple_tables( ).
  ENDMETHOD.


  METHOD is_query_mode.
    result = xsdbool( mv_mode = zif_dbbr_c_selscreen_mode=>query ).
  ENDMETHOD.


  METHOD is_table_mode.
    result = xsdbool( mv_mode = zif_dbbr_c_selscreen_mode=>table ).
  ENDMETHOD.


  METHOD set_formula.
    mr_formula = value.
  ENDMETHOD.


  METHOD set_join_active.
    mf_join_is_active = value.
  ENDMETHOD.


  METHOD set_mode.
    mv_mode = value.
  ENDMETHOD.


  METHOD set_tabfield_aggr_list.
    mr_tabfield_aggr_list = value.
  ENDMETHOD.


  METHOD set_tabfield_list.
    mr_tabfield_list = value.
  ENDMETHOD.


  METHOD store_old_join.
    mr_s_old_join_def->* = mr_s_join_def->*.
  ENDMETHOD.
ENDCLASS.
