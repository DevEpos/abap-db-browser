"! <p class="shorttext synchronized" lang="en">Data Wrapper for Selection Screen</p>
CLASS zcl_dbbr_selscreen_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mr_s_tableview TYPE REF TO cxtab_control READ-ONLY .
    DATA mr_s_current_line TYPE REF TO zdbbr_selfield READ-ONLY .
    DATA mr_t_table_data TYPE REF TO zdbbr_selfield_itab READ-ONLY .
    DATA mr_v_push_icon TYPE REF TO zdbbr_button READ-ONLY .
    DATA mr_v_multi_or_icon TYPE REF TO smp_dyntxt READ-ONLY .
    DATA mr_s_old_join_def TYPE REF TO zdbbr_join_def READ-ONLY .
    DATA mr_v_option_icon TYPE REF TO zdbbr_button READ-ONLY .
    DATA mr_s_global_data TYPE REF TO zdbbr_global_data READ-ONLY .
    DATA mr_t_sel_init_table TYPE REF TO zdbbr_selopt_control_itab READ-ONLY .
    DATA mr_t_selfields_multi TYPE REF TO zdbbr_selfield_itab READ-ONLY .
    DATA mr_v_seltext_gui TYPE REF TO smp_dyntxt READ-ONLY .
    DATA mr_s_browser_mode TYPE REF TO zdbbr_browser_mode_data READ-ONLY .
    DATA mr_v_selmask_entity_text TYPE REF TO ddtext READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Char 15</p>
    DATA mr_v_selmask_entity_type TYPE REF TO char20 READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Table Name</p>
    DATA mr_v_selmask_entity_name TYPE REF TO tabname READ-ONLY .
    DATA mr_v_variant_description TYPE REF TO ddtext READ-ONLY .
    DATA mr_v_variant_name TYPE REF TO zdbbr_variant_name READ-ONLY .
    DATA mr_v_seltable_counter_text TYPE REF TO char26 READ-ONLY .
    DATA mo_custom_f4_map TYPE REF TO zcl_dbbr_custom_f4_map READ-ONLY .
    DATA mo_selection_table TYPE REF TO zcl_dbbr_selscreen_table READ-ONLY .
    DATA mr_s_entity_info TYPE REF TO zdbbr_entity_info_simple READ-ONLY .
    DATA mr_t_table_to_alias_map TYPE REF TO zdbbr_table_to_alias_map_itab READ-ONLY .
    " attributes for data selection
    DATA mo_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list READ-ONLY .
    DATA mo_tabfield_aggr_list TYPE REF TO zcl_dbbr_tabfield_list READ-ONLY .
    DATA mr_s_settings TYPE REF TO zdbbr_selscreen_settings READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Short information about query</p>
    DATA mr_s_query_info TYPE REF TO zdbbr_query_info READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Menu Painter: Program interface for dynamic texts</p>
    DATA mr_s_top_custom_menu TYPE REF TO smp_dyntxt READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Menu Painter: Program interface for dynamic texts</p>
    DATA mr_s_entity_function1 TYPE REF TO smp_dyntxt READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Menu Painter: Program interface for dynamic texts</p>
    DATA mr_s_entity_function2 TYPE REF TO smp_dyntxt READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Menu Painter: Program interface for dynamic texts</p>
    DATA mr_s_entity_function3 TYPE REF TO smp_dyntxt READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Menu Painter: Program interface for dynamic texts</p>
    DATA mr_s_entity_function4 TYPE REF TO smp_dyntxt READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Menu Painter: Program interface for dynamic texts</p>
    DATA mr_s_entity_function5 TYPE REF TO smp_dyntxt READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Definition of a Join</p>
    DATA mr_s_join_def TYPE REF TO zdbbr_join_def READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Button for DB Browser</p>
    DATA mr_v_interval_on_off TYPE REF TO zdbbr_button READ-ONLY .
    DATA mr_f_from_central_search TYPE REF TO abap_bool READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">Stores current join definition for comparison reasons</p>
    METHODS store_old_join .
    METHODS get_mode
      RETURNING
        VALUE(result) TYPE zdbbr_selscreen_mode .
    METHODS set_mode
      IMPORTING
        !value TYPE zdbbr_selscreen_mode .
    METHODS get_custom_f4_map
      RETURNING
        VALUE(result) TYPE REF TO zcl_dbbr_custom_f4_map .
    METHODS get_formula
      RETURNING
        VALUE(result) TYPE REF TO zcl_dbbr_formula .
    METHODS set_formula
      IMPORTING
        !value TYPE REF TO zcl_dbbr_formula .
    METHODS get_multi_or_all
      RETURNING
        VALUE(result) TYPE REF TO zdbbr_or_seltab_itab .
    METHODS is_join_active
      RETURNING
        VALUE(result) TYPE boolean .
    METHODS set_join_active
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS is_query_mode
      RETURNING
        VALUE(result) TYPE boolean .
    METHODS is_cds_mode
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS is_table_mode
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS set_tabfield_aggr_list
      IMPORTING
        !value TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS set_tabfield_list
      IMPORTING
        !value TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Selection screen resides in multi table mode</p>
    METHODS is_multi_table_mode
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS constructor
      IMPORTING
        !ir_selection_table TYPE REF TO zcl_dbbr_selscreen_table .
    "! <p class="shorttext synchronized" lang="en">Clears all custom functions</p>
    METHODS clear_custom_functions .
    "! <p class="shorttext synchronized" lang="en">Clear all references values</p>
    METHODS clear .
    "! <p class="shorttext synchronized" lang="en">Clears multi or tuple values</p>
    METHODS clear_multi_or.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_multi_or_all TYPE zdbbr_or_seltab_itab .
    DATA mf_join_is_active TYPE abap_bool .
    DATA mf_group_fields_updated TYPE abap_bool .
    DATA mo_formula TYPE REF TO zcl_dbbr_formula .
    DATA mv_mode TYPE zdbbr_selscreen_mode .
ENDCLASS.



CLASS zcl_dbbr_selscreen_data IMPLEMENTATION.


  METHOD clear.
    CLEAR: mr_s_global_data->primary_table,
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
           mr_s_query_info->*,
           mr_s_join_def->*,
           mo_formula.

    mo_custom_f4_map->clear( ).
    mo_selection_table->clear( ).
    mo_tabfield_list->clear( ).
    mo_tabfield_aggr_list->clear( ).
  ENDMETHOD.


  METHOD clear_custom_functions.
    CLEAR: mr_s_entity_function1->*,
           mr_s_entity_function2->*,
           mr_s_entity_function3->*,
           mr_s_entity_function4->*,
           mr_s_entity_function5->*.
  ENDMETHOD.


  METHOD constructor.
    mo_selection_table = ir_selection_table.

    DEFINE read_cached_field.
      &1 = CAST #( lr_data_cache->get_data_ref( &2 ) ).
    END-OF-DEFINITION.

    " init global data references from cache
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

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
    mo_tabfield_list = NEW #( ).
    mo_tabfield_aggr_list = NEW #( ).
    mo_custom_f4_map = NEW #( ).
    mr_s_query_info = NEW #( ).
    mr_s_join_def = NEW #( ).
    mr_s_old_join_def = NEW #( ).
    mr_f_from_central_search = NEW #( ).
  ENDMETHOD.


  METHOD get_custom_f4_map.
    result = mo_custom_f4_map.
  ENDMETHOD.


  METHOD get_formula.
    result = mo_formula.
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
    result = mo_tabfield_list->has_multiple_tables( ).
  ENDMETHOD.


  METHOD is_query_mode.
    result = xsdbool( mv_mode = zif_dbbr_c_selscreen_mode=>query ).
  ENDMETHOD.


  METHOD is_table_mode.
    result = xsdbool( mv_mode = zif_dbbr_c_selscreen_mode=>table ).
  ENDMETHOD.


  METHOD set_formula.
    mo_formula = value.
  ENDMETHOD.


  METHOD set_join_active.
    mf_join_is_active = value.
  ENDMETHOD.


  METHOD set_mode.
    mv_mode = value.
  ENDMETHOD.


  METHOD set_tabfield_aggr_list.
    mo_tabfield_aggr_list = value.
  ENDMETHOD.


  METHOD set_tabfield_list.
    mo_tabfield_list = value.
  ENDMETHOD.


  METHOD store_old_join.
    mr_s_old_join_def->* = mr_s_join_def->*.
    IF mr_s_old_join_def->primary_table_alias IS INITIAL.
      mr_s_old_join_def->primary_table_alias = mr_s_join_def->primary_table.
    ENDIF.
  ENDMETHOD.

  METHOD clear_multi_or.
    CLEAR mt_multi_or_all.
  ENDMETHOD.

ENDCLASS.
