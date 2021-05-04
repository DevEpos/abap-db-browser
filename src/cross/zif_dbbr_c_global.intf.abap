"! <p class="shorttext synchronized" lang="en">Global constants</p>
INTERFACE zif_dbbr_c_global
  PUBLIC .

  CONSTANTS:
    "! Important domain names
    BEGIN OF c_domain_names,
      timestamp      TYPE domname VALUE 'TZNTSTMPS',
      timestamp_long TYPE domname VALUE 'TZNTSTMPL',
    END OF c_domain_names,

    "! Id of the dummy variant which is used to execute DB Browser
    "! with default settings
    c_dummy_variant          TYPE zdbbr_variant_id VALUE 'DUMMY' ##NO_TEXT,
    "! Default value for the 'Max Lines' User setting
    c_default_max_lines      TYPE sy-tabix VALUE 500 ##NO_TEXT,
    "! Default value for the 'Max History Entries' setting
    c_default_last_used_favs TYPE sy-tabix VALUE 10 ##NO_TEXT,
    "! Table Alias for formula fields
    c_formula_alias          TYPE zsat_table_alias VALUE 'X' ##NO_TEXT,
    "! DDIC Type for numeric fields
    c_default_numeric_type   TYPE dd04l-rollname VALUE 'ZDBBR_DEC17_5' ##NO_TEXT,
    "! Dummy Table for formula fields
    c_formula_dummy_table    TYPE tabname VALUE '-FORMULA-' ##NO_TEXT,
    "! Dummy Table for parameters (either from CDS Views or Queries)
    c_parameter_dummy_table  TYPE tabname VALUE '-PARAMS-' ##NO_TEXT,

    "! Modes for Sub Queries
    BEGIN OF c_subquery_modes,
      in     TYPE zdbbr_subquery_mode VALUE 'IN',
      not_in TYPE zdbbr_subquery_mode VALUE 'NOT IN',
      exists TYPE zdbbr_subquery_mode VALUE 'EXISTS',
      all    TYPE zdbbr_subquery_mode VALUE 'ALL',
      any    TYPE zdbbr_subquery_mode VALUE 'ANY',
    END OF c_subquery_modes ,

    "! Dialog answers
    BEGIN OF c_popup_answers,
      yes    TYPE char1 VALUE '1',
      no     TYPE char1 VALUE '2',
      cancel TYPE char1 VALUE 'A',
    END OF c_popup_answers ,

    "! Call types for Jumps of DB Browser Query Jump Definitions
    BEGIN OF c_jump_call_types,
      normal         TYPE zdbbr_jump_target_type VALUE 'N',
      start_new_mode TYPE zdbbr_jump_target_type VALUE 'T',
    END OF c_jump_call_types ,

    "! Types of form fields
    BEGIN OF c_formfield_types,
      calculation TYPE zdbbr_formfield_type VALUE 'CALC',
      icon        TYPE zdbbr_formfield_type VALUE 'ICON',
    END OF c_formfield_types ,

    "! Display modes<br/>
    "! To be used in dialogs
    BEGIN OF c_display_modes,
      view   TYPE zdbbr_display_mode VALUE '1',
      edit   TYPE zdbbr_display_mode VALUE '2',
      create TYPE zdbbr_display_mode VALUE '3',
    END OF c_display_modes ,

    "! Data types for storing variant values
    BEGIN OF c_variant_datatypes,
      group_by     TYPE zdbbr_variant_datatype VALUE 'GROUP_BY' ##NO_TEXT,
      aggregation  TYPE zdbbr_variant_datatype VALUE 'AGGREGATION' ##NO_TEXT,
      totals       TYPE zdbbr_variant_datatype VALUE 'TOTALS' ##NO_TEXT,
      or_selection TYPE zdbbr_variant_datatype VALUE 'OR_SELECTION' ##NO_TEXT,
    END OF c_variant_datatypes ,

    "! Modes for the Table Field Manager <br/>
    "! See {@link zcl_dbbr_tabfield_manager.meth:constructor} (ZCL_DBBR_TABFIELD_MANAGER)
    BEGIN OF c_field_chooser_modes,
      output    TYPE zdbbr_field_chooser_mode VALUE 'OUTPUT' ##NO_TEXT,
      selection TYPE zdbbr_field_chooser_mode VALUE 'SELECTION' ##NO_TEXT,
      sort      TYPE zdbbr_field_chooser_mode VALUE 'SORT' ##NO_TEXT,
    END OF c_field_chooser_modes ,

    "! Predefined variant IDs
    BEGIN OF c_variant_ids,
      dummy_variant      TYPE slis_vari VALUE 'ZDBBR######',
      dummy_join_variant TYPE slis_vari VALUE 'ZDBBRJOIN##',
    END OF c_variant_ids ,

    "! Modes for the Favorite Tree in the Object Navigator
    BEGIN OF c_fav_user_modes,
      user_specific TYPE zdbbr_fav_user_mode VALUE 'U' ##NO_TEXT,
      global        TYPE zdbbr_fav_user_mode VALUE 'G' ##NO_TEXT,
    END OF c_fav_user_modes ,

    "! Types of Search Helps
    BEGIN OF c_searchhelp_types,
      search_help       TYPE ddshlptyp VALUE 'SH' ##NO_TEXT,
      domain_fix_values TYPE ddshlptyp VALUE 'FV' ##NO_TEXT,
    END OF c_searchhelp_types ,

    "! Signs and Options for Range Tables
    BEGIN OF c_options,
      default(1)     VALUE space ##NO_TEXT,
      i(1)           VALUE 'I' ##NO_TEXT,
      e(1)           VALUE 'E' ##NO_TEXT,
      bt(2)          VALUE 'BT' ##NO_TEXT,
      nb(2)          VALUE 'NB' ##NO_TEXT,
      eq(2)          VALUE 'EQ' ##NO_TEXT,
      ne(2)          VALUE 'NE' ##NO_TEXT,
      gt(2)          VALUE 'GT' ##NO_TEXT,
      ge(2)          VALUE 'GE' ##NO_TEXT,
      lt(2)          VALUE 'LT' ##NO_TEXT,
      le(2)          VALUE 'LE' ##NO_TEXT,
      cp(2)          VALUE 'CP' ##NO_TEXT,
      np(2)          VALUE 'NP' ##NO_TEXT,
      is_null(2)     VALUE 'IN' ##NO_TEXT,
      is_not_null(2) VALUE 'NN' ##NO_TEXT,
      n(40)          VALUE 'NB,NE' ##NO_TEXT,
    END OF c_options ,

    "! Emphasize colors in ALV
    BEGIN OF c_alv_emphasize,
      key_color                   TYPE char4 VALUE 'C11' ##NO_TEXT,
      foreign_key_color           TYPE char4 VALUE 'C30' ##NO_TEXT,
      join_table_color            TYPE char4 VALUE 'C10' ##NO_TEXT,
      text_field_color            TYPE char4 VALUE 'C70' ##NO_TEXT,
      formula_fields_color        TYPE char4 VALUE 'C300' ##NO_TEXT,
      cds_calculated_fields_color TYPE char4 VALUE 'C400' ##NO_TEXT,
    END OF c_alv_emphasize ,

    "! Colors in ALV
    BEGIN OF c_alv_colors,
      light_yellow    TYPE char4 VALUE 'C300' ##NO_TEXT,
      yellow          TYPE char4 VALUE 'C310' ##NO_TEXT,
      light_green     TYPE char4 VALUE 'C500' ##NO_TEXT,
      green           TYPE char4 VALUE 'C510' ##NO_TEXT,
      light_blue      TYPE char4 VALUE 'C100' ##NO_TEXT,
      blue            TYPE char4 VALUE 'C110' ##NO_TEXT,
      light_grey_blue TYPE char4 VALUE 'C400' ##NO_TEXT,
      grey_blue       TYPE char4 VALUE 'C410' ##NO_TEXT,
      light_orange    TYPE char4 VALUE 'C700' ##NO_TEXT,
      orange          TYPE char4 VALUE 'C710' ##NO_TEXT,
      light_red       TYPE char4 VALUE 'C600' ##NO_TEXT,
      red             TYPE char4 VALUE 'C610' ##NO_TEXT,
    END OF c_alv_colors ,

    "! Sort directions in SQL
    BEGIN OF c_sort_direction,
      ascending  TYPE zdbbr_sort_direction VALUE 'ASC' ##NO_TEXT,
      descending TYPE zdbbr_sort_direction VALUE 'DES' ##NO_TEXT,
    END OF c_sort_direction ,

    "! IDs for some Screens
    BEGIN OF c_dynpro_code,
      custom_f4_create          LIKE sy-dynnr VALUE '0202' ##no_text,
      custom_built_in_f4_create LIKE sy-dynnr VALUE '0101' ##no_text,
    END OF c_dynpro_code ,
    c_container TYPE scrfname VALUE 'RESULT_LIST' ##NO_TEXT,

    "! Global Function codes
    BEGIN OF c_function_codes,
      leave_screen     TYPE sy-ucomm VALUE '&F03',
      search           TYPE sy-ucomm VALUE 'SEARCH',
      search_further   TYPE sy-ucomm VALUE 'SEARCH_F',
      quit_program     TYPE sy-ucomm VALUE '&F15',
      cancel_screen    TYPE sy-ucomm VALUE '&F12',
      cancel           TYPE sy-ucomm VALUE 'CANCEL',
      enter            TYPE sy-ucomm VALUE 'ENTER',
      scroll_to_top    TYPE sy-ucomm VALUE 'PAGE_TOP',
      scroll_page_up   TYPE sy-ucomm VALUE 'PAGE_UP',
      scroll_page_down TYPE sy-ucomm VALUE 'PAGE_DOWN',
      scroll_to_bottom TYPE sy-ucomm VALUE 'PAGE_END',
    END OF c_function_codes ,
    c_for_all_entries_select_table TYPE string VALUE '<lt_for_all>' ##NO_TEXT.
ENDINTERFACE.
