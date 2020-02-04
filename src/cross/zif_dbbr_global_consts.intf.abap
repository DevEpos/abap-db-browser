INTERFACE zif_dbbr_global_consts
  PUBLIC .

  CONSTANTS:
    BEGIN OF c_domain_names,
      timestamp      TYPE domname VALUE 'TZNTSTMPS',
      timestamp_long TYPE domname VALUE 'TZNTSTMPL',
    END OF c_domain_names.
  CONSTANTS c_dummy_variant TYPE zdbbr_variant_id VALUE 'DUMMY' ##NO_TEXT.
  CONSTANTS c_default_max_lines TYPE sy-tabix VALUE 500 ##NO_TEXT.
  CONSTANTS c_default_last_used_favs TYPE sy-tabix VALUE 10 ##NO_TEXT.
  CONSTANTS c_formula_alias TYPE ZSAT_TABLE_ALIAS VALUE 'X' ##NO_TEXT.
  CONSTANTS gc_default_numeric_type TYPE dd04l-rollname VALUE 'ZDBBR_DEC17_5' ##NO_TEXT.
  CONSTANTS gc_formula_dummy_table TYPE tabname VALUE '-FORMULA-' ##NO_TEXT.
  CONSTANTS c_parameter_dummy_table TYPE tabname VALUE '-PARAMS-' ##NO_TEXT.
  CONSTANTS:
    " constants for icons
    " special columns in output table
    BEGIN OF gc_subquery_modes,
      in     TYPE zdbbr_subquery_mode VALUE 'IN',
      not_in TYPE zdbbr_subquery_mode VALUE 'NOT IN',
      exists TYPE zdbbr_subquery_mode VALUE 'EXISTS',
      all    TYPE zdbbr_subquery_mode VALUE 'ALL',
      any    TYPE zdbbr_subquery_mode VALUE 'ANY',
    END OF gc_subquery_modes .
  CONSTANTS:
    BEGIN OF gc_popup_answers,
      yes    TYPE char1 VALUE '1',
      no     TYPE char1 VALUE '2',
      cancel TYPE char1 VALUE 'A',
    END OF gc_popup_answers .
  CONSTANTS:
    BEGIN OF gc_jump_call_types,
      normal         TYPE zdbbr_jump_target_type VALUE 'N',
      start_new_mode TYPE zdbbr_jump_target_type VALUE 'T',
    END OF gc_jump_call_types .
  CONSTANTS:
    BEGIN OF c_formfield_types,
      calculation TYPE zdbbr_formfield_type VALUE 'CALC',
      icon        TYPE zdbbr_formfield_type VALUE 'ICON',
    END OF c_formfield_types .
  CONSTANTS:
    BEGIN OF gc_display_modes,
      view   TYPE zdbbr_display_mode VALUE '1',
      edit   TYPE zdbbr_display_mode VALUE '2',
      create TYPE zdbbr_display_mode VALUE '3',
    END OF gc_display_modes .
  CONSTANTS:
    BEGIN OF gc_variant_datatypes,
      group_by     TYPE zdbbr_variant_datatype VALUE 'GROUP_BY' ##no_text,
      aggregation  TYPE zdbbr_variant_datatype VALUE 'AGGREGATION' ##no_text,
      or_selection TYPE zdbbr_variant_datatype VALUE 'OR_SELECTION' ##no_text,
    END OF gc_variant_datatypes .
  CONSTANTS:
    BEGIN OF gc_field_chooser_modes,
      output    TYPE zdbbr_field_chooser_mode VALUE 'OUTPUT' ##no_text,
      selection TYPE zdbbr_field_chooser_mode VALUE 'SELECTION' ##no_text,
      sort      TYPE zdbbr_field_chooser_mode VALUE 'SORT' ##no_text,
    END OF gc_field_chooser_modes .
  CONSTANTS:
    BEGIN OF gc_variant_ids,
      dummy_variant      TYPE slis_vari VALUE 'ZDBBR######',
      dummy_join_variant TYPE slis_vari VALUE 'ZDBBRJOIN##',
    END OF gc_variant_ids .
  CONSTANTS dummy_variant TYPE slis_vari VALUE 'ZDBBR#####' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF gc_fav_user_modes,
      user_specific TYPE zdbbr_fav_user_mode VALUE 'U' ##no_text,
      global        TYPE zdbbr_fav_user_mode VALUE 'G' ##no_text,
    END OF gc_fav_user_modes .
  CONSTANTS:
    BEGIN OF gc_searchhelp_types,
      search_help       TYPE ddshlptyp VALUE 'SH' ##no_text,
      domain_fix_values TYPE ddshlptyp VALUE 'FV' ##no_text,
    END OF gc_searchhelp_types .
  CONSTANTS:
    BEGIN OF gc_options,
      default(1)     VALUE space ##no_text,
      i(1)           VALUE 'I' ##no_text,
      e(1)           VALUE 'E' ##no_text,
      bt(2)          VALUE 'BT' ##no_text,
      nb(2)          VALUE 'NB' ##no_text,
      eq(2)          VALUE 'EQ' ##no_text,
      ne(2)          VALUE 'NE' ##no_text,
      gt(2)          VALUE 'GT' ##no_text,
      ge(2)          VALUE 'GE' ##no_text,
      lt(2)          VALUE 'LT' ##no_text,
      le(2)          VALUE 'LE' ##no_text,
      cp(2)          VALUE 'CP' ##no_text,
      np(2)          VALUE 'NP' ##no_text,
      is_null(2)     VALUE 'IN' ##no_text,
      is_not_null(2) VALUE 'NN' ##no_text,
      n(40)          VALUE 'NB,NE' ##no_text,
    END OF gc_options .
  CONSTANTS:
    BEGIN OF gc_alv_emphasize,
      key_color         TYPE char4 VALUE 'C11' ##no_text,
      foreign_key_color TYPE char4 VALUE 'C30' ##no_text,
      join_table_color  TYPE char4 VALUE 'C10' ##no_text,
      text_field_color  TYPE char4 VALUE 'C70' ##no_text,
    END OF gc_alv_emphasize .
  CONSTANTS:
    BEGIN OF gc_alv_colors,
      light_yellow    TYPE char4 VALUE 'C300' ##no_text,
      yellow          TYPE char4 VALUE 'C310' ##no_text,
      light_green     TYPE char4 VALUE 'C500' ##no_text,
      green           TYPE char4 VALUE 'C510' ##no_text,
      light_blue      TYPE char4 VALUE 'C100' ##no_text,
      blue            TYPE char4 VALUE 'C110' ##no_text,
      light_grey_blue TYPE char4 VALUE 'C400' ##no_text,
      grey_blue       TYPE char4 VALUE 'C410' ##no_text,
      light_orange    TYPE char4 VALUE 'C700' ##no_text,
      orange          TYPE char4 VALUE 'C710' ##no_text,
      light_red       TYPE char4 VALUE 'C600' ##no_text,
      red             TYPE char4 VALUE 'C610' ##no_text,
    END OF gc_alv_colors .
  CONSTANTS:
    BEGIN OF gc_order_by_type,
      ascending  TYPE se16n_toplow VALUE 'ASC' ##no_text,
      descending TYPE se16n_toplow VALUE 'DES' ##no_text,
    END OF gc_order_by_type .
  CONSTANTS:
    BEGIN OF gc_sort_direction,
      ascending  TYPE zdbbr_sort_direction VALUE 'ASC' ##no_text,
      descending TYPE zdbbr_sort_direction VALUE 'DES' ##no_text,
    END OF gc_sort_direction .
  CONSTANTS:
    BEGIN OF gc_dynpro_code,
      custom_f4_create          LIKE sy-dynnr VALUE '0202' ##no_text,
      custom_built_in_f4_create LIKE sy-dynnr VALUE '0101' ##no_text,
    END OF gc_dynpro_code .
  CONSTANTS gc_container TYPE scrfname VALUE 'RESULT_LIST' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF gc_function_codes,
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
    END OF gc_function_codes .
  CONSTANTS c_for_all_entries_select_table TYPE string VALUE '<lt_for_all>' ##NO_TEXT.
ENDINTERFACE.
