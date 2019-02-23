INTERFACE zif_dbbr_user_settings_ids
  PUBLIC .


  "! Type ABAP_BOOL
  CONSTANTS c_advanced_mode TYPE string VALUE 'P_ADVM' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_auto_layout_transfer TYPE string VALUE 'P_AUTOL' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_color_add_text_fields TYPE string VALUE 'P_COLAT' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_color_formula_fields TYPE string VALUE 'P_COLFF' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_color_sorted_columns TYPE string VALUE 'P_COLSO' ##NO_TEXT.
  "! Type ZDBBR_FAV_USER_MODE
  CONSTANTS c_favorite_mode_entry TYPE string VALUE 'P_FAVMO' ##NO_TEXT.
  "! Type SY-TABIX
  CONSTANTS c_number_fav_most_used TYPE string VALUE 'P_FLUSED' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_object_navigator_at_start TYPE string VALUE 'P_ONAST' ##NO_TEXT.
  "! Type zdbbr_obj_navigator_Mode
  CONSTANTS c_initial_obj_nav_mode TYPE string VALUE 'P_IOBJM' ##no_text.
  "! Type zdbbr_obj_browser_mode
  CONSTANTS c_initial_obj_browser_mode TYPE string VALUE 'P_IOBBRM' ##no_text.
  "! Type ABAP_BOOL
  CONSTANTS c_no_fixed_key_cols TYPE string VALUE 'P_KCOLNF' ##NO_TEXT.
  "! Type SY-TABIX
  CONSTANTS c_max_result_lines TYPE string VALUE 'P_MAXL' ##NO_TEXT.
  "! Type sy-tabix
  CONSTANTS c_disable_date_to_timest_conv TYPE string VALUE 'P_XDD2TC' ##no_text.
  "! Type ABAP_BOOL
  CONSTANTS c_no_conv_exit TYPE string VALUE 'P_NOCVEX' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_no_merging_of_srt_cols TYPE string VALUE 'P_NOSRTM' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_no_trailing_sign TYPE string VALUE 'P_NOTRSG' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_use_reduced_memory TYPE string VALUE 'P_REDMEM' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_technical_fields_first TYPE string VALUE 'P_TFIRST' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_technical_names TYPE string VALUE 'P_TNAMES' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_technical_view TYPE string VALUE 'P_TVIEW' ##NO_TEXT.
  "! Type ABAP_BOOL
  CONSTANTS c_zeros_as_blanks TYPE string VALUE 'P_ZERASB' ##NO_TEXT.
  "! Type ZDBBR_EXPERIMENTAL_MODE
  CONSTANTS c_experimental_mode TYPE string VALUE 'P_EXPM' ##NO_TEXT.
  "! Type ZDBBR_READ_DB_LENGTH
  CONSTANTS c_read_db_table_length TYPE string VALUE 'P_RDDBSZ' ##NO_TEXT.
  "! Type ZDBBR_enable_alv_default_var
  CONSTANTS c_enable_alv_default_var TYPE string VALUE 'P_DEFALV' ##NO_TEXT.
  "! Type ZDBBR_MAINT_DB_SETTING - abap_bool
  CONSTANTS c_activate_maintain_entries TYPE string VALUE 'P_XDBEDT' ##NO_TEXT.
  "! Type ZDBBR_LANGUAGE
  CONSTANTS c_description_language TYPE string VALUE 'P_DLANG' ##NO_TEXT.
  "! Type abap_bool
  CONSTANTS c_activate_alv_live_filter TYPE string VALUE 'P_XFLLIV' ##no_text.
  "! Type BOOLE
  CONSTANTS c_use_ddl_view_for_select TYPE string VALUE 'P_XDDLFS' ##no_text.
  "!Type SAP_BOOL
  CONSTANTS c_search_ignore_case TYPE string VALUE 'P_XIGNCS' ##NO_TEXT.
  CONSTANTS c_assocation_sel_mode TYPE string VALUE 'P_ASNVMO' ##NO_TEXT.
  CONSTANTS c_show_assoc_sel_at_start TYPE string VALUE 'P_ASSLST' ##NO_TEXT.
  CONSTANTS c_r_user_settings_controller TYPE string VALUE 'GR_USER_SETTINGS_CONTROLLER' ##NO_TEXT.

ENDINTERFACE.
