"! <p class="shorttext synchronized" lang="en">Function codes for Selection/Output</p>
INTERFACE zif_dbbr_c_selection_functions
  PUBLIC.

  CONSTANTS add_text_field TYPE sy-ucomm VALUE 'ADD_TEXT_FIELDS' ##NO_TEXT.
  CONSTANTS calc_sum_of_chosen_cells TYPE sy-ucomm VALUE 'CALC_SUM_OF_CELLS' ##NO_TEXT.
  CONSTANTS change_cds_parameters TYPE sy-ucomm VALUE 'CHGPARAMS' ##NO_TEXT.
  CONSTANTS change_max_row_count TYPE sy-ucomm VALUE 'CHMAXROWS' ##NO_TEXT.
  CONSTANTS clear_filter TYPE sy-ucomm VALUE '&ILD' ##NO_TEXT.
  CONSTANTS compare_selected_lines TYPE sy-ucomm VALUE 'COMPLINES' ##NO_TEXT.
  CONSTANTS control_tech_view TYPE sy-ucomm VALUE 'CTRLTECHVW' ##NO_TEXT.
  CONSTANTS copy_as_val_stmnts TYPE sy-ucomm VALUE 'CPYVALSTMN' ##NO_TEXT.
  CONSTANTS copy_as_val_stmnt_compact TYPE sy-ucomm VALUE 'CPYVALSTMNT_COMPACT' ##NO_TEXT.
  CONSTANTS define_variant TYPE sy-ucomm VALUE '&OL0' ##NO_TEXT.
  CONSTANTS delete_colors_of_columns TYPE sy-ucomm VALUE 'DEL_EMPCOL' ##NO_TEXT.
  CONSTANTS delete_colors_of_rows TYPE sy-ucomm VALUE 'DEL_EMPROW' ##NO_TEXT.
  CONSTANTS delete_data TYPE sy-ucomm VALUE 'DELETEDATA' ##NO_TEXT.
  CONSTANTS delete_filters_from_cols TYPE sy-ucomm VALUE 'RMVFILTCOL' ##NO_TEXT.
  CONSTANTS delete_hidden_lines TYPE sy-ucomm VALUE 'DEL_HIDROW' ##NO_TEXT.
  CONSTANTS determine_line_count TYPE sy-ucomm VALUE 'LINE_COUNT' ##NO_TEXT.
  CONSTANTS disable_checkbox_col_style TYPE sy-ucomm VALUE 'DISCHKBOX_STYLE' ##NO_TEXT.
  CONSTANTS disable_chkbox_col_style_all TYPE sy-ucomm VALUE 'DISCHKBOX_STYLE_ALL' ##NO_TEXT.
  CONSTANTS display_db_browser_version TYPE sy-ucomm VALUE 'SHOWVERSIO' ##NO_TEXT.
  CONSTANTS edit_data TYPE sy-ucomm VALUE 'EDITDATA' ##NO_TEXT.
  CONSTANTS export_json_data TYPE sy-ucomm value 'EXPORT_AS_JSON'.
  CONSTANTS emphasize_lines TYPE sy-ucomm VALUE 'EMPH_LINES' ##NO_TEXT.
  CONSTANTS emphasize_negative_values TYPE sy-ucomm VALUE 'EMPH_NEGAT' ##NO_TEXT.
  CONSTANTS emph_blue TYPE sy-ucomm VALUE 'EMPH_BLUE' ##NO_TEXT.
  CONSTANTS emph_green TYPE sy-ucomm VALUE 'EMPH_GREEN' ##NO_TEXT.
  CONSTANTS emph_green_inverted TYPE sy-ucomm VALUE 'EMPH_GREEN_INVERTED' ##NO_TEXT.
  CONSTANTS emph_grey_blue TYPE sy-ucomm VALUE 'EMPH_GREY_BLUE' ##NO_TEXT.
  CONSTANTS emph_light_blue TYPE sy-ucomm VALUE 'EMPH_LIGHT_BLUE' ##NO_TEXT.
  CONSTANTS emph_light_green TYPE sy-ucomm VALUE 'EMPH_LIGHT_GREEN' ##NO_TEXT.
  CONSTANTS emph_light_grey_blue TYPE sy-ucomm VALUE 'EMPH_LIGHT_GREY_BLUE' ##NO_TEXT.
  CONSTANTS emph_light_orange TYPE sy-ucomm VALUE 'EMPH_LIGHT_ORANGE' ##NO_TEXT.
  CONSTANTS emph_light_red TYPE sy-ucomm VALUE 'EMPH_LIGHT_RED' ##NO_TEXT.
  CONSTANTS emph_light_yellow TYPE sy-ucomm VALUE 'EMPH_LIGHT_YELLOW' ##NO_TEXT.
  CONSTANTS emph_orange TYPE sy-ucomm VALUE 'EMPH_ORANGE' ##NO_TEXT.
  CONSTANTS emph_red TYPE sy-ucomm VALUE 'EMPH_RED' ##NO_TEXT.
  CONSTANTS emph_red_inverted TYPE sy-ucomm VALUE 'EMPH_RED_INVERTED' ##NO_TEXT.
  CONSTANTS emph_yellow TYPE sy-ucomm VALUE 'EMPH_YELLOW' ##NO_TEXT.
  CONSTANTS filter_lines TYPE sy-ucomm VALUE '&ILT' ##NO_TEXT.
  CONSTANTS freeze_rows_cols TYPE sy-ucomm VALUE 'FIX_ROWS_COLS' ##NO_TEXT.
  CONSTANTS go_to_column TYPE sy-ucomm VALUE 'GO_TO_COL' ##NO_TEXT.
  CONSTANTS group_by_selected_columns TYPE sy-ucomm VALUE 'GROUP_ONLI' ##NO_TEXT.
  CONSTANTS hide_cols_without_values TYPE sy-ucomm VALUE 'HIDECOLNVL' ##NO_TEXT.
  CONSTANTS hide_columns TYPE sy-ucomm VALUE '&COL_INV' ##NO_TEXT.
  CONSTANTS hide_lines TYPE sy-ucomm VALUE 'HIDE_SELECTED' ##NO_TEXT.
  "! <p class="shorttext synchronized" lang="en">ABAP System Field: PAI-Triggering Function Code</p>
  CONSTANTS hide_other_columns TYPE sy-ucomm VALUE 'HIDE_OTHERS' ##NO_TEXT.
  CONSTANTS keep_lines TYPE sy-ucomm VALUE 'KEEP_SELECTED' ##NO_TEXT.
  CONSTANTS leave_screen_with_layout TYPE sy-ucomm VALUE 'BACKWLAYOT' ##NO_TEXT.
  CONSTANTS load_variant TYPE sy-ucomm VALUE '&OAD' ##NO_TEXT.
  CONSTANTS manage_text_fields TYPE sy-ucomm VALUE 'MANAGE_TEXT_FIELDS' ##NO_TEXT.
  CONSTANTS navigate_association TYPE sy-ucomm VALUE 'NAVASSOC' ##NO_TEXT.
  CONSTANTS no_emphasize_cells TYPE sy-ucomm VALUE 'NO_EMPH_CELLS' ##NO_TEXT.
  CONSTANTS no_emphasize_lines TYPE sy-ucomm VALUE 'NO_EMPH_LINES' ##NO_TEXT.
  CONSTANTS open_in_sql_console TYPE sy-ucomm VALUE 'SQLCONSOLE' ##NO_TEXT.
  CONSTANTS quick_filter TYPE sy-ucomm VALUE 'QUICK_FILT' ##NO_TEXT.
  CONSTANTS quick_filter_exclusion TYPE sy-ucomm VALUE 'QUICK_FEXC' ##NO_TEXT.
  CONSTANTS refresh TYPE sy-ucomm VALUE 'REFRESH' ##NO_TEXT.
  CONSTANTS remove_column_grouping TYPE sy-ucomm VALUE 'RMVGROUPNG' ##NO_TEXT.
  CONSTANTS remove_fixed_rows TYPE sy-ucomm VALUE 'REMOVE_FIXED_ROWS' ##NO_TEXT.
  CONSTANTS reset_alv_layout TYPE sy-ucomm VALUE 'RESET_LYT' ##NO_TEXT.
  CONSTANTS save_as_variant TYPE sy-ucomm VALUE 'SAVEVAR' ##NO_TEXT.
  CONSTANTS save_selection_as_f4 TYPE sy-ucomm VALUE 'SAVE_AS_F4' ##NO_TEXT.
  CONSTANTS set_fixed_rows TYPE sy-ucomm VALUE 'SET_FIXED_ROWS' ##NO_TEXT.
  CONSTANTS set_focus_to_assoc_list TYPE sy-ucomm VALUE 'FOCUSASSOC' ##NO_TEXT.
  CONSTANTS set_focus_to_list TYPE sy-ucomm VALUE 'FOCUSALV' ##NO_TEXT.
  CONSTANTS show_active_formula TYPE sy-ucomm VALUE 'SHOWFORM' ##NO_TEXT.
  CONSTANTS show_all_columns TYPE sy-ucomm VALUE 'SHOW_ALL_COLS' ##NO_TEXT.
  CONSTANTS show_cds_source TYPE sy-ucomm VALUE 'SHOWCDSSRC' ##NO_TEXT.
  "! <p class="shorttext synchronized" lang="en">ABAP System Field: PAI-Triggering Function Code</p>
  CONSTANTS show_details TYPE sy-ucomm VALUE 'DETAILS' ##NO_TEXT.
  CONSTANTS show_hidden_lines TYPE sy-ucomm VALUE 'DISP_HIDRO' ##NO_TEXT.
  CONSTANTS show_shortcuts TYPE sy-ucomm VALUE 'SHORTCUTS' ##NO_TEXT.
  CONSTANTS show_sql_of_select TYPE sy-ucomm VALUE 'SHOWSQL' ##NO_TEXT.
  CONSTANTS show_string_cell_content TYPE sy-ucomm VALUE 'SHOW_STRING_CELL_CONTENT' ##NO_TEXT.
  CONSTANTS show_users_settings TYPE sy-ucomm VALUE 'SETTINGS' ##NO_TEXT.
  CONSTANTS sum_column TYPE sy-ucomm VALUE '&SUM' ##NO_TEXT.
  CONSTANTS toggle_entity_info_header TYPE sy-ucomm VALUE 'HDR_VSBLTY' ##NO_TEXT.
  CONSTANTS transfer_filter_values TYPE sy-ucomm VALUE 'TRNSFFILT' ##NO_TEXT.
ENDINTERFACE.
