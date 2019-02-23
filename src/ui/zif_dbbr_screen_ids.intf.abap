INTERFACE zif_dbbr_screen_ids
  PUBLIC .


  CONSTANTS c_choose_table_query TYPE dynnr VALUE 0105 ##NO_TEXT.
  CONSTANTS c_save_query TYPE dynnr VALUE 1100 ##no_text.
  CONSTANTS c_save_sql_query TYPE dynnr VALUE 1800 ##no_text.
  CONSTANTS c_choose_table_fields TYPE dynnr VALUE 0800 ##NO_TEXT.
  CONSTANTS c_table_variant TYPE dynnr VALUE 1200 ##NO_TEXT.
  constants c_query_variant TYPE dynnr VALUE 1201 ##NO_TEXT.
  CONSTANTS c_cds_view_variant TYPE dynnr VALUE 1202 ##NO_TEXT.
  CONSTANTS c_favorite_export TYPE dynnr VALUE 1100 ##NO_TEXT.
  CONSTANTS c_jump_list_manager TYPE dynnr VALUE 0100 ##NO_TEXT.
  CONSTANTS c_jump_list_parameters TYPE dynnr VALUE 0101 ##NO_TEXT.
  CONSTANTS c_selection_screen TYPE dynnr VALUE 0100 ##NO_TEXT.
  CONSTANTS c_define_field_sorting TYPE dynnr VALUE 0820 ##NO_TEXT.
  CONSTANTS c_define_additional_texts TYPE dynnr VALUE 1300 ##NO_TEXT.
  constants c_copy_query type dynnr value 1400 ##no_text.
  CONSTANTS c_join_field_f4 TYPE dynnr VALUE 0104 ##NO_TEXT.
  CONSTANTS c_define_built_in_f4 TYPE dynnr VALUE 0101 ##NO_TEXT.
  CONSTANTS c_show_custom_f4_help TYPE dynnr VALUE 0202 ##NO_TEXT.
  CONSTANTS c_show_mult_select TYPE dynnr VALUE 0001 ##NO_TEXT.
  CONSTANTS c_show_multi_or_select TYPE dynnr VALUE 0106 ##NO_TEXT.
  CONSTANTS c_maintain_alternative_cols TYPE dynnr VALUE 0108 ##NO_TEXT.
  CONSTANTS c_selection_output TYPE dynnr VALUE 0100 ##NO_TEXT.
  CONSTANTS c_show_user_settings TYPE dynnr VALUE '0100' ##NO_TEXT.
  CONSTANTS c_show_user_settings_general TYPE dynnr VALUE '0101' ##NO_TEXT.
  CONSTANTS c_show_user_settings_selscreen TYPE dynnr VALUE '0102' ##NO_TEXT.
  CONSTANTS c_show_user_settings_favorites TYPE dynnr VALUE '0103' ##NO_TEXT.
  CONSTANTS c_show_user_settings_output TYPE dynnr VALUE '0104' ##NO_TEXT.
  CONSTANTS c_show_eb_settings TYPE dynnr VALUE '0200' ##NO_TEXT.
  CONSTANTS c_maintain_join_table TYPE dynnr VALUE 1700 ##NO_TEXT.
  CONSTANTS c_maintain_join_cond TYPE dynnr VALUE 1701 ##NO_TEXT.
ENDINTERFACE.
