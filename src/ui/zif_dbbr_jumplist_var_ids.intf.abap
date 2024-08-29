INTERFACE zif_dbbr_jumplist_var_ids
  PUBLIC.
  "! Type - ZDBBR_jumpdest_data_ui_itab
  CONSTANTS c_t_jumpfields TYPE string VALUE 'GT_JUMPFIELDS'.
  "! Type - ZDBBR_jumpdest_data_ui
  CONSTANTS c_s_jumpfield TYPE string VALUE 'GS_JUMPFIELD'.
  "! Type - ZDBBR_jumpparam_data_ui_itab
  CONSTANTS c_t_params TYPE string VALUE 'GT_PARAMS'.
  "! Type - ZDBBR_jumpparam_data_ui
  CONSTANTS c_s_param TYPE string VALUE 'GS_PARAM'.
  "! Type - cxtab_control
  CONSTANTS c_jump_fields_tc TYPE string VALUE 'JUMP_FIELDS_TC'.
  "! Type - cxtab_control
  CONSTANTS c_params_tc TYPE string VALUE 'PARAMS_TC'.
  "! Type - ZDBBR_button
  CONSTANTS c_bt_param_details TYPE string VALUE 'PARAM_DETAILS'.

  CONSTANTS c_r_jumplist_controller TYPE string VALUE 'GR_JUMPLIST_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_jumplist_table TYPE string VALUE 'GR_JUMPLIST_TABLE' ##NO_TEXT.
  CONSTANTS c_r_jumplist_param_controller TYPE string VALUE 'GR_JUMPLIST_PARAM_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_jumplist_param_table TYPE string VALUE 'GR_JUMPLIST_PARAM_TABLE' ##NO_TEXT.
ENDINTERFACE.
