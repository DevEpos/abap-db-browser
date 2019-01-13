FUNCTION-POOL ZDBBR_field_jump_list.       "MESSAGE-ID ..


DATA: ok_code TYPE sy-ucomm.

DATA: param_details TYPE ZDBBR_button.

DATA: gt_jumpfields TYPE ZDBBR_jumpdest_data_ui_itab,
      gs_jumpfield  TYPE ZDBBR_jumpdest_data_ui,

      gt_params     TYPE ZDBBR_jumpparam_data_ui_itab,
      gs_param      TYPE ZDBBR_jumpparam_data_ui.

DATA: gr_jumplist_controller       TYPE REF TO ZCL_DBBR_jumplist_controller,
      gr_jumplist_table            TYPE REF TO ZCL_DBBR_jumplist_table,
      gr_jumplist_param_controller TYPE REF TO ZCL_DBBR_jumplist_param_ctlr,
      gr_jumplist_param_table      TYPE REF TO ZCL_DBBR_jumplist_param_table.

CONTROLS: jump_fields_tc TYPE TABLEVIEW USING SCREEN 0100,
          params_tc      TYPE TABLEVIEW USING SCREEN 0101.
