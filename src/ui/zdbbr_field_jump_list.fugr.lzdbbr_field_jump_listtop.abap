FUNCTION-POOL zdbbr_field_jump_list.       " MESSAGE-ID ..

DATA ok_code TYPE sy-ucomm.

DATA param_details TYPE zdbbr_button.

DATA gt_jumpfields TYPE zdbbr_jumpdest_data_ui_itab.
DATA gs_jumpfield TYPE zdbbr_jumpdest_data_ui.

DATA gt_params TYPE zdbbr_jumpparam_data_ui_itab.
DATA gs_param TYPE zdbbr_jumpparam_data_ui.

DATA gr_jumplist_controller TYPE REF TO zcl_dbbr_jumplist_controller.
DATA gr_jumplist_table TYPE REF TO zcl_dbbr_jumplist_table.
DATA gr_jumplist_param_controller TYPE REF TO zcl_dbbr_jumplist_param_ctlr.
DATA gr_jumplist_param_table TYPE REF TO zcl_dbbr_jumplist_param_table.

CONTROLS: jump_fields_tc TYPE TABLEVIEW USING SCREEN 0100,
          params_tc      TYPE TABLEVIEW USING SCREEN 0101.
