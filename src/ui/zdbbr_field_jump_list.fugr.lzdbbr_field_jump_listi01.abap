**********************************************************************
* Forms for PAI-events
**********************************************************************

MODULE back_0100 INPUT.
  gr_jumplist_controller->ZIF_UITB_SCREEN_CONTROLLER~cancel( ok_code ).
ENDMODULE.

MODULE table_line_pai_0100 INPUT.
  gr_jumplist_table->ZIF_UITB_TABLE~update_fields( ok_code ).
ENDMODULE.

MODULE pai_0100 INPUT.
  gr_jumplist_controller->ZIF_UITB_SCREEN_CONTROLLER~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE jump_field_0100_f4 INPUT.
  gr_jumplist_table->jump_field_f4( ).
ENDMODULE.

MODULE jump_crit_0100_f4 INPUT.
  gr_jumplist_table->jump_crit_f4( ).
ENDMODULE.

MODULE table_line_validate_0100 INPUT.
  gr_jumplist_table->validate( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE determine_cursor_0100 INPUT.
  gr_jumplist_controller->ZIF_UITB_SCREEN_CONTROLLER~determine_cursor( ).
ENDMODULE.

MODULE jump_target_0100_f4 INPUT.
  gr_jumplist_table->jump_target_f4( ).
ENDMODULE.

MODULE determine_cursor_0101 INPUT.
  gr_jumplist_param_controller->ZIF_UITB_SCREEN_CONTROLLER~determine_cursor( ).
ENDMODULE.

MODULE back_0101 INPUT.
  gr_jumplist_param_controller->ZIF_UITB_SCREEN_CONTROLLER~cancel( ok_code ).
ENDMODULE.

MODULE table_line_pai_0101 INPUT.
  gr_jumplist_param_table->ZIF_UITB_TABLE~update_fields( ok_code ).
ENDMODULE.

MODULE pai_0101 INPUT.
  gr_jumplist_param_controller->ZIF_UITB_SCREEN_CONTROLLER~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE table_line_validate_0101 INPUT.
  gr_jumplist_param_table->validate( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE param_value_f4_0101 INPUT.
  gr_jumplist_param_table->param_value_f4( ).
ENDMODULE.

MODULE param_id_f4_0101 INPUT.
  gr_jumplist_param_table->param_id_f4( ).
ENDMODULE.
