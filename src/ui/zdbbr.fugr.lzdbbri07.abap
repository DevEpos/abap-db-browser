*----------------------------------------------------------------------*
***INCLUDE LZDBBRI07.
*----------------------------------------------------------------------*

MODULE exit_0800 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_tabfield_manager->ZIF_UITB_SCREEN_CONTROLLER~cancel( save_ok_code ).
ENDMODULE.

MODULE pai_0800 INPUT.
  gr_tabfield_manager->ZIF_UITB_SCREEN_CONTROLLER~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE pai_0820 INPUT.
  gr_sort_controller->ZIF_UITB_SCREEN_CONTROLLER~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE exit_0820 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_sort_controller->ZIF_UITB_SCREEN_CONTROLLER~cancel( save_ok_code ).
ENDMODULE.
