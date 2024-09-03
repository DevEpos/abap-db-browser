" -----------------------------------------------------------------------
" INCLUDE LZDBBRI07.
" -----------------------------------------------------------------------

MODULE exit_0800 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_tabfield_manager->zif_uitb_screen_controller~cancel( save_ok_code ).
ENDMODULE.

MODULE pai_0800 INPUT.
  gr_tabfield_manager->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE pai_0820 INPUT.
  gr_sort_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE exit_0820 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_sort_controller->zif_uitb_screen_controller~cancel( save_ok_code ).
ENDMODULE.
