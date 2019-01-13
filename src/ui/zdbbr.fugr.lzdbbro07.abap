*----------------------------------------------------------------------*
***INCLUDE LZDBBRO07.
*----------------------------------------------------------------------*

MODULE pbo_0800 OUTPUT.
  gr_tabfield_manager->ZIF_UITB_SCREEN_CONTROLLER~pbo( ).
ENDMODULE.

MODULE pbo_0820 OUTPUT.
  gr_sort_controller->ZIF_UITB_SCREEN_CONTROLLER~pbo( ).
ENDMODULE.
