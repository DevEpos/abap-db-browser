" -----------------------------------------------------------------------
" INCLUDE LZDBBRO07.
" -----------------------------------------------------------------------

MODULE pbo_0800 OUTPUT.
  gr_tabfield_manager->zif_uitb_screen_controller~pbo( ).
ENDMODULE.

MODULE pbo_0820 OUTPUT.
  gr_sort_controller->zif_uitb_screen_controller~pbo( ).
ENDMODULE.
