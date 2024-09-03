*&---------------------------------------------------------------------*
*&  Include           LZLS_DATA_BROWSERO02
*&---------------------------------------------------------------------*

MODULE pbo_0001 OUTPUT.
  gr_multi_select_controller->zif_uitb_screen_controller~pbo( ).
ENDMODULE.

MODULE get_linecount_0001 OUTPUT.
  gr_multi_select_controller->determine_line_count( ).
ENDMODULE.

MODULE display_lines OUTPUT.
  gr_multi_select_table->display_lines( ).
ENDMODULE.

MODULE prepare_fields OUTPUT.
  gr_multi_select_table->change_attributes( ).
ENDMODULE.

MODULE pbo_0106 OUTPUT.
  gr_multi_or_controller->zif_uitb_screen_controller~pbo( ).
ENDMODULE.

MODULE get_linecount_0106 OUTPUT.
  gr_multi_or_table->pbo( ).
ENDMODULE.

MODULE table_pbo_0106 OUTPUT.
  gr_multi_or_table->pbo( ).
ENDMODULE.

MODULE prepare_fields_0106 OUTPUT.
  gr_multi_or_table->zif_uitb_table~update_screen_attributes( ).
ENDMODULE.
