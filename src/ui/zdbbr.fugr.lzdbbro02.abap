*&---------------------------------------------------------------------*
*&  Include           LZLS_DATA_BROWSERO02
*&---------------------------------------------------------------------*

module pbo_0001 output.
    gr_multi_select_controller->ZIF_UITB_SCREEN_CONTROLLER~pbo( ).
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
  gr_multi_or_controller->ZIF_UITB_SCREEN_CONTROLLER~pbo( ).
ENDMODULE.

MODULE get_linecount_0106 OUTPUT.
  gr_multi_or_table->pbo( ).
ENDMODULE.


module table_pbo_0106 OUTPUT.
  gr_multi_or_table->pbo( ).
endmodule.

MODULE prepare_fields_0106 OUTPUT.
  gr_multi_or_table->ZIF_UITB_TABLE~update_screen_attributes( ).
ENDMODULE.
