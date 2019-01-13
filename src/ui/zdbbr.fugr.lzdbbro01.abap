*&---------------------------------------------------------------------*
*&  Include           LZLS_DATA_BROWSERO01
*&---------------------------------------------------------------------*

MODULE table_pbo_0100 OUTPUT.
  gr_selscreen_table->zif_uitb_table~update_screen_attributes( ).
ENDMODULE.

MODULE table_line_pbo_0100 OUTPUT.
  gr_selscreen_table->zif_uitb_table~pbo( ).
ENDMODULE.

MODULE get_linecount_0100 OUTPUT.
  gr_selscreen_table->zif_uitb_table~determine_line_count( ).
ENDMODULE.

MODULE display_data OUTPUT.
  gr_selscreen_table->display_lines( ).
ENDMODULE.

MODULE modify_screen OUTPUT.
  gr_selscreen_controller->zif_uitb_screen_controller~pbo( ).
ENDMODULE.

MODULE status_0202 OUTPUT.
  gr_f4_screen_controller->zif_uitb_screen_controller~pbo( ).
ENDMODULE.

MODULE status_0101 OUTPUT.
  gr_f4_screen_controller->zif_uitb_screen_controller~set_status( ).
ENDMODULE.

MODULE determine_looplines_0100 OUTPUT.
  gr_selscreen_table->determine_loop_lines( ).
ENDMODULE.

MODULE pbo_0108 OUTPUT.
  gr_altcoltext_controller->zif_uitb_screen_controller~pbo( ).
ENDMODULE.

MODULE table_line_pbo_0108 OUTPUT.
  gr_altcoltext_table->zif_uitb_table~update_screen_attributes( ).
ENDMODULE.

MODULE pbo_0105 OUTPUT.
  gr_choose_entity_controller->zif_uitb_screen_controller~pbo( ).
ENDMODULE.

MODULE pbo_0300 output.
  gr_obj_brws_search_enter_ctrl->pbo( ).
ENDMODULE.
