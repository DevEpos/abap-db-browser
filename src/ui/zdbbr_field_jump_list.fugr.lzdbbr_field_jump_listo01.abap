  " ---------------------------------------------------------------------
  " Forms for PBO-events
  " ---------------------------------------------------------------------

  MODULE pbo_0100 OUTPUT.
    gr_jumplist_controller->zif_uitb_screen_controller~pbo( ).
  ENDMODULE.

  MODULE table_line_pbo_0100 OUTPUT.
    gr_jumplist_table->zif_uitb_table~pbo( ).
  ENDMODULE.

  MODULE pbo_0101 OUTPUT.
    gr_jumplist_param_controller->zif_uitb_screen_controller~pbo( ).
  ENDMODULE.

  MODULE table_line_pbo_0101 OUTPUT.
    gr_jumplist_param_table->zif_uitb_table~pbo( ).
  ENDMODULE.
