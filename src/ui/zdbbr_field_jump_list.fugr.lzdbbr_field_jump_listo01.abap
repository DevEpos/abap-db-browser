**********************************************************************
* Forms for PBO-events
**********************************************************************

  MODULE pbo_0100 OUTPUT.
    gr_jumplist_controller->ZIF_UITB_SCREEN_CONTROLLER~pbo( ).
  ENDMODULE.

  MODULE table_line_pbo_0100 OUTPUT.
    gr_jumplist_table->ZIF_UITB_TABLE~pbo( ).
  ENDMODULE.

  MODULE pbo_0101 OUTPUT.
    gr_jumplist_param_controller->ZIF_UITB_SCREEN_CONTROLLER~pbo( ).
  ENDMODULE.

  MODULE table_line_pbo_0101 OUTPUT.
    gr_jumplist_param_table->ZIF_UITB_TABLE~pbo( ).
  ENDMODULE.
