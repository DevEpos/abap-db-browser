*&---------------------------------------------------------------------*
*&  Include           LZLS_DATA_BROWSERI02
*&---------------------------------------------------------------------*

MODULE update_selection_field_multi INPUT.
  gr_multi_select_table->update_fields( ).
ENDMODULE.

MODULE update_selfield_0106 INPUT.
  gr_multi_or_table->zif_uitb_table~update_fields( ).
ENDMODULE.

MODULE check_input_0106 INPUT.

ENDMODULE.

MODULE cancel_0001 INPUT.
  gr_multi_select_controller->zif_uitb_screen_controller~cancel( ).
ENDMODULE.

MODULE field_f4_multi_low INPUT.
  gr_multi_select_controller->call_f4_help(
      if_for_low = abap_true
  ).
ENDMODULE.

MODULE field_f4_multi_high INPUT.
  gr_multi_select_controller->call_f4_help(
      if_for_low = abap_false
  ).
ENDMODULE.

MODULE field_f4_multi_or_low INPUT.
  gr_multi_or_controller->call_selfield_value_help( if_for_low = abap_true ).
ENDMODULE.

MODULE field_f4_multi_or_high INPUT.
  gr_multi_or_controller->call_selfield_value_help( ).
ENDMODULE.

MODULE pai_0001 INPUT.
  gr_multi_select_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE pai_0106 INPUT.
  gr_multi_or_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE cancel_0106 INPUT.
  gr_multi_or_controller->zif_uitb_screen_controller~cancel( ).
ENDMODULE.
