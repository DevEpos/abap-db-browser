" ---------------------------------------------------------------------
" PAI / PBO for selection screens
" ---------------------------------------------------------------------
AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1100.
      gr_export_fav_controller->zif_uitb_screen_controller~handle_user_command(
        CHANGING cv_function_code = sscrfields-ucomm ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  DATA(lv_function) = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.

  CASE sy-dynnr.
    WHEN 1100.
      gr_export_fav_controller->zif_uitb_screen_controller~cancel( lv_function ).
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN 1100.
      gr_export_fav_controller->zif_uitb_screen_controller~pbo( ).
  ENDCASE.

  " ---------------------------------------------------------------------
