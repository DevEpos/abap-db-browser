LOAD-OF-PROGRAM.
  zcl_dbbr_system_helper=>set_locale_language( ).

AT SELECTION-SCREEN.
  CASE sy-dynnr.

    WHEN zif_dbbr_screen_ids=>c_user_settings-main_screen.
      gr_user_settings_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

    WHEN zif_dbbr_screen_ids=>c_show_eb_settings.
      gr_eb_settings_view->pai( CHANGING cv_function_code = sscrfields-ucomm ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  CASE sy-dynnr.

    WHEN zif_dbbr_screen_ids=>c_user_settings-main_screen.
      gr_user_settings_controller->zif_uitb_screen_controller~cancel( ).

    WHEN zif_dbbr_screen_ids=>c_show_eb_settings.
      gr_eb_settings_view->cancel( ).
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.

    WHEN zif_dbbr_screen_ids=>c_user_settings-main_screen OR
         zif_dbbr_screen_ids=>c_user_settings-general_tab OR
         zif_dbbr_screen_ids=>c_user_settings-favorites_tab OR
         zif_dbbr_screen_ids=>c_user_settings-selscreen_tab OR
         zif_dbbr_screen_ids=>c_user_settings-output_tab.
*.... set button texts here because of some reason not every system
*.... returns the tab buttons from program source code analysing
      btn_intr = TEXT-005.
      btn_fav = TEXT-002.
      btn_alv = TEXT-003.
      btn_sel = TEXT-001.

*.... Perform some initialization for the first call
      gr_user_settings_controller->initialize_screen(
        CHANGING cs_tabs = setting_type ).

      gr_user_settings_controller->zif_uitb_screen_controller~pbo( ).

    WHEN zif_dbbr_screen_ids=>c_show_eb_settings.
      gr_eb_settings_view->pbo( ).
  ENDCASE.
