CLASS zcl_dbbr_export_favmenu_ctrl DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_uitb_screen_controller.

    METHODS constructor.

  PRIVATE SECTION.
    ALIASES get_report_id FOR zif_uitb_screen_controller~get_report_id.
    ALIASES get_screen_id FOR zif_uitb_screen_controller~get_screen_id.

    DATA mr_ui_export_global TYPE REF TO boolean.
    DATA mr_ui_export_private TYPE REF TO boolean.

    METHODS export_favorites.
ENDCLASS.


CLASS zcl_dbbr_export_favmenu_ctrl IMPLEMENTATION.
  METHOD constructor.
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>favorite_menu ).

    mr_ui_export_global = CAST #( lr_data_cache->get_data_ref( zif_dbbr_favmenu_var_ids=>c_f_export_global_fav ) ).
    mr_ui_export_private = CAST #( lr_data_cache->get_data_ref( zif_dbbr_favmenu_var_ids=>c_f_export_private_fav ) ).
  ENDMETHOD.

  METHOD export_favorites.
    NEW zcl_dbbr_favmenu_exporter( if_global_favs  = mr_ui_export_global->*
                                   if_private_favs = mr_ui_export_private->*
    )->export_data( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen( iv_screen_id    = get_screen_id( )
                                       iv_report_id    = get_report_id( )
                                       if_selscreen    = abap_true
                                       it_object_map   = VALUE #(
                                           ( variable_name = zif_dbbr_favmenu_var_ids=>c_r_export_fav_controller
                                             global_ref    = me ) )
                                       iv_start_column = 15
                                       iv_start_line   = 6 ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>favorite_menu.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_favorite_export.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~handle_user_command.
    DATA(lv_function) = cv_function_code.
    CLEAR cv_function_code.

    CASE lv_function.
      WHEN zif_dbbr_c_global=>c_function_codes-cancel.
        zcl_dbbr_screen_helper=>leave_screen( ).

      WHEN 'OK'.
        export_favorites( ).
        zcl_dbbr_screen_helper=>leave_screen( ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~set_status.
    DATA lt_excl TYPE TABLE OF sy-ucomm.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING p_status  = 'EXPORT_FAVMENU'
      TABLES    p_exclude = lt_excl.
  ENDMETHOD.
ENDCLASS.
