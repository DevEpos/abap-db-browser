CLASS zcl_dbbr_eb_settings_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    ALIASES cancel
      FOR zif_uitb_screen_controller~cancel .
    ALIASES pai
      FOR zif_uitb_screen_controller~handle_user_command .
    ALIASES pbo
      FOR zif_uitb_screen_controller~pbo .
    ALIASES show
      FOR zif_uitb_screen_controller~call_screen .
    ALIASES was_saved
      FOR zif_uitb_screen_controller~was_not_cancelled .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mf_first_call
      FOR zif_uitb_screen_controller~mf_first_call.
    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .
    ALIASES set_status
      FOR zif_uitb_screen_controller~set_status .

    CONSTANTS c_p_entry_search_function TYPE fieldname VALUE 'P_EBENSF' ##NO_TEXT.
    CONSTANTS c_p_link_mode TYPE fieldname VALUE 'P_EBLIMO' ##NO_TEXT.
    CONSTANTS c_p_max_hits TYPE fieldname VALUE 'P_EBBMAX' ##NO_TEXT.
    CONSTANTS c_r_controller TYPE fieldname VALUE 'GR_EB_SETTINGS_VIEW' ##NO_TEXT.

    DATA:
      BEGIN OF ms_settings_ref,
        max_hits        TYPE REF TO int2,
        link_mode       TYPE REF TO zdbbr_entity_browser_link_mode,
        search_function TYPE REF TO zdbbr_obj_browser_mode,
      END OF ms_settings_ref .
    DATA mf_saved TYPE abap_bool .

    METHODS initialize_settings .
    METHODS save_settings .
ENDCLASS.



CLASS zcl_dbbr_eb_settings_view IMPLEMENTATION.


  METHOD constructor.
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( get_report_id( ) ).

    ms_settings_ref-search_function = CAST #( lr_data_cache->get_data_ref( CONV #( c_p_entry_search_function ) ) ).
    ms_settings_ref-link_mode = CAST #( lr_data_cache->get_data_ref( CONV #( c_p_link_mode ) ) ).
    ms_settings_ref-max_hits = CAST #( lr_data_cache->get_data_ref( CONV #( c_p_max_hits ) ) ).
  ENDMETHOD.


  METHOD initialize_settings.
    DATA(ls_settings) = zcl_dbbr_usersettings_factory=>get_entity_browser_settings( ).

    ms_settings_ref-search_function->* = ls_settings-entry_search_function.
    ms_settings_ref-link_mode->* = ls_settings-link_mode.
    ms_settings_ref-max_hits->* = ls_settings-max_hits.
  ENDMETHOD.


  METHOD save_settings.
    zcl_dbbr_usersettings_factory=>set_entity_browser_settings(
      VALUE #(
        link_mode       = ms_settings_ref-link_mode->*
        max_hits        = ms_settings_ref-max_hits->*
        search_function = ms_settings_ref-search_function->*
      )
    ).
    mf_saved = abap_true.
    MESSAGE s079(zdbbr_info).

    zcl_uitb_screen_util=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
*.. initialize settings from database
    initialize_settings( ).
    mf_first_call = abap_true.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = c_r_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.
    zcl_uitb_screen_util=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>user_settings.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_show_eb_settings.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
    DATA(lv_function) = cv_function_code.
    CLEAR cv_function_code.

    CASE lv_function.

      WHEN 'SAVE'.
        save_settings( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    DATA: lt_list TYPE vrm_values.

    IF mf_first_call = abap_true.
      lt_list = VALUE #(
        ( key = zif_dbbr_c_object_browser_mode=>cds_view            text = 'Find CDS Views'(001) )
        ( key = zif_dbbr_c_object_browser_mode=>database_table_view text = 'Find Database Tables/Views'(002) )
        ( key = zif_dbbr_c_object_browser_mode=>query               text = 'Find Queries'(003) )
      ).
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = CONV vrm_id( c_p_entry_search_function )
          values = lt_list.
      CLEAR mf_first_call.
    ENDIF.
    set_status( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    zcl_uitb_screen_util=>set_selscreen_status(
        iv_status              = '0200'
        iv_repid               = get_report_id( )
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_saved.
  ENDMETHOD.
ENDCLASS.
