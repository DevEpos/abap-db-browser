"! <p class="shorttext synchronized">Controller for SQL Console settings</p>
CLASS zcl_dbbr_sqlcons_settings_sc DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_uitb_screen_controller.

    ALIASES show      FOR zif_uitb_screen_controller~call_screen.
    ALIASES was_saved FOR zif_uitb_screen_controller~was_not_cancelled.
    ALIASES pbo       FOR zif_uitb_screen_controller~pbo.
    ALIASES cancel    FOR zif_uitb_screen_controller~cancel.
    ALIASES pai       FOR zif_uitb_screen_controller~handle_user_command.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    ALIASES get_report_id FOR zif_uitb_screen_controller~get_report_id.
    ALIASES get_screen_id FOR zif_uitb_screen_controller~get_screen_id.
    ALIASES mf_first_call FOR zif_uitb_screen_controller~mf_first_call.

    CONSTANTS c_r_sqlcon_settings_controller TYPE string VALUE 'GR_SQLCONS_SETTINGS_CONTROLLER' ##NO_TEXT.
    CONSTANTS c_add_entity_to_field_name TYPE fieldname VALUE 'P_ENFN' ##NO_TEXT.

    DATA ms_sqlcons_settings TYPE zdbbr_sqlcons_settings_a.

    DATA:
      BEGIN OF ms_sqlcons_settings_refs,
        add_entity_to_field_name TYPE REF TO zdbbr_sqlcons_settings_a-add_entity_to_field_name,
      END OF ms_sqlcons_settings_refs.
    DATA mf_data_changed TYPE abap_bool.

    METHODS transfer_ui_data
      IMPORTING
        if_from_screen TYPE abap_bool OPTIONAL
        if_to_screen   TYPE abap_bool OPTIONAL.

    METHODS save_settings.
ENDCLASS.


CLASS zcl_dbbr_sqlcons_settings_sc IMPLEMENTATION.
  METHOD constructor.
    " initialize the global data cache
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( get_report_id( ) ).
    ms_sqlcons_settings_refs-add_entity_to_field_name = CAST #( lr_data_cache->get_data_ref(
                                                                    CONV #( c_add_entity_to_field_name ) ) ).
  ENDMETHOD.

  METHOD save_settings.
    zcl_dbbr_usersettings_factory=>set_sql_console_settings(
        VALUE #( add_entity_to_field_name = ms_sqlcons_settings-add_entity_to_field_name ) ).
  ENDMETHOD.

  METHOD transfer_ui_data.
    IF if_from_screen = abap_true.
      ms_sqlcons_settings-add_entity_to_field_name = ms_sqlcons_settings_refs-add_entity_to_field_name->*.
    ELSEIF if_to_screen = abap_true.
      ms_sqlcons_settings = zcl_dbbr_usersettings_factory=>get_sql_console_settings( )-data.
      ms_sqlcons_settings_refs-add_entity_to_field_name->* = ms_sqlcons_settings-add_entity_to_field_name.
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~call_screen.
    transfer_ui_data( if_to_screen = abap_true ).
    mf_first_call = abap_true.

    zcl_uitb_screen_util=>call_screen( iv_screen_id    = get_screen_id( )
                                       iv_report_id    = get_report_id( )
                                       if_selscreen    = abap_true
                                       it_object_map   = VALUE #( ( variable_name = c_r_sqlcon_settings_controller
                                                                    global_ref    = me ) )
                                       iv_start_column = 10
                                       iv_start_line   = 2 ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~cancel.
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>user_settings.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_sqlcons_settings.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~handle_user_command.
    CHECK sy-dynnr = zif_dbbr_screen_ids=>c_sqlcons_settings.

    DATA(lv_function) = cv_function_code.
    CASE lv_function.

      WHEN 'SAVE'.
        transfer_ui_data( if_from_screen = abap_true ).
        save_settings( ).
        MESSAGE s105(zdbbr_info).
        zcl_dbbr_screen_helper=>leave_screen( ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~set_status.
    zcl_uitb_screen_util=>set_selscreen_status( iv_status = '0200'
                                                iv_repid  = get_report_id( ) ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_data_changed.
  ENDMETHOD.
ENDCLASS.
