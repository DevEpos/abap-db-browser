"! <p class="shorttext synchronized" lang="en">Factory for user settings</p>
CLASS zcl_dbbr_usersettings_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Retrieves the current settings of the user</p>
    CLASS-METHODS get_current_settings
      RETURNING
        VALUE(rr_s_settings) TYPE REF TO zdbbr_user_settings_a.
    "! <p class="shorttext synchronized" lang="en">Retrieve Settings for entity browser</p>
    CLASS-METHODS get_entity_browser_settings
      RETURNING
        VALUE(rs_settings) TYPE zdbbr_entbrwsus .
    "! <p class="shorttext synchronized" lang="en">Save Settings for Entity Browser</p>
    CLASS-METHODS set_entity_browser_settings
      IMPORTING
        !is_settings TYPE zdbbr_entity_browser_sttng_a .
    CLASS-METHODS get_settings
      RETURNING
        VALUE(result) TYPE zdbbr_user_settings_a .
    CLASS-METHODS get_last_used_count_setting
      RETURNING
        VALUE(result) TYPE sy-tabix .
    CLASS-METHODS save_settings
      IMPORTING
        !value TYPE zdbbr_user_settings_a .
    CLASS-METHODS update_start_settings
      IMPORTING
        !iv_entity_id   TYPE zdbbr_entity_id
        !iv_entity_type TYPE zdbbr_entity_type .
    CLASS-METHODS should_read_db_size
      RETURNING
        VALUE(result) TYPE abap_bool .
    CLASS-METHODS is_experimental_mode_active
      RETURNING
        VALUE(result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_usersettings_factory IMPLEMENTATION.

  METHOD get_entity_browser_settings.
    SELECT SINGLE *
      FROM zdbbr_entbrwsus
      WHERE username = @sy-uname
      INTO CORRESPONDING FIELDS OF @rs_settings.

    IF sy-subrc <> 0.
      rs_settings = VALUE #(
        max_hits        = 500
        link_mode       = zif_dbbr_c_eb_link_mode=>open_in_db_browser_new_task
        search_function = zif_dbbr_c_object_browser_mode=>cds_view
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_last_used_count_setting.
    DATA(ls_settings) = get_settings( ).

    result = ls_settings-last_used_count.
  ENDMETHOD.


  METHOD get_settings.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF result
      FROM zdbbr_usrsettng
      WHERE username = sy-uname.

    IF sy-subrc <> 0.
      result = VALUE #(
          color_sort_columns    = abap_true
          zero_val_as_blank     = abap_true
          max_lines             = zif_dbbr_global_consts=>c_default_max_lines
          emphasize_text_fields = abap_true
          fav_user_mode         = zif_dbbr_global_consts=>gc_fav_user_modes-global
          last_used_count       = zif_dbbr_global_consts=>c_default_last_used_favs
          color_formula_fields  = abap_true
          description_language  = sy-langu
          object_navigator_open = abap_true
          initial_obj_nav_mode = zif_dbbr_c_obj_navigator_mode=>object_browser
          initial_obj_brws_mode = zif_dbbr_c_object_browser_mode=>cds_view
      ).
    ENDIF.
  ENDMETHOD.


  METHOD is_experimental_mode_active.
    CLEAR result.

    SELECT SINGLE experimental_mode INTO @result
      FROM zdbbr_usrsettng
      WHERE username = @sy-uname.
  ENDMETHOD.


  METHOD save_settings.
    DATA(ls_settings) = CORRESPONDING zdbbr_usrsettng( value ).
    ls_settings-username = sy-uname.

    MODIFY zdbbr_usrsettng FROM ls_settings.
    COMMIT WORK.
  ENDMETHOD.


  METHOD set_entity_browser_settings.
    DATA(ls_settings) = CORRESPONDING zdbbr_entbrwsus( is_settings ).
    ls_settings-username = sy-uname.

    MODIFY zdbbr_entbrwsus FROM ls_settings.
    COMMIT WORK.
  ENDMETHOD.


  METHOD should_read_db_size.
    CLEAR result.

    SELECT SINGLE show_db_size_in_title INTO @result
      FROM zdbbr_usrsettng
      WHERE username = @sy-uname.
  ENDMETHOD.


  METHOD update_start_settings.
    DATA(ls_settings) = get_settings( ).

    ls_settings-last_entity_id = iv_entity_id.
    ls_settings-last_entity_type = iv_entity_type.

    save_settings( CORRESPONDING #( ls_settings ) ).
  ENDMETHOD.

  METHOD get_current_settings.
    DATA(lo_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).
    DATA(lr_global_data) = CAST zdbbr_global_data( lo_data_cache->get_data_ref( iv_registered_name = zif_dbbr_main_report_var_ids=>c_s_data ) ).
    rr_s_settings = REF #( lr_global_data->settings ).
  ENDMETHOD.

ENDCLASS.
