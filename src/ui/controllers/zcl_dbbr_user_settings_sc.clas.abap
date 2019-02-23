CLASS zcl_dbbr_user_settings_sc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    METHODS constructor
      IMPORTING
        is_user_settings TYPE zdbbr_user_settings_a OPTIONAL.
    METHODS get_settings
      RETURNING
        VALUE(rs_settings) TYPE zdbbr_user_settings_a.
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    TYPES:
      lty_tab_button TYPE c LENGTH 83 .

    DATA ms_user_settings TYPE zdbbr_user_settings_a .
    DATA:
      BEGIN OF ms_user_settings_refs,
        color_sort_columns         TYPE REF TO zdbbr_user_settings_a-color_sort_columns,
        tech_names                 TYPE REF TO zdbbr_user_settings_a-tech_names,
        no_merging_on              TYPE REF TO zdbbr_user_settings_a-no_merging_on,
        no_convexit                TYPE REF TO zdbbr_user_settings_a-no_convexit,
        zero_val_as_blank          TYPE REF TO zdbbr_user_settings_a-zero_val_as_blank,
        tech_first                 TYPE REF TO zdbbr_user_settings_a-tech_first,
        tech_view                  TYPE REF TO zdbbr_user_settings_a-tech_view,
        max_lines                  TYPE REF TO zdbbr_user_settings_a-max_lines,
        no_trailing_sign           TYPE REF TO zdbbr_user_settings_a-no_trailing_sign,
        emphasize_text_fields      TYPE REF TO zdbbr_user_settings_a-emphasize_text_fields,
        key_cols_not_fixed         TYPE REF TO zdbbr_user_settings_a-key_cols_not_fixed,
        fav_user_mode              TYPE REF TO zdbbr_user_settings_a-fav_user_mode,
        use_reduced_memory         TYPE REF TO zdbbr_user_settings_a-use_reduced_memory,
        auto_layout_transfer       TYPE REF TO zdbbr_user_settings_a-auto_layout_transfer,
        advanced_mode              TYPE REF TO zdbbr_user_settings_a-advanced_mode,
        object_navigator_open      TYPE REF TO zdbbr_user_settings_a-object_navigator_open,
        last_used_count            TYPE REF TO zdbbr_user_settings_a-last_used_count,
        color_formula_fields       TYPE REF TO zdbbr_user_settings_a-color_formula_fields,
        experimental_mode          TYPE REF TO zdbbr_user_settings_a-experimental_mode,
        show_db_size_in_title      TYPE REF TO zdbbr_user_settings_a-show_db_size_in_title,
        enable_alv_default_variant TYPE REF TO zdbbr_user_settings_a-enable_alv_default_variant,
        maintain_entries           TYPE REF TO zdbbr_user_settings_a-maintain_entries,
        description_language       TYPE REF TO zdbbr_user_settings_a-description_language,
        search_ignore_case         TYPE REF TO zdbbr_user_settings_a-search_ignore_case,
        assoc_sel_mode             TYPE REF TO zdbbr_user_settings_a-assoc_sel_mode,
        show_assoc_brws_at_start   TYPE REF TO zdbbr_user_settings_a-show_assoc_brws_at_start,
        activate_alv_live_filter   TYPE REF TO zdbbr_user_settings_a-activate_alv_live_filter,
        initial_obj_brws_mode      TYPE REF TO zdbbr_user_settings_a-initial_obj_brws_mode,
        initial_obj_nav_mode       TYPE REF TO zdbbr_user_settings_a-initial_obj_nav_mode,
        disable_date_to_times_conv TYPE REF TO zdbbr_user_settings_a-disable_date_to_times_conv,
        use_ddl_view_for_select    TYPE REF TO zdbbr_user_settings_a-use_ddl_view_for_select,
      END OF ms_user_settings_refs .
    DATA mf_data_changed TYPE abap_bool .

    METHODS transfer_ui_data
      IMPORTING
        !if_from_screen TYPE abap_bool OPTIONAL
        !if_to_screen   TYPE abap_bool OPTIONAL .
    METHODS save_settings .
ENDCLASS.



CLASS zcl_dbbr_user_settings_sc IMPLEMENTATION.


  METHOD constructor.
    DEFINE read_parameter_reference.
      ms_user_settings_refs-&1 = CAST #( lr_data_cache->get_data_ref( zif_dbbr_user_settings_ids=>&2 ) ).
    END-OF-DEFINITION.

    ms_user_settings = is_user_settings.

    " initialize the global data cache
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>user_settings ).

    read_parameter_reference:
        color_sort_columns          c_color_sorted_columns,
        tech_names                  c_technical_names,
        no_merging_on               c_no_merging_of_srt_cols,
        no_convexit                 c_no_conv_exit,
        zero_val_as_blank           c_zeros_as_blanks,
        tech_first                  c_technical_fields_first,
        tech_view                   c_technical_view,
        max_lines                   c_max_result_lines,
        no_trailing_sign            c_no_trailing_sign,
        emphasize_text_fields       c_color_add_text_fields,
        key_cols_not_fixed          c_no_fixed_key_cols,
        fav_user_mode               c_favorite_mode_entry,
        use_reduced_memory          c_use_reduced_memory,
        auto_layout_transfer        c_auto_layout_transfer,
        advanced_mode               c_advanced_mode,
        object_navigator_open       c_object_navigator_at_start,
        initial_obj_brws_mode       c_initial_obj_browser_mode,
        initial_obj_nav_mode        c_initial_obj_nav_mode,
        last_used_count             c_number_fav_most_used,
        color_formula_fields        c_color_formula_fields,
        experimental_mode           c_experimental_mode,
        show_db_size_in_title       c_read_db_table_length,
        enable_alv_default_variant  c_enable_alv_default_var,
        maintain_entries            c_activate_maintain_entries,
        description_language        c_description_language,
        search_ignore_case          c_search_ignore_case,
        assoc_sel_mode              c_assocation_sel_mode,
        show_assoc_brws_at_start    c_show_assoc_sel_at_start,
        activate_alv_live_filter    c_activate_alv_live_filter,
        disable_date_to_times_conv  c_disable_date_to_timest_conv,
        use_ddl_view_for_select     c_use_ddl_view_for_select.
  ENDMETHOD.


  METHOD get_settings.
    rs_settings = ms_user_settings.
  ENDMETHOD.


  METHOD save_settings.
    NEW zcl_dbbr_usersettings_factory( )->save_settings( ms_user_settings ).
  ENDMETHOD.


  METHOD transfer_ui_data.
    DATA: lr_setting_ref TYPE REF TO data.

    FIELD-SYMBOLS: <lr_settings> TYPE REF TO data.
    DATA(lt_setting_comp_names) = zcl_uitb_rtti_util=>get_struct_components( ms_user_settings ).

    LOOP AT lt_setting_comp_names ASSIGNING FIELD-SYMBOL(<ls_setting_comp>).
      ASSIGN COMPONENT <ls_setting_comp>-name OF STRUCTURE ms_user_settings_refs TO FIELD-SYMBOL(<lr_setting_ui>).
      CHECK sy-subrc = 0.

      ASSIGN <lr_setting_ui>->* TO FIELD-SYMBOL(<lv_setting_ui>).

      ASSIGN COMPONENT <ls_setting_comp>-name OF STRUCTURE ms_user_settings TO FIELD-SYMBOL(<lv_setting>).

      IF if_from_screen = abap_true.
        <lv_setting> = <lv_setting_ui>.
      ELSEIF if_to_screen = abap_true.
        <lv_setting_ui> = <lv_setting>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    transfer_ui_data( if_to_screen = abap_true ).

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_user_settings_ids=>c_r_user_settings_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>user_settings.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_show_user_settings.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
    CHECK sy-dynnr = 0100.

    DATA(lv_function) = cv_function_code.

    CASE lv_function.

      WHEN 'OK'.
        mf_data_changed = abap_true.
        transfer_ui_data( if_from_screen = abap_true ).
        zcl_dbbr_screen_helper=>leave_screen( ).

      WHEN 'SAVE'.
        transfer_ui_data( if_from_screen = abap_true ).
        save_settings( ).
        MESSAGE s011(zdbbr_info).
        mf_data_changed = abap_true.
        zcl_dbbr_screen_helper=>leave_screen( ).

    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).
    LOOP AT SCREEN.
      IF screen-name = zif_dbbr_user_settings_ids=>c_search_ignore_case.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    CHECK sy-dynnr = '0100'.

    zcl_dbbr_screen_helper=>set_selscreen_status(
        iv_status = '0100'
        iv_repid  = zif_dbbr_c_report_id=>user_settings
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_data_changed.
  ENDMETHOD.
ENDCLASS.
