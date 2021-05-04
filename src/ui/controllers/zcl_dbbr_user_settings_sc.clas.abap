"! <p class="shorttext synchronized" lang="en">Controller for user settings</p>
CLASS zcl_dbbr_user_settings_sc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    CONSTANTS:
      BEGIN OF c_tab_ids,
        general_tab        TYPE string VALUE 'INTRO',
        selscreen_tab      TYPE string VALUE 'SEL',
        data_selection_tab TYPE string VALUE 'DSEL',
        favorites_tab      TYPE string VALUE 'FAVS',
        output_tab         TYPE string VALUE 'ALV',
        cds_view_tab       TYPE string VALUE 'CDS',
      END OF c_tab_ids.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        is_user_settings TYPE zdbbr_user_settings_a OPTIONAL
        iv_start_tab     TYPE string OPTIONAL
        iv_start_dynnr   TYPE sy-dynnr OPTIONAL
        if_disable_save  TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Initialize the screen during first call</p>
    METHODS initialize_screen
      CHANGING
        cs_tabs TYPE seltabinfo.
    METHODS get_settings
      RETURNING
        VALUE(rs_settings) TYPE zdbbr_user_settings_a.
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .
    ALIASES mf_first_call
      FOR zif_uitb_screen_controller~mf_first_call.

    TYPES:
      lty_tab_button TYPE c LENGTH 83 .

    DATA ms_user_settings TYPE zdbbr_user_settings_a .
    DATA mf_disable_save TYPE abap_bool.
    DATA mv_start_tab TYPE string.
    DATA mv_start_dynnr TYPE string.
    DATA:
      BEGIN OF ms_user_settings_refs,
        color_sort_columns            TYPE REF TO zdbbr_user_settings_a-color_sort_columns,
        tech_names                    TYPE REF TO zdbbr_user_settings_a-tech_names,
        no_merging_on                 TYPE REF TO zdbbr_user_settings_a-no_merging_on,
        no_convexit                   TYPE REF TO zdbbr_user_settings_a-no_convexit,
        zero_val_as_blank             TYPE REF TO zdbbr_user_settings_a-zero_val_as_blank,
        tech_first                    TYPE REF TO zdbbr_user_settings_a-tech_first,
        tech_view                     TYPE REF TO zdbbr_user_settings_a-tech_view,
        max_lines                     TYPE REF TO zdbbr_user_settings_a-max_lines,
        no_trailing_sign              TYPE REF TO zdbbr_user_settings_a-no_trailing_sign,
        emphasize_text_fields         TYPE REF TO zdbbr_user_settings_a-emphasize_text_fields,
        key_cols_not_fixed            TYPE REF TO zdbbr_user_settings_a-key_cols_not_fixed,
        fav_user_mode                 TYPE REF TO zdbbr_user_settings_a-fav_user_mode,
        use_reduced_memory            TYPE REF TO zdbbr_user_settings_a-use_reduced_memory,
        auto_layout_transfer          TYPE REF TO zdbbr_user_settings_a-auto_layout_transfer,
        advanced_mode                 TYPE REF TO zdbbr_user_settings_a-advanced_mode,
        object_navigator_open         TYPE REF TO zdbbr_user_settings_a-object_navigator_open,
        last_used_count               TYPE REF TO zdbbr_user_settings_a-last_used_count,
        color_formula_fields          TYPE REF TO zdbbr_user_settings_a-color_formula_fields,
        experimental_mode             TYPE REF TO zdbbr_user_settings_a-experimental_mode,
        show_db_size_in_title         TYPE REF TO zdbbr_user_settings_a-show_db_size_in_title,
        enable_alv_default_variant    TYPE REF TO zdbbr_user_settings_a-enable_alv_default_variant,
        maintain_entries              TYPE REF TO zdbbr_user_settings_a-maintain_entries,
        search_ignore_case            TYPE REF TO zdbbr_user_settings_a-search_ignore_case,
        assoc_sel_mode                TYPE REF TO zdbbr_user_settings_a-assoc_sel_mode,
        show_assoc_brws_at_start      TYPE REF TO zdbbr_user_settings_a-show_assoc_brws_at_start,
        activate_alv_live_filter      TYPE REF TO zdbbr_user_settings_a-activate_alv_live_filter,
        initial_obj_brws_mode         TYPE REF TO zdbbr_user_settings_a-initial_obj_brws_mode,
        initial_obj_nav_mode          TYPE REF TO zdbbr_user_settings_a-initial_obj_nav_mode,
        disable_date_to_times_conv    TYPE REF TO zdbbr_user_settings_a-disable_date_to_times_conv,
        use_ddl_view_for_select       TYPE REF TO zdbbr_user_settings_a-use_ddl_view_for_select,
        deactvt_highltng_in_cqe       TYPE REF TO zdbbr_user_settings_a-deactvt_highltng_in_cqe,
        code_viewer_theme             TYPE REF TO zdbbr_user_settings_a-code_viewer_theme,
        auto_sel_filter_saving        TYPE REF TO zdbbr_user_settings_a-auto_sel_filter_saving,
        always_load_def_variant_first TYPE REF TO zdbbr_user_settings_a-always_load_def_variant_first,
        dock_obj_nav_on_right         TYPE REF TO zdbbr_user_settings_a-dock_obj_nav_on_right,
        selscr_compact_col_widths     TYPE REF TO zdbbr_user_settings_a-selscr_compact_col_widths,
        auto_hide_empty_cols          TYPE REF TO zdbbr_user_settings_a-auto_hide_empty_cols,
        calculate_virtual_element     TYPE REF TO zdbbr_user_settings_a-calculate_virtual_element,
        ignore_error_virt_elem_calc   TYPE REF TO zdbbr_user_settings_a-ignore_error_virt_elem_calc,
        color_cds_calculated_fields   TYPE REF TO zdbbr_user_settings_a-color_cds_calculated_fields,
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
    mf_disable_save = if_disable_save.
    mv_start_tab = iv_start_tab.
    mv_start_dynnr = iv_start_dynnr.

    " initialize the global data cache
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>user_settings ).

    read_parameter_reference:
        color_sort_columns            c_color_sorted_columns,
        tech_names                    c_technical_names,
        no_merging_on                 c_no_merging_of_srt_cols,
        no_convexit                   c_no_conv_exit,
        zero_val_as_blank             c_zeros_as_blanks,
        tech_first                    c_technical_fields_first,
        tech_view                     c_technical_view,
        max_lines                     c_max_result_lines,
        no_trailing_sign              c_no_trailing_sign,
        emphasize_text_fields         c_color_add_text_fields,
        key_cols_not_fixed            c_no_fixed_key_cols,
        fav_user_mode                 c_favorite_mode_entry,
        use_reduced_memory            c_use_reduced_memory,
        auto_layout_transfer          c_auto_layout_transfer,
        advanced_mode                 c_advanced_mode,
        object_navigator_open         c_object_navigator_at_start,
        initial_obj_brws_mode         c_initial_obj_browser_mode,
        initial_obj_nav_mode          c_initial_obj_nav_mode,
        last_used_count               c_number_fav_most_used,
        color_formula_fields          c_color_formula_fields,
        experimental_mode             c_experimental_mode,
        show_db_size_in_title         c_read_db_table_length,
        enable_alv_default_variant    c_enable_alv_default_var,
        maintain_entries              c_activate_maintain_entries,
        search_ignore_case            c_search_ignore_case,
        assoc_sel_mode                c_assocation_sel_mode,
        show_assoc_brws_at_start      c_show_assoc_sel_at_start,
        activate_alv_live_filter      c_activate_alv_live_filter,
        disable_date_to_times_conv    c_disable_date_to_timest_conv,
        use_ddl_view_for_select       c_use_ddl_view_for_select,
        deactvt_highltng_in_cqe       c_deactvt_highltng_in_cqe,
        code_viewer_theme             c_code_viewer_theme,
        auto_sel_filter_saving        c_auto_select_criteria_saving,
        always_load_def_variant_first c_always_load_def_var_first,
        dock_obj_nav_on_right         c_dock_obj_nav_on_right,
        selscr_compact_col_widths     c_selscr_compact_col_widths,
        auto_hide_empty_cols          c_auto_hide_empty_cols,
        calculate_virtual_element     c_calculate_virtual_elements,
        ignore_error_virt_elem_calc   c_ignore_error_virt_elem_calc,
        color_cds_calculated_fields   c_color_cds_calculated_fields.
  endmethod.

    METHOD initialize_screen.
      CHECK mf_first_call = abap_true.

      IF mv_start_tab IS NOT INITIAL.
        cs_tabs-dynnr = mv_start_dynnr.
        cs_tabs-activetab = mv_start_tab.
      ENDIF.
    ENDMETHOD.

    METHOD get_settings.
      rs_settings = ms_user_settings.
    ENDMETHOD.


    METHOD save_settings.
      zcl_dbbr_usersettings_factory=>save_settings( ms_user_settings ).
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
      mf_first_call = abap_true.

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
      result = zif_dbbr_screen_ids=>c_user_settings-main_screen.
    ENDMETHOD.


    METHOD zif_uitb_screen_controller~handle_user_command.
      CHECK sy-dynnr = 0100.

      DATA(lv_function) = cv_function_code.

      CASE lv_function.

        WHEN 'OK'.
          transfer_ui_data( if_from_screen = abap_true ).
          IF mf_disable_save = abap_false.
            save_settings( ).
            MESSAGE s011(zdbbr_info).
          ENDIF.
          mf_data_changed = abap_true.
          zcl_dbbr_screen_helper=>leave_screen( ).

      ENDCASE.
    ENDMETHOD.


    METHOD zif_uitb_screen_controller~pbo.
      IF mf_first_call = abap_true.
        CLEAR mf_first_call.
      ENDIF.

      zif_uitb_screen_controller~set_status( ).
      LOOP AT SCREEN.
***      IF screen-name = zif_dbbr_user_settings_ids=>c_search_ignore_case.
***        screen-input = 0.
***        MODIFY SCREEN.
***      ENDIF.
        IF mv_start_tab = c_tab_ids-output_tab AND
           mf_disable_save = abap_true AND
           (
*           screen-name = 'BTN_INTR' OR
             screen-name = 'BTN_DSEL' OR
             screen-name = 'BTN_FAV' OR
             screen-name = 'BTN_SEL' OR
             screen-name = 'BTN_CDS' ).
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDMETHOD.


    METHOD zif_uitb_screen_controller~set_status.
      CHECK sy-dynnr = zif_dbbr_screen_ids=>c_user_settings-main_screen.

      zcl_dbbr_screen_helper=>set_selscreen_status(
          iv_status              = '0100'
          iv_repid               = zif_dbbr_c_report_id=>user_settings
          it_excluding_functions = COND #( WHEN mf_disable_save = abap_true THEN VALUE #( ( 'SAVE' ) ) )
      ).
    ENDMETHOD.


    METHOD zif_uitb_screen_controller~was_not_cancelled.
      rf_not_cancelled = mf_data_changed.
    ENDMETHOD.
ENDCLASS.
