CLASS zcl_dbbr_addtextfield_ctrl DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_uitb_screen_controller.

    METHODS call_id_field_f4.
    METHODS call_id_field2_f4.
    METHODS call_id_table_f4.
    METHODS call_text_field_f4.
    METHODS call_language_field_f4.

    METHODS constructor
      IMPORTING
        is_addtext_data TYPE zdbbr_addtext
        it_id_tables    TYPE zdbbr_tabinfo_itab
        iv_mode         TYPE zdbbr_display_mode.

    METHODS get_current_data
      RETURNING
        VALUE(rs_add_text) TYPE zdbbr_addtext.

    METHODS call_keyfield_f4.
    METHODS call_keyfield2_f4.
    METHODS call_cond_field_f4.

  PRIVATE SECTION.
    ALIASES get_report_id FOR zif_uitb_screen_controller~get_report_id.
    ALIASES get_screen_id FOR zif_uitb_screen_controller~get_screen_id.

    DATA mr_language_field TYPE REF TO fieldname.
    DATA mr_id_field TYPE REF TO fieldname.
    DATA mr_id_field2 TYPE REF TO fieldname.
    DATA mr_id_table TYPE REF TO tabname.
    DATA mr_text_field TYPE REF TO fieldname.
    DATA mr_text_table TYPE REF TO tabname.
    DATA mf_data_saved TYPE boolean.
    DATA ms_addtext_data TYPE zdbbr_addtext.
    DATA mr_key_field TYPE REF TO fieldname.
    DATA mr_key_field2 TYPE REF TO fieldname.
    DATA mv_display_mode TYPE zdbbr_display_mode.
    DATA mt_id_tables TYPE zdbbr_tabinfo_itab.
    DATA mr_condition_field TYPE REF TO fieldname.
    DATA mr_condition_operation TYPE REF TO zdbbr_addtext_cond_operation.
    DATA mr_condition_value TYPE REF TO zdbbr_addtext_cond_val.
ENDCLASS.


CLASS zcl_dbbr_addtextfield_ctrl IMPLEMENTATION.
  METHOD call_cond_field_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_idtab )
                iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_condf )
                iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING  cv_fieldname          = mr_condition_field->* ).
  ENDMETHOD.

  METHOD call_id_field2_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_idtab )
                iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_idfld2 )
                iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING  cv_fieldname          = mr_id_field2->* ).
  ENDMETHOD.

  METHOD call_id_field_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_idtab )
                iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_idfld )
                iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING  cv_fieldname          = mr_id_field->* ).
  ENDMETHOD.

  METHOD call_id_table_f4.
    mr_id_table->* = zcl_dbbr_f4_helper=>call_int_table_f4( it_table_search      = mt_id_tables
                                                            iv_f4_window_title   = 'Mögliche Tabellen'
                                                            iv_return_field_name = 'TABNAME' ).
  ENDMETHOD.

  METHOD call_keyfield2_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
                iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_keyfld2 )
                iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING  cv_fieldname          = mr_key_field2->* ).
  ENDMETHOD.

  METHOD call_keyfield_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
                iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_keyfld )
                iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING  cv_fieldname          = mr_key_field->* ).
  ENDMETHOD.

  METHOD call_language_field_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
                iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_lngfld )
                iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING  cv_fieldname          = mr_language_field->* ).
  ENDMETHOD.

  METHOD call_text_field_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
                iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_txtfld )
                iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING  cv_fieldname          = mr_text_field->* ).
  ENDMETHOD.

  METHOD constructor.
    ms_addtext_data = is_addtext_data.
    mv_display_mode = iv_mode.
    mt_id_tables = it_id_tables.

    " Retrieve some global data references
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_id_field = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_idfld ) ).
    mr_id_field2 = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_idfld2 ) ).
    mr_id_table = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_idtab ) ).
    mr_text_field = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_txtfld ) ).
    mr_text_table = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_txttab ) ).
    mr_language_field = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_lngfld ) ).
    mr_key_field = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_keyfld ) ).
    mr_key_field2 = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_keyfld2 ) ).
    mr_condition_field = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_condf ) ).
    mr_condition_value = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_condv ) ).
    mr_condition_operation = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_condo ) ).

    " transfer data to ui controls
    mr_id_field->* = ms_addtext_data-id_field.
    mr_id_field2->* = ms_addtext_data-id_field2.
    mr_id_table->* = ms_addtext_data-id_table.
    mr_text_table->* = ms_addtext_data-text_table.
    mr_text_field->* = ms_addtext_data-text_field.
    mr_language_field->* = ms_addtext_data-language_field.
    mr_key_field->* = ms_addtext_data-key_field.
    mr_key_field2->* = ms_addtext_data-key_field2.
    mr_condition_field->* = ms_addtext_data-condition_field.
    mr_condition_operation->* = ms_addtext_data-condition_op.
    mr_condition_value->* = ms_addtext_data-condition_value.
  ENDMETHOD.

  METHOD get_current_data.
    rs_add_text = ms_addtext_data.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen( iv_screen_id    = get_screen_id( )
                                       iv_report_id    = get_report_id( )
                                       if_selscreen    = abap_true
                                       it_object_map   = VALUE #(
                                           ( variable_name = zif_dbbr_main_report_var_ids=>c_r_addtextfield_controller
                                             global_ref    = me ) )
                                       iv_start_column = 10
                                       iv_start_line   = 2 ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~cancel.
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_define_additional_texts.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~handle_user_command.
    CASE cv_function_code.
      WHEN 'SAVE'.
        " TODO: perform mandatory field checks
        " TODO: validation checks against fields
        DATA(lr_addtext_f) = NEW zcl_dbbr_addtext_factory( ).
        ms_addtext_data-id_table        = mr_id_table->*.
        ms_addtext_data-id_field        = mr_id_field->*.
        ms_addtext_data-id_field2       = mr_id_field2->*.
        ms_addtext_data-text_table      = mr_text_table->*.
        ms_addtext_data-text_field      = mr_text_field->*.
        ms_addtext_data-language_field  = mr_language_field->*.
        ms_addtext_data-key_field       = mr_key_field->*.
        ms_addtext_data-key_field2      = mr_key_field2->*.
        ms_addtext_data-condition_field = mr_condition_field->*.
        ms_addtext_data-condition_value = mr_condition_value->*.
        ms_addtext_data-condition_op    = mr_condition_operation->*.

        " TODO: check that the key combination does not exist yet
        lr_addtext_f->save_add_text( CHANGING cs_addtext_data = ms_addtext_data ).
        mf_data_saved = abap_true.
        zcl_dbbr_screen_helper=>leave_screen( ).
      WHEN OTHERS.

    ENDCASE.

    CLEAR cv_function_code.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).

    " deactivate key fields during edit mode
    IF mv_display_mode = zif_dbbr_c_global=>c_display_modes-edit.
      LOOP AT SCREEN INTO DATA(ls_screen).
        IF ls_screen-group1 = 'CON'.
          ls_screen-input = 0.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~set_status.
    DATA lt_excl TYPE TABLE OF sy-ucomm.

*    lt_excl = VALUE #( ( 'SAVE_LOAD' ) ).

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING p_status  = 'ADDTEXT_FIELD'
      TABLES    p_exclude = lt_excl.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_data_saved.
  ENDMETHOD.
ENDCLASS.
