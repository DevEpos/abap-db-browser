CLASS ZCL_DBBR_addtextfield_ctrl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES ZIF_UITB_SCREEN_CONTROLLER .

    METHODS call_id_field_f4 .
    METHODS call_id_field2_f4 .
    METHODS call_id_table_f4.
    METHODS call_text_field_f4.
    METHODS call_language_field_f4 .
    METHODS constructor
      IMPORTING
        !is_addtext_data TYPE ZDBBR_addtext
        it_id_tables     TYPE ZDBBR_tabinfo_itab
        iv_mode          TYPE ZDBBR_display_mode.
    METHODS get_current_data
      RETURNING
        VALUE(rs_add_text) TYPE ZDBBR_addtext .
    METHODS call_keyfield_f4.
    METHODS call_keyfield2_f4.
    METHODS call_cond_field_f4.
  PROTECTED SECTION.
private section.

  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

  data MR_LANGUAGE_FIELD type ref to FIELDNAME .
  data MR_ID_FIELD type ref to FIELDNAME .
  data MR_ID_FIELD2 type ref to FIELDNAME .
  data MR_ID_TABLE type ref to TABNAME .
  data MR_TEXT_FIELD type ref to FIELDNAME .
  data MR_TEXT_TABLE type ref to TABNAME .
  data MF_DATA_SAVED type BOOLEAN .
  data MS_ADDTEXT_DATA type ZDBBR_ADDTEXT .
  data MR_KEY_FIELD type ref to FIELDNAME .
  data MR_KEY_FIELD2 type ref to FIELDNAME .
  data MV_DISPLAY_MODE type ZDBBR_DISPLAY_MODE .
  data MT_ID_TABLES type ZDBBR_TABINFO_ITAB .
  data MR_CONDITION_FIELD type ref to FIELDNAME .
  data MR_CONDITION_OPERATION type ref to ZDBBR_ADDTEXT_COND_OPERATION .
  data MR_CONDITION_VALUE type ref to ZDBBR_ADDTEXT_COND_VAL .
ENDCLASS.



CLASS ZCL_DBBR_ADDTEXTFIELD_CTRL IMPLEMENTATION.


  METHOD call_cond_field_f4.

    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING
        iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_idtab )
        iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_condf )
        iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING
        cv_fieldname          = mr_condition_field->*
    ).

  ENDMETHOD.


  METHOD call_id_field2_f4.

    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING
        iv_dynpname_tablename = CONV #( ZIF_DBBR_main_report_var_ids=>c_p_idtab )
        iv_dynpname_fieldname = CONV #( ZIF_DBBR_main_report_var_ids=>c_p_idfld2 )
        iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING
        cv_fieldname          = mr_id_field2->*
    ).

  ENDMETHOD.


  METHOD call_id_field_f4.

    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING
        iv_dynpname_tablename = CONV #( ZIF_DBBR_main_report_var_ids=>c_p_idtab )
        iv_dynpname_fieldname = CONV #( ZIF_DBBR_main_report_var_ids=>c_p_idfld )
        iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING
        cv_fieldname          = mr_id_field->*
    ).

  ENDMETHOD.


  METHOD call_id_table_f4.
    mr_id_table->* =  ZCL_DBBR_f4_helper=>call_int_table_f4(
        it_table_search      = mt_id_tables
        iv_f4_window_title   = 'Mögliche Tabellen'
        iv_return_field_name = 'TABNAME'
    ).
  ENDMETHOD.


  METHOD call_keyfield2_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING
        iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
        iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_keyfld2 )
        iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING
        cv_fieldname          = mr_key_field2->*
    ).
  ENDMETHOD.


  METHOD call_keyfield_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING
        iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
        iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_keyfld )
        iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING
        cv_fieldname          = mr_key_field->*
    ).
  ENDMETHOD.


  METHOD call_language_field_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING
        iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
        iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_lngfld )
        iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING
        cv_fieldname          = mr_language_field->*
    ).
  ENDMETHOD.


  METHOD call_text_field_f4.
    zcl_dbbr_ddic_util=>call_table_field_f4(
      EXPORTING
        iv_dynpname_tablename = CONV #( zif_dbbr_main_report_var_ids=>c_p_txttab )
        iv_dynpname_fieldname = CONV #( zif_dbbr_main_report_var_ids=>c_p_txtfld )
        iv_repid              = zif_dbbr_c_report_id=>main
      CHANGING
        cv_fieldname          = mr_text_field->*
    ).

  ENDMETHOD.


  METHOD constructor.

    ms_addtext_data = is_addtext_data.
    mv_display_mode = iv_mode.
    mt_id_tables = it_id_tables.

    " Retrieve some global data references
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_id_field = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_idfld ) ).
    mr_id_field2 = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_idfld2 ) ).
    mr_id_table = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_idtab ) ).
    mr_text_field = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_txtfld ) ).
    mr_text_table = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_txttab ) ).
    mr_language_field = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_lngfld ) ).
    mr_key_field = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_keyfld ) ).
    mr_key_field2 = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_keyfld2 ) ).
    mr_condition_field = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_condf ) ).
    mr_condition_value = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_condv ) ).
    mr_condition_operation = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_p_condo ) ).

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
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_addtextfield_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~cancel.
    ZCL_DBBR_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_define_additional_texts.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~handle_user_command.
    CASE cv_function_code.
      WHEN 'SAVE'.
        " TODO: perform mandatory field checks
        " TODO: validation checks against fields
        DATA(lr_addtext_f) = NEW ZCL_DBBR_addtext_factory( ).
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
        ZCL_DBBR_screen_helper=>leave_screen( ).
      WHEN OTHERS.

    ENDCASE.

    CLEAR cv_function_code.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~pbo.
    ZIF_UITB_SCREEN_CONTROLLER~set_status( ).

    " deactivate key fields during edit mode
    IF mv_display_mode = zif_dbbr_c_global=>c_display_modes-edit.
      LOOP AT SCREEN INTO DATA(ls_screen).
        IF ls_screen-group1 = 'CON'.
          ls_screen-input = 0.
          MODIFY screen FROM ls_screen.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~set_status.
    DATA: lt_excl TYPE TABLE OF sy-ucomm.

*    lt_excl = VALUE #( ( 'SAVE_LOAD' ) ).

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'ADDTEXT_FIELD'
      TABLES
        p_exclude = lt_excl.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~was_not_cancelled.
    rf_not_cancelled = mf_data_saved.
  ENDMETHOD.
ENDCLASS.
