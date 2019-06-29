CLASS zcl_dbbr_generic_f4_sc DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_screen_controller
      ABSTRACT METHODS call_screen.

    ALIASES was_saved
      FOR zif_uitb_screen_controller~was_not_cancelled.

    METHODS constructor
      IMPORTING
        iv_display_mode TYPE zdbbr_display_mode.

    CLASS-METHODS delete_f4_from_field
      IMPORTING
        !ir_selfield_ref TYPE REF TO zdbbr_selfield .
  PROTECTED SECTION.
    CONSTANTS c_save_func TYPE ui_func VALUE 'SAVE' ##NO_TEXT.
    CONSTANTS c_enter_func TYPE ui_func VALUE 'ENTER' ##NO_TEXT.
    CONSTANTS c_test_f4_func TYPE ui_func VALUE 'TEST_F4' ##NO_TEXT.
    CONSTANTS c_cancel_func TYPE ui_func VALUE 'CANCEL' ##NO_TEXT.

    DATA mf_delete_existing TYPE boolean .
    DATA mr_ui_global_data TYPE REF TO zdbbr_global_data.
    DATA mt_function_exclude TYPE ui_functions.

    DATA mr_ui_ok_code TYPE REF TO syst_ucomm.

    DATA mt_existing_f4 TYPE zdbbr_f4_data_itab .
    DATA ms_f4_def TYPE zdbbr_f4_data .
    DATA mf_saved TYPE abap_bool.
    DATA mv_display_mode TYPE zdbbr_display_mode.

    METHODS test_f4.
    METHODS save_search_help ABSTRACT.
    METHODS should_save ABSTRACT
      RETURNING
        VALUE(rf_save) TYPE boolean .
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_dbbr_generic_f4_sc IMPLEMENTATION.


  METHOD constructor.
    CLEAR: ms_f4_def.

    mv_display_mode = iv_display_mode.

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_ui_global_data = CAST zdbbr_global_data( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).
    mr_ui_ok_code = CAST syst_ucomm( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_ok_code ) ).

  ENDMETHOD.


  METHOD delete_f4_from_field.
*&---------------------------------------------------------------------*
*& Description: Deletes all F4 helps from this table field
*&---------------------------------------------------------------------*
    DATA: lv_answer                TYPE char1.

    IF ir_selfield_ref->has_cust_f4_help = abap_true.

      IF zcl_dbbr_appl_util=>popup_to_confirm(
            iv_title                 = 'Remove all Value Helps?'
            iv_query                 = |Do you really want to remove all Value Helps for the Field { ir_selfield_ref->fieldname }| &&
                                       | in { ir_selfield_ref->tabname }?|
            if_display_cancel_button = abap_false
            iv_icon_type             = 'ICON_MESSAGE_WARNING'  ) <> '1'.
        RETURN.
      ENDIF.

      " first delete all user specific search helps
      zcl_dbbr_custom_f4_factory=>delete_f4_assignments(
          iv_tabname     = ir_selfield_ref->tabname
          iv_fieldname   = ir_selfield_ref->fieldname
      ).
      ir_selfield_ref->has_cust_f4_help = abap_false.
      MESSAGE s000(zdbbr_info) WITH ir_selfield_ref->fieldname ir_selfield_ref->tabname.
    ELSE.
      MESSAGE s055(zdbbr_info) WITH ir_selfield_ref->fieldname ir_selfield_ref->tabname.
    ENDIF.

  ENDMETHOD.


  METHOD test_f4 ##needed.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.
    zif_uitb_screen_controller~free_screen_resources( ).
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_saved.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~determine_cursor.
    zcl_uitb_cursor=>get_cursor( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
*&---------------------------------------------------------------------*
*& Description: User command handling in dynpro 202
*&---------------------------------------------------------------------*
    DATA: lv_field(40).
    DATA(lv_function_code) = cv_function_code.
    CLEAR cv_function_code.

    TRY.
        CASE lv_function_code.
          WHEN c_save_func.
            save_search_help( ).
            zcl_dbbr_screen_helper=>leave_screen( ).

          WHEN c_enter_func.

          WHEN c_test_f4_func.
            test_f4( ).

          WHEN c_cancel_func.
            zif_uitb_screen_controller~free_screen_resources( ).
            zcl_dbbr_screen_helper=>leave_screen( ).
        ENDCASE.
      CATCH zcx_dbbr_validation_exception INTO DATA(lr_validation_exc).
        lr_validation_exc->show_message( iv_message_type = 'I' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).
    zcl_uitb_cursor=>refresh_cursor( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    DATA(lt_exclude) = mt_function_exclude.

    IF mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-view.
      lt_exclude = VALUE #( ( c_save_func ) ).
    ENDIF.

    SET PF-STATUS '0202' OF PROGRAM zif_dbbr_c_report_id=>main EXCLUDING lt_exclude.
    DATA(lv_title) = SWITCH string(
      mv_display_mode
      WHEN zif_dbbr_global_consts=>gc_display_modes-view THEN |{ 'Display' }|
      WHEN zif_dbbr_global_consts=>gc_display_modes-create THEN |{ 'Create' }|
      WHEN zif_dbbr_global_consts=>gc_display_modes-edit THEN |{ 'Edit' }|
    ).
    DATA(lv_title2) = | { 'Value Help' }|.
    SET TITLEBAR 'PROGTITLE' OF PROGRAM zif_dbbr_c_report_id=>main WITH lv_title lv_title2.
  ENDMETHOD.
ENDCLASS.
