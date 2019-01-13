CLASS ZCL_DBBR_generic_f4_sc DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_UITB_SCREEN_CONTROLLER
      ABSTRACT METHODS call_screen.

    METHODS constructor
      IMPORTING
        iv_display_mode TYPE ZDBBR_display_mode.

    CLASS-METHODS delete_f4_from_field
      IMPORTING
        !ir_selfield_ref TYPE REF TO ZDBBR_selfield .
  PROTECTED SECTION.
    DATA mf_delete_existing TYPE boolean .
    DATA mr_ui_global_data TYPE REF TO ZDBBR_global_data.

    DATA mr_ui_ok_code TYPE REF TO syst_ucomm.

    DATA mr_custom_f4_factory TYPE REF TO ZCL_DBBR_custom_f4_factory.
    DATA mt_existing_f4 TYPE ZDBBR_f4_data_itab .
    DATA ms_f4_def TYPE ZDBBR_f4_data .
    DATA mv_display_mode TYPE ZDBBR_display_mode.

    METHODS test_f4.
    METHODS save_search_help ABSTRACT.
    METHODS should_save ABSTRACT
      RETURNING
        VALUE(rf_save) TYPE boolean .
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_DBBR_GENERIC_F4_SC IMPLEMENTATION.


  METHOD constructor.
    CLEAR: ms_f4_def.

    mv_display_mode = iv_display_mode.

    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_ui_global_data = CAST ZDBBR_global_data( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_s_data ) ).
    mr_ui_ok_code = CAST syst_ucomm( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_ok_code ) ).

    mr_custom_f4_factory = NEW #( ).

  ENDMETHOD.


  METHOD delete_f4_from_field.
*&---------------------------------------------------------------------*
*& Description: Deletes all F4 helps from this table field
*&---------------------------------------------------------------------*
    DATA: lv_answer                TYPE char1.

    IF ir_selfield_ref->has_cust_f4_help = abap_true.

      IF ZCL_DBBR_appl_util=>popup_to_confirm(
            iv_title                 = 'Remove all Value Helps?'
            iv_query                 = |Do you really want to remove all Value Helps for the Field { ir_selfield_ref->fieldname }| &&
                                       | in { ir_selfield_ref->tabname }?|
            if_display_cancel_button = abap_false
            iv_icon_type             = 'ICON_MESSAGE_WARNING'  ) <> '1'.
        RETURN.
      ENDIF.

      " first delete all user specific search helps
      NEW ZCL_DBBR_custom_f4_factory( )->delete_f4_assignments(
          iv_tabname     = ir_selfield_ref->tabname
          iv_fieldname   = ir_selfield_ref->fieldname
      ).
      ir_selfield_ref->has_cust_f4_help = abap_false.
      message s000(zdbbr_info) with ir_selfield_ref->fieldname ir_selfield_ref->tabname.
    ELSE.
      Message s055(zdbbr_info) with ir_selfield_ref->fieldname ir_selfield_ref->tabname.
    ENDIF.

  ENDMETHOD.


  METHOD test_f4 ##needed.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~cancel.
    ZIF_UITB_SCREEN_CONTROLLER~free_screen_resources( ).
    ZCL_DBBR_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~determine_cursor.
    zcl_uitb_cursor=>get_cursor( ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~handle_user_command.
*&---------------------------------------------------------------------*
*& Description: User command handling in dynpro 202
*&---------------------------------------------------------------------*
    DATA: lv_field(40).
    DATA(lv_function_code) = cv_function_code.
    CLEAR cv_function_code.

    TRY.
        CASE lv_function_code.
          WHEN 'SAVE'.
            save_search_help( ).
            ZCL_DBBR_screen_helper=>leave_screen( ).

          WHEN 'ENTER'.

          WHEN 'TEST_F4'.
            test_f4( ).

          WHEN 'CANCEL'.
            ZIF_UITB_SCREEN_CONTROLLER~free_screen_resources( ).
            ZCL_DBBR_screen_helper=>leave_screen( ).
        ENDCASE.
      CATCH ZCX_DBBR_validation_exception INTO DATA(lr_validation_exc).
        lr_validation_exc->show_message( iv_message_type = 'I' ).
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~pbo.
    ZIF_UITB_SCREEN_CONTROLLER~set_status( ).
    zcl_uitb_cursor=>refresh_cursor( ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~set_status.
    DATA: lt_exclude TYPE ui_functions.

    IF mv_display_mode = ZIF_DBBR_global_consts=>gc_display_modes-view.
      lt_exclude = VALUE #( ( 'SAVE' ) ).
    ENDIF.

    SET PF-STATUS '0202' OF PROGRAM zif_dbbr_c_report_id=>main EXCLUDING lt_exclude.
    SET TITLEBAR 'SAVE_F4_CUSTOM' OF PROGRAM zif_dbbr_c_report_id=>main.
  ENDMETHOD.
ENDCLASS.
