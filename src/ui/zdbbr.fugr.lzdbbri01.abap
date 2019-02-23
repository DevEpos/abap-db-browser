**********************************************************************
" Include LZLS_DATA_BROWSERIO1
**********************************************************************
MODULE cancel INPUT.
  gr_selscreen_controller->zif_uitb_screen_controller~cancel( ok_code ).
  CLEAR ok_code.
ENDMODULE.

MODULE determine_cursor_0202 INPUT.
  gr_f4_screen_controller->zif_uitb_screen_controller~determine_cursor( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  zcl_uitb_screen_util=>handle_gui_command( CHANGING cv_ok_code = ok_code ).
  gr_selscreen_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE update_selfields INPUT.
  gr_selscreen_table->zif_uitb_table~update_fields( ok_code ).
ENDMODULE.

MODULE validate_table_name INPUT.
  TRY.
      zcl_dbbr_dictionary_helper=>validate_table_name( gs_data-primary_table ).
    CATCH zcx_dbbr_validation_exception INTO DATA(lr_validation_exception).
      lr_validation_exception->show_message( ).
  ENDTRY.
ENDMODULE.

MODULE reset_flags INPUT.
  gr_selscreen_controller->reset_flags( ).
ENDMODULE.

MODULE field_f4_low INPUT.
  gr_selscreen_controller->call_f4_help( abap_true ).
ENDMODULE.

MODULE field_f4_high INPUT.
  gr_selscreen_controller->call_f4_help( abap_false ).
ENDMODULE.

MODULE check_edit_mode INPUT.
  gr_selscreen_controller->check_edit_mode( ).
ENDMODULE.

MODULE back_0202 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.

  gr_f4_screen_controller->zif_uitb_screen_controller~cancel( save_ok_code ).
ENDMODULE.

MODULE back_0101 INPUT.
  zcl_dbbr_screen_helper=>leave_screen( ).
ENDMODULE.

MODULE user_command_0202 INPUT.
  gr_f4_screen_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE user_command_0101 INPUT.
  gr_f4_screen_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE search_table_f4 INPUT.
  IF sy-dynnr = zif_dbbr_global_consts=>gc_dynpro_code-custom_built_in_f4_create.
    gr_selscreen_controller->call_table_f4(
      EXPORTING iv_dynp_field_name = 'GS_BUILT_IN_F4-SEARCH_TABLE'
      CHANGING  cv_table           = gs_built_in_f4-search_table ).
  ELSE.
    gr_selscreen_controller->call_table_f4(
      EXPORTING iv_dynp_field_name = 'GS_CUSTOM_SEARCH_HELP-TABNAME'
      CHANGING  cv_table           = gs_custom_search_help-tabname ).
  ENDIF.
ENDMODULE.

MODULE search_field_f4 INPUT.
  IF sy-dynnr = zif_dbbr_global_consts=>gc_dynpro_code-custom_built_in_f4_create.
    zcl_dbbr_dictionary_helper=>call_table_field_f4(
      EXPORTING
        iv_repid              = sy-repid
        iv_dynpname_tablename = 'GS_BUILT_IN_F4-SEARCH_TABLE'
        iv_dynpname_fieldname = 'GS_BUILT_IN_F4-SEARCH_FIELD'
      CHANGING
        cv_fieldname          = gs_built_in_f4-search_field
    ).
  ELSE.
    zcl_dbbr_dictionary_helper=>call_table_field_f4(
      EXPORTING
        iv_repid              = sy-repid
        iv_dynpname_tablename = 'GS_CUSTOM_SEARCH_HELP-TABNAME'
        iv_dynpname_fieldname = 'GS_CUSTOM_SEARCH_HELP-FIELDNAME'
      CHANGING
        cv_fieldname          = gs_custom_search_help-fieldname
    ).
  ENDIF.
ENDMODULE.

MODULE check_search_table INPUT.
  IF sy-dynnr = zif_dbbr_global_consts=>gc_dynpro_code-custom_built_in_f4_create.
    TRY.
        zcl_dbbr_dictionary_helper=>validate_table_name(
            iv_table_name               = gs_built_in_f4-search_table
            if_customizing_view_allowed = abap_false
        ).
      CATCH zcx_dbbr_validation_exception INTO lr_validation_exception.
        lr_validation_exception->show_message( ).
    ENDTRY.
  ELSE.
*    ZCL_DBBR_dictionary_helper=>validate_table_name( gs_custom_search_help-tabname ).
  ENDIF.
ENDMODULE.

MODULE check_search_field INPUT.
  IF sy-dynnr = zif_dbbr_global_consts=>gc_dynpro_code-custom_built_in_f4_create.
    TRY.
        zcl_dbbr_dictionary_helper=>validate_table_field( iv_table_field = gs_built_in_f4-search_field
                                                           iv_table_name  = gs_built_in_f4-search_table ).
      CATCH zcx_dbbr_validation_exception INTO DATA(lr_exc).
        lr_exc->show_message( iv_message_type = 'I' ).
    ENDTRY.
  ELSE.
*    ZCL_DBBR_dictionary_helper=>validate_table_field( iv_table_field = gs_custom_search_help-fieldname
*                                                 iv_table_name  = gs_custom_search_help-tabname ).
  ENDIF.
ENDMODULE.

MODULE pai_0108 INPUT.
  gr_altcoltext_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = ok_code ).
ENDMODULE.

MODULE table_pai_0108 INPUT.
  gr_altcoltext_table->zif_uitb_table~update_fields( ).
ENDMODULE.

MODULE back_0108 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_altcoltext_controller->zif_uitb_screen_controller~cancel( save_ok_code ).
ENDMODULE.

MODULE table_line_pai_0108 INPUT.
  gr_altcoltext_table->zif_uitb_table~update_fields( ).
ENDMODULE.

MODULE pai_0105 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_choose_entity_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = save_ok_code ).
ENDMODULE.

MODULE validate_input_0101 INPUT.
  gr_choose_entity_controller->validate_user_input( iv_function_code = ok_code ).
ENDMODULE.

MODULE check_entity INPUT.
  gr_selscreen_controller->check_entity( ).
ENDMODULE.

MODULE pai_0300 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_obj_brws_search_enter_ctrl->pai( CHANGING cv_function_code = save_ok_code ).
ENDMODULE.

MODULE cancel_0300 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  gr_obj_brws_search_enter_ctrl->cancel( save_ok_code ).
ENDMODULE.
