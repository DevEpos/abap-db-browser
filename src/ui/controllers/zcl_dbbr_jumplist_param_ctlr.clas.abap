CLASS zcl_dbbr_jumplist_param_ctlr DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_uitb_screen_controller.

    METHODS constructor
      IMPORTING
        ir_table       TYPE REF TO zcl_dbbr_jumplist_param_table
        iv_transaction TYPE tcode.

    METHODS get_parameters
      RETURNING
        VALUE(result) TYPE zdbbr_jumpparam_data_ui_itab.

  PRIVATE SECTION.
    ALIASES get_report_id FOR zif_uitb_screen_controller~get_report_id.
    ALIASES get_screen_id FOR zif_uitb_screen_controller~get_screen_id.

    CONSTANTS:
      BEGIN OF mc_function_codes,
        select_all_fields   TYPE sy-ucomm VALUE 'SELECT_ALL',
        unselect_all_fields TYPE sy-ucomm VALUE 'UNSLCT_ALL',
        create_param        TYPE sy-ucomm VALUE 'CREATE',
        delete_params       TYPE sy-ucomm VALUE 'DELETE',
        take_values         TYPE sy-ucomm VALUE 'ENTER',
      END OF mc_function_codes.

    DATA mr_table TYPE REF TO zcl_dbbr_jumplist_param_table.
    DATA mv_transaction TYPE tcode.
    DATA mf_was_changed TYPE boolean.
ENDCLASS.


CLASS zcl_dbbr_jumplist_param_ctlr IMPLEMENTATION.
  METHOD constructor.
    mr_table = ir_table.
    mv_transaction = iv_transaction.
  ENDMETHOD.

  METHOD get_parameters.
    result = CAST zdbbr_jumpparam_data_ui_itab( mr_table->zif_uitb_table~get_table_data( ) )->*.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen( iv_screen_id    = get_screen_id( )
                                       iv_report_id    = get_report_id( )
                                       it_object_map   = VALUE #(
                                           ( variable_name = zif_dbbr_jumplist_var_ids=>c_r_jumplist_param_controller
                                             global_ref    = me )
                                           ( variable_name = zif_dbbr_jumplist_var_ids=>c_r_jumplist_param_table
                                             global_ref    = mr_table ) )
                                       iv_start_column = 10
                                       iv_start_line   = 2 ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~cancel.
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~determine_cursor.
    zcl_uitb_cursor=>get_cursor( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>jump_list_manager.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_jump_list_parameters.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~handle_user_command.
    DATA(lv_function) = cv_function_code.
    CLEAR cv_function_code.

    CASE lv_function.

      WHEN mc_function_codes-select_all_fields.

      WHEN mc_function_codes-unselect_all_fields.

      WHEN mc_function_codes-create_param.
        mr_table->zif_uitb_table~add_line( ).

      WHEN mc_function_codes-delete_params.
        mr_table->delete_selected_params( ).

      WHEN mc_function_codes-take_values.
        mf_was_changed = abap_true.
        zcl_dbbr_screen_helper=>leave_screen( ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).

    zcl_uitb_cursor=>refresh_cursor( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~set_status.
    SET PF-STATUS 'JUMP_PARAM_STATUS' OF PROGRAM zif_dbbr_c_report_id=>jump_list_manager.
    SET TITLEBAR 'JUMP_PARAM' OF PROGRAM zif_dbbr_c_report_id=>jump_list_manager WITH mv_transaction.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_was_changed.
  ENDMETHOD.
ENDCLASS.
