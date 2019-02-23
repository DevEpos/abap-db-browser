CLASS zcl_dbbr_copy_query_ctrl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    ALIASES show
      FOR zif_uitb_screen_controller~call_screen .
    ALIASES was_copied
      FOR zif_uitb_screen_controller~was_not_cancelled .

    METHODS constructor
      IMPORTING
        !is_query_info TYPE zdbbr_query_info_ui .
    METHODS get_new_query
      RETURNING
        VALUE(rs_new_query) TYPE zdbbr_query_info_ui .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    DATA mf_new_query_created TYPE boolean .
*   DATA mr_ui_global_data     TYPE REF TO ZDBBR_global_data.
    DATA ms_new_query TYPE zdbbr_query_info_ui .
    DATA mr_ui_query_name TYPE REF TO zdbbr_query_name .
    DATA mr_ui_query_desc TYPE REF TO ddtext .
    DATA mr_ui_is_global TYPE REF TO boolean .
    DATA mr_ui_query_name_target TYPE REF TO zdbbr_query_name .
    DATA mr_ui_query_desc_target TYPE REF TO ddtext .
    DATA mr_ui_is_global_target TYPE REF TO boolean .
    DATA mr_ui_copy_variants TYPE REF TO boolean .
    DATA mv_query_id TYPE zdbbr_query_id .

    METHODS create_copy .
ENDCLASS.



CLASS zcl_dbbr_copy_query_ctrl IMPLEMENTATION.


  METHOD constructor.

    mv_query_id = is_query_info-query_id.

    " init some global data references from ui
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_ui_is_global = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_xglob ) ).
    mr_ui_copy_variants = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_copyv ) ).
    mr_ui_is_global_target = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_xglobt ) ).
    mr_ui_query_name = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_scrnam ) ).
    mr_ui_query_desc = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_scrdec ) ).
    mr_ui_query_name_target = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_scrnt ) ).
    mr_ui_query_desc_target = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_scrdtg ) ).

    mr_ui_query_name->* = mr_ui_query_name_target->* = is_query_info-query_name.
    mr_ui_query_desc->* = mr_ui_query_desc_target->* = is_query_info-description.
    mr_ui_is_global->* = mr_ui_is_global_target->* = is_query_info-is_global.
  ENDMETHOD.


  METHOD create_copy.
    DATA(lr_query_copier) = NEW zcl_dbbr_query_copier(
        iv_src_query_name = mr_ui_query_name->*
        iv_src_query_desc = mr_ui_query_desc->*
        iv_src_query_id   = mv_query_id
        iv_trg_query_name = mr_ui_query_name_target->*
        iv_trg_query_desc = mr_ui_query_desc_target->*
        if_trg_is_global   = mr_ui_is_global_target->*
        if_copy_variants   = mr_ui_copy_variants->*
     ).

    TRY .
        ms_new_query = lr_query_copier->copy_query( ).
        " copy successful
        mf_new_query_created = abap_true.
        zcl_dbbr_screen_helper=>leave_screen( ).
      CATCH zcx_dbbr_exception INTO DATA(lr_exception).
        lr_exception->show_message(
            iv_message_type = 'S'
        ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_new_query.
    rs_new_query = ms_new_query.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_copy_query_controller
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
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_copy_query.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
    DATA(lv_function_code) = cv_function_code.
    CLEAR cv_function_code.

    CASE lv_function_code.
      WHEN 'ENTER'.
        create_copy( ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).

    LOOP AT SCREEN INTO DATA(ls_screen).
      IF ls_screen-name = zif_dbbr_main_report_var_ids=>c_p_scrnam OR
         ls_screen-name = zif_dbbr_main_report_var_ids=>c_p_scrdec OR
         ls_screen-name = zif_dbbr_main_report_var_ids=>c_p_xglob.
        ls_screen-input = 0.
        MODIFY SCREEN FROM ls_screen.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    zcl_uitb_screen_util=>set_selscreen_status(
        iv_status = '0600'
        iv_repid  = zif_dbbr_c_report_id=>main
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_new_query_created.
  ENDMETHOD.
ENDCLASS.
