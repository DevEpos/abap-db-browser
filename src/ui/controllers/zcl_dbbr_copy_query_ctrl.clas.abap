class ZCL_DBBR_COPY_query_CTRL definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_SCREEN_CONTROLLER .

  aliases SHOW
    for ZIF_UITB_SCREEN_CONTROLLER~CALL_SCREEN .
  aliases WAS_COPIED
    for ZIF_UITB_SCREEN_CONTROLLER~WAS_NOT_CANCELLED .

  methods CONSTRUCTOR
    importing
      !IS_query_INFO type ZDBBR_query_INFO_UI .
  methods GET_NEW_query
    returning
      value(RS_NEW_query) type ZDBBR_query_INFO_UI .
  PROTECTED SECTION.
private section.

  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

  data MF_NEW_query_CREATED type BOOLEAN .
*   DATA mr_ui_global_data     TYPE REF TO ZDBBR_global_data.
  data MS_NEW_query type ZDBBR_query_INFO_UI .
  data MR_UI_query_NAME type ref to ZDBBR_query_NAME .
  data MR_UI_query_DESC type ref to DDTEXT .
  data MR_UI_IS_GLOBAL type ref to BOOLEAN .
  data MR_UI_query_NAME_TARGET type ref to ZDBBR_query_NAME .
  data MR_UI_query_DESC_TARGET type ref to DDTEXT .
  data MR_UI_IS_GLOBAL_TARGET type ref to BOOLEAN .
  data MR_UI_COPY_VARIANTS type ref to BOOLEAN .
  data MV_query_ID type ZDBBR_query_ID .

  methods CREATE_COPY .
ENDCLASS.



CLASS ZCL_DBBR_COPY_query_CTRL IMPLEMENTATION.


  METHOD constructor.

    mv_query_id = is_query_info-query_id.

    " init some global data references from ui
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

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
    DATA(lr_query_copier) = NEW ZCL_DBBR_query_copier(
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
        ZCL_DBBR_screen_helper=>leave_screen( ).
      CATCH ZCX_DBBR_exception INTO DATA(lr_exception).
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


  METHOD ZIF_UITB_SCREEN_CONTROLLER~cancel.
    ZCL_DBBR_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_copy_query.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~handle_user_command.
    DATA(lv_function_code) = cv_function_code.
    CLEAR cv_function_code.

    CASE lv_function_code.
      WHEN 'ENTER'.
        create_copy( ).
    ENDCASE.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~pbo.
    ZIF_UITB_SCREEN_CONTROLLER~set_status( ).

    LOOP AT SCREEN INTO DATA(ls_screen).
      IF ls_screen-name = ZIF_DBBR_main_report_var_ids=>c_p_scrnam OR
         ls_screen-name = ZIF_DBBR_main_report_var_ids=>c_p_scrdec OR
         ls_screen-name = ZIF_DBBR_main_report_var_ids=>c_p_xglob.
        ls_screen-input = 0.
        MODIFY screen FROM ls_screen.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~set_status.
    DATA: lt_excl TYPE TABLE OF sy-ucomm.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '0600'
      TABLES
        p_exclude = lt_excl.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~was_not_cancelled.
    rf_not_cancelled = mf_new_query_created.
  ENDMETHOD.
ENDCLASS.
