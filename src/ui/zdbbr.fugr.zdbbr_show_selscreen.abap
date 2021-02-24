FUNCTION ZDBBR_SHOW_SELSCREEN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_ENTITY_ID) TYPE  ZSAT_ENTITY_ID
*"     VALUE(IV_ENTITY_TYPE) TYPE  ZSAT_ENTITY_TYPE
*"     VALUE(IV_VARIANT_ID) TYPE  ZDBBR_VARIANT_ID OPTIONAL
*"     VALUE(IF_SKIP_SELSCREEN) TYPE  SAP_BOOL OPTIONAL
*"     VALUE(IF_LOAD_PARAMETERS) TYPE  SAP_BOOL OPTIONAL
*"     VALUE(IS_SETTINGS) TYPE  ZDBBR_SELSCREEN_SETTINGS OPTIONAL
*"     VALUE(IF_FROM_CENTRAL_SEARCH) TYPE  SAP_BOOL OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
  DATA(lv_entity_id) = iv_entity_id.
  DATA(lv_entity_type) = iv_entity_type.

  " check if RFC connection is enabled
  DATA: lv_caller_destination         TYPE rfcdisplay-rfcdest.
  CALL FUNCTION 'RFC_GET_ATTRIBUTES'
    IMPORTING
      caller_destination        = lv_caller_destination
    EXCEPTIONS
      system_call_not_supported = 1
      no_rfc_communication      = 2
      internal_error            = 3
      OTHERS                    = 4.
  IF sy-subrc = 0 AND lv_caller_destination IS NOT INITIAL.
    zcl_dbbr_screen_helper=>disable_program_quit( ).
  ENDIF.

  IF if_load_parameters = abap_true.
    gs_data-settings = zcl_dbbr_usersettings_factory=>get_settings( ).
  ENDIF.

  IF if_skip_selscreen = abap_true.
    DATA(lr_variant_starter) = zcl_dbbr_variant_starter_fac=>create_variant_starter(
        iv_variant_id        = COND #( WHEN iv_variant_id IS NOT INITIAL THEN iv_variant_id ELSE zif_dbbr_c_global=>c_dummy_variant )
        iv_entity_type       = lv_entity_type
        iv_variant_entity_id = CONV #( lv_entity_id )
    ).

    lr_variant_starter->initialize( ).
    TRY.
        DATA(lf_no_data) = lr_variant_starter->execute_variant( ).
        IF lf_no_data = abap_true.
          RAISE no_data.
        ENDIF.
      CATCH zcx_dbbr_variant_error INTO DATA(lx_variant_error).
        lx_variant_error->show_message( ).
    ENDTRY.
    RETURN.
  ENDIF.

  CLEAR: gt_selection_fields,
         gt_selection_fields_multi,

         " clear selection screen parameters
         p_varnam,
         p_vartxt.

  CASE lv_entity_type.

    WHEN ZIF_SAT_C_ENTITY_TYPE=>table.
      gs_data-primary_table = lv_entity_id.

    WHEN ZIF_SAT_C_ENTITY_TYPE=>query.
      gs_data-query_name = lv_entity_id.

    WHEN ZIF_SAT_C_ENTITY_TYPE=>cds_view.
      gs_data-primary_table = lv_entity_id.
  ENDCASE.

  gr_selscreen_table = NEW #( is_selscreen_settings = is_settings ).
  gr_selscreen_controller = NEW #(
    iv_mode                 = lv_entity_type
    ir_selection_table      = gr_selscreen_table
    if_from_central_search  = if_from_central_search
    is_settings             = is_settings
  ).

  IF gs_data-called_from_adt = abap_true.
    gr_selscreen_controller->load_entity(
        if_fill_history = abap_true
        iv_entity_id    = lv_entity_id
        iv_entity_type  = lv_entity_type
    ).
  ELSE.
    gr_selscreen_controller->load_entity( if_fill_history = abap_true ). "gs_data-called_from_adt ).
  ENDIF.

* Information that DB Browser was opened from ADT is no longer needed at this point, so
* it will be cleared to avoid any side effects
  CLEAR gs_data-called_from_adt.

  gr_selscreen_controller->zif_uitb_screen_controller~call_screen( ).

  CLEAR gr_selscreen_controller.
ENDFUNCTION.
