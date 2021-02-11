FUNCTION ZDBBR_CALL_TRANSACTION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_JUMP_DEST) TYPE  ZDBBR_JUMPDEST_DATA_UI
*"----------------------------------------------------------------------
  DATA: lt_rsparams TYPE STANDARD TABLE OF rsparams.

  IF is_jump_dest-is_report_transaction = abap_true.
    DATA(lv_report_name) = CONV raldb_repo( is_jump_dest-jump_target ).
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = lv_report_name
      TABLES
        selection_table = lt_rsparams
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    LOOP AT lt_rsparams ASSIGNING FIELD-SYMBOL(<ls_rsparam>).
      ASSIGN is_jump_dest-parameters[ parameter_id = <ls_rsparam>-selname
                                      active       = abap_true            ] TO FIELD-SYMBOL(<ls_jump_param>).
      IF sy-subrc <> 0.
        DELETE lt_rsparams.
        CONTINUE.
      ENDIF.

      IF <ls_jump_param>-param_field IS NOT INITIAL.
        <ls_rsparam>-low = <ls_jump_param>-param_value.
      ELSE.
        <ls_rsparam>-low = replace( val = <ls_jump_param>-param_value sub = `'` with = space occ = 0 ).
      ENDIF.
    ENDLOOP.

    IF lt_rsparams IS NOT INITIAL.
      IF is_jump_dest-skip_1st_screen = abap_true.
        SUBMIT (is_jump_dest-program_name)
          WITH SELECTION-TABLE lt_rsparams AND RETURN.
      ELSE.
        SUBMIT (is_jump_dest-program_name)
        VIA SELECTION-SCREEN
        WITH SELECTION-TABLE lt_rsparams AND RETURN.
      ENDIF.
    ENDIF.

  ELSE. " normal transaction
    LOOP AT is_jump_dest-parameters ASSIGNING FIELD-SYMBOL(<ls_param>) WHERE active = abap_true.
      SET PARAMETER ID <ls_param>-parameter_id FIELD <ls_param>-param_value.
    ENDLOOP.

    IF is_jump_dest-skip_1st_screen = abap_true.
      CALL TRANSACTION is_jump_dest-jump_target AND SKIP FIRST SCREEN.
    ELSE.
      CALL TRANSACTION is_jump_dest-jump_target.
    ENDIF.

  ENDIF.
ENDFUNCTION.
