FUNCTION ZDBBR_EXECUTE_SQL_QUERY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_QUERY) TYPE  STRING OPTIONAL
*"     VALUE(IV_ROW_COUNT) TYPE  I DEFAULT 100
*"     VALUE(IF_COUNT_ONLY) TYPE  SAP_BOOL OPTIONAL
*"     VALUE(IF_SYNTAX_CHECK) TYPE  SAP_BOOL OPTIONAL
*"  EXPORTING
*"     VALUE(EV_ERROR_MESSAGE) TYPE  STRING
*"     VALUE(EV_EXECUTION_TIME) TYPE  I
*"     VALUE(ES_QUERY_RESULT) TYPE  ZDBBR_DP_TABLE_DATA
*"     VALUE(ES_CHECK_RESULT) TYPE  ZDBBR_DP_CHECK_RESULT
*"----------------------------------------------------------------------
  IF if_syntax_check = abap_false AND if_count_only = abap_false.
    TRY.
        es_query_result = zcl_dbbr_open_sql_dp_util=>execute_data_preview(
            iv_query        = iv_query
            iv_row_count    = iv_row_count
        ).
      CATCH cx_adt_datapreview_common INTO DATA(lx_preview_error).
        es_query_result-message_severity = 'E'.
        es_query_result-message = lx_preview_error->get_text( ).
    ENDTRY.
  ELSEIF if_count_only = abap_true.
    TRY.
        DATA(lv_count_query) = zcl_dbbr_open_sql_dp_util=>get_count_star_query( iv_query ).
        es_query_result-line_count = zcl_dbbr_open_sql_dp_util=>get_count_star_value( lv_count_query ).
      CATCH cx_adt_datapreview_common INTO lx_preview_error.
        es_query_result-message_severity = 'E'.
        es_query_result-message = lx_preview_error->get_text( ).
    ENDTRY.
  ELSEIF if_syntax_check = abap_true.
    TRY.
        es_check_result = zcl_dbbr_open_sql_dp_util=>execute_query_syntax_check( iv_query = iv_query ).
      CATCH cx_adt_datapreview_common.
    ENDTRY.
  ENDIF.
ENDFUNCTION.
