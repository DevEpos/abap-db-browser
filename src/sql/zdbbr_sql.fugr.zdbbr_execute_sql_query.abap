FUNCTION ZDBBR_EXECUTE_SQL_QUERY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_QUERY) TYPE  ZDBBR_SQL_QUERY
*"     VALUE(IT_PARAMETERS) TYPE  ZDBBR_QUERY_PARAMETER_T OPTIONAL
*"     VALUE(IV_ROW_COUNT) TYPE  I DEFAULT 100
*"     VALUE(IF_COUNT_ONLY) TYPE  SAP_BOOL OPTIONAL
*"  EXPORTING
*"     VALUE(ES_QUERY_RESULT) TYPE  ZDBBR_DP_TABLE_DATA
*"----------------------------------------------------------------------
  DATA(lo_proxy_executor) = NEW zcl_dbbr_sql_query_exec_proxy(
    is_query      = is_query
    it_parameters = it_parameters
  ).

  TRY.
      es_query_result = lo_proxy_executor->execute_select(
          iv_row_count = iv_row_count
      ).
    CATCH zcx_dbbr_sql_query_error INTO DATA(lx_sql_error).
      es_query_result-message = lx_sql_error->zif_sat_exception_message~get_message( ).
      es_query_result-message_severity = 'E'.
  ENDTRY.
ENDFUNCTION.
