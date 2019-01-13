*&---------------------------------------------------------------------*
*& Report  ZDBBR_TEST_SQL_CONSOLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdbbr_test_sql_console.
FIELD-SYMBOLS: <ft_dynamic_table> type table.

NEW zcl_dbbr_sql_query_console( )->zif_uitb_view~show( ).
