MODULE query_f4_0101 INPUT.
  gs_favmode-query_name = ZCL_DBBR_query_helper=>call_query_f4( iv_repid = sy-repid ).
ENDMODULE.

MODULE tab_f4_0101 INPUT.
  DATA(lv_tabname) = CONV tabname( gs_favmode-tab_name ).

  ZCL_DBBR_dictionary_helper=>call_table_f4(
    EXPORTING
      iv_repid           = sy-repid
      iv_dynp_field_name = 'GS_FAVMODE-TAB_NAME'
    CHANGING
      cv_table           = lv_tabname
  ).

  gs_favmode-tab_name = lv_tabname.
ENDMODULE.
