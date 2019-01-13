interface ZIF_DBBR_EXCEPTION_MESSAGE
  public .


  methods PRINT
    importing
      !IV_MSG_TYPE type SY-MSGTY default 'S'
      !IV_DISPLAY_TYPE type SY-MSGTY default 'E'
      !IF_TO_SCREEN type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_MESSAGE) type STRING .
  methods GET_MESSAGE
    returning
      value(RESULT) type STRING .
endinterface.
