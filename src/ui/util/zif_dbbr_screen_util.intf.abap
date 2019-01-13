interface ZIF_DBBR_SCREEN_UTIL
  public .


  methods GET_DEACTIVATED_FUNCTIONS
    returning
      value(RESULT) type UI_FUNCTIONS .
  methods HANDLE_UI_FUNCTION
    changing
      !CV_FUNCTION type UI_FUNC .
  methods HANDLE_PBO default ignore
    importing
      !IF_FIRST_CALL type ABAP_BOOL optional .
  methods FREE_RESOURCES default ignore .
endinterface.
