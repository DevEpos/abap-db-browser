interface ZIF_DBBR_C_REPORT_ID
  public .


  "! Id of the main program of DB Browser
  constants MAIN type SY-REPID value 'SAPLZDBBR' ##NO_TEXT.
  "! Id of the program that manages the favorite menu of DB Browser
  constants FAVORITE_MENU type SY-REPID value 'SAPLZDBBR_FAVORITE_MENU' ##NO_TEXT.
  "! Id of the program that manages the Jump lists for DB Browser querys
  constants JUMP_LIST_MANAGER type SY-REPID value 'SAPLZDBBR_FIELD_JUMP_LIST' ##NO_TEXT.
  "! Id of the report that handles the result output of DB Browser
  constants OUTPUT type SY-REPID value 'SAPLZDBBR_OUTPUT' ##NO_TEXT.
  "! Id of the function group for user settings
  constants USER_SETTINGS type SY-REPID value 'SAPLZDBBR_USER_SETTINGS' ##NO_TEXT.
  constants SEARCH_HELP_EXIT type SY-REPID value 'SAPLZDBBR_SH_EXIT' ##NO_TEXT.
endinterface.
