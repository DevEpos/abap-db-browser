INTERFACE zif_dbbr_c_report_id
  PUBLIC.


  "! Id of the main program of DB Browser
  CONSTANTS main TYPE sy-repid VALUE 'SAPLZDBBR' ##NO_TEXT.
  "! Id of the program that manages the favorite menu of DB Browser
  CONSTANTS favorite_menu TYPE sy-repid VALUE 'SAPLZDBBR_FAVORITE_MENU' ##NO_TEXT.
  "! Id of the program that manages the Jump lists for DB Browser querys
  CONSTANTS jump_list_manager TYPE sy-repid VALUE 'SAPLZDBBR_FIELD_JUMP_LIST' ##NO_TEXT.
  "! Id of the report that handles the result output of DB Browser
  CONSTANTS output TYPE sy-repid VALUE 'SAPLZDBBR_OUTPUT' ##NO_TEXT.
  "! Id of the function group for user settings
  CONSTANTS user_settings TYPE sy-repid VALUE 'SAPLZDBBR_USER_SETTINGS' ##NO_TEXT.
  CONSTANTS search_help_exit TYPE sy-repid VALUE 'SAPLZDBBR_SH_EXIT' ##NO_TEXT.
ENDINTERFACE.
