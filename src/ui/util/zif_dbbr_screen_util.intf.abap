INTERFACE zif_dbbr_screen_util
  PUBLIC.


  METHODS get_deactivated_functions
    RETURNING
      VALUE(result) TYPE ui_functions.

  METHODS handle_ui_function
    CHANGING
      cv_function TYPE ui_func.

  METHODS handle_pbo DEFAULT IGNORE
    IMPORTING
      if_first_call TYPE abap_bool OPTIONAL.

  METHODS free_resources DEFAULT IGNORE.
ENDINTERFACE.
