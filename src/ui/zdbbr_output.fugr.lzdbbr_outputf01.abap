**********************************************************************
" General functions
**********************************************************************
FORM pai.
  gr_controller->handle_user_command( CHANGING cv_function_code = ok_code ).
ENDFORM.

FORM pbo.
*... try to instantiate controller instance from memory data
  IF gr_controller IS INITIAL.
    gr_controller = zcl_dbbr_app_starter=>start_selection_from_memory( ).
  ENDIF.

*... import was not  successful -> leave the screen
  IF gr_controller IS INITIAL.
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDIF.
  gr_controller->pbo( ).
ENDFORM.
