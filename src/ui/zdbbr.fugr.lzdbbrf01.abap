FORM refresh_searchfield_tc_0202.
  REFRESH CONTROL 'SEARCHFIELDS_TC' FROM SCREEN 0202.
ENDFORM.

* Context menu for low/high fields of a selection field
FORM on_ctmenu_0100_selfield USING ob_menu TYPE REF TO cl_ctmenu .
  gr_selscreen_controller->load_context_menu(
    ir_menu             = ob_menu
  ).
ENDFORM.
