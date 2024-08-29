
AT SELECTION-SCREEN.
  " TODO: variable is assigned but never used (ABAP cleaner)
  DATA(lv_function) = sscrfields-ucomm.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  " TODO: variable is assigned but never used (ABAP cleaner)
  DATA(lv_function) = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.

AT SELECTION-SCREEN OUTPUT.
