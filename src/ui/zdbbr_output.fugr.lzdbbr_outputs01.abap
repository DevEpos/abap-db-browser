
AT SELECTION-SCREEN.
  DATA(lv_function) = sscrfields-ucomm.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  DATA(lv_function) = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.

AT SELECTION-SCREEN OUTPUT.
