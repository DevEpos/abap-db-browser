FUNCTION CONVERSION_EXIT_NUM_OUTPUT.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  NUMERIC
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  DATA: text1(1) TYPE c.

  " handle zero values
  IF input IS INITIAL.
    IF gs_data-zero_val_as_blank = abap_true.
      output = ''.
    ELSE.
      output = |{ input NUMBER = USER }|.
    ENDIF.
    RETURN.
  ENDIF.

  DATA(lv_number_as_string) = |{ input NUMBER = USER }|.

  lv_number_as_string = replace( val = lv_number_as_string sub = '-' with = '' ).

  " handle negative values
  IF input < 0.
    IF gs_data-no_trailing_sign = abap_false.
      lv_number_as_string = lv_number_as_string && '-'.
    ELSE.
      lv_number_as_string = `- ` && lv_number_as_string.
    ENDIF.
  ENDIF.

  output = lv_number_as_string.

ENDFUNCTION.
