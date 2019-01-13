FUNCTION CONVERSION_EXIT_NUM_INPUT.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  NUMERIC
*"----------------------------------------------------------------------
  DATA: text1(1) TYPE c.

  " handle zero values
  IF input = space.
    RETURN.
  ENDIF.


  input = replace( val = input sub = '.' with = '' ).
  input = replace( val = input sub = ',' with = '.' ).


  if contains( val = input sub = '-' ).
    data(lf_is_negative) = abap_true.
    input = replace( val = input sub = '-' with = '' ).
    input = condense( val = input ).
    input = input && '-'.
  endif.

  output = input.
ENDFUNCTION.
