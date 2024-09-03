FUNCTION conversion_exit_num_input.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  NUMERIC
*"----------------------------------------------------------------------

  " handle zero values
  IF input = space.
    RETURN.
  ENDIF.

  input = replace( val  = input
                   sub  = '.'
                   with = '' ).
  input = replace( val  = input
                   sub  = ','
                   with = '.' ).

  IF contains( val = input
               sub = '-' ).
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lf_is_negative) = abap_true.
    input = replace( val  = input
                     sub  = '-'
                     with = '' ).
    input = condense( val = input ).
    input = |{ input }-|.
  ENDIF.

  output = input.
ENDFUNCTION.
