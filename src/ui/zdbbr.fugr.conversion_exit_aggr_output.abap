FUNCTION CONVERSION_EXIT_AGGR_OUTPUT.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------
  DATA: lt_dd07v TYPE TABLE OF dd07v.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name      = 'ZDBBR_AGGR'
      langu     = sy-langu
    TABLES
      dd07v_tab = lt_dd07v.

  output = VALUE #( lt_dd07v[ domvalue_l = input ]-ddtext OPTIONAL ).


ENDFUNCTION.
