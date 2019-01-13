FUNCTION CONVERSION_EXIT_TZSTL_OUTPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------
  DATA: lv_timestmp          TYPE tzntstmpl,
        lv_date              TYPE d,
        lv_time              TYPE t,
        lv_input             TYPE string,
        lv_init_timestmp(14) TYPE n,
        lv_time_zone         TYPE sy-zonlo,
        lv_output            TYPE rschavl60.

  output = input.

  lv_input = input.
  CONDENSE lv_input.
  IF NOT lv_input IS INITIAL
  AND lv_input CO '0123456789.'.
    lv_timestmp = lv_input.
    CONVERT TIME STAMP lv_timestmp TIME ZONE lv_time_zone
            INTO DATE lv_date TIME lv_time.

    IF sy-subrc = 0 OR
       sy-subrc = 4 or
       lv_input = lv_init_timestmp.
      WRITE lv_date TO lv_output.
      WRITE lv_time TO lv_output+12.
      output = lv_output.
    ELSE.
      IF 1 = 2.
      ENDIF.
    ENDIF.

  ENDIF.


ENDFUNCTION.
