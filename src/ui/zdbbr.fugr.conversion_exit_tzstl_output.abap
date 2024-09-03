FUNCTION conversion_exit_tzstl_output.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------
  DATA lv_timestmp TYPE tzntstmpl.
  DATA lv_date TYPE d.
  DATA lv_time TYPE t.
  DATA lv_input TYPE string.
  DATA lv_init_timestmp TYPE n LENGTH 14.
  DATA lv_time_zone TYPE sy-zonlo.
  DATA lv_output TYPE rschavl60.

  output = input.

  lv_input = input.
  lv_input = condense( lv_input ).
  IF     lv_input IS NOT INITIAL
     AND lv_input CO '0123456789.'.
    lv_timestmp = lv_input.
    CONVERT TIME STAMP lv_timestmp TIME ZONE lv_time_zone
            INTO DATE lv_date TIME lv_time.

    IF    sy-subrc = 0
       OR sy-subrc = 4
       OR lv_input = lv_init_timestmp.
      WRITE lv_date TO lv_output.
      WRITE lv_time TO lv_output+12.
      output = lv_output.
    ELSE.
      IF 1 = 2.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFUNCTION.
