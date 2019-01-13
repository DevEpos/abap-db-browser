FUNCTION CONVERSION_EXIT_TZSTL_INPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------
  STATICS: sv_init_date(10) TYPE c,
           sv_init_time(8)  TYPE c.

  DATA: lv_data       LIKE sy-datum,
        l_time        LIKE sy-uzeit,
        lv_data_c(15) TYPE c,
        lv_time_c(15) TYPE c,
        lv_init_date  TYPE d,
        lv_init_time  TYPE t,
        lv_time_zone  TYPE sy-zonlo,
        lv_tstamp     TYPE tzntstmpl.

  IF input IS INITIAL.
    CLEAR output.
    EXIT.
  ENDIF.

  IF sv_init_date IS INITIAL.
    WRITE lv_init_date TO sv_init_date.
    WRITE lv_init_time TO sv_init_time.
  ENDIF.

  SPLIT input AT space INTO lv_data_c lv_time_c.
  CONDENSE lv_data_c.
  CONDENSE lv_time_c.

  IF lv_data_c = sv_init_date.
    CLEAR output.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
    EXPORTING
      input                     = lv_data_c
    IMPORTING
      output                    = lv_data
    EXCEPTIONS
      plausibility_check_failed = 1
      wrong_format_in_input     = 2.
  IF sy-subrc <> 0.
    IF sy-subrc = 1.
      MESSAGE s070(rsdd) DISPLAY LIKE 'E' WITH lv_data_c.
    ELSEIF sy-subrc = 2.
      MESSAGE s071(rsdd) DISPLAY LIKE 'E' WITH lv_data_c.
    ENDIF.
    lv_data = sy-datum.
  ENDIF.
  IF lv_time_c IS INITIAL.
    l_time = sy-uzeit.
  ELSE.
    CALL FUNCTION 'CONVERT_TIME_INPUT'
      EXPORTING
        input                     = lv_time_c
      IMPORTING
        output                    = l_time
      EXCEPTIONS
        plausibility_check_failed = 1
        wrong_format_in_input     = 2.
    IF sy-subrc <> 0.
      IF sy-subrc = 1.
        MESSAGE s072(rsdd) DISPLAY LIKE 'E' WITH lv_time_c.
      ELSEIF sy-subrc = 2.
        MESSAGE s073(rsdd) DISPLAY LIKE 'E' WITH lv_time_c.
      ENDIF.
      CLEAR l_time.
    ENDIF.
  ENDIF.
  CONVERT DATE lv_data TIME l_time INTO
     TIME STAMP lv_tstamp TIME ZONE lv_time_zone.

  output = lv_tstamp.

ENDFUNCTION.
