"! <p class="shorttext synchronized" lang="en">Duration for measurements</p>
CLASS zcl_dbbr_timer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Starts timer</p>
    METHODS start.
    "! <p class="shorttext synchronized" lang="en">Stops timer</p>
    METHODS stop.
    "! <p class="shorttext synchronized" lang="en">Retrieves duration in milliseconds</p>
    METHODS get_duration
      RETURNING
        VALUE(rv_duration_in_ms) TYPE i.
    "! <p class="shorttext synchronized" lang="en">Retrieves duration as string</p>
    METHODS get_duration_string
      RETURNING
        VALUE(rv_duration_in_ms) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_start TYPE timestampl.
    DATA mv_stop TYPE timestampl.
    DATA mv_duration TYPE i.
ENDCLASS.



CLASS zcl_dbbr_timer IMPLEMENTATION.

  METHOD get_duration.
    rv_duration_in_ms = mv_duration.
  ENDMETHOD.

  METHOD get_duration_string.
    rv_duration_in_ms = |{ mv_duration NUMBER = USER } ms|.
  ENDMETHOD.

  METHOD start.
    GET TIME STAMP FIELD mv_start.
  ENDMETHOD.

  METHOD stop.
    DATA: lv_seconds TYPE timestampl.

    GET TIME STAMP FIELD mv_stop.
    cl_abap_tstmp=>subtract(
      EXPORTING
        tstmp1                     =   mv_stop
        tstmp2                     =   mv_start
      RECEIVING
        r_secs                     =   lv_seconds
    ).
    mv_duration = lv_seconds * 1000.
  ENDMETHOD.

ENDCLASS.
