CLASS zcx_dbbr_dynpro_update_exc DEFINITION
  PUBLIC
  INHERITING FROM zcx_sat_nc_exception
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE sy-msgv1 OPTIONAL
        !msgv2    TYPE sy-msgv2 OPTIONAL
        !msgv3    TYPE sy-msgv3 OPTIONAL
        !msgv4    TYPE sy-msgv4 OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_dbbr_dynpro_update_exc IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
