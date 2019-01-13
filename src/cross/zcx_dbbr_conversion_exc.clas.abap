CLASS ZCX_DBBR_conversion_exc DEFINITION
  PUBLIC
  INHERITING FROM ZCX_DBBR_NC_exception
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create_from_sy
      RETURNING
        VALUE(rr_exception) TYPE REF TO ZCX_DBBR_conversion_exc.
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



CLASS ZCX_DBBR_conversion_exc IMPLEMENTATION.


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

  METHOD create_from_sy.
*&---------------------------------------------------------------------*
*& Description: creates exception from current system message
*&---------------------------------------------------------------------*
    rr_exception = new ZCX_DBBR_conversion_exc(
        textid   = value scx_t100key(
            msgid = sy-msgid
            msgno = sy-msgno
            attr1 = 'MSGV1'
            attr2 = 'MSGV2'
            attr3 = 'MSGV3'
            attr4 = 'MSGV4'
        )
        msgv1    = sy-msgv1
        msgv2    = sy-msgv2
        msgv3    = sy-msgv3
        msgv4    = sy-msgv4
    ).
  ENDMETHOD.
ENDCLASS.
