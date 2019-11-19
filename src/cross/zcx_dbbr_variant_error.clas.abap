CLASS zcx_dbbr_variant_error DEFINITION
  PUBLIC
  INHERITING FROM ZCX_DBBR_APPLICATION_EXC
  CREATE PUBLIC .

  PUBLIC SECTION.

        CONSTANTS:
          BEGIN OF missing_params,
            msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
            msgno TYPE symsgno VALUE '056',
            attr1 TYPE scx_attrname VALUE 'MSGV1',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
          END OF missing_params .

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



CLASS zcx_dbbr_variant_error IMPLEMENTATION.


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
