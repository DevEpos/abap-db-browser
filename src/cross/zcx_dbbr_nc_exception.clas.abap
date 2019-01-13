CLASS ZCX_DBBR_NC_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES ZIF_DBBR_exception_message.

    ALIASES print_message FOR ZIF_DBBR_exception_message~print.

    DATA msgv1 TYPE sy-msgv1 .
    DATA msgv2 TYPE sy-msgv2 .
    DATA msgv3 TYPE sy-msgv3 .
    DATA msgv4 TYPE sy-msgv4 .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE sy-msgv1 OPTIONAL
        !msgv2    TYPE sy-msgv2 OPTIONAL
        !msgv3    TYPE sy-msgv3 OPTIONAL
        !msgv4    TYPE sy-msgv4 OPTIONAL .
    METHODS show_message
      IMPORTING
        !iv_message_type LIKE sy-msgty DEFAULT 'E'
        !iv_display_type TYPE sy-msgty DEFAULT 'E' .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_DBBR_NC_EXCEPTION IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD show_message.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/07/16
*&---------------------------------------------------------------------*
*& Description: Prints message "only" from current exception
*&---------------------------------------------------------------------*
    MESSAGE ID if_t100_message~t100key-msgid
        TYPE   iv_message_type
        NUMBER if_t100_message~t100key-msgno
        WITH   msgv1
               msgv2
               msgv3
               msgv4
        DISPLAY LIKE iv_display_type.
  ENDMETHOD.


  METHOD ZIF_DBBR_exception_message~get_message.
    result = ZCL_DBBR_appl_util=>print_exc_message(
        is_textid      = if_t100_message~t100key
        if_to_screen   = abap_false
        ir_previous    = previous
        ir_exc_message = me
        iv_msgv1       = msgv1
        iv_msgv2       = msgv2
        iv_msgv3       = msgv3
        iv_msgv4       = msgv4
    ).
  ENDMETHOD.


  METHOD zif_dbbr_exception_message~print.
    rv_message = zcl_dbbr_appl_util=>print_exc_message(
        is_textid       = if_t100_message~t100key
        if_to_screen    = if_to_screen
        iv_message_type = iv_msg_type
        iv_display_type = iv_display_type
        ir_previous     = previous
        ir_exc_message  = me
        iv_msgv1        = msgv1
        iv_msgv2        = msgv2
        iv_msgv3        = msgv3
        iv_msgv4        = msgv4
    ).
  ENDMETHOD.
ENDCLASS.
