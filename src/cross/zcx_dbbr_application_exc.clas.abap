"! <p class="shorttext synchronized" lang="en">Exception for application errors</p>
CLASS zcx_dbbr_application_exc DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      if_t100_message,
      zif_sat_exception_message.

    CONSTANTS:
      BEGIN OF db_entity_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '063',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF db_entity_not_existing .
    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error .
    CONSTANTS:
      BEGIN OF no_extractor_found,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_extractor_found .
    DATA msgv1 TYPE sy-msgv1 .
    DATA msgv2 TYPE sy-msgv2 .
    DATA msgv3 TYPE sy-msgv3 .
    DATA msgv4 TYPE sy-msgv4 .

    METHODS constructor
      IMPORTING
        text     TYPE string OPTIONAL
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        msgv1    TYPE sy-msgv1 OPTIONAL
        msgv2    TYPE sy-msgv2 OPTIONAL
        msgv3    TYPE sy-msgv3 OPTIONAL
        msgv4    TYPE sy-msgv4 OPTIONAL .
    METHODS show_message
      IMPORTING
        !iv_message_type LIKE sy-msgty DEFAULT 'E'
        !iv_display_type TYPE sy-msgty DEFAULT 'E' .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_dbbr_application_exc IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA: lf_fill_t100_from_sy TYPE abap_bool.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.

    IF text IS NOT INITIAL.
      lf_fill_t100_from_sy = abap_true.
      zcl_uitb_message_util=>split_string_to_symsg( text ).
    ELSEIF sy-msgid IS NOT INITIAL.
      lf_fill_t100_from_sy = abap_true.
    ELSEIF textid IS NOT INITIAL.
      if_t100_message~t100key = textid.
      me->msgv1 = msgv1.
      me->msgv2 = msgv2.
      me->msgv3 = msgv3.
      me->msgv4 = msgv4.
    ELSE.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ENDIF.

    IF lf_fill_t100_from_sy = abap_true.
      me->msgv1 = sy-msgv1.
      me->msgv2 = sy-msgv2.
      me->msgv3 = sy-msgv3.
      me->msgv4 = sy-msgv4.
      if_t100_message~t100key = VALUE #(
        msgid = sy-msgid
        msgno = sy-msgno
        attr1 = 'MSGV1'
        attr2 = 'MSGV2'
        attr3 = 'MSGV3'
        attr4 = 'MSGV4' ).
    ENDIF.

  ENDMETHOD.


  METHOD show_message.
    MESSAGE ID if_t100_message~t100key-msgid
        TYPE   iv_message_type
        NUMBER if_t100_message~t100key-msgno
        WITH   msgv1
               msgv2
               msgv3
               msgv4
        DISPLAY LIKE iv_display_type.
  ENDMETHOD.


  METHOD zif_sat_exception_message~get_message.
    result = zcl_sat_message_helper=>print_exc_message(
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


  METHOD zif_sat_exception_message~print.
    zcl_sat_message_helper=>print_exc_message(
        is_textid       = if_t100_message~t100key
        iv_display_type = iv_display_type
        if_to_screen    = if_to_screen
        iv_message_type = iv_msg_type
        ir_previous     = previous
        ir_exc_message  = me
        iv_msgv1        = msgv1
        iv_msgv2        = msgv2
        iv_msgv3        = msgv3
        iv_msgv4        = msgv4
    ).
  ENDMETHOD.
ENDCLASS.
