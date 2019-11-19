CLASS zcx_dbbr_sql_query_error DEFINITION
  PUBLIC
  INHERITING FROM ZCX_DBBR_APPLICATION_EXC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA line_number TYPE i.
    DATA message TYPE string.
    CONSTANTS:
      BEGIN OF invalid_token_in_data_declare,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '068',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_token_in_data_declare .
    CONSTANTS:
      BEGIN OF invalid_statement,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '064',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_statement .
    CONSTANTS:
      BEGIN OF invalid_token,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '065',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_token .
    CONSTANTS:
      BEGIN OF too_many_select_stmnt,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '066',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF too_many_select_stmnt .
    CONSTANTS:
      BEGIN OF no_select_statement,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '067',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_select_statement .

    METHODS constructor
      IMPORTING
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !msgv1      TYPE sy-msgv1 OPTIONAL
        !msgv2      TYPE sy-msgv2 OPTIONAL
        !msgv3      TYPE sy-msgv3 OPTIONAL
        !msgv4      TYPE sy-msgv4 OPTIONAL
        message     TYPE string OPTIONAL
        line_number TYPE i OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Raises sql query error with system variables</p>
    "!
    CLASS-METHODS raise_from_sy
      IMPORTING
        iv_line_number TYPE i OPTIONAL
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Raises sql query error with given text</p>
    "!
    CLASS-METHODS raise_with_text
      IMPORTING
        iv_text        TYPE string
        iv_line_number TYPE i OPTIONAL
      RAISING
        zcx_dbbr_sql_query_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_dbbr_sql_query_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4.
    CLEAR me->textid.


    me->line_number = line_number.
    me->message = message.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_from_sy.
    RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
      EXPORTING
        textid      = VALUE #(
          msgid = sy-msgid
          msgno = sy-msgno
          attr1 = 'MSGV1'
          attr2 = 'MSGV2'
          attr3 = 'MSGV3'
          attr4 = 'MSGV4'
        )
        msgv1       = sy-msgv1
        msgv2       = sy-msgv2
        msgv3       = sy-msgv3
        msgv4       = sy-msgv4
        line_number = iv_line_number.
  ENDMETHOD.

  METHOD raise_with_text.
    ZCL_SAT_MESSAGE_HELPER=>split_string_for_message(
      EXPORTING
        iv_string = iv_text
      IMPORTING
        ev_msgv1  = DATA(lv_msgv1)
        ev_msgv2  = DATA(lv_msgv2)
        ev_msgv3  = DATA(lv_msgv3)
        ev_msgv4  = DATA(lv_msgv4)
    ).

    RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
      EXPORTING
        textid      = ZCX_DBBR_APPLICATION_EXC=>general_error
        msgv1       = lv_msgv1
        msgv2       = lv_msgv2
        msgv3       = lv_msgv3
        msgv4       = lv_msgv4
        message     = iv_text
        line_number = iv_line_number.
  ENDMETHOD.

ENDCLASS.
