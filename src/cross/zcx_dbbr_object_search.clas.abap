CLASS zcx_dbbr_object_search DEFINITION
  PUBLIC
  INHERITING FROM zcx_dbbr_application_exc
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

    CONSTANTS:
      "! Package '&1' could not be found
      BEGIN OF invalid_package,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '070',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_package .

    CONSTANTS:
      "! Search Option &1 is incomplete
      BEGIN OF option_incomplete,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '062',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF option_incomplete .
    CONSTANTS:
      "! Option &1 requires a numeric value
      BEGIN OF option_val_not_numeric,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '061',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF option_val_not_numeric .
    CONSTANTS:
      "! Value &1 is not supported for Query option &2
      BEGIN OF invalid_option_value,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '059',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_option_value .
    CONSTANTS:
      "! Option &1 does not support multiple values
      BEGIN OF no_intervals_for_option,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '058',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_intervals_for_option .
    CONSTANTS:
      "! No query string was supplied
      BEGIN OF no_query_string,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '057',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_query_string .
    CONSTANTS:
      "! Error during parsing the query
      BEGIN OF parse_error,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '053',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF parse_error.
    CONSTANTS:
      "! Parameter '&1' is not a valid search parameter
      BEGIN OF invalid_query_parameter,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '052',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_query_parameter.

    CONSTANTS:
      "! Invalid query option '&1' for search type
      BEGIN OF invalid_query_option,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '054',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_query_option.

    "! <p class="shorttext synchronized" lang="en">Raise generic object search error</p>
    "!
    "! @parameter iv_text | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_dbbr_object_search | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS raise_object_search_error
      IMPORTING
        iv_text TYPE string OPTIONAL
      RAISING
        zcx_dbbr_object_search.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_dbbr_object_search IMPLEMENTATION.


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

  METHOD raise_object_search_error.
    IF iv_text IS NOT INITIAL.
      zcl_dbbr_appl_util=>split_string_for_message(
        EXPORTING
          iv_string = iv_text
        IMPORTING
          ev_msgv1  = DATA(lv_msgv1)
          ev_msgv2  = DATA(lv_msgv2)
          ev_msgv3  = DATA(lv_msgv3)
          ev_msgv4  = DATA(lv_msgv4)
      ).

      RAISE EXCEPTION TYPE zcx_dbbr_object_search
        EXPORTING
          textid = general_error
          msgv1  = lv_msgv1
          msgv2  = lv_msgv2
          msgv3  = lv_msgv3
          msgv4  = lv_msgv4.
    ELSE.
      RAISE EXCEPTION TYPE zcx_dbbr_object_search
        EXPORTING
          textid = VALUE scx_t100key(
             msgid = sy-msgid
             msgno = sy-msgno
             attr1 = 'MSGV1'
             attr2 = 'MSGV2'
             attr3 = 'MSGV3'
             attr4 = 'MSGV4' )
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
