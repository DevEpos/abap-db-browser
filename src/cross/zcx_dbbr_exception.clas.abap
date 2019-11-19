CLASS zcx_dbbr_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_sat_nc_exception
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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
      BEGIN OF query_global_name_error,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_global_name_error .
    CONSTANTS:
      BEGIN OF query_not_global_name_error,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_not_global_name_error .
    CONSTANTS:
      BEGIN OF query_self_copy_impossible,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_self_copy_impossible .
    CONSTANTS:
      BEGIN OF query_overwrite_not_possible,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_overwrite_not_possible .
    CONSTANTS:
      BEGIN OF query_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_not_existing .
    CONSTANTS:
      BEGIN OF query_no_authority,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_no_authority .
    CONSTANTS:
      BEGIN OF no_query_selected,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_query_selected .
    CONSTANTS:
      BEGIN OF more_than_one_query_selected,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF more_than_one_query_selected .
    CONSTANTS:
      BEGIN OF cancelled_by_user,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cancelled_by_user .
    CONSTANTS:
      BEGIN OF query_already_exists,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_already_exists .
    CONSTANTS:
      BEGIN OF icon_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '033',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF icon_not_existing .

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



CLASS zcx_dbbr_exception IMPLEMENTATION.


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
