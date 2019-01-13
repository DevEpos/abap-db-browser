class ZCX_DBBR_EXCEPTION definition
  public
  inheriting from ZCX_DBBR_NC_EXCEPTION
  final
  create public .

public section.

  constants:
    BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error .
  constants:
    BEGIN OF query_global_name_error,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_global_name_error .
  constants:
    BEGIN OF query_not_global_name_error,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_not_global_name_error .
  constants:
    BEGIN OF query_self_copy_impossible,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_self_copy_impossible .
  constants:
    BEGIN OF query_overwrite_not_possible,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_overwrite_not_possible .
  constants:
    BEGIN OF query_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_not_existing .
  constants:
    BEGIN OF query_no_authority,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_no_authority .
  constants:
    BEGIN OF no_query_selected,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_query_selected .
  constants:
    BEGIN OF more_than_one_query_selected,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF more_than_one_query_selected .
  constants:
    BEGIN OF cancelled_by_user,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cancelled_by_user .
  constants:
    BEGIN OF query_already_exists,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_already_exists .
  constants:
    begin of ICON_NOT_EXISTING,
      msgid type symsgid value 'ZDBBR_EXCEPTION',
      msgno type symsgno value '033',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ICON_NOT_EXISTING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SY-MSGV1 optional
      !MSGV2 type SY-MSGV2 optional
      !MSGV3 type SY-MSGV3 optional
      !MSGV4 type SY-MSGV4 optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_DBBR_EXCEPTION IMPLEMENTATION.


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
