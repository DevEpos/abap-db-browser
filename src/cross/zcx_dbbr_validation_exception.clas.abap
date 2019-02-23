class ZCX_DBBR_VALIDATION_EXCEPTION definition
  public
  inheriting from ZCX_DBBR_NC_EXCEPTION
  create public .

public section.

  constants:
    BEGIN OF package_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '041',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF package_not_existing .
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
    BEGIN OF no_mapping_defined,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_mapping_defined .
  constants:
    BEGIN OF no_field_specified,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_field_specified .
  constants:
    BEGIN OF field_exists_not_in_table,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_exists_not_in_table .
  constants:
    BEGIN OF field_does_not_exists,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_does_not_exists .
  constants:
    BEGIN OF mandatory_fields_empty,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mandatory_fields_empty .
  constants:
    BEGIN OF duplicate_entries_exist,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF duplicate_entries_exist .
  constants:
    BEGIN OF field_mismatch,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_mismatch .
  constants:
    begin of PARAMETER_VALUE_MISSING,
      msgid type symsgid value 'ZDBBR_EXCEPTION',
      msgno type symsgno value '047',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PARAMETER_VALUE_MISSING .
  data PARAMETER_NAME type DYNFNAM read-only .
  data LOOP_LINE type SY-TABIX read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SY-MSGV1 optional
      !MSGV2 type SY-MSGV2 optional
      !MSGV3 type SY-MSGV3 optional
      !MSGV4 type SY-MSGV4 optional
      !PARAMETER_NAME type DYNFNAM optional
      !LOOP_LINE type SY-TABIX optional .
  class-methods RAISE_FROM_SY
    importing
      !IV_PARAMETER type DYNFNAM optional
      !IV_LINE type I optional .
  class-methods RAISE_WITH_TEXT
    importing
      !IV_TEXT type STRING
      !IV_PARAMETER type DYNFNAM optional
      !IV_LINE type I optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DBBR_VALIDATION_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
me->PARAMETER_NAME = PARAMETER_NAME .
me->LOOP_LINE = LOOP_LINE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise_from_sy.
    RAISE EXCEPTION TYPE zcx_dbbr_validation_exception
      EXPORTING
        textid         = VALUE scx_t100key(
           msgid = sy-msgid
           msgno = sy-msgno
           attr1 = 'MSGV1'
           attr2 = 'MSGV2'
           attr3 = 'MSGV3'
           attr4 = 'MSGV4' )
        msgv1          = sy-msgv1
        msgv2          = sy-msgv2
        msgv3          = sy-msgv3
        msgv4          = sy-msgv4
        parameter_name = iv_parameter
        loop_line      = iv_line.
  ENDMETHOD.


  METHOD raise_with_text.
    zcl_dbbr_appl_util=>split_string_for_message(
      EXPORTING
        iv_string = iv_text
      IMPORTING
        ev_msgv1  = DATA(lv_msgv1)
        ev_msgv2  = DATA(lv_msgv2)
        ev_msgv3  = DATA(lv_msgv3)
        ev_msgv4  = DATA(lv_msgv4)
    ).

    RAISE EXCEPTION TYPE zcx_dbbr_validation_exception
      EXPORTING
        textid         = general_error
        msgv1          = lv_msgv1
        msgv2          = lv_msgv2
        msgv3          = lv_msgv3
        msgv4          = lv_msgv4
        parameter_name = iv_parameter
        loop_line      = iv_line.

  ENDMETHOD.
ENDCLASS.
