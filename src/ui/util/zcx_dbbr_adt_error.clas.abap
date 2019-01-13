class ZCX_DBBR_ADT_ERROR definition
  public
  inheriting from ZCX_DBBR_APPLICATION_EXC
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SY-MSGV1 optional
      !MSGV2 type SY-MSGV2 optional
      !MSGV3 type SY-MSGV3 optional
      !MSGV4 type SY-MSGV4 optional .
  class-methods RAISE_ADT_ERROR_FROM_SY
    raising
      ZCX_DBBR_ADT_ERROR .
  class-methods RAISE_ADT_ERROR_WITH_TEXT
    importing
      !IV_TEXT type STRING
    raising
      ZCX_DBBR_ADT_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DBBR_ADT_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise_adt_error_from_sy.
    RAISE EXCEPTION TYPE zcx_dbbr_adt_error
      EXPORTING
        textid         = VALUE scx_t100key(
           msgid = sy-msgid
           msgno = sy-msgno
           attr1 = 'ATTR1'
           attr2 = 'ATTR2'
           attr3 = 'ATTR3'
           attr4 = 'ATTR4' )
        msgv1          = sy-msgv1
        msgv2          = sy-msgv2
        msgv3          = sy-msgv3
        msgv4          = sy-msgv4.
  ENDMETHOD.


  METHOD raise_adt_error_with_text.
    zcl_dbbr_appl_util=>split_string_for_message(
      EXPORTING
        iv_string = iv_text
      IMPORTING
        ev_msgv1  = DATA(lv_msgv1)
        ev_msgv2  = DATA(lv_msgv2)
        ev_msgv3  = DATA(lv_msgv3)
        ev_msgv4  = DATA(lv_msgv4)
    ).

    RAISE EXCEPTION TYPE zcx_dbbr_adt_error
      EXPORTING
        textid         = general_error
        msgv1          = lv_msgv1
        msgv2          = lv_msgv2
        msgv3          = lv_msgv3
        msgv4          = lv_msgv4.
  ENDMETHOD.
ENDCLASS.
