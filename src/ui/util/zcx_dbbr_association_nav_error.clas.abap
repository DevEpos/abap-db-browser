CLASS zcx_dbbr_association_nav_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_dbbr_application_exc FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF max_navigation_level_reached,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '048',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF max_navigation_level_reached.

    CONSTANTS:
      BEGIN OF assoc_param_not_available,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '071',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF assoc_param_not_available.

    METHODS constructor
      IMPORTING
        textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous                 OPTIONAL
        msgv1     TYPE sy-msgv1                 OPTIONAL
        msgv2     TYPE sy-msgv2                 OPTIONAL
        msgv3     TYPE sy-msgv3                 OPTIONAL
        msgv4     TYPE sy-msgv4                 OPTIONAL.

    CLASS-METHODS raise_association_nav_error
      IMPORTING
        iv_text TYPE string OPTIONAL
      RAISING
        zcx_dbbr_association_nav_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_dbbr_association_nav_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous
                        msgv1    = msgv1
                        msgv2    = msgv2
                        msgv3    = msgv3
                        msgv4    = msgv4 ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_association_nav_error.
    IF iv_text IS NOT INITIAL.
      zcl_sat_message_helper=>split_string_for_message( EXPORTING iv_string = iv_text
                                                        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                                                                  ev_msgv2  = DATA(lv_msgv2)
                                                                  ev_msgv3  = DATA(lv_msgv3)
                                                                  ev_msgv4  = DATA(lv_msgv4) ).

      RAISE EXCEPTION TYPE zcx_dbbr_association_nav_error
        EXPORTING textid = general_error
                  msgv1  = lv_msgv1
                  msgv2  = lv_msgv2
                  msgv3  = lv_msgv3
                  msgv4  = lv_msgv4.
    ELSE.
      RAISE EXCEPTION TYPE zcx_dbbr_association_nav_error
        EXPORTING textid = VALUE scx_t100key( msgid = sy-msgid
                                              msgno = sy-msgno
                                              attr1 = 'ATTR1'
                                              attr2 = 'ATTR2'
                                              attr3 = 'ATTR3'
                                              attr4 = 'ATTR4' )
                  msgv1  = sy-msgv1
                  msgv2  = sy-msgv2
                  msgv3  = sy-msgv3
                  msgv4  = sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
