CLASS zcx_dbbr_data_read_error DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcx_dbbr_application_exc.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF cds_view_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '049',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cds_view_not_existing.

    CONSTANTS:
      BEGIN OF package_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '050',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF package_not_existing.

    CONSTANTS:
      BEGIN OF db_table_view_not_existing,
        msgid TYPE symsgid VALUE 'ZDBBR_EXCEPTION',
        msgno TYPE symsgno VALUE '051',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF db_table_view_not_existing.

    CLASS-METHODS raise_data_read_error_sy
      RAISING
        zcx_dbbr_data_read_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_dbbr_data_read_error IMPLEMENTATION.
  METHOD raise_data_read_error_sy.
    RAISE EXCEPTION TYPE zcx_dbbr_data_read_error
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
  ENDMETHOD.

ENDCLASS.
