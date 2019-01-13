CLASS ZCL_DBBR_fe_dnd_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_text         TYPE string
        iv_db_id type string optional
        if_is_long_text TYPE abap_bool OPTIONAL.

    methods get_db_id
    returning
    value(result) type string.
    METHODS get_text
      RETURNING
        VALUE(result) TYPE string.
    METHODS is_long_text
      RETURNING
        VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_text TYPE string.
    DATA mt_text TYPE string.
    DATA mf_is_long_text TYPE abap_bool.
    DATA mv_db_id TYPE string.
ENDCLASS.



CLASS ZCL_DBBR_FE_DND_OBJECT IMPLEMENTATION.


  METHOD constructor.
    mv_text = iv_text.
    mv_db_id = iv_db_id.
    mf_is_long_text = if_is_long_text.
  ENDMETHOD.


  METHOD get_text.
      result = mv_text.
  ENDMETHOD.


  METHOD is_long_text.
    result = mf_is_long_text.
  ENDMETHOD.

  METHOD get_db_id.
    result = mv_db_id.
  ENDMETHOD.

ENDCLASS.
