CLASS zcl_dbbr_fe_dnd_object DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_uitb_gui_editor_dnd_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_text         TYPE string
        iv_db_id        TYPE string OPTIONAL
        if_is_long_text TYPE abap_bool OPTIONAL.
    METHODS get_db_id
      RETURNING
        VALUE(result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mf_is_long_text TYPE abap_bool.
    DATA mv_db_id TYPE string.
ENDCLASS.



CLASS zcl_dbbr_fe_dnd_object IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      iv_text         = iv_text
      if_is_long_text = if_is_long_text
    ).
    mv_db_id = iv_db_id.
  ENDMETHOD.


  METHOD get_db_id.
    result = mv_db_id.
  ENDMETHOD.

ENDCLASS.
