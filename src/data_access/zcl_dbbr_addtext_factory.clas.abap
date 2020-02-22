CLASS zcl_dbbr_addtext_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS exists_for_bus_key
      IMPORTING
        iv_id_table       TYPE tabname
        iv_id_field1      TYPE fieldname
        iv_id_field2      TYPE fieldname
        iv_cond_field     TYPE fieldname
        iv_cond_value     TYPE zdbbr_addtext_cond_val
        iv_cond_operation TYPE zdbbr_addtext_cond_operation
      RETURNING
        VALUE(result)     TYPE abap_bool.
    METHODS find_add_texts
      IMPORTING
        !iv_id_table  TYPE tabname OPTIONAL
        !iv_id_field  TYPE fieldname OPTIONAL
      EXPORTING
        !et_add_texts TYPE zdbbr_addtext_itab .
    METHODS find_add_texts_for_tablist
      IMPORTING
        !it_tabname_selopt TYPE zif_sat_ty_global=>ty_t_selopt
      EXPORTING
        !et_addtext        TYPE zdbbr_addtext_itab .
    METHODS add_text_exists
      IMPORTING
        !iv_id_table     TYPE tabname
        !iv_id_field     TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE boolean .
    METHODS get_add_texts
      IMPORTING
        !iv_id_table       TYPE tabname
        !iv_id_field       TYPE fieldname
      RETURNING
        VALUE(rt_add_text) TYPE zdbbr_addtext_itab .
    METHODS save_add_text
      CHANGING
        !cs_addtext_data TYPE zdbbr_addtext .
    METHODS delete_add_text_by_id
      IMPORTING
        !is_addtext_k TYPE zdbbr_addtext_k .
    METHODS exists
      IMPORTING
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE boolean.
    METHODS get_all
      RETURNING
        VALUE(result) TYPE zdbbr_addtext_itab.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_addtext_factory IMPLEMENTATION.


  METHOD add_text_exists.
  ENDMETHOD.


  METHOD delete_add_text_by_id.
    DELETE FROM zdbbr_addtext WHERE addtext_id = is_addtext_k-addtext_id.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD find_add_texts.

    DATA(lt_id_field_selopt) = zcl_dbbr_appl_util=>build_selopt( iv_id_field ).
    DATA(lt_id_table_selopt) = zcl_dbbr_appl_util=>build_selopt( iv_id_table ).

    SELECT * FROM zdbbr_addtext INTO TABLE et_add_texts
      WHERE id_table IN lt_id_table_selopt
        AND id_field IN lt_id_field_selopt.
  ENDMETHOD.


  METHOD find_add_texts_for_tablist.
    SELECT * FROM zdbbr_addtext INTO TABLE et_addtext
      WHERE id_table IN it_tabname_selopt.
  ENDMETHOD.


  METHOD get_add_texts.
    SELECT * FROM zdbbr_addtext INTO CORRESPONDING FIELDS OF TABLE rt_add_text
      WHERE id_table = iv_id_table
        AND id_field = iv_id_field.
  ENDMETHOD.


  METHOD save_add_text.
*&---------------------------------------------------------------------*
*& Author: stockbal     Date: 2017/02/07
*&---------------------------------------------------------------------*
*& Description: Saves given addtext definition to db
*&---------------------------------------------------------------------*

    IF cs_addtext_data-addtext_id IS INITIAL.
      DATA(lf_new_entry) = abap_true.
      cs_addtext_data-addtext_id = zcl_sat_system_helper=>create_guid_22( ).
      INSERT zdbbr_addtext FROM cs_addtext_data.
    ELSE.
      MODIFY zdbbr_addtext FROM cs_addtext_data.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.

  METHOD exists.
    SELECT COUNT( * ) INTO @DATA(lv_count)
     FROM zdbbr_addtext
        WHERE id_table = @iv_tabname
          AND id_field = @iv_fieldname.

    rf_exists = xsdbool( lv_count > 0 ).
  ENDMETHOD.


  METHOD get_all.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE result
     FROM zdbbr_addtext.
  ENDMETHOD.

  METHOD exists_for_bus_key.
    SELECT SINGLE addtext_id INTO @DATA(lv_id)
     FROM zdbbr_addtext
     WHERE id_table = @iv_id_table
       AND id_field = @iv_id_field1
       AND id_field2 = @iv_id_field2
       AND condition_field = @iv_cond_field
       AND condition_op = @iv_cond_operation
       AND condition_value = @iv_cond_value.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

ENDCLASS.
