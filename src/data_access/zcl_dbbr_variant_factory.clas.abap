CLASS zcl_dbbr_variant_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS variant_exists
      IMPORTING
        !iv_variant_name TYPE zdbbr_variant_name
        !iv_entity_id    TYPE zdbbr_entity_id OPTIONAL
        !iv_entity_type  TYPE zdbbr_entity_type OPTIONAL
      RETURNING
        VALUE(rf_exists) TYPE boolean .
    METHODS find_variants
      IMPORTING
        !iv_variant_name TYPE zdbbr_variant_name OPTIONAL
        !iv_entity_id    TYPE zdbbr_entity_id OPTIONAL
        !iv_entity_type  TYPE zdbbr_entity_type OPTIONAL
      EXPORTING
        !et_variant_info TYPE zdbbr_variant_info_itab .
    METHODS find_variants_for_query
      IMPORTING
        !iv_query_id       TYPE zdbbr_query_id
      RETURNING
        VALUE(rt_variants) TYPE zdbbr_variant_data_itab .
    METHODS find_variant_infos_for_query
      IMPORTING
        !iv_query_id       TYPE zdbbr_query_id
      RETURNING
        VALUE(rt_variants) TYPE zdbbr_variant_info_itab .
    METHODS find_variant_infos_for_type
      IMPORTING
        !iv_entity_id      TYPE zdbbr_entity_id
        !iv_entity_type    TYPE zdbbr_entity_type
      RETURNING
        VALUE(rt_variants) TYPE zdbbr_variant_info_itab .
    METHODS save_variant
      IMPORTING
        !is_var_data TYPE zdbbr_variant_data .
    METHODS save_variants
      IMPORTING
        !it_variants TYPE zdbbr_variant_data_itab .
    METHODS get_variant
      IMPORTING
        !iv_variant_id      TYPE zdbbr_variant_id OPTIONAL
        !iv_variant_name    TYPE zdbbr_variant_name OPTIONAL
        !iv_entity_id       TYPE zdbbr_entity_id OPTIONAL
        !iv_entity_type     TYPE zdbbr_entity_type OPTIONAL
        !if_load_completely TYPE boolean DEFAULT abap_true
      EXPORTING
        !es_variant         TYPE zdbbr_variant_data .
    METHODS find_default_query_variant
      IMPORTING
        !iv_query_id         TYPE zdbbr_query_id
      RETURNING
        VALUE(rv_variant_id) TYPE zdbbr_variant_id .
    METHODS delete_variant
      IMPORTING
        !iv_variant_id TYPE zdbbr_variant_id .
    METHODS delete_variants_by_query_id
      IMPORTING
        !iv_query_id TYPE zdbbr_query_id .
    METHODS variant_exists_for_entity
      IMPORTING
        !iv_entity_id    TYPE zdbbr_entity_id
        !iv_entity_type  TYPE zdbbr_entity_type
      RETURNING
        VALUE(rf_exists) TYPE boolean .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS delete_corresponding_vardata
      IMPORTING
        !iv_variant_id TYPE zdbbr_variant_id .
    METHODS fill_corresponding_data
      CHANGING
        !cs_variant TYPE zdbbr_variant_data .
ENDCLASS.



CLASS zcl_dbbr_variant_factory IMPLEMENTATION.


  METHOD delete_corresponding_vardata.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/05
*&---------------------------------------------------------------------*
*& Description: Delete corresponding data of variant
*&---------------------------------------------------------------------*

    DELETE FROM zdbbr_vardata WHERE ref_variant_id = iv_variant_id.
    DELETE FROM zdbbr_tabf WHERE ref_id = iv_variant_id.

    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_variant.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/11
*&---------------------------------------------------------------------*
*& Description: Deletes the variant with the specified name
*&---------------------------------------------------------------------*

    " delete the variants' dependent data and then the variant itself
    delete_corresponding_vardata( iv_variant_id = iv_variant_id ).

    DELETE FROM zdbbr_variant WHERE variant_id = iv_variant_id.

    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_variants_by_query_id.
    find_variants( EXPORTING iv_entity_id    = CONV #( iv_query_id )
                             iv_entity_type  = zif_dbbr_c_entity_type=>query
                   IMPORTING et_variant_info = DATA(lt_variants) ).

    LOOP AT lt_variants ASSIGNING FIELD-SYMBOL(<ls_variant>).
      delete_corresponding_vardata( iv_variant_id = <ls_variant>-variant_id ).
      DELETE FROM zdbbr_variant WHERE variant_id = <ls_variant>-variant_id.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_corresponding_data.
    " fill field information if there is any
    SELECT * FROM zdbbr_tabf INTO CORRESPONDING FIELDS OF TABLE cs_variant-fields
      WHERE ref_id = cs_variant-variant_id.

    " fill variant data information
    SELECT * FROM zdbbr_vardata INTO CORRESPONDING FIELDS OF TABLE cs_variant-variant_data
      WHERE ref_variant_id = cs_variant-variant_id.
  ENDMETHOD.


  METHOD find_default_query_variant.
    SELECT SINGLE variant_id
      FROM zdbbr_variant
      WHERE entity_id = @iv_query_id
        AND entity_type = @zif_dbbr_c_entity_type=>query
        AND variant_name = ''
      INTO @rv_variant_id.
  ENDMETHOD.


  METHOD find_variants.
    DATA(lt_entity_id_sel) = COND #( WHEN iv_entity_id  IS NOT INITIAL THEN
                                        VALUE zdbbr_selopt_itab( ( sign = 'I' option = 'EQ' low = iv_entity_id ) ) ).
    DATA(lt_entity_type_sel) = COND #( WHEN iv_entity_type IS NOT INITIAL THEN
                                         VALUE zdbbr_selopt_itab( ( sign = 'I' option = 'EQ' low = iv_entity_type ) ) ).

    IF iv_variant_name IS NOT INITIAL.
      DATA(lv_option) = COND #( WHEN contains( val = iv_variant_name sub = '*' ) THEN 'CP' ELSE 'EQ' ).
      DATA(lt_variant_selopt) = VALUE zdbbr_selopt_itab( ( sign = 'I' option = lv_option low = iv_variant_name ) ).
    ENDIF.

    SELECT * FROM zdbbr_variant INTO CORRESPONDING FIELDS OF TABLE et_variant_info
      WHERE variant_name <> space
        AND variant_name  IN lt_variant_selopt
        AND entity_type   IN lt_entity_type_sel
        AND entity_id     IN lt_entity_id_sel.
  ENDMETHOD.


  METHOD find_variants_for_query.
    SELECT * FROM zdbbr_variant INTO CORRESPONDING FIELDS OF TABLE rt_variants
      WHERE entity_id = iv_query_id
        AND entity_type = zif_dbbr_c_entity_type=>query.

    LOOP AT rt_variants ASSIGNING FIELD-SYMBOL(<ls_variant>).
      fill_corresponding_data( CHANGING cs_variant = <ls_variant> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD find_variant_infos_for_query.
    SELECT * FROM zdbbr_variant INTO CORRESPONDING FIELDS OF TABLE rt_variants
      WHERE entity_id = iv_query_id
        AND entity_type = zif_dbbr_c_entity_type=>query.
  ENDMETHOD.


  METHOD find_variant_infos_for_type.
    DATA(lv_entity_id) = iv_entity_id.

    IF iv_entity_type = zif_dbbr_c_entity_type=>query.
      NEW zcl_dbbr_query_factory( )->find_queries( EXPORTING iv_query_name = lv_entity_id IMPORTING et_queries = DATA(lt_queries) ).
      CHECK lt_queries IS NOT INITIAL AND lines( lt_queries ) = 1.
      lv_entity_id = lt_queries[ 1 ]-query_id.
    ENDIF.

    SELECT * FROM zdbbr_variant
      WHERE variant_name <> @space
        AND entity_id   = @lv_entity_id
        AND entity_type = @iv_entity_type
    INTO CORRESPONDING FIELDS OF TABLE @rt_variants.
  ENDMETHOD.


  METHOD get_variant.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/05
*&---------------------------------------------------------------------*
*& Description: Reads and returns existing variant
*&---------------------------------------------------------------------*
    IF iv_variant_id IS NOT INITIAL.
      " read variant for technical key
      SELECT SINGLE * FROM zdbbr_variant INTO CORRESPONDING FIELDS OF es_variant
        WHERE variant_id = iv_variant_id.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE. " read variant for business key
      DATA(lt_entity_id_sel) = COND #( WHEN iv_entity_id  IS NOT INITIAL THEN
                                           VALUE zdbbr_selopt_itab( ( sign = 'I' option = 'EQ' low = iv_entity_id ) ) ).
      DATA(lt_entity_type_sel) = COND #( WHEN iv_entity_type IS NOT INITIAL THEN
                                           VALUE zdbbr_selopt_itab( ( sign = 'I' option = 'EQ' low = iv_entity_type ) ) ).
      DATA(lt_variant_name_selopt) = COND #( WHEN iv_variant_name IS NOT INITIAL THEN
                                               VALUE zdbbr_selopt_itab( ( sign = 'I' option = 'EQ' low = iv_variant_name ) ) ).

      SELECT SINGLE * FROM zdbbr_variant INTO CORRESPONDING FIELDS OF es_variant
        WHERE variant_name  IN lt_variant_name_selopt
          AND entity_type IN lt_entity_type_sel
          AND entity_id   IN lt_entity_id_sel.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    " load corresponding data
    fill_corresponding_data( CHANGING cs_variant = es_variant ).
  ENDMETHOD.


  METHOD save_variant.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/05
*&---------------------------------------------------------------------*
*& Description: Saves variant information to db
*&---------------------------------------------------------------------*

    DATA(ls_variant) = CORRESPONDING zdbbr_variant( is_var_data ).
    DATA(lt_fields) = is_var_data-fields.
    DATA(lt_vardata) = is_var_data-variant_data.

    " check if there are existing entries that need to be deleted
    IF ls_variant-variant_id IS NOT INITIAL.
      delete_corresponding_vardata( ls_variant-variant_id ).
      SELECT SINGLE created_by, created_date
        FROM zdbbr_variant
        WHERE variant_id = @ls_variant-variant_id
      INTO CORRESPONDING FIELDS OF @ls_variant.
      MODIFY zdbbr_variant FROM ls_variant.
    ELSE.
      ls_variant-variant_id = zcl_dbbr_system_helper=>create_guid_22( ).
      ls_variant-created_by = sy-uname.
      ls_variant-created_date = sy-datum.
      INSERT zdbbr_variant FROM ls_variant.
    ENDIF.

    " save variant data / fields
    IF lt_fields IS NOT INITIAL.
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
        <ls_field>-ref_id = ls_variant-variant_id.
        <ls_field>-table_field_id = zcl_dbbr_system_helper=>create_guid_22( ).
      ENDLOOP.

      INSERT zdbbr_tabf FROM TABLE lt_fields.
    ENDIF.

    IF lt_vardata IS NOT INITIAL.
      LOOP AT lt_vardata ASSIGNING FIELD-SYMBOL(<ls_var_data>).
        <ls_var_data>-ref_variant_id = ls_variant-variant_id.
        <ls_var_data>-variant_data_id = zcl_dbbr_system_helper=>create_guid_22( ).
      ENDLOOP.

      INSERT zdbbr_vardata FROM TABLE lt_vardata.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.


  METHOD save_variants.
    LOOP AT it_variants ASSIGNING FIELD-SYMBOL(<ls_variant>).
      save_variant( is_var_data = <ls_variant> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD variant_exists.
*& Description: Checks if variant exists for business key
*&---------------------------------------------------------------------*

    DATA(lt_entity_id_sel) = COND #( WHEN iv_entity_id  IS NOT INITIAL THEN
                                         VALUE zdbbr_selopt_itab( ( sign = 'I' option = 'EQ' low = iv_entity_id ) ) ).

    DATA(lt_entity_type_sel) = COND #( WHEN iv_entity_type IS NOT INITIAL THEN
                                         VALUE zdbbr_selopt_itab( ( sign = 'I' option = 'EQ' low = iv_entity_type ) ) ).

    SELECT COUNT( * ) FROM zdbbr_variant INTO @DATA(lv_count)
      WHERE variant_name  =  @iv_variant_name
        AND entity_id     IN @lt_entity_id_sel
        AND entity_type   IN @lt_entity_type_sel.

    rf_exists = xsdbool( lv_count = 1 ).

  ENDMETHOD.


  METHOD variant_exists_for_entity.
    rf_exists = abap_false.
    DATA(lv_entity_id) = iv_entity_id.

    IF iv_entity_type = zif_dbbr_c_entity_type=>query.
      DATA(lr_query_f) = NEW zcl_dbbr_query_factory( ).
      DATA(ls_query) = lr_query_f->get_query(
        iv_query_name     = iv_entity_id
        if_load_completely = abap_false
      ).
      IF ls_query IS INITIAL.
        RETURN.
      ELSE.
        lv_entity_id = ls_query-query_id.
      ENDIF.
    ENDIF.

    SELECT COUNT( * ) FROM zdbbr_variant INTO @DATA(lv_count)
      WHERE variant_name <> @space
        AND entity_id     = @lv_entity_id
        AND entity_type   = @iv_entity_type.

    rf_exists = xsdbool( lv_count >= 1 ).
  ENDMETHOD.
ENDCLASS.
