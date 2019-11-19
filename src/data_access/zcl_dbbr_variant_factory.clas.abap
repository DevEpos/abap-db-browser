"! <p class="shorttext synchronized" lang="en">Factory for variant accesses</p>
CLASS zcl_dbbr_variant_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Checks if the variant exists</p>
    CLASS-METHODS variant_exists
      IMPORTING
        !iv_variant_name TYPE zdbbr_variant_name
        !iv_entity_id    TYPE ZSAT_ENTITY_ID OPTIONAL
        !iv_entity_type  TYPE ZSAT_ENTITY_TYPE OPTIONAL
      RETURNING
        VALUE(rf_exists) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Finds variants for several criteria</p>
    CLASS-METHODS find_variants
      IMPORTING
        !iv_variant_name TYPE zdbbr_variant_name OPTIONAL
        !iv_entity_id    TYPE ZSAT_ENTITY_ID OPTIONAL
        !iv_entity_type  TYPE ZSAT_ENTITY_TYPE OPTIONAL
      EXPORTING
        !et_variant_info TYPE zdbbr_variant_info_itab .
    "! <p class="shorttext synchronized" lang="en">Finds variants for the given query id</p>
    CLASS-METHODS find_variants_for_query
      IMPORTING
        !iv_query_id       TYPE zdbbr_query_id
      RETURNING
        VALUE(rt_variants) TYPE zdbbr_variant_data_itab .
    "! <p class="shorttext synchronized" lang="en">Finds variant information for the given query id</p>
    CLASS-METHODS find_variant_infos_for_query
      IMPORTING
        !iv_query_id       TYPE zdbbr_query_id
      RETURNING
        VALUE(rt_variants) TYPE zdbbr_variant_info_itab .
    "! <p class="shorttext synchronized" lang="en">Finds variant information for the given entity/type</p>
    CLASS-METHODS find_variant_infos_for_type
      IMPORTING
        !iv_entity_id      TYPE ZSAT_ENTITY_ID
        !iv_entity_type    TYPE ZSAT_ENTITY_TYPE
      RETURNING
        VALUE(rt_variants) TYPE zdbbr_variant_info_itab .
    "! <p class="shorttext synchronized" lang="en">Saves the given variant</p>
    CLASS-METHODS save_variant
      IMPORTING
        !is_var_data TYPE zdbbr_variant_data .
    "! <p class="shorttext synchronized" lang="en">Saves the default variant of an entity</p>
    CLASS-METHODS save_default_variant
      IMPORTING
        is_var_data TYPE zdbbr_variant_data.
    "! <p class="shorttext synchronized" lang="en">Delete the default variant of an entity</p>
    CLASS-METHODS delete_default_variant
      IMPORTING
        iv_entity_id      TYPE ZSAT_ENTITY_ID
        iv_entity_type    TYPE ZSAT_ENTITY_TYPE
      RETURNING
        VALUE(rf_deleted) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Saves the auto variant of an entity</p>
    CLASS-METHODS save_auto_variant
      IMPORTING
        is_var_data TYPE zdbbr_variant_data.
    "! <p class="shorttext synchronized" lang="en">Deletes an auto variant of an entity</p>
    CLASS-METHODS delete_auto_variant
      IMPORTING
        iv_entity_id   TYPE ZSAT_ENTITY_ID
        iv_entity_type TYPE ZSAT_ENTITY_TYPE.

    "! <p class="shorttext synchronized" lang="en">Saves a list of variants</p>
    CLASS-METHODS save_variants
      IMPORTING
        !it_variants TYPE zdbbr_variant_data_itab .
    "! <p class="shorttext synchronized" lang="en">Retrieves a variant with all its stored information</p>
    CLASS-METHODS get_variant
      IMPORTING
        !iv_variant_id      TYPE zdbbr_variant_id OPTIONAL
        !iv_variant_name    TYPE zdbbr_variant_name OPTIONAL
        !iv_entity_id       TYPE ZSAT_ENTITY_ID OPTIONAL
        !iv_entity_type     TYPE ZSAT_ENTITY_TYPE OPTIONAL
        !if_load_completely TYPE boolean DEFAULT abap_true
      EXPORTING
        !es_variant         TYPE zdbbr_variant_data .
    "! <p class="shorttext synchronized" lang="en">Retrieves automatically saved variant for the given entity</p>
    CLASS-METHODS get_automatic_variant
      IMPORTING
        !iv_entity_id       TYPE ZSAT_ENTITY_ID OPTIONAL
        !iv_entity_type     TYPE ZSAT_ENTITY_TYPE OPTIONAL
        !if_load_completely TYPE boolean DEFAULT abap_true
      RETURNING
        VALUE(rs_variant)   TYPE zdbbr_variant_data .
    "! <p class="shorttext synchronized" lang="en">Retrieves default variant for the given entity</p>
    CLASS-METHODS get_default_variant
      IMPORTING
        !iv_entity_id       TYPE ZSAT_ENTITY_ID OPTIONAL
        !iv_entity_type     TYPE ZSAT_ENTITY_TYPE OPTIONAL
        !if_load_completely TYPE boolean DEFAULT abap_true
      RETURNING
        VALUE(rs_variant)   TYPE zdbbr_variant_data .
    "! <p class="shorttext synchronized" lang="en">Finds the default variant of a query</p>
    CLASS-METHODS find_default_query_variant
      IMPORTING
        !iv_query_id         TYPE zdbbr_query_id
      RETURNING
        VALUE(rv_variant_id) TYPE zdbbr_variant_id .
    "! <p class="shorttext synchronized" lang="en">Deletes the variant for the given id</p>
    CLASS-METHODS delete_variant
      IMPORTING
        !iv_variant_id TYPE zdbbr_variant_id .
    "! <p class="shorttext synchronized" lang="en">Deletes variants for the given query id</p>
    CLASS-METHODS delete_variants_by_query_id
      IMPORTING
        !iv_query_id TYPE zdbbr_query_id .
    "! <p class="shorttext synchronized" lang="en">Checks if there is variant for the given entity</p>
    CLASS-METHODS variant_exists_for_entity
      IMPORTING
        !iv_entity_id    TYPE ZSAT_ENTITY_ID
        !iv_entity_type  TYPE ZSAT_ENTITY_TYPE
      RETURNING
        VALUE(rf_exists) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Checks if there is a default variant for the given entity</p>
    CLASS-METHODS default_variant_exists
      IMPORTING
        iv_entity_id     TYPE ZSAT_ENTITY_ID
        iv_entity_type   TYPE ZSAT_ENTITY_TYPE
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Deletes dependent data of the given variant id</p>
    CLASS-METHODS delete_corresponding_vardata
      IMPORTING
        !iv_variant_id TYPE zdbbr_variant_id .
    "! <p class="shorttext synchronized" lang="en">Loads dependent data of the passed variant</p>
    CLASS-METHODS fill_corresponding_data
      CHANGING
        !cs_variant TYPE zdbbr_variant_data .
ENDCLASS.



CLASS zcl_dbbr_variant_factory IMPLEMENTATION.


  METHOD delete_corresponding_vardata.
*& Author:    stockbal     Date: 2016/12/05
*&---------------------------------------------------------------------*
*& Description: Delete corresponding data of variant
*&---------------------------------------------------------------------*

    DELETE FROM zdbbr_vardata WHERE ref_variant_id = iv_variant_id.
    DELETE FROM zdbbr_tabf WHERE ref_id = iv_variant_id.

    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_variant.
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
                             iv_entity_type  = ZIF_SAT_C_ENTITY_TYPE=>query
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
        AND entity_type = @ZIF_SAT_C_ENTITY_TYPE=>query
        AND variant_name = ''
      INTO @rv_variant_id.
  ENDMETHOD.


  METHOD find_variants.
    DATA(lt_entity_id_sel) = COND #( WHEN iv_entity_id  IS NOT INITIAL THEN
                                        VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = 'EQ' low = iv_entity_id ) ) ).
    DATA(lt_entity_type_sel) = COND #( WHEN iv_entity_type IS NOT INITIAL THEN
                                         VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = 'EQ' low = iv_entity_type ) ) ).

    IF iv_variant_name IS NOT INITIAL.
      DATA(lv_option) = COND #( WHEN contains( val = iv_variant_name sub = '*' ) THEN 'CP' ELSE 'EQ' ).
      DATA(lt_variant_selopt) = VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = lv_option low = iv_variant_name ) ).
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
        AND entity_type = ZIF_SAT_C_ENTITY_TYPE=>query.

    LOOP AT rt_variants ASSIGNING FIELD-SYMBOL(<ls_variant>).
      fill_corresponding_data( CHANGING cs_variant = <ls_variant> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD find_variant_infos_for_query.
    SELECT * FROM zdbbr_variant INTO CORRESPONDING FIELDS OF TABLE rt_variants
      WHERE entity_id = iv_query_id
        AND entity_type = ZIF_SAT_C_ENTITY_TYPE=>query.
  ENDMETHOD.


  METHOD find_variant_infos_for_type.
    DATA(lv_entity_id) = iv_entity_id.

    IF iv_entity_type = ZIF_SAT_C_ENTITY_TYPE=>query.
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

    IF iv_variant_id IS NOT INITIAL.
      " read variant for technical key
      SELECT SINGLE * FROM zdbbr_variant INTO CORRESPONDING FIELDS OF es_variant
        WHERE variant_id = iv_variant_id.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE. " read variant for business key
      DATA(lt_entity_id_sel) = COND #( WHEN iv_entity_id  IS NOT INITIAL THEN
                                           VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = 'EQ' low = iv_entity_id ) ) ).
      DATA(lt_entity_type_sel) = COND #( WHEN iv_entity_type IS NOT INITIAL THEN
                                           VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = 'EQ' low = iv_entity_type ) ) ).
      DATA(lt_variant_name_selopt) = COND #( WHEN iv_variant_name IS NOT INITIAL THEN
                                               VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = 'EQ' low = iv_variant_name ) ) ).

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

  METHOD get_automatic_variant.
    " read variant for technical key
    SELECT SINGLE *
      FROM zdbbr_variantaf
      WHERE entity_id = @iv_entity_id
        AND entity_type = @iv_entity_type
        AND created_by  = @sy-uname
    INTO CORRESPONDING FIELDS OF @rs_variant.

    CHECK sy-subrc = 0.

    " load corresponding data
    fill_corresponding_data( CHANGING cs_variant = rs_variant ).
  ENDMETHOD.

  METHOD get_default_variant.
    " read variant for technical key
    SELECT SINGLE *
      FROM zdbbr_variantd
      WHERE entity_id = @iv_entity_id
        AND entity_type = @iv_entity_type
        AND created_by = @sy-uname
    INTO CORRESPONDING FIELDS OF @rs_variant.

    CHECK: sy-subrc = 0,
           if_load_completely = abap_true.

    " load corresponding data
    fill_corresponding_data( CHANGING cs_variant = rs_variant ).
  ENDMETHOD.


  METHOD save_variant.

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
      ls_variant-variant_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
      ls_variant-created_by = sy-uname.
      ls_variant-created_date = sy-datum.
      INSERT zdbbr_variant FROM ls_variant.
    ENDIF.

    " save variant data / fields
    IF lt_fields IS NOT INITIAL.
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
        <ls_field>-ref_id = ls_variant-variant_id.
        <ls_field>-table_field_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
      ENDLOOP.

      INSERT zdbbr_tabf FROM TABLE lt_fields.
    ENDIF.

    IF lt_vardata IS NOT INITIAL.
      LOOP AT lt_vardata ASSIGNING FIELD-SYMBOL(<ls_var_data>).
        <ls_var_data>-ref_variant_id = ls_variant-variant_id.
        <ls_var_data>-variant_data_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
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

  METHOD save_default_variant.
    DATA(ls_variant) = CORRESPONDING zdbbr_variantd( is_var_data ).
    DATA(lt_fields) = is_var_data-fields.
    DATA(lt_vardata) = is_var_data-variant_data.

    DATA(ls_existing_var) = get_default_variant(
       iv_entity_id       = is_var_data-entity_id
       iv_entity_type     = is_var_data-entity_type
       if_load_completely = abap_false
    ).

*.. check if there are existing entries that need to be deleted
    IF ls_existing_var IS NOT INITIAL AND ls_existing_var-variant_id IS NOT INITIAL.
      delete_corresponding_vardata( ls_existing_var-variant_id ).
      ls_variant-variant_id = ls_existing_var-variant_id.
      ls_variant-created_by = ls_existing_var-created_by.
      ls_variant-created_date = ls_existing_var-created_date.
      MODIFY zdbbr_variantd FROM ls_variant.
    ELSE.
      ls_variant-variant_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
      ls_variant-created_by = sy-uname.
      ls_variant-created_date = sy-datum.
      INSERT zdbbr_variantd FROM ls_variant.
    ENDIF.

*.. save variant data / fields
    IF lt_fields IS NOT INITIAL.
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
        <ls_field>-ref_id = ls_variant-variant_id.
        <ls_field>-table_field_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
      ENDLOOP.

      INSERT zdbbr_tabf FROM TABLE lt_fields.
    ENDIF.

    IF lt_vardata IS NOT INITIAL.
      LOOP AT lt_vardata ASSIGNING FIELD-SYMBOL(<ls_var_data>).
        <ls_var_data>-ref_variant_id = ls_variant-variant_id.
        <ls_var_data>-variant_data_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
      ENDLOOP.

      INSERT zdbbr_vardata FROM TABLE lt_vardata.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.

  METHOD delete_default_variant.
    SELECT SINGLE variant_id
      FROM zdbbr_variantd
      WHERE entity_id = @iv_entity_id
        AND entity_type = @iv_entity_type
        AND created_by = @sy-uname
    INTO @DATA(lv_default_variant_id).

    rf_deleted = xsdbool( sy-subrc = 0 ).
    CHECK rf_deleted = abap_true.

    delete_corresponding_vardata( iv_variant_id = lv_default_variant_id ).

    DELETE FROM zdbbr_variantd WHERE variant_id = lv_default_variant_id.

    COMMIT WORK.
  ENDMETHOD.

  METHOD save_auto_variant.
    DATA(ls_variant) = CORRESPONDING zdbbr_variantaf( is_var_data ).
    DATA(lt_fields) = is_var_data-fields.
    DATA(lt_vardata) = is_var_data-variant_data.

    delete_auto_variant(
        iv_entity_id   = is_var_data-entity_id
        iv_entity_type = is_var_data-entity_type
    ).

    ls_variant-variant_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
    ls_variant-created_by = sy-uname.
    ls_variant-created_date = sy-datum.
    INSERT zdbbr_variantaf FROM ls_variant.

    IF lt_vardata IS NOT INITIAL.
      LOOP AT lt_vardata ASSIGNING FIELD-SYMBOL(<ls_var_data>).
        <ls_var_data>-ref_variant_id = ls_variant-variant_id.
        <ls_var_data>-variant_data_id = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).
      ENDLOOP.

      INSERT zdbbr_vardata FROM TABLE lt_vardata.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.

  METHOD delete_auto_variant.
    SELECT SINGLE variant_id
      FROM zdbbr_variantaf
      WHERE entity_id = @iv_entity_id
        AND entity_type = @iv_entity_type
        AND created_by = @sy-uname
    INTO @DATA(lv_auto_variant_id).

    CHECK sy-subrc = 0.

    delete_corresponding_vardata( iv_variant_id = lv_auto_variant_id ).

    DELETE FROM zdbbr_variantaf WHERE variant_id = lv_auto_variant_id.

    COMMIT WORK.
  ENDMETHOD.


  METHOD variant_exists.
*& Description: Checks if variant exists for business key
*&---------------------------------------------------------------------*

    DATA(lt_entity_id_sel) = COND #( WHEN iv_entity_id  IS NOT INITIAL THEN
                                         VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = 'EQ' low = iv_entity_id ) ) ).

    DATA(lt_entity_type_sel) = COND #( WHEN iv_entity_type IS NOT INITIAL THEN
                                         VALUE ZIF_SAT_TY_GLOBAL=>ty_t_selopt( ( sign = 'I' option = 'EQ' low = iv_entity_type ) ) ).

    SELECT COUNT( * ) FROM zdbbr_variant INTO @DATA(lv_count)
      WHERE variant_name  =  @iv_variant_name
        AND entity_id     IN @lt_entity_id_sel
        AND entity_type   IN @lt_entity_type_sel.

    rf_exists = xsdbool( lv_count = 1 ).

  ENDMETHOD.


  METHOD variant_exists_for_entity.
    rf_exists = abap_false.
    DATA(lv_entity_id) = iv_entity_id.

    IF iv_entity_type = ZIF_SAT_C_ENTITY_TYPE=>query.
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

  METHOD default_variant_exists.
    SELECT SINGLE @abap_true
      FROM zdbbr_variantd
      WHERE entity_id   = @iv_entity_id
        AND entity_type = @iv_entity_type
    INTO @rf_exists.
  ENDMETHOD.

ENDCLASS.
