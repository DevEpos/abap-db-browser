CLASS zcl_dbbr_custom_f4_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_f4
      IMPORTING
        iv_f4_id          TYPE zdbbr_f4_id
      EXPORTING
        es_f4_data        TYPE zdbbr_f4_data
        et_f4_assignments TYPE zdbbr_f4_assignment_itab.
    CLASS-METHODS save_custom
      IMPORTING
        is_f4_data          TYPE zdbbr_f4_data
        if_no_message       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_new_f4_id) TYPE zdbbr_f4_id.
    CLASS-METHODS get_f4_assignments
      IMPORTING
        iv_ref_f4_id          TYPE zdbbr_f4_id OPTIONAL
      RETURNING
        VALUE(rt_assignments) TYPE zdbbr_f4_assignment_itab.
    CLASS-METHODS get_f4_overviews
      RETURNING
        VALUE(rt_f4_overview) TYPE zdbbr_f4_overview_itab.
    CLASS-METHODS assign_f4_to_table_field
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname
        iv_f4_id     TYPE zdbbr_f4_id.
    CLASS-METHODS delete_f4_assignments
      IMPORTING
        iv_tabname        TYPE tabname
        iv_fieldname      TYPE fieldname
      RETURNING
        VALUE(rf_deleted) TYPE abap_bool.

    CLASS-METHODS exists_assignment_for_tabfield
      IMPORTING
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    CLASS-METHODS exists_f4_for_search_field
      IMPORTING
        iv_search_tab    TYPE tabname
        iv_search_field  TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE boolean.
    CLASS-METHODS exists_built_in_f4
      IMPORTING
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
      EXPORTING
        ev_f4_id         TYPE zdbbr_f4_id
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    CLASS-METHODS find_f4_for_table
      IMPORTING
        iv_tabname TYPE tabname
      EXPORTING
        et_f4      TYPE zdbbr_f4_data_itab.
    CLASS-METHODS find_f4_for_datatype
      IMPORTING
        iv_rollname           TYPE rollname OPTIONAL
        if_apply_to_same_data TYPE abap_bool OPTIONAL
        is_built_in_type      TYPE zdbbr_built_in_data_type OPTIONAL
      EXPORTING
        et_f4                 TYPE zdbbr_f4_data_itab.        .
    CLASS-METHODS delete_f4_by_id
      IMPORTING
        iv_f4_id TYPE zdbbr_f4_id.
    CLASS-METHODS delete_multiple_by_id
      IMPORTING
        it_f4_id TYPE zdbbr_selopt_itab.
    CLASS-METHODS update_f4_assignments
      IMPORTING
        it_f4_assignments    TYPE zdbbr_f4_assignment_itab OPTIONAL
        it_f4_assgnmt_delete TYPE zdbbr_f4_assignment_itab OPTIONAL
      RETURNING
        VALUE(rf_updated)    TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS fill_corresponding_data
      CHANGING
        ct_f4_data TYPE zdbbr_f4_data_itab OPTIONAL
        cs_f4_data TYPE zdbbr_f4_data OPTIONAL.

ENDCLASS.



CLASS zcl_dbbr_custom_f4_factory IMPLEMENTATION.


  METHOD assign_f4_to_table_field.
    DATA(ls_assignment) = VALUE zdbbr_f4assnmt( entity_id   = iv_tabname
                                                fieldname = iv_fieldname
                                                ref_f4_id = iv_f4_id     ).
    INSERT zdbbr_f4assnmt FROM ls_assignment.

    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_f4_assignments.
    DELETE FROM zdbbr_f4assnmt WHERE entity_id   = iv_tabname
                                 AND fieldname = iv_fieldname.
    IF sy-subrc = 0 AND sy-dbcnt > 0.
      COMMIT WORK.
      rf_deleted = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD delete_f4_by_id.
    SELECT SINGLE * FROM zdbbr_f4h INTO @DATA(ls_f4)
     WHERE f4_id = @iv_f4_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " f4 obviously exists, delete it and corresponding data
    IF ls_f4-ref_join_id IS NOT INITIAL.
      NEW zcl_dbbr_join_factory( )->delete_join(
          iv_join_id      = ls_f4-ref_join_id
      ).
    ENDIF.

    DELETE FROM zdbbr_f4f WHERE ref_f4_id = iv_f4_id.
    DELETE FROM zdbbr_f4h WHERE f4_id = iv_f4_id.

    COMMIT WORK.

  ENDMETHOD.


  METHOD delete_multiple_by_id.
    CHECK it_f4_id IS NOT INITIAL.

    SELECT * FROM zdbbr_f4h INTO TABLE @DATA(lt_f4)
     WHERE f4_id IN @it_f4_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " collect joins to delete
    DATA(lt_join_id) = VALUE zdbbr_selopt_itab(
       FOR f4 IN lt_f4
       WHERE ( ref_join_id IS NOT INITIAL )
       ( sign   = 'I'
         option = 'EQ'
         low    = f4-ref_join_id )
    ).

    " delete joins
    NEW zcl_dbbr_join_factory( )->delete_multiple_joins( lt_join_id ).

    DELETE FROM zdbbr_f4f WHERE ref_f4_id IN it_f4_id.
    DELETE FROM zdbbr_f4h WHERE f4_id IN it_f4_id.
    DELETE FROM zdbbr_f4assnmt WHERE ref_f4_id IN it_f4_id.

    COMMIT WORK.
  ENDMETHOD.


  METHOD exists_assignment_for_tabfield.
    SELECT SINGLE ref_f4_id FROM zdbbr_f4assnmt INTO @DATA(lv_f4_id)
      WHERE entity_id = @iv_tabname
        AND fieldname = @iv_fieldname.

    rf_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD exists_built_in_f4.
    SELECT SINGLE f4_id
    INTO @DATA(lv_f4_id)
    FROM zdbbr_f4h AS f4_head
      INNER JOIN zdbbr_f4f AS f4_field ON f4_head~f4_id = f4_field~ref_f4_id
      WHERE f4_field~search_table  = @iv_tabname
        AND f4_field~search_field  = @iv_fieldname
        AND f4_field~is_search_key = @abap_true
        AND is_built_in            = @abap_true.

    rf_exists = xsdbool( sy-subrc = 0 ).
    ev_f4_id = lv_f4_id.
  ENDMETHOD.


  METHOD exists_f4_for_search_field.
    SELECT SINGLE f4_id
      INTO @DATA(lv_f4_id)
      FROM zdbbr_f4h AS f4_head
        INNER JOIN zdbbr_f4f AS f4_field ON f4_head~f4_id = f4_field~ref_f4_id
        WHERE f4_field~search_table  = @iv_search_tab
          AND f4_field~search_field  = @iv_search_field
          AND f4_field~is_search_key = @abap_true.

    rf_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD fill_corresponding_data.

    DATA(lr_join_factory) = NEW zcl_dbbr_join_factory( ).

    IF cs_f4_data IS SUPPLIED.

      IF cs_f4_data-ref_join_id IS NOT INITIAL.

        cs_f4_data-join_def = lr_join_factory->read_join(
          cs_f4_data-ref_join_id
        ).
      ENDIF.

      " select fields for f4 help
      SELECT * FROM zdbbr_f4f INTO CORRESPONDING FIELDS OF TABLE cs_f4_data-fields
        WHERE ref_f4_id = cs_f4_data-f4_id.

    ELSEIF ct_f4_data IS SUPPLIED.

      LOOP AT ct_f4_data ASSIGNING FIELD-SYMBOL(<ls_f4>).
        IF <ls_f4>-ref_join_id IS NOT INITIAL.
          <ls_f4>-join_def = lr_join_factory->read_join(
            <ls_f4>-ref_join_id
          ).
        ENDIF.

        " select fields for f4 help
        SELECT * FROM zdbbr_f4f INTO CORRESPONDING FIELDS OF TABLE <ls_f4>-fields
          WHERE ref_f4_id = <ls_f4>-f4_id.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD find_f4_for_datatype.
    DATA: lt_rollname      TYPE RANGE OF rollname,
          lt_datatype      TYPE RANGE OF datatype_d,
          lt_inttype       TYPE RANGE OF inttype,
          lt_apply_to_same TYPE RANGE OF abap_bool,
          lt_length        TYPE RANGE OF ddleng.

    IF if_apply_to_same_data IS SUPPLIED.
      lt_apply_to_same = VALUE #( ( sign = 'I' option = 'EQ' low = if_apply_to_same_data ) ).
    ENDIF.

    DATA(lt_from) = VALUE string_table(
      ( |zdbbr_f4f AS f4_field | )
      ( |INNER JOIN zdbbr_f4h AS f4_head ON  f4_field~ref_f4_id = f4_head~f4_id | )
      ( |                                AND f4_field~is_search_key = @abap_true | )
    ).

    DATA(lt_select) = VALUE string_table(
       ( |f4_head~f4_id,| )
       ( |f4_head~description,| )
       ( |f4_head~created_by,| )
       ( |f4_head~is_built_in,| )
       ( |f4_head~ref_join_id,| )
       ( |f4_head~apply_to_same_type,| )
       ( |f4_head~perform_alpha_conversion| )
    ).

    IF iv_rollname IS NOT INITIAL.
      SELECT DISTINCT (lt_select)
        FROM (lt_from)
        WHERE f4_head~apply_to_same_type IN @lt_apply_to_same
          AND f4_field~rollname = @iv_rollname
      APPENDING CORRESPONDING FIELDS OF TABLE @et_f4.
    ELSEIF is_built_in_type-datatype IS NOT INITIAL AND is_built_in_type-leng IS NOT INITIAL.
      SELECT DISTINCT (lt_select)
        FROM (lt_from)
        WHERE f4_head~apply_to_same_type IN @lt_apply_to_same
          AND f4_field~datatype = @is_built_in_type-datatype
          AND f4_field~leng     = @is_built_in_type-leng
      APPENDING CORRESPONDING FIELDS OF TABLE @et_f4.
    ELSEIF is_built_in_type-datatype IS NOT INITIAL.
      SELECT DISTINCT (lt_select)
        FROM (lt_from)
        WHERE f4_head~apply_to_same_type IN @lt_apply_to_same
          AND f4_field~datatype = @is_built_in_type-datatype
      APPENDING CORRESPONDING FIELDS OF TABLE @et_f4.
    ENDIF.

    SORT et_f4 BY f4_id.
    DELETE ADJACENT DUPLICATES FROM et_f4 COMPARING f4_id.
    CHECK et_f4 IS NOT INITIAL.

    fill_corresponding_data( CHANGING ct_f4_data = et_f4 ).
  ENDMETHOD.

  METHOD find_f4_for_table.
    SELECT assignment~entity_id AS tabname,
           assignment~fieldname,
           assignment~perform_alpha_conversion AS perform_alpha_conv_assgmt,
           f4_head~perform_alpha_conversion,
           f4_head~f4_id,
           description,
           created_by,
           is_built_in,
           ref_join_id
    FROM zdbbr_f4assnmt AS assignment
      INNER JOIN zdbbr_f4h AS f4_head ON assignment~ref_f4_id = f4_head~f4_id
    INTO CORRESPONDING FIELDS OF TABLE @et_f4
    WHERE entity_id = @iv_tabname.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " find corresponding entries for f4
    fill_corresponding_data( CHANGING ct_f4_data = et_f4 ).
  ENDMETHOD.


  METHOD get_f4.
    SELECT SINGLE *
    FROM zdbbr_f4h
    INTO CORRESPONDING FIELDS OF es_f4_data
    WHERE f4_id = iv_f4_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " get fields for value help
    fill_corresponding_data( CHANGING cs_f4_data = es_f4_data ).

    " get assignments
    et_f4_assignments = get_f4_assignments( iv_f4_id ).
  ENDMETHOD.


  METHOD get_f4_assignments.
    DATA: lt_ref_f4_id_selopt TYPE RANGE OF zdbbr_f4_id.

    lt_ref_f4_id_selopt = COND #( WHEN iv_ref_f4_id IS NOT INITIAL THEN VALUE #( ( sign = 'I' option = 'EQ' low = iv_ref_f4_id ) ) ).

    SELECT *
      FROM zdbbr_f4assnmt
      WHERE ref_f4_id IN @lt_ref_f4_id_selopt
    INTO CORRESPONDING FIELDS OF TABLE @rt_assignments.
  ENDMETHOD.


  METHOD get_f4_overviews.
    SELECT f4_head~*,
           f4_fld~search_table,
           f4_fld~search_field INTO CORRESPONDING FIELDS OF TABLE @rt_f4_overview
    FROM zdbbr_f4h AS f4_head INNER JOIN zdbbr_f4f AS f4_fld ON f4_head~f4_id = f4_fld~ref_f4_id
    WHERE f4_fld~is_search_key = @abap_true.
  ENDMETHOD.


  METHOD save_custom.
*& Description: Adjusts/saves f4 help data for given tablename/fieldname
*&---------------------------------------------------------------------*
    DATA(ls_f4) =  CORRESPONDING zdbbr_f4h( is_f4_data ).
    DATA(lt_f4_fields) = is_f4_data-fields.
    DATA(ls_join_def) = is_f4_data-join_def.

    IF is_f4_data-f4_id IS NOT INITIAL.
      " --- check if this f4 help still exists
      SELECT SINGLE f4_id
      FROM zdbbr_f4h
      INTO @DATA(lv_f4_id_existing)
      WHERE f4_id = @is_f4_data-f4_id.

      IF sy-subrc <> 0.
        MESSAGE |{ 'Value help with Id'(005) } { is_f4_data-f4_id } { 'no longer exists'(006) }| TYPE 'E'.
        RETURN.
      ENDIF.

      " --- only the field properties can be updated so do that now
      MODIFY zdbbr_f4f FROM TABLE lt_f4_fields.
      MODIFY zdbbr_f4h FROM ls_f4.
    ELSE.
      ls_f4-f4_id = zcl_dbbr_system_helper=>create_guid_22( ).

      " 1) save fields
      LOOP AT lt_f4_fields ASSIGNING FIELD-SYMBOL(<ls_f4_field>).
        <ls_f4_field>-f4_field_id = zcl_dbbr_system_helper=>create_guid_22( ).
        <ls_f4_field>-ref_f4_id = ls_f4-f4_id.
      ENDLOOP.

      INSERT zdbbr_f4f FROM TABLE lt_f4_fields.

      " 2) save join - if it exists
      IF ls_join_def-tables IS NOT INITIAL.
        " delete join name, description, and possible filled id
        CLEAR: ls_join_def-join_id.

        ls_f4-ref_join_id = NEW zcl_dbbr_join_factory( )->save_join(
            is_join_def          = ls_join_def
        ).
      ENDIF.

      " 3) save the f4 help itself
      INSERT zdbbr_f4h FROM ls_f4.
    ENDIF.

    COMMIT WORK.

    IF if_no_message = abap_false.
      MESSAGE |{ 'Value help'(003) } '{ is_f4_data-description }' { 'was'(004) } | &&
              COND string( WHEN is_f4_data-f4_id IS NOT INITIAL THEN 'updated'(002) ELSE 'saved'(001) ) &&
              |!|
              TYPE 'S'.
    ENDIF.

    rv_new_f4_id = ls_f4-f4_id.
  ENDMETHOD.


  METHOD update_f4_assignments.
    DATA: lt_f4_assgmt TYPE STANDARD TABLE OF zdbbr_f4assnmt.

    lt_f4_assgmt = CORRESPONDING #( it_f4_assignments ).

*    INSERT zdbbr_f4assnmt FROM TABLE lt_f4_assgmt_insert ACCEPTING DUPLICATE KEYS.
    MODIFY zdbbr_f4assnmt FROM TABLE lt_f4_assgmt.
    IF sy-dbcnt > 0.
      rf_updated = abap_true.
      COMMIT WORK.
    ENDIF.

    LOOP AT it_f4_assgnmt_delete ASSIGNING FIELD-SYMBOL(<ls_delete>).
      DELETE FROM zdbbr_f4assnmt WHERE entity_id  = <ls_delete>-entity_id
                                    AND fieldname = <ls_delete>-fieldname
                                    AND ref_f4_id = <ls_delete>-ref_f4_id.
      IF sy-dbcnt > 0.
        rf_updated = abap_true.
        COMMIT WORK.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
