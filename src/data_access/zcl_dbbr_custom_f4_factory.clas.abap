CLASS ZCL_DBBR_custom_f4_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_f4
      IMPORTING
        iv_f4_id          TYPE ZDBBR_f4_id
      EXPORTING
        es_f4_data        TYPE ZDBBR_f4_data
        et_f4_assignments TYPE ZDBBR_f4_assignment_itab.
    METHODS save_custom
      IMPORTING
        is_f4_data          TYPE ZDBBR_f4_data
        if_no_message       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_new_f4_id) TYPE ZDBBR_f4_id.
    METHODS get_f4_assignments
      IMPORTING
        iv_ref_f4_id          TYPE ZDBBR_f4_id OPTIONAL
      RETURNING
        VALUE(rt_assignments) TYPE ZDBBR_f4_assignment_itab.
    METHODS get_f4_overviews
      RETURNING
        VALUE(rt_f4_overview) TYPE ZDBBR_f4_overview_itab.
    METHODS assign_f4_to_table_field
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname
        iv_f4_id     TYPE ZDBBR_f4_id.
    METHODS delete_f4_assignments
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname.

    METHODS exists_assignment_for_tabfield
      IMPORTING
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    METHODS exists_f4_for_search_field
      IMPORTING
        iv_search_tab    TYPE tabname
        iv_search_field  TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE boolean.
    METHODS exists_built_in_f4
      IMPORTING
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
      EXPORTING
        ev_f4_id         TYPE ZDBBR_f4_id
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    METHODS find_f4_for_table
      IMPORTING
        iv_tabname TYPE tabname
      EXPORTING
        et_f4      TYPE ZDBBR_f4_data_itab.
    METHODS delete_f4_by_id
      IMPORTING
        iv_f4_id TYPE ZDBBR_f4_id.
    METHODS delete_multiple_by_id
      IMPORTING
        it_f4_id TYPE ZDBBR_SELOPT_itab.
    METHODS update_f4_assignments
      IMPORTING
        it_f4_assignments    TYPE ZDBBR_f4_assignment_itab OPTIONAL
        it_f4_assgnmt_delete TYPE ZDBBR_f4_assignment_itab OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS fill_corresponding_data
      CHANGING
        ct_f4_data TYPE ZDBBR_f4_data_itab OPTIONAL
        cs_f4_data TYPE ZDBBR_f4_data OPTIONAL.

ENDCLASS.



CLASS ZCL_DBBR_CUSTOM_F4_FACTORY IMPLEMENTATION.


  METHOD assign_f4_to_table_field.
    DATA(ls_assignment) = VALUE ZDBBR_f4assnmt( entity_id   = iv_tabname
                                                fieldname = iv_fieldname
                                                ref_f4_id = iv_f4_id     ).
    INSERT ZDBBR_f4assnmt FROM ls_assignment.

    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_f4_assignments.
    DELETE FROM ZDBBR_f4assnmt WHERE entity_id   = iv_tabname
                                 AND fieldname = iv_fieldname.
    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_f4_by_id.
    SELECT SINGLE * FROM ZDBBR_f4h INTO @DATA(ls_f4)
     WHERE f4_id = @iv_f4_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " f4 obviously exists, delete it and corresponding data
    IF ls_f4-ref_join_id IS NOT INITIAL.
      NEW ZCL_DBBR_join_factory( )->delete_join(
          iv_join_id      = ls_f4-ref_join_id
      ).
    ENDIF.

    DELETE FROM ZDBBR_f4f WHERE ref_f4_id = iv_f4_id.
    DELETE FROM ZDBBR_f4h WHERE f4_id = iv_f4_id.

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
    SELECT SINGLE ref_f4_id FROM ZDBBR_f4assnmt INTO @DATA(lv_f4_id)
      WHERE entity_id = @iv_tabname
        AND fieldname = @iv_fieldname.

    rf_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD exists_built_in_f4.
    SELECT SINGLE f4_id
    INTO @DATA(lv_f4_id)
    FROM ZDBBR_f4h AS f4_head
      INNER JOIN ZDBBR_f4f AS f4_field ON f4_head~f4_id = f4_field~ref_f4_id
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
      FROM ZDBBR_f4h AS f4_head
        INNER JOIN ZDBBR_f4f AS f4_field ON f4_head~f4_id = f4_field~ref_f4_id
        WHERE f4_field~search_table  = @iv_search_tab
          AND f4_field~search_field  = @iv_search_field
          AND f4_field~is_search_key = @abap_true.

    rf_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD fill_corresponding_data.

    DATA(lr_join_factory) = NEW ZCL_DBBR_join_factory( ).

    IF cs_f4_data IS SUPPLIED.

      IF cs_f4_data-ref_join_id IS NOT INITIAL.

        cs_f4_data-join_def = lr_join_factory->read_join(
          cs_f4_data-ref_join_id
        ).
      ENDIF.

      " select fields for f4 help
      SELECT * FROM ZDBBR_f4f INTO CORRESPONDING FIELDS OF TABLE cs_f4_data-fields
        WHERE ref_f4_id = cs_f4_data-f4_id.

    ELSEIF ct_f4_data IS SUPPLIED.

      LOOP AT ct_f4_data ASSIGNING FIELD-SYMBOL(<ls_f4>).
        IF <ls_f4>-ref_join_id IS NOT INITIAL.
          <ls_f4>-join_def = lr_join_factory->read_join(
            <ls_f4>-ref_join_id
          ).
        ENDIF.

        " select fields for f4 help
        SELECT * FROM ZDBBR_f4f INTO CORRESPONDING FIELDS OF TABLE <ls_f4>-fields
          WHERE ref_f4_id = <ls_f4>-f4_id.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD find_f4_for_table.
    SELECT assignment~entity_id as tabname,
           assignment~fieldname,
           f4_head~f4_id,
           description,
           created_by,
           is_built_in,
           ref_join_id
    FROM ZDBBR_f4assnmt AS assignment
      INNER JOIN ZDBBR_f4h AS f4_head ON assignment~ref_f4_id = f4_head~f4_id
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
    FROM ZDBBR_f4h
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
    DATA: lt_ref_f4_id_selopt TYPE RANGE OF ZDBBR_f4_id.

    lt_ref_f4_id_selopt = COND #( WHEN iv_ref_f4_id IS NOT INITIAL THEN VALUE #( ( sign = 'I' option = 'EQ' low = iv_ref_f4_id ) ) ).

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE rt_assignments
    FROM ZDBBR_f4assnmt
    WHERE ref_f4_id IN lt_ref_f4_id_selopt.
  ENDMETHOD.


  METHOD get_f4_overviews.
    SELECT f4_head~*,
           f4_fld~search_table,
           f4_fld~search_field INTO CORRESPONDING FIELDS OF TABLE @rt_f4_overview
    FROM ZDBBR_f4h AS f4_head INNER JOIN ZDBBR_f4f AS f4_fld ON f4_head~f4_id = f4_fld~ref_f4_id
    WHERE f4_fld~is_search_key = @abap_true.
  ENDMETHOD.


  METHOD save_custom.
*& Description: Adjusts/saves f4 help data for given tablename/fieldname
*&---------------------------------------------------------------------*
    DATA(ls_f4) =  CORRESPONDING ZDBBR_f4h( is_f4_data ).
    DATA(lt_f4_fields) = is_f4_data-fields.
    DATA(ls_join_def) = is_f4_data-join_def.

    IF is_f4_data-f4_id IS NOT INITIAL.
      " --- check if this f4 help still exists
      SELECT SINGLE f4_id
      FROM ZDBBR_f4h
      INTO @DATA(lv_f4_id_existing)
      WHERE f4_id = @is_f4_data-f4_id.

      IF sy-subrc <> 0.
        MESSAGE |{ 'Value help with Id'(005) } { is_f4_data-f4_id } { 'no longer exists'(006) }| TYPE 'E'.
        RETURN.
      ENDIF.

      " --- only the field properties can be updated so do that now
      MODIFY ZDBBR_f4f FROM TABLE lt_f4_fields.
    ELSE.
      ls_f4-f4_id = ZCL_DBBR_system_helper=>create_guid_22( ).

      " 1) save fields
      LOOP AT lt_f4_fields ASSIGNING FIELD-SYMBOL(<ls_f4_field>).
        <ls_f4_field>-f4_field_id = ZCL_DBBR_system_helper=>create_guid_22( ).
        <ls_f4_field>-ref_f4_id = ls_f4-f4_id.
      ENDLOOP.

      INSERT ZDBBR_f4f FROM TABLE lt_f4_fields.

      " 2) save join - if it exists
      IF ls_join_def-tables IS NOT INITIAL.
        " delete join name, description, and possible filled id
        CLEAR: ls_join_def-join_id.

        ls_f4-ref_join_id = NEW ZCL_DBBR_join_factory( )->save_join(
            is_join_def          = ls_join_def
        ).
      ENDIF.

      " 3) save the f4 help itself
      INSERT ZDBBR_f4h FROM ls_f4.
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
    DATA: lt_f4_assignments TYPE STANDARD TABLE OF ZDBBR_f4assnmt.

    lt_f4_assignments = CORRESPONDING #( it_f4_assignments ).

    INSERT ZDBBR_f4assnmt FROM TABLE lt_f4_assignments ACCEPTING DUPLICATE KEYS.
    IF sy-dbcnt > 0.
      COMMIT WORK.
    ENDIF.

    LOOP AT it_f4_assgnmt_delete ASSIGNING FIELD-SYMBOL(<ls_delete>).
      DELETE FROM ZDBBR_f4assnmt WHERE entity_id  = <ls_delete>-entity_id
                                    AND fieldname = <ls_delete>-fieldname
                                    AND ref_f4_id = <ls_delete>-ref_f4_id.
    ENDLOOP.

    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.
