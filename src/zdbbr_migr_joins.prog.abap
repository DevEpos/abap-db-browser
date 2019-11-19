*&---------------------------------------------------------------------*
*& Report  ZDBBR_MIGR_JOINS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdbbr_migr_joins.

PARAMETERS: p_test TYPE abap_bool AS CHECKBOX.

START-OF-SELECTION.
  DATA: lt_join_cond            TYPE STANDARD TABLE OF zdbbr_joinc,
        lt_join_table           TYPE STANDARD TABLE OF zdbbr_joint,
        lt_join_head            TYPE STANDARD TABLE OF zdbbr_joinh,
        lt_join_field_cond      TYPE STANDARD TABLE OF zdbbr_joinfld,
        lt_join_field_cond_all  TYPE STANDARD TABLE OF zdbbr_joinfld,
        lt_join_filter_cond     TYPE STANDARD TABLE OF zdbbr_joinfil,
        lt_join_filter_cond_all TYPE STANDARD TABLE OF zdbbr_joinfil,

        lv_migrated_tables      TYPE sy-tabix,
        lv_migrated_joins       TYPE sy-tabix.

* Select head entries for joins
  SELECT *
    FROM zdbbr_joinh
    INTO CORRESPONDING FIELDS OF TABLE @lt_join_head.

  CHECK sy-subrc = 0.

* select the join tables
  SELECT *
    FROM zdbbr_joint
    FOR ALL ENTRIES IN @lt_join_head
    WHERE ref_join_id = @lt_join_head-join_id
  INTO CORRESPONDING FIELDS of TABLE @lt_join_table.

* select the join conditions
  SELECT *
    FROM zdbbr_joinc
    FOR ALL ENTRIES IN @lt_join_head
    WHERE ref_join_id = @lt_join_head-join_id
  INTO CORRESPONDING FIELDS of TABLE @lt_join_cond.

* Start the migration
  LOOP AT lt_join_head ASSIGNING FIELD-SYMBOL(<ls_head>).
    IF p_test = abap_true.
      cl_demo_output=>write_text( |Migrated Values for Join of Primary Table { <ls_head>-primary_table }| ).
    ENDIF.

    CLEAR lv_migrated_tables.

    LOOP AT lt_join_table ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE ref_join_id = <ls_head>-join_id.

      LOOP AT lt_join_cond ASSIGNING FIELD-SYMBOL(<ls_join_cond>) WHERE ref_join_id = <ls_head>-join_id
                                                                    AND add_table   = <ls_join_table>-add_table.

        CASE <ls_join_cond>-method.
          WHEN zif_dbbr_c_join_cond_method=>reference_field.
            lt_join_field_cond = VALUE #( BASE lt_join_field_cond
              ( join_field_id     = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( )
                ref_join_table_id = <ls_join_table>-join_table_id
                ref_join_id       = <ls_head>-join_id
                field             = <ls_join_cond>-field
                ref_field         = <ls_join_cond>-value
                ref_table         = <ls_join_cond>-ref_table
                operator          = '='
                off_offset        = <ls_join_cond>-off_offset
                off_length        = <ls_join_cond>-off_length
              )
            ).

          WHEN zif_dbbr_c_join_cond_method=>constant OR
               zif_dbbr_c_join_cond_method=>system_constant.

            lt_join_filter_cond = VALUE #( BASE lt_join_filter_cond
              ( join_filter_id    = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( )
                ref_join_table_id = <ls_join_table>-join_table_id
                ref_join_id       = <ls_head>-join_id
                tabname           = <ls_join_table>-add_table
                fieldname         = <ls_join_cond>-field
                value_type        = SWITCH #( <ls_join_cond>-method
                  WHEN zif_dbbr_c_join_cond_method=>constant THEN ZIF_SAT_C_JOIN_COND_VAL_TYPE=>typed_input
                  WHEN zif_dbbr_c_join_cond_method=>system_constant THEN ZIF_SAT_C_JOIN_COND_VAL_TYPE=>system_value_input
                )
                operator          = '='
                value             = <ls_join_cond>-value
                and_or            = COND #( WHEN <ls_join_cond>-and_or IS NOT INITIAL THEN <ls_join_cond>-and_or ELSE 'AND' )
              )
            ).

          WHEN zif_dbbr_c_join_cond_method=>constant_reference_field.
            lt_join_filter_cond = VALUE #( BASE lt_join_filter_cond
              ( join_filter_id    = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( )
                ref_join_table_id = <ls_join_table>-join_table_id
                ref_join_id       = <ls_head>-join_id
                tabname           = <ls_join_cond>-ref_table
                fieldname         = <ls_join_cond>-value
                value_type        = ZIF_SAT_C_JOIN_COND_VAL_TYPE=>typed_input " no other value was possible before
                operator          = '='
                value             = <ls_join_cond>-value2
                and_or            = COND #( WHEN <ls_join_cond>-and_or IS NOT INITIAL THEN <ls_join_cond>-and_or ELSE 'AND' )
              )
            ).

        ENDCASE.

      ENDLOOP.

      IF sy-subrc = 0.
        ADD 1 TO lv_migrated_tables.
      ENDIF.

      IF lt_join_filter_cond IS NOT INITIAL.
        CLEAR: lt_join_filter_cond[ lines( lt_join_filter_cond ) ]-and_or.
      ENDIF.

      IF p_test = abap_true.
        cl_demo_output=>write_text( |Migrated Values for join Table { <ls_join_table>-add_table }| ).
        cl_demo_output=>write_text( |New Field Conditions| ).
        cl_demo_output=>write_data( lt_join_field_cond ).
        cl_demo_output=>write_text( |New Filter Conditions| ).
        cl_demo_output=>write_data( lt_join_filter_cond ).
      ENDIF.

      lt_join_field_cond_all = VALUE #( BASE lt_join_field_cond_all ( LINES OF lt_join_field_cond ) ).
      lt_join_filter_cond_all = VALUE #( BASE lt_join_filter_cond_all ( LINES OF lt_join_filter_cond ) ).
      CLEAR: lt_join_field_cond,
             lt_join_filter_cond.

    ENDLOOP.

    IF lv_migrated_tables > 0.
      ADD 1 TO lv_migrated_joins.
    ENDIF.

  ENDLOOP.

  IF p_test = abap_true.
    cl_demo_output=>display( ).
    WRITE: / |{ lv_migrated_joins } Joins were successfully migrated!|.
  ELSE.
    INSERT zdbbr_joinfil FROM TABLE lt_join_filter_cond_all.
    INSERT zdbbr_joinfld FROM TABLE lt_join_field_cond_all.
    DELETE FROM zdbbr_joinc.
    COMMIT WORK.

    WRITE: / |{ lv_migrated_joins } Joins were successfully migrated!|.
  ENDIF.
