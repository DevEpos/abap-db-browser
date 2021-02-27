*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_executor IMPLEMENTATION.
  METHOD constructor.
    mo_query = io_query.
    mv_row_count = iv_row_count.
    mf_count_only = if_count_only.
  ENDMETHOD.

  METHOD execute_query.
    CLEAR: mf_async_finished,
           ms_query_result.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |Query is being executed...| iv_progress = 1 ).

    CALL FUNCTION 'ZDBBR_EXECUTE_SQL_QUERY' STARTING NEW TASK 'QUERY_EXEC' DESTINATION 'NONE'
      CALLING execute_query_finished ON END OF TASK
      EXPORTING
        is_query      = mo_query->ms_data
        iv_row_count  = mv_row_count
        if_count_only = mf_count_only
        it_parameters = mo_query->mt_parameters.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL mf_async_finished = abap_true.

    process_query_result( ).

    ev_line_count = ms_query_result-line_count.
    et_data_info = VALUE #(
        FOR <ls_result_col> IN ms_query_result-columns
        ( <ls_result_col>-metadata )
    ).
    ev_message = ms_query_result-message.
    ev_message_type = ms_query_result-message_severity.
    ev_execution_time = ms_query_result-query_execution_time.
    er_data = mr_query_result.
  ENDMETHOD.


  METHOD execute_query_finished.
    DATA: ls_query_result TYPE zdbbr_dp_table_data.

    RECEIVE RESULTS FROM FUNCTION 'ZDBBR_EXECUTE_SQL_QUERY'
      IMPORTING
        es_query_result = ms_query_result.

    mf_async_finished = abap_true.
  ENDMETHOD.

  METHOD process_query_result.
    DATA: lt_abap_comp_type TYPE cl_abap_structdescr=>component_table,
          lr_new_line       TYPE REF TO data,
          lr_type           TYPE REF TO cl_abap_datadescr.

    FIELD-SYMBOLS: <lt_result> TYPE table.

    CHECK: ms_query_result IS NOT INITIAL.

    " Check if an error occurred
    IF ms_query_result-message IS NOT INITIAL.
      RETURN.
    ELSE.
      CLEAR mr_query_result.

      LOOP AT ms_query_result-columns ASSIGNING FIELD-SYMBOL(<ls_column>) .
        CLEAR lr_type.
        DATA(ls_metadata) = <ls_column>-metadata.

        IF ls_metadata-rollname IS NOT INITIAL.
          lr_type = CAST #( cl_abap_typedescr=>describe_by_name( ls_metadata-rollname ) ).
        ELSE.
          TRY.
              lr_type = zcl_uitb_rtti_util=>get_elemdescr_by_kind(
                iv_type_kind = ls_metadata-typekind
                iv_length    = ls_metadata-length
                iv_decimals  = CONV #( ls_metadata-decimals ) ).
            CATCH cx_parameter_invalid_range.
              CASE ls_metadata-typekind.
                WHEN cl_abap_typedescr=>typekind_struct1.
                  lr_type = cl_abap_elemdescr=>get_string( ).
                WHEN OTHERS.
                  ms_query_result-message = |Error during type creation. Type for Tablecolumn: { ls_metadata-name } could not be determined|.
                  ms_query_result-message_severity = 'E'.
                  RETURN.
              ENDCASE.
          ENDTRY.
        ENDIF.

        lt_abap_comp_type = VALUE #(
          BASE lt_abap_comp_type
          ( name = ls_metadata-name
            type = lr_type )
        ).
      ENDLOOP.

      IF lt_abap_comp_type IS NOT INITIAL.

        TRY.
            DATA(lr_line_type) = cl_abap_structdescr=>create(
                p_components = lt_abap_comp_type
            ).
            DATA(lr_table_type) = cl_abap_tabledescr=>create(
                p_line_type = lr_line_type
            ).
            CREATE DATA mr_query_result TYPE HANDLE lr_table_type.
            ASSIGN mr_query_result->* TO <lt_result>.

*.......... fill query result table
            DO ms_query_result-line_count TIMES.
              DATA(lv_index) = sy-index.

              CREATE DATA lr_new_line TYPE HANDLE lr_line_type.
              ASSIGN lr_new_line->* TO FIELD-SYMBOL(<ls_new_line>).

              LOOP AT ms_query_result-columns ASSIGNING <ls_column>.
                ASSIGN COMPONENT <ls_column>-metadata-name OF STRUCTURE <ls_new_line> TO FIELD-SYMBOL(<lv_component>).
                " TODO: Use WRITE to write the true database value in the target field.
                "    -> The current approach ignores cases, like a database value of '' in a NUMC field
*                write <lv_column_value> to <lv_component>.
                <lv_component> = |{ <ls_column>-dataset[ lv_index ] }|.
              ENDLOOP.

              APPEND <ls_new_line> TO <lt_result>.
            ENDDO.

          CATCH cx_sy_struct_creation
                cx_sy_table_creation.
            ms_query_result-message = |Error during type creation. Preview not possible|.
            ms_query_result-message_severity = 'E'.
        ENDTRY.
      ENDIF.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
