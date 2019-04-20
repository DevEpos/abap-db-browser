CLASS zcl_dbbr_sql_query_exec_proxy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_query      TYPE zdbbr_sql_query
        it_parameters TYPE zdbbr_query_parameter_t.
    "! <p class="shorttext synchronized" lang="en">Execute Sql SELECT</p>
    "!
    METHODS execute_select
      IMPORTING
        iv_row_count           TYPE i DEFAULT 100
      RETURNING
        VALUE(rs_table_result) TYPE zdbbr_dp_table_data
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Execute Sql COUNT</p>
    "!
    METHODS execute_count
      RETURNING
        VALUE(rs_table_result) TYPE zdbbr_dp_table_data
      RAISING
        zcx_dbbr_sql_query_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_parameter TYPE zdbbr_query_parameter_t.
    DATA ms_query TYPE zdbbr_sql_query.

    "! <p class="shorttext synchronized" lang="en">Create subroutine for sql statment</p>
    "!
    METHODS create_subroutine_code
      IMPORTING
        iv_row_count      TYPE i OPTIONAL
        if_count_only     TYPE abap_bool OPTIONAL
      EXPORTING
        VALUE(ev_program) TYPE program
        VALUE(ev_message) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Check and correct the query result</p>
    "!
    CLASS-METHODS check_and_correct_result
      IMPORTING
        ir_query_result TYPE REF TO data
      CHANGING
        cs_result       TYPE zdbbr_dp_table_data
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Remove/adjust NULL values</p>
    "!
    CLASS-METHODS remove_null
      IMPORTING
        !i_lv_col_value TYPE string
      RETURNING
        VALUE(r_result) TYPE string.
ENDCLASS.



CLASS zcl_dbbr_sql_query_exec_proxy IMPLEMENTATION.

  METHOD constructor.
    ms_query = is_query.
    mt_parameter = it_parameters.
  ENDMETHOD.

  METHOD execute_count.

  ENDMETHOD.

  METHOD execute_select.
    DATA: lr_query_result TYPE REF TO data,
          lv_time1        TYPE timestampl,
          lv_time2        TYPE timestampl,
          lv_duration     TYPE timestampl.

    create_subroutine_code(
      EXPORTING
        iv_row_count = iv_row_count
      IMPORTING
        ev_program   = DATA(lv_program)
        ev_message   = rs_table_result-message
    ).

    IF rs_table_result-message IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_class) = |\\PROGRAM={ lv_program }\\CLASS=MAIN|.
    GET TIME STAMP FIELD lv_time1.
    IF ms_query-is_single_result_query = abap_true.
      CALL METHOD (lv_class)=>execute RECEIVING rv_count = rs_table_result-line_count.
    ELSE.
      CALL METHOD (lv_class)=>execute RECEIVING rr_result = lr_query_result .
    ENDIF.
    GET TIME STAMP FIELD lv_time2.

    cl_abap_tstmp=>subtract(
      EXPORTING
        tstmp1                     =   lv_time2
        tstmp2                     =   lv_time1
      RECEIVING
        r_secs                     =   lv_duration
    ).
    lv_duration  = lv_duration  * 1000.
    rs_table_result-query_execution_time = |{ lv_duration NUMBER = USER } ms|.

    IF ms_query-is_single_result_query = abap_false.
      check_and_correct_result(
        EXPORTING ir_query_result = lr_query_result
        CHANGING  cs_result       = rs_table_result
      ).
    ENDIF.
  ENDMETHOD.

  METHOD create_subroutine_code.
    DATA: lt_lines      TYPE string_table,
          lt_lines_temp TYPE string_table.

    lt_lines = VALUE #(
        ( |REPORT ZSQL_QUERY.| )
        ( )
        ( |CLASS main DEFINITION.| )
        ( |  PUBLIC SECTION.| )
        ( |    CLASS-METHODS execute| )
        ( |      RETURNING| )
    ).
    IF ms_query-is_single_result_query = abap_true.
      lt_lines = VALUE #( BASE lt_lines
          ( |        VALUE(rv_count) TYPE ZDBBR_NO_OF_LINES| )
      ).
    ELSE.
      lt_lines = VALUE #( BASE lt_lines
          ( |        VALUE(rr_result) TYPE REF TO data| )
      ).
    ENDIF.
    lt_lines = VALUE #( BASE lt_lines
        ( |      RAISING| )
        ( |        ZCX_DBBR_SQL_QUERY_ERROR.| )
        ( |ENDCLASS.| )
        ( )
        ( |CLASS main IMPLEMENTATION.| )
        ( |  METHOD execute.| )
        ( )
    ).

*.. Add parameters with their chosen values
    LOOP AT mt_parameter ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      DATA(lv_param_line) = |    DATA { <ls_parameter>-name } TYPE|.

      IF <ls_parameter>-type IS NOT INITIAL.
        IF <ls_parameter>-is_range = abap_true.
          lv_param_line = |{ lv_param_line } RANGE OF { <ls_parameter>-type }|.
        ELSE.
          lv_param_line = |{ lv_param_line } { <ls_parameter>-type }|.
        ENDIF.
      ELSE.
        lv_param_line = |{ lv_param_line } { <ls_parameter>-inttype } LENGTH { <ls_parameter>-length }|.
        IF <ls_parameter>-decimals > 0.
          lv_param_line = |{ lv_param_line } DECIMALS { <ls_parameter>-decimals }|.
        ENDIF.
      ENDIF.

*.... Fill the value for the parameter
      IF <ls_parameter>-value IS NOT INITIAL.
        lv_param_line = |{ lv_param_line } VALUE '{ <ls_parameter>-value }'|.
      ELSEIF <ls_parameter>-default_value_raw IS NOT INITIAL.
        lv_param_line = |{ lv_param_line } VALUE { <ls_parameter>-default_value_raw }|.
      ENDIF.
      lv_param_line = |{ lv_param_line }.|.
      lt_lines = VALUE #( BASE lt_lines
        ( lv_param_line )
      ).
    ENDLOOP.

    lt_lines = VALUE #( BASE lt_lines ( ) ).
    CLEAR lt_lines_temp.

*.. Fill range parameters with values
    LOOP AT mt_parameter ASSIGNING <ls_parameter> WHERE value_list IS NOT INITIAL.
      lt_lines = VALUE #( BASE lt_lines ( |    { <ls_parameter>-name } = VALUE #( | ) ).

*      clear lt_lines_temp
      LOOP AT <ls_parameter>-value_list ASSIGNING FIELD-SYMBOL(<ls_range_value>).

        DATA(lv_line) = |      ( sign = '{ <ls_range_value>-sign }' option = '{ <ls_range_value>-option }' low = '{ <ls_range_value>-low }'|.
        IF <ls_range_value>-high IS NOT INITIAL.
          lv_line = |{ lv_line } high = '{ <ls_range_value>-high }'|.
        ENDIF.
        lv_line = |{ lv_line } )|.

        lt_lines = VALUE #( BASE lt_lines ( lv_line ) ).
      ENDLOOP.

      lt_lines = VALUE #( BASE lt_lines ( |{ lv_line } ).| ) ).
    ENDLOOP.

    lt_lines = VALUE #( BASE lt_lines ( ) ).

*.. Add lines of the select statement
    SPLIT ms_query-select_source AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines_temp.

    ASSIGN lt_lines_temp[ ms_query-last_row_in_select_stmnt ] TO FIELD-SYMBOL(<lv_last_query_line>).
    IF ms_query-is_single_result_query = abap_true.
      <lv_last_query_line> = |{ <lv_last_query_line>(ms_query-last_row_offset) } INTO @rv_count|.
    ELSE.
      <lv_last_query_line> = |{ <lv_last_query_line>(ms_query-last_row_offset) } INTO TABLE @DATA(result)|.
    ENDIF.
    IF  iv_row_count IS SUPPLIED AND
        iv_row_count > 0 AND
        ms_query-is_single_result_query = abap_false AND
        ms_query-main_select_stmnt_type <> zcl_dbbr_sql_query_parser=>c_keywords-union.
      <lv_last_query_line> = |{ <lv_last_query_line> } UP TO { iv_row_count } ROWS.|.
    ELSE.
      <lv_last_query_line> = |{ <lv_last_query_line> }.|.
    ENDIF.

    DATA: lr_result TYPE REF TO data.

    lt_lines = VALUE #( BASE lt_lines
       ( |    TRY.| )
       ( LINES OF lt_lines_temp )
    ).
    IF ms_query-is_single_result_query = abap_false.
      lt_lines = VALUE #( BASE lt_lines
         ( |        CREATE DATA rr_result LIKE result.| )
         ( |        ASSIGN rr_result->* to FIELD-SYMBOL(<lt_result>).| )
         ( |        <lt_result> = result.| )
      ).
    ENDIF.
    lt_lines = VALUE #( BASE lt_lines
       ( |      CATCH cx_root INTO DATA(lx_root).| )
       ( |        RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error| )
       ( |          EXPORTING| )
       ( |            previous = lx_root.| )
       ( |    ENDTRY.| )
    ).

    lt_lines = VALUE #( BASE lt_lines
        ( |  ENDMETHOD.| )
        ( |ENDCLASS.| )
    ).

*.. Generate the subroutine
    GENERATE SUBROUTINE POOL lt_lines
                        NAME ev_program
                        MESSAGE ev_message
                        LINE    DATA(lv_error_line)
                        OFFSET  DATA(lv_error_offset)
                        WORD    DATA(lv_error_word).   "#EC CI_GENERATE
  ENDMETHOD.

  METHOD remove_null.
    DATA:           null_char    TYPE c.
    FIELD-SYMBOLS:  <ls_help>     TYPE x.

*.. generate a 'null'-character
    ASSIGN null_char TO <ls_help> CASTING.
    IF sy-subrc = 0.
      CLEAR <ls_help>.
    ENDIF.
    r_result = translate( val = i_lv_col_value  from = null_char  to = '' ).
  ENDMETHOD.


  METHOD check_and_correct_result.
    DATA: lo_ref_table_des       TYPE REF TO cl_abap_tabledescr,
          lo_ref_table_line_des  TYPE REF TO cl_abap_structdescr,
          lo_data_ref            TYPE REF TO cl_abap_structdescr,
          lt_table_field_details TYPE abap_compdescr_tab,
          ls_table_field_details TYPE abap_compdescr,
          lt_data_fields         TYPE abap_compdescr_tab,
          ls_data_field          TYPE abap_compdescr,

          lv_col_value           TYPE string,
          lv_data_cell_col_val   TYPE string,

          lt_types               TYPE RANGE OF abap_typekind.

    FIELD-SYMBOLS: <lt_dyn_table>         TYPE ANY TABLE,
                   <ls_dyn_table_row>     TYPE any,
                   <ls_column_value>      TYPE any,
                   <ls_data_cell_col_val> TYPE any,
                   <ls_table_col>         TYPE zdbbr_dp_column.

    lt_types = VALUE #(
      ( sign = 'I' option = 'EQ' low = cl_abap_typedescr=>typekind_char   )
      ( sign = 'I' option = 'EQ' low = cl_abap_typedescr=>typekind_date   )
      ( sign = 'I' option = 'EQ' low = cl_abap_typedescr=>typekind_string )
      ( sign = 'I' option = 'EQ' low = cl_abap_typedescr=>typekind_num    )
      ( sign = 'I' option = 'EQ' low = cl_abap_typedescr=>typekind_time   )
    ).

    lo_ref_table_des ?=  cl_abap_typedescr=>describe_by_data_ref( p_data_ref = ir_query_result ).

    TRY.
        lo_ref_table_line_des  ?=  lo_ref_table_des->get_table_line_type( ).
        lt_table_field_details =  lo_ref_table_line_des->components.
      CATCH cx_sy_move_cast_error INTO DATA(lx_sy_move_cast_error).
        RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error.
    ENDTRY.

    LOOP AT lt_table_field_details INTO ls_table_field_details.
      DATA(lv_rollname) = VALUE rollname( ).
      DATA(lv_domname) = VALUE domname( ).
      DATA(lv_length) = VALUE ddleng( ).
      data(lv_int_length) = value intlen( ).
      DATA(lv_decimals) = VALUE decimals( ).
      DATA(lv_description) = VALUE ddtext( ).

      DATA(lr_comp_type) = lo_ref_table_line_des->get_component_type( p_name = ls_table_field_details-name ).
      IF lr_comp_type->is_ddic_type( ) AND lr_comp_type->kind <> lr_comp_type->kind_struct.
        DATA(ls_ddic_header) = CAST cl_abap_elemdescr( lr_comp_type )->get_ddic_field( ).
        lv_rollname = ls_ddic_header-rollname.
        lv_domname = ls_ddic_header-domname.
        lv_decimals = ls_ddic_header-decimals.
        lv_length = ls_ddic_header-leng.
        lv_int_length = ls_ddic_header-intlen.

        lv_description = COND #(
          WHEN ls_ddic_header-scrtext_m IS NOT INITIAL THEN ls_ddic_header-scrtext_m
          WHEN ls_ddic_header-scrtext_l IS NOT INITIAL THEN ls_ddic_header-scrtext_l
          WHEN ls_ddic_header-scrtext_s IS NOT INITIAL THEN ls_ddic_header-scrtext_s
          ELSE ls_ddic_header-fieldname
        ).
      ELSE.
        lv_decimals = lr_comp_type->decimals.
        lv_description = ls_table_field_details-name.
        lv_length = lr_comp_type->length.
        lv_int_length = lr_comp_type->length.
      ENDIF.

*.... add metadata
      cs_result-columns = VALUE #( BASE cs_result-columns
        ( metadata = VALUE #(
            name        = ls_table_field_details-name
            description = lv_description
            length      = lv_length
            int_length  = lv_int_length
            domname     = lv_domname
            rollname    = lv_rollname
            decimals    = lv_decimals
            typekind    = ls_table_field_details-type_kind )
        )
      ).
    ENDLOOP.

    ASSIGN ir_query_result->* TO <lt_dyn_table>.

    cs_result-line_count = lines( <lt_dyn_table> ).

*.. Fill the data sets of the columns
    LOOP AT <lt_dyn_table> ASSIGNING <ls_dyn_table_row>.

      LOOP AT lt_table_field_details INTO ls_table_field_details.

        CLEAR  lv_col_value .
        UNASSIGN <ls_column_value>.

        ASSIGN COMPONENT ls_table_field_details-name OF STRUCTURE <ls_dyn_table_row> TO <ls_column_value>.
        ASSIGN cs_result-columns[ metadata-name = ls_table_field_details-name ] TO <ls_table_col>.

        IF <ls_table_col>  IS ASSIGNED.

          IF <ls_column_value> IS ASSIGNED.

            IF ls_table_field_details-type_kind EQ cl_abap_typedescr=>typekind_struct1 OR
               ls_table_field_details-type_kind EQ cl_abap_typedescr=>typekind_struct2.
              TRY.
                  lo_data_ref ?= cl_abap_typedescr=>describe_by_data( p_data = <ls_column_value>  ).
                  lt_data_fields = lo_data_ref->components.

                  LOOP AT lt_data_fields INTO ls_data_field.
                    CLEAR lv_data_cell_col_val.
                    ASSIGN COMPONENT ls_data_field-name OF STRUCTURE <ls_column_value>  TO <ls_data_cell_col_val>.

                    IF <ls_data_cell_col_val> IS ASSIGNED.
                      lv_data_cell_col_val = <ls_data_cell_col_val>.
                      CONCATENATE lv_col_value lv_data_cell_col_val INTO lv_col_value SEPARATED BY space.

                      IF strlen( lv_col_value ) > 97.
                        EXIT.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.

                  IF strlen( lv_col_value ) > 97.
                    lv_col_value = lv_col_value+0(97).
                  ENDIF.
                  lv_col_value = |{ lv_col_value }...|.

                CATCH cx_sy_move_cast_error.

              ENDTRY.
            ELSE.
              lv_col_value = <ls_column_value>.

            ENDIF.
          ENDIF.

          IF lv_col_value IS NOT INITIAL.
            IF ls_table_field_details-type_kind IN lt_types.
              lv_col_value = remove_null( lv_col_value ).
            ENDIF.

          ENDIF.

          APPEND lv_col_value TO <ls_table_col>-dataset.
        ENDIF.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
