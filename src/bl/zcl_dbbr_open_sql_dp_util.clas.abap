CLASS zcl_dbbr_open_sql_dp_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Execute data preview for the given query
    "!
    "! @parameter iv_query | the query to be executed
    "! @parameter iv_row_count | the rows which should be retrieved
    "! @parameter rs_table_result | the serialized result of the query
    "! @raising cx_adt_datapreview_common | Error during query execution
    CLASS-METHODS execute_data_preview
      IMPORTING
        !iv_query              TYPE string
        !iv_row_count          TYPE i DEFAULT 100
      RETURNING
        VALUE(rs_table_result) TYPE zdbbr_dp_table_data
      RAISING
        cx_adt_datapreview_common .
    CLASS-METHODS execute_query_syntax_check
      IMPORTING
        !iv_query              TYPE string
      RETURNING
        VALUE(rs_check_result) TYPE zdbbr_dp_check_result
      RAISING
        cx_adt_datapreview_common .
    CLASS-METHODS get_count_star_query
      IMPORTING
        iv_query        TYPE string
      RETURNING
        VALUE(rv_query) TYPE string.
    CLASS-METHODS get_count_star_value
      IMPORTING
        iv_query        TYPE string
      RETURNING
        VALUE(rv_value) TYPE i
      RAISING
        cx_adt_datapreview_common.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS remove_null
      IMPORTING
        !i_lv_col_value TYPE string
      RETURNING
        VALUE(r_result) TYPE string .
    CLASS-METHODS custom_to_upper
      CHANGING
        !cv_query_string TYPE string.
ENDCLASS.



CLASS zcl_dbbr_open_sql_dp_util IMPLEMENTATION.
  METHOD get_count_star_query.
    DATA(lv_query_string) = iv_query.
    custom_to_upper( CHANGING cv_query_string = lv_query_string ).

    SPLIT lv_query_string AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(lt_query_table).
    LOOP AT lt_query_table INTO DATA(ls_query_line).
      CONCATENATE lv_temp space space ls_query_line space INTO DATA(lv_temp) RESPECTING BLANKS.
    ENDLOOP.

    lv_query_string = lv_temp.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_query_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_query_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_query_string WITH space.

    CONDENSE lv_query_string. "This has to be replaced by SHIFT lv_query_string LEFT DELETING LEADING space. and SHIFT lv_query_string RIGHT DELETING TRAILING space.

    IF lv_query_string CS ' GROUP BY '.
      rv_query = 'SELECT 1 AS DUMMY FROM '.
    ELSE.
      rv_query = 'SELECT COUNT( * ) FROM '.
    ENDIF.
    SPLIT lv_query_string AT ' FROM ' INTO TABLE lt_query_table.
    IF lines( lt_query_table ) EQ 1.
      SPLIT lv_query_string AT 'FROM ' INTO TABLE lt_query_table.
      IF lines( lt_query_table ) EQ 1.
        SPLIT lv_query_string AT 'FROM' INTO TABLE lt_query_table.
      ENDIF.
    ENDIF.
    DESCRIBE TABLE lt_query_table LINES DATA(lv_total_lines).
    LOOP AT lt_query_table INTO ls_query_line.
      IF  sy-tabix GE 2.
        IF sy-tabix EQ lv_total_lines.
          CONCATENATE lv_query_temp space ls_query_line  INTO DATA(lv_query_temp) RESPECTING BLANKS.
        ELSE.
          CONCATENATE lv_query_temp space ls_query_line space 'FROM' space INTO lv_query_temp RESPECTING BLANKS.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SPLIT lv_query_temp AT ' ORDER BY ' INTO TABLE lt_query_table.
    READ TABLE lt_query_table INTO lv_query_temp INDEX 1.
    CONCATENATE rv_query space lv_query_temp INTO rv_query RESPECTING BLANKS.
    CONDENSE rv_query.
  ENDMETHOD.

  METHOD custom_to_upper.
    DATA: lt_definition TYPE rswsourcet.

    lt_definition = cl_oo_section_source=>convert_string_to_table( p_string = cv_query_string ).

    TRY.
        NEW zcl_dbbr_pretty_printer( )->format_source(
          EXPORTING
            ir_settings = NEW lcl_upper_case_settings( )
          CHANGING
            ct_source   = lt_definition
        ).
        cv_query_string = cl_oo_section_source=>convert_table_to_string( p_source = CONV #( lt_definition )  ).

      CATCH cx_sedi_pretty_printer.
        cv_query_string = to_upper( cv_query_string ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_count_star_value.
    DATA: lv_row_count TYPE i.

    DATA(lv_query_string) = iv_query.
    SPLIT lv_query_string AT ' GROUP BY ' INTO TABLE DATA(lt_query_table).

    DATA(ls_query_result) = execute_data_preview(
        iv_query     = iv_query
        iv_row_count = 0
    ).

    LOOP AT ls_query_result-columns ASSIGNING FIELD-SYMBOL(<ls_result>).
      IF lines( lt_query_table ) = 1.
        rv_value = <ls_result>-dataset[ 1 ].
      ELSE.
        rv_value = lines( <ls_result>-dataset ).
      ENDIF.
    ENDLOOP.



  ENDMETHOD.

  METHOD execute_data_preview.

    DATA: lr_query_result        TYPE REF TO data,
          lo_ref_table_des       TYPE REF TO cl_abap_tabledescr,
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

    DATA(lr_query_handler) = cl_adt_dp_open_sql_handler=>get_instance(
      iv_query_string = iv_query
      iv_new_format = abap_true
      iv_remove_comments = abap_true
    ).

    lr_query_handler->get_query_result(
      EXPORTING
        iv_row_count              = iv_row_count
      IMPORTING
        er_result                 = lr_query_result
        ev_execution_time         = rs_table_result-query_execution_time
        ev_executed_query         = rs_table_result-executed_query_string

    ).

    lo_ref_table_des ?=  cl_abap_typedescr=>describe_by_data_ref( p_data_ref = lr_query_result ).

    TRY.
        lo_ref_table_line_des  ?=  lo_ref_table_des->get_table_line_type( ).
        lt_table_field_details =  lo_ref_table_line_des->components.
      CATCH cx_sy_move_cast_error INTO DATA(lx_sy_move_cast_error).
        RAISE EXCEPTION TYPE cx_adt_datapreview_common
          EXPORTING
            previous = lx_sy_move_cast_error.
    ENDTRY.

    LOOP AT lt_table_field_details INTO ls_table_field_details.
      DATA(lv_rollname) = VALUE rollname( ).
      DATA(lv_domname) = VALUE domname( ).
      DATA(lv_length) = VALUE ddleng( ).
      DATA(lv_decimals) = VALUE decimals( ).
      DATA(lv_description) = VALUE ddtext( ).

      DATA(lr_comp_type) = lo_ref_table_line_des->get_component_type( p_name = ls_table_field_details-name ).
      IF lr_comp_type->is_ddic_type( ) AND lr_comp_type->kind <> lr_comp_type->kind_struct.
        DATA(ls_ddic_header) = CAST cl_abap_elemdescr( lr_comp_type )->get_ddic_field( ).
        lv_rollname = ls_ddic_header-rollname.
        lv_domname = ls_ddic_header-domname.
        lv_decimals = ls_ddic_header-decimals.
        lv_length = ls_ddic_header-leng.
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
      ENDIF.

*.... add metadata
      rs_table_result-columns = VALUE #( BASE rs_table_result-columns
        ( metadata = VALUE #(
            name        = ls_table_field_details-name
            description = lv_description
            length      = lv_length
            domname     = lv_domname
            rollname    = lv_rollname
            decimals    = lv_decimals
            typekind    = ls_table_field_details-type_kind )
        )
      ).
    ENDLOOP.

    ASSIGN lr_query_result->* TO <lt_dyn_table>.

    rs_table_result-line_count = lines( <lt_dyn_table> ).

*.. Fill the data sets of the columns
    LOOP AT <lt_dyn_table> ASSIGNING <ls_dyn_table_row>.

      LOOP AT lt_table_field_details INTO ls_table_field_details.

        CLEAR  lv_col_value .
        UNASSIGN <ls_column_value>.

        ASSIGN COMPONENT ls_table_field_details-name OF STRUCTURE <ls_dyn_table_row> TO <ls_column_value>.
        ASSIGN rs_table_result-columns[ metadata-name = ls_table_field_details-name ] TO <ls_table_col>.

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
*            ELSEIF ls_table_field_details-type_kind = cl_abap_typedescr=>typekind_int8.
*              data(lv_int8) = conv int8( lv_col_value ).
*              lv_col_value = |{ lv_int8 number = raw }|.
            ENDIF.

          ENDIF.

          APPEND lv_col_value TO <ls_table_col>-dataset.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD execute_query_syntax_check.
    DATA: ls_check_run_report TYPE if_data_preview=>ty_check_run_report,
          lt_message          TYPE TABLE OF string,
          lv_error_message    TYPE string.

    TRY.

        ls_check_run_report = cl_adt_dp_open_sql_handler=>get_syntax_check_result(
             iv_query_string = iv_query
         ).

        IF ls_check_run_report-results IS NOT INITIAL.
          DATA(ls_check_result) = ls_check_run_report-results[ 1 ].
          SPLIT ls_check_result-shorttext AT '.' INTO TABLE lt_message.

          rs_check_result = VALUE #(
            message          = VALUE #( lt_message[ 1 ] OPTIONAL )
            message_severity = ls_check_result-type
            position         = ls_check_result-uri
          ).
        ENDIF.

      CATCH cx_adt_datapreview_common INTO DATA(lx_data_preview).
        lv_error_message = lx_data_preview->get_text( ).

        IF lv_error_message IS NOT INITIAL.
          rs_check_result = VALUE zdbbr_dp_check_result(
              message          = lv_error_message
              message_severity = 'E'
          ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD remove_null.
    DATA:           null_char    TYPE c.
    FIELD-SYMBOLS:  <fs_hlp>     TYPE x.

*.. generate a 'null'-character
    ASSIGN null_char TO <fs_hlp> CASTING.
    IF sy-subrc = 0.
      CLEAR <fs_hlp>.
    ENDIF.
    r_result = translate( val = i_lv_col_value  from = null_char  to = '' ).
  ENDMETHOD.
ENDCLASS.
