"! <p class="shorttext synchronized" lang="en">Selection Util for SQL Query</p>
CLASS zcl_dbbr_sql_query_selctn_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selection_util
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS execute_selection
        REDEFINITION.
    METHODS get_entity_name
        REDEFINITION.
    METHODS handle_alv_ctx_menu_request
        REDEFINITION.
    METHODS init
        REDEFINITION.
    METHODS refresh_selection
        REDEFINITION.
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION.
    METHODS build_simple_alv_title
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_dynamic_table
        REDEFINITION.
    METHODS create_field_catalog
        REDEFINITION.
  PRIVATE SECTION.
    DATA mo_query TYPE REF TO zcl_dbbr_sql_query.
    DATA mo_query_result_line_type TYPE REF TO cl_abap_structdescr.
    DATA mr_query_result TYPE REF TO data.
    DATA mv_execution_time_str TYPE string.
    DATA mt_query_result_col TYPE zdbbr_dp_col_metadata_t.

    "! <p class="shorttext synchronized" lang="en">Parse/Set parameters for query</p>
    METHODS set_parameters_in_query.
    METHODS update_result.
ENDCLASS.


CLASS zcl_dbbr_sql_query_selctn_util IMPLEMENTATION.
  METHOD execute_selection.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    zcl_dbbr_sql_query_exec=>execute_query(
      EXPORTING
        io_query          = mo_query
        iv_row_count      = ms_technical_info-max_lines
        if_count_only     = mf_count_lines
      IMPORTING
        et_data_info      = mt_query_result_col
        ev_execution_time = mv_execution_time_str
        er_data           = mr_query_result
        ev_line_count     = ms_control_info-number
    ).

    IF mo_query->ms_data-is_single_result_query = abap_true.
      DATA(lv_number_of_lines) = |{ ms_control_info-number NUMBER = USER }|.
      MESSAGE i024(zdbbr_info) WITH lv_number_of_lines.
      RETURN.
    ENDIF.

    IF mr_query_result IS NOT BOUND.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    ASSIGN mr_query_result->* TO <lt_data>.
    ms_control_info-number = lines( <lt_data> ).


*.. only count lines for current selection and display result
    IF mf_count_lines = abap_true.
      RETURN.
    ENDIF.

    create_dynamic_table( ).

    create_field_catalog( ).

    " if no selection occurred, prevent screen visibility
    IF ms_control_info-number <= 0.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    RAISE EVENT selection_finished
      EXPORTING
         ef_first_select = abap_true.
  ENDMETHOD.

  METHOD refresh_selection.
    FIELD-SYMBOLS: <lt_data>        TYPE table,
                   <lt_output_data> TYPE table.

    zcl_dbbr_sql_query_exec=>execute_query(
      EXPORTING
        io_query          = mo_query
        iv_row_count      = ms_technical_info-max_lines
        if_count_only     = mf_count_lines
      IMPORTING
        et_data_info      = mt_query_result_col
        ev_execution_time = mv_execution_time_str
        er_data           = mr_query_result
    ).

    IF mr_query_result IS INITIAL.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    ASSIGN mr_query_result->* TO <lt_data>.
    ms_control_info-number = lines( <lt_data> ).

    IF ms_control_info-number = 0.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    ASSIGN mr_t_data->* TO <lt_output_data>.
    MOVE-CORRESPONDING <lt_data> TO <lt_output_data>.

    RAISE EVENT selection_finished.

  ENDMETHOD.

  METHOD build_simple_alv_title.
    result = COND #( WHEN mv_entity_id IS NOT INITIAL THEN |{ 'Result of'(001) } { mv_entity_id }| ELSE |Result of Querytest| ).
  ENDMETHOD.

  METHOD get_entity_name.

  ENDMETHOD.

  METHOD handle_alv_ctx_menu_request.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = VALUE #(
      ( LINES OF super->get_deactivated_functions( ) )
      ( zif_dbbr_c_selection_functions=>navigate_association )
      ( zif_dbbr_c_selection_functions=>toggle_entity_info_header )
      ( zif_dbbr_c_selection_functions=>transfer_filter_values )
      ( zif_dbbr_c_selection_functions=>leave_screen_with_layout )
      ( zif_dbbr_c_selection_functions=>save_selection_as_f4 )
    ).
  ENDMETHOD.

  METHOD init.
    DATA: ls_query TYPE zdbbr_query_data.

    " as long as the where clause has not been parsed completely a live filter is not possible
    ms_technical_info-activate_alv_live_filter = abap_false.
    mf_custom_query_active = abap_true.

*... read the query description
    DATA(lo_query_f) = NEW zcl_dbbr_query_factory( ).
    IF mv_entity_id IS INITIAL.
      ASSERT mv_query_string IS NOT INITIAL.
      ls_query-source = mv_query_string.
    ELSE.
      ls_query = lo_query_f->get_query(
          iv_query_name     = mv_entity_id
          if_load_completely = abap_false
      ).
    ENDIF.

    TRY.
        mo_query = NEW zcl_dbbr_sql_query_parser( iv_query = ls_query-source )->parse( ).
      CATCH zcx_dbbr_sql_query_error.
        RETURN.
    ENDTRY.

    set_parameters_in_query( ).

  ENDMETHOD.



  METHOD set_parameters_in_query.

    LOOP AT mt_param_values ASSIGNING FIELD-SYMBOL(<ls_param>)
      GROUP BY ( fieldname = <ls_param>-fieldname )
      ASSIGNING FIELD-SYMBOL(<ls_param_grouped>).

      DATA(ls_query_param) = mo_query->mt_parameters[ name = <ls_param_grouped>-fieldname ].


      DATA(lt_values) = VALUE zuitb_generic_range_itab(
         FOR param IN GROUP <ls_param_grouped>
         ( sign   = param-sign
           option = COND #( WHEN param-option IS NOT INITIAL THEN param-option
                            WHEN param-high   IS NOT INITIAL THEN 'BT'
                            WHEN param-low    CS '*'         THEN 'CP'
                            ELSE                                  'EQ' )
           low    = param-low
           high   = param-high
         )
      ).

      IF ls_query_param-is_range = abap_true.
        mo_query->set_parameter_value(
            iv_name        = ls_query_param-name
            it_value_range = lt_values
        ).
      ELSE.
        mo_query->set_parameter_value(
            iv_name  = ls_query_param-name
            iv_value = CONV #( lt_values[ 1 ]-low )
        ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD create_dynamic_table.
    FIELD-SYMBOLS: <lt_data>   TYPE table,
                   <lt_result> TYPE table.

    zcl_dbbr_output_tab_builder=>enhance_output_table_comps(
      EXPORTING
        ir_table      = mr_query_result
      IMPORTING
        eo_line_type  = mo_query_result_line_type
        eo_table_type = DATA(lo_table_type)
    ).
*.. create table
    CREATE DATA mr_t_data TYPE HANDLE lo_table_type.
    CREATE DATA mr_t_temp_data TYPE HANDLE lo_table_type.

    ASSIGN mr_t_data->* TO <lt_data>.
    ASSIGN mr_query_result->* TO <lt_result>.

    IF <lt_data> IS ASSIGNED AND <lt_result> IS ASSIGNED.
      MOVE-CORRESPONDING <lt_result> TO <lt_data>.
      CLEAR <lt_result>.
    ENDIF.

  ENDMETHOD.

  METHOD create_field_catalog.
    DATA(lt_dfies) = zcl_uitb_alv_data_descr=>read_structdescr(
        ir_structdescr = mo_query_result_line_type
        iv_language    = zcl_dbbr_appl_util=>get_description_language( )
    ).

    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).
      DATA(lf_tech) = COND #(
        WHEN <ls_dfies>-fieldname = zif_dbbr_c_special_out_columns=>hide_flag OR
             <ls_dfies>-fieldname = zif_dbbr_c_special_out_columns=>line_index THEN abap_true
      ).
      DATA(ls_field) = VALUE lvc_s_fcat(
          fieldname    = <ls_dfies>-fieldname
          ref_table    = <ls_dfies>-reftable
          ref_field    = <ls_dfies>-reffield
          rollname     = <ls_dfies>-rollname
          dd_roll      = <ls_dfies>-rollname
          datatype     = <ls_dfies>-datatype
          inttype      = <ls_dfies>-inttype
          intlen       = <ls_dfies>-leng
          dd_outlen    = <ls_dfies>-outputlen
          no_sign      = COND #( WHEN <ls_dfies>-sign = abap_false THEN abap_true )
          key          = <ls_dfies>-keyflag
          lowercase    = <ls_dfies>-lowercase
          reptext      = <ls_dfies>-reptext
          scrtext_s    = <ls_dfies>-scrtext_s
          scrtext_m    = <ls_dfies>-scrtext_m
          scrtext_l    = <ls_dfies>-scrtext_l
          domname      = <ls_dfies>-domname
          f4availabl   = <ls_dfies>-f4availabl
          decimals     = <ls_dfies>-decimals
          convexit     = <ls_dfies>-convexit
          tech         = lf_tech
      ).

      DATA(ls_tabfield_info) = CORRESPONDING zdbbr_tabfield_info_ui(
        ls_field MAPPING tabname            = ref_table
                         sql_fieldname      = fieldname
                         sql_fieldname_long = fieldname
                         alv_fieldname      = fieldname
                         std_short_text     = scrtext_s
                         std_medium_text    = scrtext_m
                         std_long_text      = scrtext_l
                         outputlen          = dd_outlen
      ).

      mo_tabfields->add( ir_s_element = REF #( ls_tabfield_info ) ).

      IF ls_field-scrtext_s IS INITIAL AND
         ls_field-scrtext_m IS INITIAL AND
         ls_field-scrtext_l IS INITIAL.
        ls_field-scrtext_l = ls_field-fieldname.
      ENDIF.

      mo_tabfields_all->add( ir_s_element = REF #( ls_tabfield_info ) ).

      mt_fieldcat = VALUE #(
        BASE mt_fieldcat
        ( ls_field )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD update_result.

  ENDMETHOD.


ENDCLASS.
