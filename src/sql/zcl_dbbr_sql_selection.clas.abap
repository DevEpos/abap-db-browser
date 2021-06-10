"! <p class="shorttext synchronized" lang="en">Creates subroutine pool program for selection data from db</p>
CLASS zcl_dbbr_sql_selection DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new SQL selection</p>
      create
        IMPORTING
          it_select          TYPE string_table
          it_from            TYPE string_table
          it_where           TYPE string_table OPTIONAL
          it_order_by        TYPE string_table OPTIONAL
          it_group_by        TYPE string_table OPTIONAL
          it_having          TYPE string_table OPTIONAL
          iv_max_size        TYPE i
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_dbbr_sql_selection
        RAISING
          zcx_dbbr_dyn_prog_generation.

    EVENTS:
      "! <p class="shorttext synchronized" lang="en">Event to signal that count was determined</p>
      count_query_finished
        EXPORTING
          VALUE(ev_count) TYPE zdbbr_no_of_lines.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine during active aggregation</p>
      determine_size_for_group_by
        IMPORTING
          ir_t_data      TYPE REF TO data
        RETURNING
          VALUE(rv_size) TYPE zdbbr_no_of_lines
        RAISING
          zcx_dbbr_selection_common,
      "! <p class="shorttext synchronized" lang="en">Determines the size of existing entries</p>
      determine_size
        RETURNING
          VALUE(rv_size) TYPE zdbbr_no_of_lines
        RAISING
          zcx_dbbr_selection_common,
      "! <p class="shorttext synchronized" lang="en">Determine during active aggregation</p>
      determine_group_by_size_async
        IMPORTING
          ir_t_data TYPE REF TO data
        RAISING
          zcx_dbbr_selection_common,
      "! <p class="shorttext synchronized" lang="en">Determines the size of existing entries</p>
      determine_size_async
        RAISING
          zcx_dbbr_selection_common,
      "! <p class="shorttext synchronized" lang="en">Returns SQL String for current select</p>
      get_select_sql
        RETURNING
          VALUE(rv_select_sql) TYPE string,
      "! <p class="shorttext synchronized" lang="en">Selects data</p>
      select_data
        EXPORTING
          VALUE(et_data) TYPE table
        RAISING
          zcx_dbbr_selection_common,
      "! <p class="shorttext synchronized" lang="en">Set maximum number of rows</p>
      set_max_rows
        IMPORTING
          iv_max_rows TYPE i,
      "! <p class="shorttext synchronized" lang="en">Update from clause</p>
      update_from
        IMPORTING
          it_from TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Unregisters handlers for async query exec.</p>
      unregister_evt_handlers.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      "! List of Strings
      mt_select   TYPE string_table,
      "! List of Strings
      mt_from     TYPE string_table,
      "! List of Strings
      mt_where    TYPE string_table,
      "! List of Strings
      mt_order_by TYPE string_table,
      "! List of Strings
      mt_group_by TYPE string_table,
      mt_having   TYPE string_table,
      mv_max_size TYPE i.

    METHODS: fill_having
      CHANGING
        ct_lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with from clause</p>
      "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
      fill_from
        CHANGING
          ct_lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with group by clause</p>
      "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
      fill_group_by
        CHANGING
          ct_lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with order by clause</p>
      "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
      fill_order_by
        CHANGING
          ct_lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with select clause</p>
      "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
      fill_select
        CHANGING
          ct_lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with where clause</p>
      "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
      fill_where
        CHANGING
          ct_lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Determines existing line count with CTE</p>
      get_group_by_size_by_cte
        RETURNING
          VALUE(rv_size) TYPE zdbbr_no_of_lines
        RAISING
          zcx_dbbr_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Creates count query with CTE</p>
      create_count_query_for_cte
        RETURNING
          VALUE(ro_query) TYPE REF TO  zcl_dbbr_sql_query
        RAISING
          zcx_dbbr_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Handler for query finished event</p>
      on_async_query_finished
        FOR EVENT query_finished OF zcl_dbbr_sql_query_exec
        IMPORTING
          er_data
          et_data_info
          ev_execution_time
          ev_line_count
          ev_message
          ev_message_type.
ENDCLASS.



CLASS zcl_dbbr_sql_selection IMPLEMENTATION.


  METHOD create.
    ro_instance = NEW zcl_dbbr_sql_selection( ).

    ro_instance->mt_select   = it_select.
    ro_instance->mt_from     = it_from.
    ro_instance->mt_having   = it_having.
    ro_instance->mt_where    = it_where.
    ro_instance->mt_order_by = it_order_by.
    ro_instance->mt_group_by = it_group_by.
    ro_instance->mv_max_size = iv_max_size.

    SET HANDLER ro_instance->on_async_query_finished ACTIVATION 'X'.
  ENDMETHOD.


  METHOD unregister_evt_handlers.
    SET HANDLER on_async_query_finished ACTIVATION space.
  ENDMETHOD.


  METHOD determine_size_for_group_by.
    DATA: lx_root TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_data> TYPE table.

    IF sy-saprl > 751. " Common table expressions exist
      TRY.
          rv_size = get_group_by_size_by_cte( ).
          RETURN.
        CATCH zcx_dbbr_sql_query_error ##NEEDED.
      ENDTRY.
    ENDIF.

    ASSIGN ir_t_data->* TO <lt_data>.

    TRY.
        SELECT (mt_select)
          FROM (mt_from)
          WHERE (mt_where)
          GROUP BY (mt_group_by)
          HAVING (mt_having)
          ORDER BY (mt_order_by)
          INTO CORRESPONDING FIELDS OF TABLE @<lt_data>.

        rv_size = lines( <lt_data> ).
        CLEAR <lt_data>.
      CATCH cx_root INTO lx_root.
        RAISE EXCEPTION TYPE zcx_dbbr_selection_common
          EXPORTING
            previous = lx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD determine_size.
    TRY.
        SELECT COUNT( * )
          FROM (mt_from)
          WHERE (mt_where)
          INTO @rv_size.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_dbbr_selection_common
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD determine_size_async.
    DATA: lt_sql_lines TYPE string_table.

    lt_sql_lines = VALUE #( ( `SELECT COUNT(*)` ) ).
    fill_from( CHANGING ct_lines = lt_sql_lines ).
    fill_where( CHANGING ct_lines = lt_sql_lines ).

    TRY.
        zcl_dbbr_sql_query_exec=>execute_query_async(
          io_query      = NEW zcl_dbbr_sql_query_parser(
              iv_query                 = concat_lines_of( table = lt_sql_lines  sep = cl_abap_char_utilities=>cr_lf )
              if_fill_log_for_messages = abap_false
            )->parse( ) ).
      CATCH zcx_dbbr_sql_query_error INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_dbbr_selection_common
          EXPORTING
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD determine_group_by_size_async.
    DATA: lt_sql_lines                   TYPE string_table,
          lo_count_query                 TYPE REF TO zcl_dbbr_sql_query,
          lf_create_fallback_count_query TYPE abap_bool.


    IF sy-saprl > 751.
      TRY.
          lo_count_query = create_count_query_for_cte( ).
        CATCH zcx_dbbr_sql_query_error.
          lf_create_fallback_count_query = abap_true.
      ENDTRY.
    ELSE.
      lf_create_fallback_count_query = abap_true.
    ENDIF.

    IF lf_create_fallback_count_query = abap_true.
      fill_select( CHANGING ct_lines = lt_sql_lines ).
      fill_from( CHANGING ct_lines = lt_sql_lines ).
      fill_where( CHANGING ct_lines = lt_sql_lines ).
      fill_group_by( CHANGING ct_lines = lt_sql_lines ).
      fill_having( CHANGING ct_lines = lt_sql_lines ).

      TRY.
          lo_count_query = NEW zcl_dbbr_sql_query_parser(
              iv_query                 = concat_lines_of( table = lt_sql_lines  sep = cl_abap_char_utilities=>cr_lf )
              if_fill_log_for_messages = abap_false
            )->parse( ).
        CATCH zcx_dbbr_sql_query_error INTO DATA(lx_error).
          RAISE EXCEPTION TYPE zcx_dbbr_selection_common
            EXPORTING
              previous = lx_error.
      ENDTRY.
    ENDIF.

    IF lo_count_query IS NOT INITIAL.
      zcl_dbbr_sql_query_exec=>execute_query_async(
        io_query              = lo_count_query
        if_show_progress_text = abap_false
        " we want all rows to get the full group by count
        iv_row_count          = 0 ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_having.
    DATA: lv_having TYPE string.

    LOOP AT mt_having ASSIGNING FIELD-SYMBOL(<lv_having>).
      CLEAR: lv_having.
      IF sy-tabix = 1.
        lv_having = |  HAVING { <lv_having> }|.
      ELSE.
        lv_having = |         { <lv_having> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_having ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_from.
    DATA: lv_from TYPE string.

    LOOP AT mt_from ASSIGNING FIELD-SYMBOL(<lv_from>).
      CLEAR: lv_from.
      IF sy-tabix = 1.
        lv_from = |  FROM { <lv_from> }|.
      ELSE.
        lv_from = |       { <lv_from> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_from ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_group_by.
    DATA: lv_group_by TYPE string.

    LOOP AT mt_group_by ASSIGNING FIELD-SYMBOL(<lv_group_by>).
      CLEAR: lv_group_by.
      IF sy-tabix = 1.
        lv_group_by = |  GROUP BY { <lv_group_by> }|.
      ELSE.
        lv_group_by = |           { <lv_group_by> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_group_by ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_order_by.
    DATA: lv_order_by TYPE string.

    LOOP AT mt_order_by ASSIGNING FIELD-SYMBOL(<lv_order_by>).
      CLEAR: lv_order_by.
      IF sy-tabix = 1.
        lv_order_by = |  ORDER BY { <lv_order_by> }|.
      ELSE.
        lv_order_by = |           { <lv_order_by> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_order_by ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_select.
    DATA: lv_select TYPE string.

    LOOP AT mt_select ASSIGNING FIELD-SYMBOL(<lv_select>).
      CLEAR: lv_select.
      IF sy-tabix = 1.
        lv_select = |SELECT { <lv_select> }|.
      ELSE.
        lv_select = |       { <lv_select> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_select ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_where.
    DATA: lv_where TYPE string.

    LOOP AT mt_where ASSIGNING FIELD-SYMBOL(<lv_where>).
      CLEAR: lv_where.
      IF sy-tabix = 1.
        lv_where = |WHERE { <lv_where> }|.
      ELSE.
        lv_where = |   { <lv_where> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_where ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD select_data.
    DATA: lx_root TYPE REF TO cx_root.

    TRY.
        SELECT (mt_select)
          FROM (mt_from)
          WHERE (mt_where)
          GROUP BY (mt_group_by)
          HAVING (mt_having)
          ORDER BY (mt_order_by)
          INTO CORRESPONDING FIELDS OF TABLE @et_data
          UP TO @mv_max_size ROWS.
      CATCH cx_root INTO lx_root.
        RAISE EXCEPTION TYPE zcx_dbbr_selection_common
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD set_max_rows.
    mv_max_size = iv_max_rows.
  ENDMETHOD.


  METHOD update_from.
    mt_from = it_from.
  ENDMETHOD.

  METHOD get_select_sql.
    DATA: lt_sql_lines TYPE string_table.

    fill_select( CHANGING ct_lines = lt_sql_lines ).
    fill_from( CHANGING ct_lines = lt_sql_lines ).
    fill_where( CHANGING ct_lines = lt_sql_lines ).
    fill_group_by( CHANGING ct_lines = lt_sql_lines ).
    fill_having( CHANGING ct_lines = lt_sql_lines ).
    fill_order_by( CHANGING ct_lines = lt_sql_lines ).

    CONCATENATE LINES OF lt_sql_lines INTO rv_select_sql SEPARATED BY cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.

  METHOD create_count_query_for_cte.
    DATA: lt_sql_lines TYPE string_table,
          lv_query     TYPE string.

    lt_sql_lines = VALUE #(
      ( |WITH| )
      ( |  +group_select as (| ) ).

    fill_select( CHANGING ct_lines = lt_sql_lines ).
    fill_from( CHANGING ct_lines = lt_sql_lines ).
    fill_where( CHANGING ct_lines = lt_sql_lines ).
    fill_group_by( CHANGING ct_lines = lt_sql_lines ).
    fill_having( CHANGING ct_lines = lt_sql_lines ).
    lt_sql_lines = VALUE #( BASE lt_sql_lines
      ( |)| )
      ( |SELECT COUNT(*) FROM +group_select| ) ).
    CONCATENATE LINES OF lt_sql_lines INTO lv_query SEPARATED BY cl_abap_char_utilities=>cr_lf.

    ro_query = NEW zcl_dbbr_sql_query_parser(
        iv_query                 = lv_query
        if_fill_log_for_messages = abap_false
      )->parse( ).
  ENDMETHOD.


  METHOD get_group_by_size_by_cte.

    DATA(lo_count_query) = create_count_query_for_cte( ).
    CHECK lo_count_query IS NOT INITIAL.

    zcl_dbbr_sql_query_exec=>execute_query(
      EXPORTING
        io_query              = lo_count_query
        iv_row_count          = mv_max_size
        if_show_progress_text = abap_false
      IMPORTING
        et_data_info          = DATA(lt_data_info)
        ev_execution_time     = DATA(lv_exec_time)
        ev_message            = DATA(lv_message)
        ev_message_type       = DATA(lv_message_type)
        er_data               = DATA(lr_t_result) ).

    zcl_dbbr_sql_query_exec=>get_single_value_from_result(
      EXPORTING
        it_result_info = lt_data_info
        ir_t_data      = lr_t_result
      IMPORTING
        ev_value       = rv_size ).
  ENDMETHOD.


  METHOD on_async_query_finished.
    DATA: lv_count TYPE zdbbr_no_of_lines.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    ASSIGN er_data->* TO <lt_data>.
    IF sy-subrc = 0 AND lines( <lt_data> ) > 1.
      lv_count = lines( <lt_data> ).
    ELSE.
      zcl_dbbr_sql_query_exec=>get_single_value_from_result(
        EXPORTING it_result_info = et_data_info
                  ir_t_data      = er_data
        IMPORTING ev_value       = lv_count ).
    ENDIF.

    RAISE EVENT count_query_finished
      EXPORTING
        ev_count = lv_count.
  ENDMETHOD.

ENDCLASS.
