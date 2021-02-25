"! <p class="shorttext synchronized" lang="en">Creates subroutine pool program for selection data from db</p>
CLASS zcl_dbbr_select_prog_creator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Creates new instance of select program</p>
    "!
    CLASS-METHODS create_program
      IMPORTING
        !if_only_create_count_logic TYPE abap_bool OPTIONAL
        !if_create_for_all          TYPE abap_bool OPTIONAL
        !is_association_target      TYPE zsat_cds_association OPTIONAL
        !it_select                  TYPE string_table
        !it_from                    TYPE string_table
        !it_where                   TYPE string_table OPTIONAL
        !it_order_by                TYPE string_table OPTIONAL
        !it_group_by                TYPE string_table OPTIONAL
        it_having                   TYPE string_table OPTIONAL
        !iv_max_size                TYPE i
      RETURNING
        VALUE(rr_instance)          TYPE REF TO zcl_dbbr_select_prog_creator
      RAISING
        zcx_dbbr_dyn_prog_generation .
    "! <p class="shorttext synchronized" lang="en">Determine during active aggregation</p>
    METHODS determine_size_for_group_by
      IMPORTING
        ir_t_data      TYPE REF TO data
      RETURNING
        VALUE(rv_size) TYPE zdbbr_no_of_lines
      RAISING
        zcx_dbbr_selection_common.
    "! <p class="shorttext synchronized" lang="en">Determines the size of existing entries</p>
    "!
    METHODS determine_size
      IMPORTING
        !ir_t_for_all  TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(rv_size) TYPE zdbbr_no_of_lines
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Returns SQL String for current select</p>
    "!
    METHODS get_select_sql
      RETURNING
        VALUE(rv_select_sql) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Selects data</p>
    "!
    METHODS select_data
      IMPORTING
        !ir_t_for_all  TYPE REF TO data OPTIONAL
      EXPORTING
        VALUE(et_data) TYPE table
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Set maximum number of rows</p>
    "!
    METHODS set_max_rows
      IMPORTING
        !iv_max_rows TYPE i .
    "! <p class="shorttext synchronized" lang="en">Update from clause</p>
    "!
    METHODS update_from
      IMPORTING
        it_from TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! List of Strings
    DATA mt_select TYPE string_table .
    "! List of Strings
    DATA mt_from TYPE string_table .
    "! List of Strings
    DATA mt_where TYPE string_table .
    "! List of Strings
    DATA mt_order_by TYPE string_table .
    "! List of Strings
    DATA mt_group_by TYPE string_table .
    DATA mt_having TYPE string_table.
    DATA mv_max_size TYPE i .
    DATA mv_class TYPE string .
    DATA mf_only_create_count_logic TYPE abap_bool .
    DATA mf_create_for_all TYPE abap_bool .
    "! Association Information for CDS View
    DATA ms_assocication_target TYPE zsat_cds_association .

    METHODS fill_having
      CHANGING
        ct_lines TYPE string_table.
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with from clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_from
      CHANGING
        !ct_lines TYPE string_table .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with group by clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_group_by
      CHANGING
        !ct_lines TYPE string_table .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with order by clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_order_by
      CHANGING
        !ct_lines TYPE string_table .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with select clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_select
      CHANGING
        !ct_lines TYPE string_table .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with where clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_where
      CHANGING
        !ct_lines TYPE string_table .

    "! <p class="shorttext synchronized" lang="en">Determines existing line count with CTE</p>
    METHODS get_group_by_size_by_cte
      RETURNING
        VALUE(rv_size) TYPE zdbbr_no_of_lines.
    "! <p class="shorttext synchronized" lang="en">Creates count query with CTE</p>
    METHODS create_count_query_for_cte
      RETURNING
        VALUE(ro_query) TYPE REF TO  zcl_dbbr_sql_query .
ENDCLASS.



CLASS zcl_dbbr_select_prog_creator IMPLEMENTATION.


  METHOD create_program.
    rr_instance = NEW zcl_dbbr_select_prog_creator( ).

    rr_instance->mf_only_create_count_logic = if_only_create_count_logic.
    rr_instance->mt_select   = it_select.
    rr_instance->mt_from     = it_from.
    rr_instance->mt_having   = it_having.
    rr_instance->mt_where    = it_where.
    rr_instance->mt_order_by = it_order_by.
    rr_instance->mt_group_by = it_group_by.
    rr_instance->mv_max_size = iv_max_size.
  ENDMETHOD.

  METHOD determine_size_for_group_by.
    DATA: lx_root TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_data> TYPE table.

***    IF sy-saprl > 751. " Common table expressions exist
***      rv_size = get_group_by_size_by_cte( ).
***    ELSE.

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
***    ENDIF.
  ENDMETHOD.

  METHOD determine_size.
    DATA: lx_root TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_for_all_data> TYPE table.

    TRY.
        SELECT COUNT( * )
          FROM (mt_from)
          WHERE (mt_where)
        INTO @rv_size.
      CATCH cx_root INTO lx_root.
        RAISE EXCEPTION TYPE zcx_dbbr_selection_common
          EXPORTING
            previous = lx_root.
    ENDTRY.

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
      ( |  +group_select as (| )
      ( || )
    ).

    fill_select( CHANGING ct_lines = lt_sql_lines ).
    fill_from( CHANGING ct_lines = lt_sql_lines ).
    fill_where( CHANGING ct_lines = lt_sql_lines ).
    fill_group_by( CHANGING ct_lines = lt_sql_lines ).
    fill_having( CHANGING ct_lines = lt_sql_lines ).
    lt_sql_lines = VALUE #( BASE lt_sql_lines
      ( |)| )
      ( |SELECT COUNT(*) FROM +group_select| )
    ).
    CONCATENATE LINES OF lt_sql_lines INTO lv_query SEPARATED BY cl_abap_char_utilities=>cr_lf.

    TRY.
        ro_query = NEW zcl_dbbr_sql_query_parser(
          iv_query                 = lv_query
          if_fill_log_for_messages = abap_false
        )->parse( ).
      CATCH zcx_dbbr_sql_query_error INTO DATA(lx_error).
        lx_error->zif_sat_exception_message~print( ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_group_by_size_by_cte.

    DATA(lo_count_query) = create_count_query_for_cte( ).
    CHECK lo_count_query IS NOT INITIAL.

    zcl_dbbr_sql_query_exec=>execute_query(
      EXPORTING
        io_query          = lo_count_query
        iv_row_count      = mv_max_size
      IMPORTING
        et_data_info      = DATA(lt_data_info)
        ev_execution_time = DATA(lv_exec_time)
        ev_message        = DATA(lv_message)
        ev_message_type   = DATA(lv_message_type)
        er_data           = DATA(lr_t_result)
    ).

    zcl_dbbr_sql_query_exec=>get_single_value_from_result(
      EXPORTING
        it_result_info = lt_data_info
        ir_t_data      = lr_t_result
      IMPORTING
        ev_value       = rv_size ).
  ENDMETHOD.

ENDCLASS.
