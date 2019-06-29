"! <p class="shorttext synchronized" lang="en">Generic Searcher for Object Browser</p>
CLASS zcl_dbbr_ob_generic_searcher DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_c_object_browser.
    INTERFACES zif_dbbr_c_join_cond_type.

    ALIASES c_field_cond
      FOR zif_dbbr_c_join_cond_type~field.
    ALIASES c_filter_cond
      FOR zif_dbbr_c_join_cond_type~filter.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        ir_query TYPE REF TO zcl_dbbr_object_search_query.
  PROTECTED SECTION.
    CONSTANTS c_cr_lf TYPE string VALUE cl_abap_char_utilities=>cr_lf.

    DATA mo_search_query TYPE REF TO zcl_dbbr_object_search_query.
    DATA mt_result TYPE zdbbr_entity_t.
    DATA mt_criteria TYPE zdbbr_seltab_t.
    DATA mt_criteria_or TYPE zdbbr_or_seltab_sql_t.
    DATA mt_criteria_and TYPE zdbbr_and_seltab_t.
    DATA mt_where TYPE zdbbr_string_t.
    DATA mt_select TYPE zdbbr_string_t.
    DATA mt_order_by TYPE zdbbr_string_t.
    DATA mt_group_by TYPE zdbbr_string_t.
    DATA mt_having TYPE zdbbr_string_t.
    DATA mt_from TYPE TABLE OF string.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mf_search_executed TYPE abap_bool.
    DATA mf_excluding_found TYPE abap_bool.
    DATA mv_description_filter_field TYPE string.

    "! <p class="shorttext synchronized" lang="en">Start new criteria table connected with OR</p>
    METHODS new_or_cond_list.
    "! <p class="shorttext synchronized" lang="en">Start new criteria table connected with AND</p>
    METHODS new_and_cond_list.

    "! <p class="shorttext synchronized" lang="en">Starts the object search</p>
    METHODS search
      RAISING
        zcx_dbbr_object_search.
    "! <p class="shorttext synchronized" lang="en">Get SQL name for CDS view</p>
    "! Depending on the current release the internal ddic sql views are taken
    "! instead of the cds view name
    METHODS get_cds_sql_name
      IMPORTING
        iv_entity_id        TYPE zdbbr_entity_id
      RETURNING
        VALUE(rv_ddic_view) TYPE string.

    "! <p class="shorttext synchronized" lang="en">Adds the given select field</p>
    METHODS add_select_field
      IMPORTING
        iv_fieldname       TYPE string
        iv_fieldname_alias TYPE string OPTIONAL
        iv_entity          TYPE string OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Add filter to the search</p>
    METHODS add_filter
      IMPORTING
        is_filter TYPE zdbbr_seltab.

    "! <p class="shorttext synchronized" lang="en">Add base select entity</p>
    METHODS set_base_select_table
      IMPORTING
        iv_entity     TYPE string
        iv_alias      TYPE string OPTIONAL
        it_parameters TYPE zdbbr_table_parameter_t OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Add Join table to the filter</p>
    METHODS add_join_table
      IMPORTING
        iv_join_table TYPE tabname
        iv_alias      TYPE string OPTIONAL
        iv_join_type  TYPE zdbbr_jointype DEFAULT zif_dbbr_c_join_types=>inner_join
        it_parameters TYPE zdbbr_table_parameter_t OPTIONAL
        it_conditions TYPE zdbbr_join_condition_data_t OPTIONAL
        it_fields     TYPE zdbbr_join_field_cond_t OPTIONAL
        it_filter     TYPE zdbbr_join_filter_cond_t OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Creates where condition table for given OR seltab</p>
    METHODS create_or_where_condition
      IMPORTING
        iv_fieldname    TYPE string
        iv_alt_option   TYPE ddoption OPTIONAL
        iv_alt_sign     TYPE ddsign OPTIONAL
        it_values       TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range
      RETURNING
        VALUE(rt_where) TYPE string_table.
    "! <p class="shorttext synchronized" lang="en">Create FROM clause for SQL select</p>
    METHODS create_from_clause.
    "! <p class="shorttext synchronized" lang="en">Create Where clause for SQL</p>
    METHODS create_where_clause.
    "! <p class="shorttext synchronized" lang="en">Create Order By clause for SQL</p>
    METHODS create_order_by_clause.
    "! <p class="shorttext synchronized" lang="en">Creates SELECT clause for SQL</p>
    METHODS create_select_clause.
    "! <p class="shorttext synchronized" lang="en">Add filter(s) for search option</p>
    METHODS add_option_filter
      IMPORTING
        iv_fieldname    TYPE string
        iv_sql_function TYPE zdbbr_sql_function OPTIONAL
        it_values       TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Add subquery filter for field</p>
    METHODS add_subquery_filter
      IMPORTING
        iv_fieldname TYPE string
        iv_sign      TYPE ddsign DEFAULT zif_dbbr_c_options=>including
        iv_option    TYPE ddoption
        iv_subquery  TYPE string.
    "! <p class="shorttext synchronized" lang="en">Adds new having clause for the given field</p>
    METHODS add_having_clause
      IMPORTING
        iv_field           TYPE string
        iv_counter_compare TYPE i.
    "! <p class="shorttext synchronized" lang="en">Add sort order for field</p>
    METHODS add_order_by
      IMPORTING
        iv_fieldname  TYPE string
        iv_entity     TYPE string
        if_descending TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Adds field to GROUP BY clause</p>
    METHODS add_group_by_clause
      IMPORTING
        iv_field TYPE string.
    "! <p class="shorttext synchronized" lang="en">Determine grouping state</p>
    METHODS determine_grouping.
    "! <p class="shorttext synchronized" lang="en">Splits including/excluding from given value range list</p>
    METHODS split_including_excluding
      IMPORTING
        it_values    TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range
      EXPORTING
        et_including TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range
        et_excluding TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Creates NOT IN filter for the given fieldname</p>
    METHODS create_not_in_filter_for_where
      IMPORTING
        it_where     TYPE string_table
        iv_fieldname TYPE string
        iv_subquery  TYPE string.
    "! <p class="shorttext synchronized" lang="en">Creates NOT IN where filter for the given fieldname</p>
    METHODS create_not_in_filter
      IMPORTING
        it_excluding          TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range
        iv_subquery_fieldname TYPE string
        iv_subquery           TYPE string
        iv_fieldname          TYPE string.
  PRIVATE SECTION.
    METHODS get_select_string
      RETURNING
        VALUE(rv_result) TYPE string.
ENDCLASS.



CLASS zcl_dbbr_ob_generic_searcher IMPLEMENTATION.

  METHOD constructor.
    mo_search_query = ir_query.
    mv_description_filter_field = COND #( WHEN sy-saprl >= 751 THEN 'descriptionupper' ELSE 'description' ).
  ENDMETHOD.

  METHOD search.
    IF mf_search_executed = abap_false.
      create_select_clause( ).
      create_from_clause( ).
      create_where_clause( ).
      create_order_by_clause( ).
      determine_grouping( ).
    ENDIF.

    mf_search_executed = abap_true.

    DATA(lv_max_rows) = COND #( WHEN mo_search_query->ms_search_engine_params-get_all = abap_true
                                    THEN 0
                                ELSE  mo_search_query->mv_max_rows + 1 ).

    TRY.
        IF mt_group_by IS NOT INITIAL.
          SELECT DISTINCT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            GROUP BY (mt_group_by)
            HAVING (mt_having)
            ORDER BY (mt_order_by)
          INTO CORRESPONDING FIELDS OF TABLE @mt_result
            UP TO @lv_max_rows ROWS.
        ELSE.
          SELECT DISTINCT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            ORDER BY (mt_order_by)
          INTO CORRESPONDING FIELDS OF TABLE @mt_result
            UP TO @lv_max_rows ROWS.
        ENDIF.
        DATA(lv_sql) = get_select_string( ).
      CATCH cx_sy_open_sql_error INTO DATA(lx_sql_error).
        RAISE EXCEPTION TYPE zcx_dbbr_object_search
          EXPORTING
            previous = lx_sql_error.
    ENDTRY.
  ENDMETHOD.

  METHOD create_order_by_clause.
    CHECK: mt_order_by IS NOT INITIAL,
           lines( mt_order_by ) > 1.

    DATA(lv_line_count) = lines( mt_order_by ).
    LOOP AT mt_order_by ASSIGNING FIELD-SYMBOL(<lv_order_by>).
      IF sy-tabix <> lv_line_count.
        <lv_order_by> = <lv_order_by> && |, |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_not_in_filter_for_where.
    DATA(lv_excluding_subquery) = iv_subquery.
    LOOP AT it_where ASSIGNING FIELD-SYMBOL(<ls_where_excl>).
      lv_excluding_subquery = |{ lv_excluding_subquery }{ c_cr_lf }{ <ls_where_excl> }|.
    ENDLOOP.

    add_subquery_filter(
        iv_fieldname = iv_fieldname
        iv_subquery  = lv_excluding_subquery
        iv_option    = zif_dbbr_c_options=>not_in_subquery
    ).
  ENDMETHOD.

  METHOD create_not_in_filter.

    CHECK it_excluding IS NOT INITIAL.

    DATA(lt_excl_for_where) = create_or_where_condition(
      iv_fieldname = iv_subquery_fieldname
      iv_alt_sign  = zif_dbbr_c_options=>including
      it_values    = it_excluding
    ).

    DATA(lv_excluding_subquery) = iv_subquery.
    LOOP AT lt_excl_for_where ASSIGNING FIELD-SYMBOL(<ls_where_excl>).
      lv_excluding_subquery = |{ lv_excluding_subquery }{ c_cr_lf }{ <ls_where_excl> }|.
    ENDLOOP.

    add_subquery_filter(
        iv_fieldname = iv_fieldname
        iv_subquery  = lv_excluding_subquery
        iv_option    = zif_dbbr_c_options=>not_in_subquery
    ).

  ENDMETHOD.

  METHOD determine_grouping.
    RETURN.
  ENDMETHOD.

  METHOD add_order_by.
    mt_order_by = VALUE #( BASE mt_order_by
      ( |{ iv_entity }~{ iv_fieldname } { COND #( WHEN if_descending = abap_true THEN 'DESCENDING' ELSE 'ASCENDING' ) }| )
    ).
  ENDMETHOD.



  METHOD get_cds_sql_name.
    IF sy-saprl < 750.
      rv_ddic_view = zcl_dbbr_cds_view_factory=>read_ddl_ddic_view( iv_ddl_name = |{ iv_entity_id }| ).
    ELSE.
      rv_ddic_view = iv_entity_id.
    ENDIF.
  ENDMETHOD.


  METHOD add_option_filter.
    mt_criteria = VALUE #(
      BASE mt_criteria
      ( LINES OF VALUE #(
          FOR value IN it_values
          ( sqlfieldname = iv_fieldname
            sql_function = iv_sql_function
            sign         = value-sign
            option       = value-option
            low          = value-low ) )
      )
    ).

    CHECK mf_excluding_found = abap_false.

    IF line_exists( it_values[ sign = zif_dbbr_c_options=>excluding ] ).
      mf_excluding_found = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_subquery_filter.
    mt_criteria = VALUE #(
      BASE mt_criteria
      ( sqlfieldname = iv_fieldname
        sign         = iv_sign
        option       = iv_option
        subquery     = iv_subquery )
    ).
  ENDMETHOD.

  METHOD add_join_table.
    ms_join_def-tables = VALUE #( BASE ms_join_def-tables
      ( add_table         = iv_join_table
        add_table_alias   = COND #( WHEN iv_alias IS NOT INITIAL THEN iv_alias ELSE iv_join_table )
        join_type         = iv_join_type
        conditions        = it_conditions
        field_conditions  = it_fields
        filter_conditions = it_filter
        parameters        = it_parameters )
    ).
  ENDMETHOD.

  METHOD create_or_where_condition.
    DATA(lt_options) = VALUE zdbbr_seltab_t(
      ( LINES OF VALUE #(
          FOR value IN it_values
          LET opt = COND #( WHEN iv_alt_option IS NOT INITIAL THEN iv_alt_option ELSE value-option )
              sign = COND #( WHEN iv_alt_sign IS NOT INITIAL THEN iv_alt_sign ELSE value-sign ) IN
          ( sqlfieldname = iv_fieldname
            sign         = sign
            option       = opt
            low          = value-low ) )
      )
    ).

    DATA(lt_or_seltab) = VALUE zdbbr_or_seltab_sql_t(
     ( values = lt_options )
    ).
    rt_where = zcl_dbbr_where_clause_builder=>create_or_condition( lt_or_seltab ).
  ENDMETHOD.

  METHOD add_filter.
    mt_criteria = VALUE #(
      BASE mt_criteria
      ( is_filter )
    ).

    IF is_filter-sign = zif_dbbr_c_options=>excluding.
      mf_excluding_found = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_select_field.
    mt_select = VALUE #( BASE mt_select
      ( COND #( WHEN iv_entity IS NOT INITIAL THEN |{ iv_entity }~| ) &&
        |{ iv_fieldname }| &&
        COND #( WHEN iv_fieldname_alias IS NOT INITIAL THEN | AS { iv_fieldname_alias }| ) )
    ).
  ENDMETHOD.

  METHOD create_from_clause.
    mt_from = zcl_dbbr_join_helper=>build_from_clause_for_join_def(
      is_join_def        = ms_join_def
    ).
  ENDMETHOD.

  METHOD set_base_select_table.
    ms_join_def-primary_table = iv_entity.
    ms_join_def-primary_table_alias = COND #( WHEN iv_alias IS NOT INITIAL THEN iv_alias ELSE iv_entity ).
    ms_join_def-parameters = it_parameters.
  ENDMETHOD.

  METHOD add_having_clause.
    DATA(lv_and_operator) = COND #( WHEN mt_having IS NOT INITIAL THEN |AND | ELSE |    | ).

    mt_having = VALUE #( BASE mt_having
      ( |{ lv_and_operator }COUNT( DISTINCT { iv_field } ) >= { iv_counter_compare }| )
    ).
  ENDMETHOD.

  METHOD add_group_by_clause.
    IF mt_group_by IS NOT INITIAL.
      DATA(lr_last_group_by) = REF #( mt_group_by[ lines( mt_group_by ) ] ).
      lr_last_group_by->* = |{ lr_last_group_by->* },|.
    ENDIF.

    mt_group_by = VALUE #( BASE mt_group_by ( iv_field ) ).
  ENDMETHOD.

  METHOD create_where_clause.

    CHECK mt_criteria_and IS NOT INITIAL.

    mt_where = zcl_dbbr_where_clause_builder=>create_and_condition(
        it_and_seltab = mt_criteria_and
    ).
  ENDMETHOD.

  METHOD create_select_clause.
    CHECK: mt_select IS NOT INITIAL,
           lines( mt_select ) > 1.

    DATA(lv_line_count) = lines( mt_select ).
    LOOP AT mt_select ASSIGNING FIELD-SYMBOL(<lv_select>).
      IF sy-tabix <> lv_line_count.
        <lv_select> = <lv_select> && |, |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD new_and_cond_list.
    IF mt_criteria_or IS NOT INITIAL.
      IF mt_criteria IS NOT INITIAL.
        new_or_cond_list( ).
      ENDIF.
      mt_criteria_and = VALUE #( BASE mt_criteria_and ( VALUE #( ( LINES OF mt_criteria_or ) ) ) ).
      CLEAR mt_criteria_or.
    ELSEIF mt_criteria IS NOT INITIAL.
      mt_criteria_and = VALUE #( BASE mt_criteria_and ( VALUE #( ( values = mt_criteria ) ) ) ).
      CLEAR mt_criteria.
    ENDIF.
  ENDMETHOD.

  METHOD new_or_cond_list.
    IF mt_criteria IS NOT INITIAL.
      mt_criteria_or = VALUE #( BASE mt_criteria_or ( values = mt_criteria ) ).
      CLEAR mt_criteria.
    ENDIF.
  ENDMETHOD.

  METHOD split_including_excluding.
    et_excluding = VALUE #( FOR excluding IN it_values WHERE ( sign = zif_dbbr_c_options=>excluding OR sign2 = zif_dbbr_c_options=>excluding ) ( excluding ) ).
    et_including = VALUE #( FOR including IN it_values WHERE ( sign = zif_dbbr_c_options=>including AND ( sign2 = zif_dbbr_c_options=>including OR sign2 = space ) ) ( including ) ).
  ENDMETHOD.


  METHOD get_select_string.
    FIELD-SYMBOLS: <lv_part> TYPE string.

    DEFINE add_part.
      IF &2 IS NOT INITIAL.
        rv_result = |{ rv_result }{ c_cr_lf }{ &1 } |.
        LOOP AT &2 ASSIGNING <lv_part>.
           rv_result = |{ rv_result }{ c_cr_lf }    { <lv_part> }|.
        ENDLOOP.
      ENDIF.
    END-OF-DEFINITION.

    add_part:
       'SELECT DISTINCT' mt_select,
       'FROM'            mt_from,
       'WHERE'           mt_where,
       'GROUP BY'        mt_group_by,
       'HAVING'          mt_having,
       'ORDER BY'        mt_order_by.
  ENDMETHOD.

ENDCLASS.
