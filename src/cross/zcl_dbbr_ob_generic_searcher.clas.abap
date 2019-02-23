"! <p class="shorttext synchronized" lang="en">Generic Searcher for Object Browser</p>
CLASS zcl_dbbr_ob_generic_searcher DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_c_object_browser.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        ir_query TYPE REF TO zcl_dbbr_object_search_query.
  PROTECTED SECTION.
    DATA mr_search_query TYPE REF TO zcl_dbbr_object_search_query.
    DATA mt_result TYPE zdbbr_entity_t.
    DATA mt_criteria TYPE zdbbr_seltab_t.
    DATA mt_criteria_or TYPE zdbbr_or_seltab_sql_t.
    DATA mt_criteria_and TYPE zdbbr_and_seltab_t.
    DATA mt_where TYPE zdbbr_string_t.
    DATA mt_select TYPE zdbbr_string_t.
    DATA mt_order_by TYPE zdbbr_string_t.
    DATA mt_from TYPE TABLE OF string.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mf_search_executed TYPE abap_bool.
    DATA mv_description_filter_field TYPE string.

    "! <p class="shorttext synchronized" lang="en">Start new criteria table connected with OR</p>
    "!
    METHODS new_or_cond_list.
    "! <p class="shorttext synchronized" lang="en">Start new criteria table connected with AND</p>
    "!
    METHODS new_and_cond_list.

    "! <p class="shorttext synchronized" lang="en">Starts the object search</p>
    "!
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
    "!
    METHODS add_select_field
      IMPORTING
        iv_fieldname       TYPE string
        iv_fieldname_alias TYPE string OPTIONAL
        iv_entity          TYPE string OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Add filter to the search</p>
    "!
    METHODS add_filter
      IMPORTING
        is_filter TYPE zdbbr_seltab.

    "! <p class="shorttext synchronized" lang="en">Add base select entity</p>
    "!
    METHODS set_base_select_table
      IMPORTING
        iv_entity     TYPE string
        iv_alias      TYPE string OPTIONAL
        it_parameters TYPE zdbbr_table_parameter_t OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Add Join table to the filter</p>
    "!
    METHODS add_join_table
      IMPORTING
        iv_join_table TYPE tabname
        iv_alias      TYPE string OPTIONAL
        iv_join_type  TYPE zdbbr_jointype DEFAULT zif_dbbr_c_join_types=>inner_join
        it_parameters TYPE zdbbr_table_parameter_t OPTIONAL
        it_fields     TYPE zdbbr_join_field_cond_t
        it_filter     TYPE zdbbr_join_filter_cond_t OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Create FROM clause for SQL select</p>
    "!
    METHODS create_from_clause.
    "! <p class="shorttext synchronized" lang="en">Create Where clause for SQL</p>
    "!
    METHODS create_where_clause.
    "! <p class="shorttext synchronized" lang="en">Create Order By clause for SQL</p>
    "!
    METHODS create_order_by_clause.
    "! <p class="shorttext synchronized" lang="en">Creates SELECT clause for SQL</p>
    "!
    METHODS create_select_clause.
    "! <p class="shorttext synchronized" lang="en">Add filter(s) for search option</p>
    "!
    METHODS add_option_filter
      IMPORTING
        iv_fieldname    TYPE string
        iv_sql_function TYPE zdbbr_sql_function OPTIONAL
        it_values       TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Add sort order for field</p>
    "!
    METHODS add_order_by
      IMPORTING
        iv_fieldname  TYPE string
        iv_entity     TYPE string
        if_descending TYPE abap_bool OPTIONAL.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_ob_generic_searcher IMPLEMENTATION.

  METHOD add_order_by.
    mt_order_by = VALUE #( BASE mt_order_by
      ( |{ iv_entity }~{ iv_fieldname } { COND #( WHEN if_descending = abap_true THEN 'DESCENDING' ELSE 'ASCENDING' ) }| )
    ).
  ENDMETHOD.


  METHOD constructor.
    mr_search_query = ir_query.
    mv_description_filter_field = COND #( WHEN sy-saprl >= 751 THEN 'descriptionupper' ELSE 'description' ).
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
  ENDMETHOD.

  METHOD add_join_table.
    ms_join_def-tables = VALUE #( BASE ms_join_def-tables
      ( add_table         = iv_join_table
        add_table_alias   = COND #( WHEN iv_alias IS NOT INITIAL THEN iv_alias ELSE iv_join_table )
        join_type         = iv_join_type
        field_conditions  = it_fields
        filter_conditions = it_filter
        parameters        = it_parameters )
    ).
  ENDMETHOD.

  METHOD add_filter.
    mt_criteria = VALUE #(
      BASE mt_criteria
      ( is_filter )
    ).
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

  METHOD search.
    IF mf_search_executed = abap_false.
      create_select_clause( ).
      create_from_clause( ).
      create_where_clause( ).
      create_order_by_clause( ).
    ENDIF.

    mf_search_executed = abap_true.

    TRY.
        SELECT DISTINCT (mt_select)
          FROM (mt_from)
          WHERE (mt_where)
          ORDER BY (mt_order_by)
        INTO CORRESPONDING FIELDS OF TABLE @mt_result
          UP TO @mr_search_query->mv_max_rows ROWS.
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

ENDCLASS.
