"! <p class="shorttext synchronized" lang="en">Database table/view searcher for Object browser</p>
CLASS zcl_dbbr_ob_dbtab_searcher DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_dbbr_ob_generic_searcher.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_object_searcher.

    "! <p class="shorttext synchronized" lang="en">CLASS CONSTRUCTOR</p>
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
    METHODS determine_grouping
        REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      c_base_table  TYPE string VALUE 'base',
      c_field_table TYPE string VALUE 'field'.

    CLASS-DATA gv_field_subquery TYPE string.

    DATA mv_field_filter_count TYPE i.
    DATA mv_entity_fieldname     TYPE string.
    DATA mv_raw_entity_fieldname TYPE string.

    "! <p class="shorttext synchronized" lang="en">Create filter for TYPE option</p>
    "!
    METHODS add_type_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FIELD option</p>
    METHODS add_field_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Fills the names of base table and field</p>
    METHODS get_base_table_and_field
      EXPORTING
        ev_base_table           TYPE string
        ev_entity_fieldname     TYPE string
        ev_raw_entity_fieldname TYPE string.
ENDCLASS.



CLASS zcl_dbbr_ob_dbtab_searcher IMPLEMENTATION.

  METHOD class_constructor.
    gv_field_subquery = |SELECT DISTINCT tablename | && c_cr_lf &&
                        | FROM { zif_dbbr_c_select_source_id=>zdbbr_i_tablefield } | && c_cr_lf &&
                        | WHERE |.
  ENDMETHOD.

  METHOD zif_dbbr_object_searcher~search.
    DATA: lf_has_type_option TYPE abap_bool,
          lv_base_table      TYPE string.

    get_base_table_and_field(
      IMPORTING ev_base_table           = lv_base_table
                ev_entity_fieldname     = mv_entity_fieldname
                ev_raw_entity_fieldname = mv_raw_entity_fieldname
    ).

    set_base_select_table(
        iv_entity     = lv_base_table
        iv_alias      = c_base_table
        it_parameters = VALUE #(
          ( param_name = 'p_language' param_value = zcl_dbbr_system_helper=>get_system_language( ) )
        )
    ).

    LOOP AT mo_search_query->get_options( ) ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

*.......... Find objects via its description
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_description.
          add_option_filter(
            iv_fieldname = mv_description_filter_field
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects with a certain responsible person
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_owner.
          add_option_filter(
            iv_fieldname = 'createdby'
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects which exist in a certain development package
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_package.
          add_option_filter(
            iv_fieldname = 'developmentpackage'
            it_values    = <ls_option>-value_range
          ).

*.......... Find only objects with a certain type
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_type.
          IF lines( <ls_option>-value_range ) = 1.
            DATA(ls_option) = <ls_option>-value_range[ 1 ].

          ENDIF.

*.......... Find objects by field
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_field.
          add_field_filter( <ls_option>-value_range ).

      ENDCASE.
    ENDLOOP.

    IF mo_search_query->has_search_string( ).
      add_filter( VALUE #( sqlfieldname = |{ c_base_table }~{ mv_entity_fieldname }|
                           option       = mo_search_query->mv_search_option
                           sign         = 'I'
                           low          = mo_search_query->mv_search_string ) ).
    ENDIF.

    add_select_field( iv_fieldname = mv_entity_fieldname iv_fieldname_alias = 'entity_id' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = mv_raw_entity_fieldname iv_fieldname_alias = 'entity_id_raw' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'description' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'createdby' iv_fieldname_alias = 'created_by' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'developmentpackage' iv_fieldname_alias = 'devclass' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'type' iv_fieldname_alias = 'entity_type' iv_entity = c_base_table ).

    add_order_by( iv_fieldname = mv_entity_fieldname iv_entity = c_base_table  ).

    new_and_cond_list( ).

    search( ).
    rt_result = mt_result.
  ENDMETHOD.

  METHOD determine_grouping.
    CHECK mo_search_query->ms_search_engine_params-use_and_cond_for_options = abap_true.

***.. Excluding would break the relational division logic and would lead to unreliable results
***    CHECK mf_excluding_found = abap_false.

    IF NOT ( mv_field_filter_count > 1 ).
      RETURN.
    ENDIF.

    add_group_by_clause( |{ c_base_table }~{ mv_entity_fieldname }| ).
    add_group_by_clause( |{ c_base_table }~{ mv_raw_entity_fieldname }| ).
    add_group_by_clause( |{ c_base_table }~description| ).
    add_group_by_clause( |{ c_base_table }~createdby| ).
    add_group_by_clause( |{ c_base_table }~developmentpackage| ).
    add_group_by_clause( |{ c_base_table }~type| ).

    IF mv_field_filter_count > 1.
      add_having_clause( iv_field = |{ c_field_table }~fieldname| iv_counter_compare = mv_field_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD add_type_option_filter.
    DATA: lt_type_filters TYPE zif_dbbr_ty_object_browser=>ty_t_value_range.

    LOOP AT it_values INTO DATA(ls_value).
      CASE ls_value-low.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-table.
          ls_value-low = zif_dbbr_c_entity_type=>table.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-view.
          ls_value-low = zif_dbbr_c_entity_type=>view.

      ENDCASE.

      lt_type_filters = VALUE #( BASE lt_type_filters ( ls_value ) ).
    ENDLOOP.

    add_option_filter(
        iv_fieldname = 'type'
        it_values    = lt_type_filters
    ).
  ENDMETHOD.


  METHOD get_base_table_and_field.
    DATA: lf_set_default_table TYPE abap_bool.

    IF mo_search_query->has_options( ).
      DATA(ls_type_option) = mo_search_query->get_option( zif_dbbr_c_object_browser=>c_search_option-by_type ).

      IF ls_type_option IS NOT INITIAL AND lines( ls_type_option-value_range ) = 1.
        DATA(ls_type_value) = ls_type_option-value_range[ 1 ].

        IF ls_type_value-low = zif_dbbr_c_object_browser=>c_type_option_value-table.
          ev_base_table = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_databasetable }| ).
          ev_entity_fieldname =
          ev_raw_entity_fieldname = 'tablename'.
        ELSEIF ls_type_value-low = zif_dbbr_c_object_browser=>c_type_option_value-view.
          ev_base_table = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_databaseview }| ).
          ev_entity_fieldname =
          ev_raw_entity_fieldname = 'viewname'.
        ENDIF.

      ELSE.
        lf_set_default_table = abap_true.
      ENDIF.
    ELSE.
      lf_set_default_table = abap_true.
    ENDIF.

    IF lf_set_default_table = abap_true.
      ev_base_table = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_databasetablesandviews }| ).
      ev_entity_fieldname = 'entity'.
      ev_raw_entity_fieldname = 'entityraw'.
    ENDIF.
  ENDMETHOD.


  METHOD add_field_filter.
    add_join_table(
        iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_tablefield }| ) }|
        iv_alias      = |{ c_field_table }|
        it_fields     = VALUE #(
          ( field = 'tablename' ref_field = mv_entity_fieldname ref_table_alias = c_base_table )
        )
    ).

    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).

    create_not_in_filter(
        iv_subquery_fieldname = 'fieldname'
        iv_fieldname          = |{ c_base_table }~tablename|
        it_excluding          = lt_excluding
        iv_subquery           = gv_field_subquery
    ).

    add_option_filter(
        iv_fieldname = |{ c_field_table }~fieldname|
        it_values    = it_values
    ).

    mv_field_filter_count = lines( it_values ).
  ENDMETHOD.

ENDCLASS.
