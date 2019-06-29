"! <p class="shorttext synchronized" lang="en">CDS View Searcher for Object Browser</p>
CLASS zcl_dbbr_ob_cds_searcher DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_dbbr_ob_generic_searcher.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_object_searcher.
    "! <p class="shorttext synchronized" lang="en">CLASS-CONSTRUCTOR</p>
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
    METHODS determine_grouping
        REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA gv_field_subquery TYPE string.
    CLASS-DATA gv_anno_subquery TYPE string.
    CLASS-DATA gv_select_from_subquery TYPE string.
    CLASS-DATA gv_assoc_subquery TYPE string.

    CONSTANTS:
      c_base_alias                TYPE string VALUE 'base' ##NO_TEXT,
      c_anno_alias                TYPE string VALUE 'anno' ##NO_TEXT,
      c_extension_view_alias      TYPE string VALUE 'ext' ##no_text,
      c_api_alias                 TYPE string VALUE 'api' ##NO_TEXT,
      c_select_from_alias         TYPE string VALUE 'frompart' ##NO_TEXT,
      c_parameterized_view_alias  TYPE string VALUE 'paramviews' ##NO_TEXT,
      c_used_in_association_alias TYPE string VALUE 'associationusage' ##NO_TEXT,
      c_field_alias               TYPE string VALUE 'field' ##NO_TEXT.

    DATA mv_field_filter_count TYPE i.
    DATA mv_anno_filter_count TYPE i.
    DATA mv_from_filter_count TYPE i.
    DATA mv_assoc_filter_count TYPE i.

    "! <p class="shorttext synchronized" lang="en">Create filter for TYPE option</p>
    "!
    METHODS add_type_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for ANNO option</p>
    "!
    METHODS add_anno_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FIELD option</p>
    METHODS add_field_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for ASSOC option</p>
    METHODS add_association_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FROM option</p>
    METHODS add_from_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for API option</p>
    METHODS add_api_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.

    "! <p class="shorttext synchronized" lang="en">Adds extensions filter to query</p>
    METHODS add_extensions_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Add API state information for results</p>
    METHODS add_api_state_info.
ENDCLASS.



CLASS zcl_dbbr_ob_cds_searcher IMPLEMENTATION.
  METHOD class_constructor.
    gv_field_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                        | FROM { zif_dbbr_c_select_source_id=>zdbbr_i_cdsviewfield } | && c_cr_lf &&
                        | WHERE |.
    gv_anno_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                       | FROM { zif_dbbr_c_select_source_id=>zdbbr_i_cdsannotation } | && c_cr_lf &&
                       | WHERE |.
    gv_assoc_subquery = |SELECT DISTINCT ddlname | && c_cr_lf &&
                        | FROM { zif_dbbr_c_select_source_id=>zdbbr_i_associatedincds } | && c_cr_lf &&
                        | WHERE |.
    gv_select_from_subquery = |SELECT DISTINCT ddlviewname | && c_cr_lf &&
                              | FROM { zif_dbbr_c_select_source_id=>zdbbr_i_cdsfrompartentity } | && c_cr_lf &&
                              | WHERE |.
  ENDMETHOD.

  METHOD zif_dbbr_object_searcher~search.

    set_base_select_table(
        iv_entity = zif_dbbr_c_select_source_id=>zdbbr_i_cdsentity
        iv_alias  = c_base_alias
    ).

    add_select_field( iv_fieldname = 'entityid' iv_fieldname_alias = 'entity_id' iv_entity = c_base_alias ).
    IF sy-saprl >= 751.
      add_select_field( iv_fieldname = 'sourcetype' iv_fieldname_alias = 'source_type' iv_entity = c_base_alias ).
    ENDIF.
    add_select_field( iv_fieldname = 'ddlname'  iv_fieldname_alias = 'secondary_entity_id' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'createdby' iv_fieldname_alias = 'created_by' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'rawentityid' iv_fieldname_alias = 'entity_id_raw' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'description' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'developmentpackage' iv_fieldname_alias = 'devclass' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'type' iv_entity = c_base_alias iv_fieldname_alias = 'entity_type' ).

    add_order_by( iv_fieldname = 'entityid' iv_entity = c_base_alias  ).

    IF mo_search_query->has_search_string( ).
      new_and_cond_list( ).
      add_filter( VALUE #( sqlfieldname = |{ c_base_alias }~entityid|
                           option       = mo_search_query->mv_search_option
                           sign         = 'I'
                           low          = mo_search_query->mv_search_string ) ).

      new_or_cond_list( ).
      add_filter( VALUE #( sqlfieldname = |{ c_base_alias }~ddlname|
                           option       = mo_search_query->mv_search_option
                           sign         = 'I'
                           low          = mo_search_query->mv_search_string ) ).
      new_and_cond_list( ).
    ENDIF.

    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).
      CASE <ls_option>-option.

*.......... Find views which have a certain extension
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_extensions.
          add_extensions_filter( EXPORTING it_values = <ls_option>-value_range ).

*.......... Find views via its description
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_description.
          add_option_filter(
            iv_fieldname = mv_description_filter_field
            it_values    = <ls_option>-value_range
          ).

*.......... Find views with a certain responsible person
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_owner.
          add_option_filter(
            iv_fieldname = 'createdby'
            it_values    = <ls_option>-value_range
          ).

*.......... Find views which exist in a certain development package
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_package.
          add_option_filter(
            iv_fieldname = 'developmentpackage'
            it_values    = <ls_option>-value_range
          ).

*.......... Find views regarding the release status of the cds view
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_api.
          DATA(lv_api_state_table) = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_apistates }| ).
          add_join_table(
              iv_join_table = |{ lv_api_state_table }|
              iv_alias      = c_api_alias
              it_fields     = VALUE #(
                ( field = 'objectname' ref_field = 'ddlname' ref_table_alias = c_base_alias )
              )
              it_filter     = VALUE #(
                ( fieldname = 'objecttype' tabname_alias = CONV #( c_api_alias ) value = 'DDLS'  )
              )
          ).
          add_api_option_filter( it_values = <ls_option>-value_range ).

*.......... Find views where the filter exists in the FROM part of the cds view
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_select_from.
          add_from_option_filter( <ls_option>-value_range ).

*.......... Find views which have a certain annotation
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_anno.
          add_anno_option_filter(
            it_values = <ls_option>-value_range
          ).

*.......... Find views that are parameterized
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_params.
          add_join_table(
              iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsviewwithparameter }| ) }|
              iv_alias      = c_parameterized_view_alias
              it_fields     = VALUE #(
                ( field = 'entityid' ref_field = 'entityid' ref_table_alias = c_base_alias )
              )
          ).

*.......... Find views which have a certain field a component
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_field.
          add_field_option_filter( <ls_option>-value_range ).

*.......... Find views where an entity is used as an association
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_association.
          add_association_option_filter( <ls_option>-value_range ).

*.......... Find views for a certain type e.g. function, hierarchy, view
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_type.
          IF sy-saprl >= 750.
            add_type_option_filter( it_values = <ls_option>-value_range ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

    new_and_cond_list( ).

    search( ).

    IF mt_result IS NOT INITIAL AND mo_search_query->ms_search_engine_params-with_api_state = abap_true.
      add_api_state_info( ).
    ENDIF.

    rt_result = mt_result.
  ENDMETHOD.


  METHOD  determine_grouping.
    CHECK mo_search_query->ms_search_engine_params-use_and_cond_for_options = abap_true.

****.. Excluding would break the relational division logic and would lead to unreliable results
****    CHECK mf_excluding_found = abap_false.
    IF NOT ( mv_anno_filter_count > 1 OR
             mv_field_filter_count > 1 OR
             mv_assoc_filter_count > 1 OR
             mv_from_filter_count > 1 ).
      RETURN.
    ENDIF.

*.. Create grouping clause
    add_group_by_clause( |{ c_base_alias }~entityid| ).
    add_group_by_clause( |{ c_base_alias }~rawentityid| ).
    add_group_by_clause( |{ c_base_alias }~ddlname| ).
    add_group_by_clause( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsviewt }~description| ).
    add_group_by_clause( |{ c_base_alias }~createdby| ).
    add_group_by_clause( |{ c_base_alias }~developmentpackage| ).

    IF mv_anno_filter_count > 1.
      add_having_clause( iv_field = |{ c_anno_alias }~name| iv_counter_compare = mv_anno_filter_count ).
    ENDIF.

    IF mv_assoc_filter_count > 1.
      add_having_clause( iv_field = |{ c_used_in_association_alias }~usedentity| iv_counter_compare = mv_assoc_filter_count ).
    ENDIF.

    IF mv_from_filter_count > 1.
      add_having_clause( iv_field = |{ c_select_from_alias }~sourceentity| iv_counter_compare = mv_from_filter_count ).
    ENDIF.

    IF mv_field_filter_count > 1.
      add_having_clause( iv_field = |{ c_field_alias }~fieldname| iv_counter_compare = mv_field_filter_count ).
    ENDIF.

  ENDMETHOD.

  METHOD add_anno_option_filter.
    DATA: lt_or_seltab TYPE zdbbr_or_seltab_sql_t.


    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding)
    ).

*.. Create sub query for negated annotation key/value pairs
    IF lt_excluding IS NOT INITIAL.
      LOOP AT lt_excluding ASSIGNING FIELD-SYMBOL(<ls_excluding>).
        DATA(lt_and_seltab) = VALUE zdbbr_seltab_t(
           ( sqlfieldname = 'name'
             sign         = zif_dbbr_c_options=>including
             option       = <ls_excluding>-option
             low          = <ls_excluding>-low )
        ).
        IF <ls_excluding>-high IS NOT INITIAL.
          lt_and_seltab = VALUE #(
            BASE lt_and_seltab
            ( sqlfieldname = 'value'
              sign         = zif_dbbr_c_options=>including
              option       = <ls_excluding>-option2
              low          = <ls_excluding>-high )
          ).
        ENDIF.
        lt_or_seltab = VALUE #( BASE lt_or_seltab ( values = lt_and_seltab ) ).
      ENDLOOP.

      create_not_in_filter_for_where(
          it_where     = zcl_dbbr_where_clause_builder=>create_or_condition( lt_or_seltab )
          iv_fieldname = |{ c_base_alias }~entityid|
          iv_subquery  = gv_anno_subquery
      ).
    ENDIF.

*.. Add filters for including annotation key/value pairs
    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsannotation }| ) }|
          iv_alias      = c_anno_alias
          it_fields     = VALUE #(
            ( field = 'entityid' ref_field = 'entityid' ref_table_alias = c_base_alias )
          )
      ).
      mv_anno_filter_count = lines( lt_including ).
      new_and_cond_list( ).
      LOOP AT lt_including INTO DATA(ls_filter_value).
        add_filter( VALUE #( sqlfieldname = |{ c_anno_alias }~name| sign = ls_filter_value-sign option = ls_filter_value-option low = ls_filter_value-low )  ).

        IF ls_filter_value-high IS NOT INITIAL.
          add_filter( VALUE #( sqlfieldname = |{ c_anno_alias }~value| sign = ls_filter_value-sign2 option = ls_filter_value-option2 low = ls_filter_value-high )  ).
        ENDIF.
        new_or_cond_list( ).
      ENDLOOP.

      new_and_cond_list( ).
    ENDIF.
  ENDMETHOD.

  METHOD add_api_option_filter.
    DATA: lt_api_filters TYPE zif_dbbr_ty_object_browser=>ty_t_value_range.

    LOOP AT it_values INTO DATA(ls_value).

      CASE ls_value-low.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-released.
          CONTINUE.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-custom_fields.
          ls_value-low = zif_dbbr_c_cds_api_state=>add_custom_fields.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-key_user OR
             zif_dbbr_c_object_browser=>c_api_option_value-key_user_long.
          ls_value-low = zif_dbbr_c_cds_api_state=>use_in_key_user_apps.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-remote_api OR
             zif_dbbr_c_object_browser=>c_api_option_value-remote_api_long.
          ls_value-low = zif_dbbr_c_cds_api_state=>use_as_remote_api.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-cloud_user OR
             zif_dbbr_c_object_browser=>c_api_option_value-cloud_user_long.
          ls_value-low = zif_dbbr_c_cds_api_state=>use_in_sap_cloud_platform.

      ENDCASE.

      lt_api_filters = VALUE #( BASE lt_api_filters ( ls_value ) ).
    ENDLOOP.

    CHECK lt_api_filters IS NOT INITIAL.

    add_option_filter(
        iv_fieldname = |{ c_api_alias }~FilterValue|
        it_values    = lt_api_filters
    ).
  ENDMETHOD.


  METHOD add_type_option_filter.
    DATA: lt_type_filters TYPE zif_dbbr_ty_object_browser=>ty_t_value_range.

    LOOP AT it_values INTO DATA(ls_value).
      CASE ls_value-low.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-function.
          ls_value-low = zif_dbbr_c_cds_view_type=>table_function.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-hierarchy.
          ls_value-low = zif_dbbr_c_cds_view_type=>hierarchy.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-view.
          ls_value-low = zif_dbbr_c_cds_view_type=>view.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-abstract_entity.
          ls_value-low = zif_dbbr_c_cds_view_type=>abstract_entity.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-custom_entity.
          ls_value-low = zif_dbbr_c_cds_view_type=>custom_entity.
      ENDCASE.

      lt_type_filters = VALUE #( BASE lt_type_filters ( ls_value ) ).
    ENDLOOP.

    add_option_filter(
        iv_fieldname = 'sourcetype'
        it_values    = lt_type_filters
    ).
  ENDMETHOD.


  METHOD add_association_option_filter.
    DATA(lv_association_usage_table) = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_associatedincds }| ).
    add_join_table(
        iv_join_table = |{ lv_association_usage_table }|
        iv_alias      = c_used_in_association_alias
        it_fields     = VALUE #(
          ( field = 'ddlname' ref_field = 'ddlname' ref_table_alias = c_base_alias )
        )
    ).

    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).

    create_not_in_filter(
        iv_subquery_fieldname = 'usedentity'
        iv_fieldname          = |{ c_base_alias }~ddlname|
        it_excluding          = lt_excluding
        iv_subquery           = gv_assoc_subquery
    ).

    add_option_filter(
        iv_fieldname = |{ c_used_in_association_alias }~usedentity|
        it_values    = it_values
    ).
    mv_assoc_filter_count = lines( it_values ).
  ENDMETHOD.

  METHOD add_field_option_filter.
    add_join_table(
        iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsviewfield }| ) }|
        iv_alias      = c_field_alias
        it_fields     = VALUE #(
          ( field = 'entityid' ref_field = 'entityid' ref_table_alias = c_base_alias )
        )
    ).

    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).

    create_not_in_filter(
        iv_subquery_fieldname = 'fieldname'
        iv_fieldname          = |{ c_base_alias }~entityid|
        it_excluding          = lt_excluding
        iv_subquery           = gv_field_subquery
    ).

    add_option_filter(
        iv_fieldname = |{ c_field_alias }~fieldname|
        it_values    = lt_including
    ).
    mv_field_filter_count = lines( lt_including ).
  ENDMETHOD.

  METHOD add_from_option_filter.
    DATA(lv_from_part_table) = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsfrompartentity }| ).
    add_join_table(
        iv_join_table = |{ lv_from_part_table }|
        iv_alias      = c_select_from_alias
        it_fields     = VALUE #( ( field = 'ddlviewname' ref_field = 'viewname' ref_table_alias = c_base_alias ) )
    ).

    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).

    create_not_in_filter(
        iv_subquery_fieldname = 'sourceentity'
        iv_fieldname          = |{ c_base_alias }~viewname|
        it_excluding          = lt_excluding
        iv_subquery           = gv_select_from_subquery
    ).

    add_option_filter(
        iv_fieldname = |{ c_select_from_alias }~sourceentity|
        it_values    = it_values
    ).
    mv_from_filter_count = lines( it_values ).
  ENDMETHOD.


  METHOD add_extensions_filter.
    DATA(lv_and_or) = ``.
    DATA(lt_ext_join_filter) = VALUE zdbbr_join_filter_cond_t( ).

    LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_value_range>).

      lt_ext_join_filter = VALUE #( BASE lt_ext_join_filter
        ( fieldname     = 'entityname'
          operator      = COND #( WHEN <ls_value_range>-option = 'CP' THEN zif_dbbr_c_operator=>like ELSE zif_dbbr_c_operator=>equals )
          value         = <ls_value_range>-low
          value_type    = zif_dbbr_c_join_cond_val_type=>typed_input
          tabname_alias = c_extension_view_alias
          and_or        = lv_and_or )
      ).
      lv_and_or = 'OR'.
    ENDLOOP.

    add_join_table(
        iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsextensionviews }| ) }|
        iv_alias      = c_extension_view_alias
        it_fields     = VALUE #(
          ( field = 'parentddl' ref_field = 'ddlname' ref_table_alias = c_base_alias )
        )
        it_filter     = lt_ext_join_filter
    ).
  ENDMETHOD.


  METHOD add_api_state_info.
    SELECT DISTINCT objectname AS entity_id, filtervalue AS api_state
      FROM zdbbr_i_apistates
      FOR ALL ENTRIES IN @mt_result
      WHERE objectname = @mt_result-secondary_entity_id
        AND filtervalue <> '4'
    INTO TABLE @DATA(lt_api_states).

    CHECK sy-subrc = 0.

    LOOP AT mt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
      <ls_result>-api_state = VALUE #( lt_api_states[ entity_id = <ls_result>-secondary_entity_id ]-api_state OPTIONAL ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
