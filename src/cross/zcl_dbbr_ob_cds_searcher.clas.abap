"! <p class="shorttext synchronized" lang="en">CDS View Searcher for Object Browser</p>
CLASS zcl_dbbr_ob_cds_searcher DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_dbbr_ob_generic_searcher.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_object_searcher.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_base_alias                TYPE string VALUE 'base' ##NO_TEXT,
      c_ddl_source_table          TYPE string VALUE 'source' ##NO_TEXT,
      c_api_alias                 TYPE string VALUE 'api' ##NO_TEXT,
      c_source_ddlview_alias      TYPE string VALUE 'sourceddlview' ##NO_TEXT,
      c_select_from_alias         TYPE string VALUE 'frompart' ##NO_TEXT,
      c_parameterized_view_alias  TYPE string VALUE 'paramviews' ##NO_TEXT,
      c_used_in_association_alias TYPE string VALUE 'associationusage' ##NO_TEXT.

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
    "! <p class="shorttext synchronized" lang="en">Create filter for API option</p>
    "!
    METHODS add_api_option_filter
      IMPORTING
        it_values TYPE zif_dbbr_ty_object_browser=>ty_search_option_values-value_range.

ENDCLASS.



CLASS zcl_dbbr_ob_cds_searcher IMPLEMENTATION.

  METHOD add_anno_option_filter.
    new_and_cond_list( ).

    add_join_table(
        iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsannotation }| ) }|
        iv_alias      = 'anno'
        it_fields     = VALUE #(
          ( field = 'entityid' ref_field = 'entityid' ref_table_alias = c_base_alias )
        )
    ).

    LOOP AT it_values INTO DATA(ls_filter_value).
      add_filter( VALUE #( sqlfieldname = 'anno~name' sign = 'I' option = COND #( WHEN ls_filter_value-low CS '*' THEN 'CP' ELSE 'EQ' ) low = ls_filter_value-low )  ).

      IF ls_filter_value-high IS NOT INITIAL.
        add_filter( VALUE #( sqlfieldname = 'anno~value' sign = 'I' option = COND #( WHEN ls_filter_value-high CS '*' THEN 'CP' ELSE 'EQ' ) low = ls_filter_value-high )  ).
      ENDIF.
      new_or_cond_list( ).
    ENDLOOP.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD add_api_option_filter.
    DATA: lt_api_filters TYPE RANGE OF string.

    LOOP AT it_values INTO DATA(ls_value).

      CASE ls_value-low.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-released.
          CONTINUE.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-custom_fields.
          ls_value-low = zif_dbbr_c_cds_api_state=>add_custom_fields.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-key_user.
          ls_value-low = zif_dbbr_c_cds_api_state=>use_in_key_user_apps.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-remote_api.
          ls_value-low = zif_dbbr_c_cds_api_state=>use_as_remote_api.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-cloud_user.
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
    DATA: lt_type_filters TYPE RANGE OF string.

    LOOP AT it_values INTO DATA(ls_value).
      CASE ls_value-low.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-function.
          ls_value-low = zif_dbbr_c_cds_view_type=>table_function.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-hierarchy.
          ls_value-low = zif_dbbr_c_cds_view_type=>hierarchy.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-view.
          ls_value-low = zif_dbbr_c_cds_view_type=>view.
      ENDCASE.

      lt_type_filters = VALUE #( BASE lt_type_filters ( ls_value ) ).
    ENDLOOP.

    add_option_filter(
        iv_fieldname = 'source_type'
        it_values    = lt_type_filters
    ).
  ENDMETHOD.


  METHOD zif_dbbr_object_searcher~search.

    set_base_select_table(
        iv_entity = zif_dbbr_c_select_source_id=>ddddlsrc
        iv_alias  = c_ddl_source_table
    ).

    add_join_table(
        iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_p_cdsviewbase }| ) }|
        iv_alias      = c_base_alias
        it_fields     = VALUE #(
          ( field = 'ddlname' ref_field = 'ddlname' ref_table_alias = c_ddl_source_table )
        )
        it_filter     = VALUE #(
          ( fieldname = 'parentname' tabname_alias = c_ddl_source_table  )
        )
    ).
    DATA(lv_cds_text_view) = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsviewt }| ).
    add_join_table(
        iv_join_table = |{ lv_cds_text_view }|
        iv_join_type  = zif_dbbr_c_join_types=>left_outer_join
        it_fields     = VALUE #(
          ( field = 'ddlname' ref_field = 'ddlname' ref_table_alias = c_base_alias )
        )
        it_filter     = VALUE #(
          ( fieldname = 'language' tabname = lv_cds_text_view value = zcl_dbbr_appl_util=>get_description_language( ) )
        )
    ).

    add_select_field( iv_fieldname = 'entityid' iv_fieldname_alias = 'entity_id' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'createdby' iv_fieldname_alias = 'created_by' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'rawentityid' iv_fieldname_alias = 'entity_id_raw' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'description' iv_entity = lv_cds_text_view ).
    add_select_field( iv_fieldname = 'developmentpackage' iv_fieldname_alias = 'devclass' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = |'C'| iv_fieldname_alias = 'entity_type' ).

    add_order_by( iv_fieldname = 'entityid' iv_entity = c_base_alias  ).

    IF mr_search_query->has_search_string( ).
      add_filter( VALUE #( sqlfieldname = |{ c_base_alias }~entityid|
                           option       = mr_search_query->mv_search_option
                           sign         = 'I'
                           low          = mr_search_query->mv_search_string ) ).
    ENDIF.

    IF mr_search_query->has_options( ).
      LOOP AT mr_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).
        CASE <ls_option>-option.

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
                  ( field = 'objectname' ref_field = 'ddlname' ref_table_alias = c_ddl_source_table )
                )
                it_filter     = VALUE #(
                  ( fieldname = 'objecttype' tabname_alias = CONV #( c_api_alias ) value = 'DDLS'  )
                )
            ).

            add_api_option_filter( it_values = <ls_option>-value_range ).

*.......... Find views where the filter exists in the FROM part of the cds view
          WHEN zif_dbbr_c_object_browser=>c_search_option-by_select_from.
            add_join_table(
                iv_join_table = |{ zif_dbbr_c_select_source_id=>ddldependency }|
                iv_alias      = c_source_ddlview_alias
                it_fields     = VALUE #(
                  ( field = 'ddlname' ref_field = 'ddlname' ref_table_alias = c_base_alias )
                )
                it_filter     = VALUE #(
                  ( fieldname = 'objecttype' value = 'VIEW' tabname_alias = c_source_ddlview_alias  )
                )
            ).
            DATA(lv_from_part_table) = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsfrompartentity }| ).
            add_join_table(
                iv_join_table = |{ lv_from_part_table }|
                iv_alias      = c_select_from_alias
                it_fields     = VALUE #( ( field = 'ddlviewname' ref_field = 'objectname' ref_table_alias = c_source_ddlview_alias ) )
            ).

            add_option_filter(
                iv_fieldname = |{ c_select_from_alias }~sourceentity|
                it_values    = <ls_option>-value_range
            ).

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
            add_join_table(
                iv_join_table = |{ get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_cdsviewfield }| ) }|
                iv_alias      = 'field'
                it_fields     = VALUE #(
                  ( field = 'entityid' ref_field = 'entityid' ref_table_alias = c_base_alias )
                )
            ).
            add_option_filter(
                iv_fieldname = |field~fieldname|
                it_values    = <ls_option>-value_range
            ).

*.......... Find views where an entity is used as an association
          WHEN zif_dbbr_c_object_browser=>c_search_option-by_association.
            DATA(lv_association_usage_table) = get_cds_sql_name( |{ zif_dbbr_c_select_source_id=>zdbbr_i_associatedincds }| ).
            add_join_table(
                iv_join_table = |{ lv_association_usage_table }|
                iv_alias      = c_used_in_association_alias
                it_fields     = VALUE #(
                  ( field = 'ddlname' ref_field = 'ddlname' ref_table_alias = c_base_alias )
                )
            ).
            add_option_filter(
                iv_fieldname = |{ c_used_in_association_alias }~usedentity|
                it_values    = <ls_option>-value_range
            ).

*.......... Find views for a certain type e.g. function, hierarchy, view
          WHEN zif_dbbr_c_object_browser=>c_search_option-by_type.
            IF sy-saprl >= 750.
              add_type_option_filter( it_values = <ls_option>-value_range ).
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    new_and_cond_list( ).

    search( ).

    rt_result = mt_result.
  ENDMETHOD.
ENDCLASS.
