"! <p class="shorttext synchronized" lang="en">Query searcher for Object Browser</p>
CLASS zcl_dbbr_os_query_provider DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_base_search_provider.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS prepare_search
        REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS c_base_table TYPE string VALUE 'queryhead'.
ENDCLASS.


CLASS zcl_dbbr_os_query_provider IMPLEMENTATION.

  METHOD prepare_search.

    set_base_select_table(
        iv_entity     = zif_dbbr_c_select_source_id=>zdbbr_queryh
        iv_alias      = c_base_table
    ).

    IF mo_search_query->has_search_terms( ).
      add_search_terms_to_search( it_field_names = VALUE #( ( `query_name` ) ) ).
    ENDIF.

    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

*...... Find queries with a specific description
        WHEN c_general_search_options-description.
          add_option_filter(
            iv_fieldname    = 'description'
            iv_sql_function = zif_sat_c_sql_function=>upper
            it_values       = <ls_option>-value_range
          ).

*...... Find objects which were created by a specific user
        WHEN c_general_search_options-user.
          add_option_filter(
            iv_fieldname    = 'created_by'
            it_values       = <ls_option>-value_range
          ).

*...... Find queries whoose tables match the filters condition
        WHEN zif_dbbr_c_object_browser=>c_query_options-from.
          add_join_table(
              iv_join_table = |{ zif_dbbr_c_select_source_id=>zdbbr_queryt }|
              iv_alias      = 'table'
              it_conditions = VALUE #(
                ( field = 'ref_query_id' ref_field = 'query_id' ref_table_alias = c_base_table type = zif_sat_c_join_cond_type=>field )
              )
          ).

          add_option_filter(
              iv_fieldname = 'tabname'
              it_values    = <ls_option>-value_range
          ).
      ENDCASE.

    ENDLOOP.

    add_select_field( iv_fieldname = 'created_by' iv_fieldname_alias = 'created_by' ).
    add_select_field( iv_fieldname = 'query_name' iv_fieldname_alias = 'entity_id' ).
    add_select_field( iv_fieldname = 'query_name' iv_fieldname_alias = 'entity_id_raw' ).
    add_select_field( iv_fieldname = 'description' ).
    add_select_field( iv_fieldname = |'Q'| iv_fieldname_alias = 'entity_type' ).

    add_order_by( iv_entity = c_base_table iv_fieldname = 'query_name' ).

    new_and_cond_list( ).
  ENDMETHOD.

ENDCLASS.
