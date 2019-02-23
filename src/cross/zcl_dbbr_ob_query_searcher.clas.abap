"! <p class="shorttext synchronized" lang="en">Query searcher for Object Browser</p>
CLASS zcl_dbbr_ob_query_searcher DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_dbbr_ob_generic_searcher.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_object_searcher.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_base_table TYPE string VALUE 'queryhead'.
ENDCLASS.


CLASS zcl_dbbr_ob_query_searcher IMPLEMENTATION.

  METHOD zif_dbbr_object_searcher~search.

    set_base_select_table(
        iv_entity     = zif_dbbr_c_select_source_id=>zdbbr_queryh
        iv_alias      = c_base_table
    ).

    IF mr_search_query->has_search_string( ).
      add_filter( VALUE #( sqlfieldname  = 'query_name'
                           option        = mr_search_query->mv_search_option
                           sign          = 'I'
                           low           = mr_search_query->mv_search_string ) ).
    ENDIF.

    LOOP AT mr_search_query->get_options( ) ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

*...... Find queries with a specific description
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_description.
          add_option_filter(
            iv_fieldname    = 'description'
            iv_sql_function = zif_dbbr_c_sql_function=>upper
            it_values       = <ls_option>-value_range
          ).

*...... Find objects which were created by a specific user
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_owner.
          add_option_filter(
            iv_fieldname    = 'created_by'
            it_values       = <ls_option>-value_range
          ).

*...... Find queries whoose tables match the filters condition
        WHEN zif_dbbr_c_object_browser=>c_search_option-by_select_from.
          add_join_table(
              iv_join_table = |{ zif_dbbr_c_select_source_id=>zdbbr_queryt }|
              iv_alias      = 'table'
              it_fields     = VALUE #(
                ( field = 'ref_query_id' ref_field = 'query_id' ref_table_alias = c_base_table )
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

    search( ).

    rt_result = mt_result.
  ENDMETHOD.

ENDCLASS.
