CLASS zcl_dbbr_query_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Check if user has authority for query</p>
    "!
    "! @parameter iv_query_id | <p class="shorttext synchronized" lang="en"></p>
    METHODS check_query_authority
      IMPORTING
        !iv_query_id TYPE zdbbr_query_id .
    "! <p class="shorttext synchronized" lang="en">Retrieve join tables of query</p>
    "!
    "! @parameter iv_query_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rt_entities | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_tables_of_query
      IMPORTING
        iv_query_name      TYPE zdbbr_query_name
      RETURNING
        VALUE(rt_entities) TYPE zdbbr_entity_t.
    "! <p class="shorttext synchronized" lang="en">Find queries by name/primary table</p>
    "!
    "! @parameter iv_query_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_primary_table | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter et_queries | <p class="shorttext synchronized" lang="en"></p>
    METHODS find_queries
      IMPORTING
        !iv_query_name    TYPE zdbbr_query_name OPTIONAL
        !iv_primary_table TYPE tabname OPTIONAL
      EXPORTING
        !et_queries       TYPE zdbbr_query_info_itab .
    "! <p class="shorttext synchronized" lang="en">Checks if query exists</p>
    "!
    "! @parameter iv_query_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_global | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_created_by | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_saving_context | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rf_exists | <p class="shorttext synchronized" lang="en"></p>
    METHODS query_exists
      IMPORTING
        !iv_query_name     TYPE zdbbr_query_name
        !if_global         TYPE boolean OPTIONAL
        !iv_created_by     TYPE zdbbr_created_by OPTIONAL
        !if_saving_context TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rf_exists)   TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Retrieve the query by name</p>
    "!
    "! @parameter iv_query_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_load_completely | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rs_query | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_query
      IMPORTING
        !iv_query_name      TYPE zdbbr_query_name
        !if_load_completely TYPE boolean DEFAULT abap_true
      RETURNING
        VALUE(rs_query)     TYPE zdbbr_query_data .
    "! <p class="shorttext synchronized" lang="en">Retriev the query by id</p>
    "!
    "! @parameter iv_query_id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_load_jump_destinations | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_load_formulas | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_load_variants | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_load_completely | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rs_query | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_query_by_id
      IMPORTING
        !iv_query_id               TYPE zdbbr_query_id
        !if_load_jump_destinations TYPE boolean OPTIONAL
        !if_load_formulas          TYPE boolean OPTIONAL
        !if_load_variants          TYPE boolean OPTIONAL
        !if_load_completely        TYPE boolean DEFAULT abap_true
      RETURNING
        VALUE(rs_query)            TYPE zdbbr_query_data .
    "! <p class="shorttext synchronized" lang="en">Saves the given query</p>
    "!
    "! @parameter is_query | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_query_id | <p class="shorttext synchronized" lang="en"></p>
    METHODS save_query
      IMPORTING
        !is_query          TYPE zdbbr_query_data
      RETURNING
        VALUE(rv_query_id) TYPE zdbbr_query_id .
    "! <p class="shorttext synchronized" lang="en">Delete query for the given id</p>
    "!
    "! @parameter iv_query_id | <p class="shorttext synchronized" lang="en"></p>
    METHODS delete_query_by_id
      IMPORTING
        !iv_query_id TYPE zdbbr_query_id .
    "! <p class="shorttext synchronized" lang="en">Update flags of query</p>
    "!
    "! @parameter iv_query_id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_has_jump_fields | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_has_output_fields | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_has_sort_fields | <p class="shorttext synchronized" lang="en"></p>
    METHODS update_query_flags
      IMPORTING
        !iv_query_id          TYPE zdbbr_query_info_ui-query_id
        !if_has_jump_fields   TYPE abap_bool OPTIONAL
        !if_has_output_fields TYPE abap_bool OPTIONAL
        !if_has_sort_fields   TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Retrieve query id for query name</p>
    "!
    "! @parameter iv_query_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_query_id | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_query_id
      IMPORTING
        !iv_query_name     TYPE zdbbr_query_name
      RETURNING
        VALUE(rv_query_id) TYPE zdbbr_query_id .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS delete_related_by_query_name
      IMPORTING
        !iv_query_name TYPE zdbbr_query_name .
    METHODS delete_related_by_query_id
      IMPORTING
        !iv_query_id   TYPE zdbbr_query_id
        !if_delete_all TYPE boolean OPTIONAL .
    METHODS fill_corresponding_data
      IMPORTING
        !if_load_formulas          TYPE abap_bool OPTIONAL
        !if_load_jump_destinations TYPE abap_bool OPTIONAL
        !if_load_variants          TYPE abap_bool OPTIONAL
      CHANGING
        !cs_query_data             TYPE zdbbr_query_data .
ENDCLASS.



CLASS zcl_dbbr_query_factory IMPLEMENTATION.


  METHOD check_query_authority.
    SELECT SINGLE * FROM zdbbr_queryh INTO @DATA(ls_query)
      WHERE query_id = @iv_query_id.

    IF ls_query IS NOT INITIAL AND
       ls_query-is_global = abap_false AND
       ls_query-created_by <> sy-uname.
      RAISE EXCEPTION TYPE zcx_dbbr_exception
        EXPORTING
          textid = zcx_dbbr_exception=>query_no_authority
          msgv1  = |{ ls_query-query_name }|.
    ENDIF.
  ENDMETHOD.


  METHOD delete_related_by_query_id.
    DELETE FROM zdbbr_queryt WHERE ref_query_id = iv_query_id.
    DELETE FROM zdbbr_queryp WHERE ref_query_id = iv_query_id.
    DELETE FROM zdbbr_tabf WHERE ref_id = iv_query_id.

    IF if_delete_all = abap_true.
      DATA(lr_jumpdest_f) = NEW zcl_dbbr_jump_destination_f( ).
      lr_jumpdest_f->delete_jumpdest_by_query_id( iv_query_id ).

      zcl_dbbr_variant_factory=>delete_variants_by_query_id( iv_query_id ).

    ENDIF.

    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_related_by_query_name.
    DELETE FROM zdbbr_favmenu WHERE fav_entry = iv_query_name
                                AND favtype   = zif_dbbr_c_favmenu_type=>query.

    DELETE FROM zdbbr_mostused WHERE most_used_entry = iv_query_name
                                 AND type            = zif_dbbr_c_favmenu_type=>query.
  ENDMETHOD.


  METHOD delete_query_by_id.
    SELECT SINGLE * FROM zdbbr_queryh INTO @DATA(ls_query)
      WHERE query_id = @iv_query_id.

    IF sy-subrc <> 0.
      RETURN. " no query exists for this id
    ENDIF.

    IF ls_query-ref_join_id IS NOT INITIAL.
      NEW zcl_dbbr_join_factory( )->delete_join(
          iv_join_id      = ls_query-ref_join_id
      ).
    ENDIF.

    delete_related_by_query_name( iv_query_name = ls_query-query_name ).
    delete_related_by_query_id( iv_query_id = iv_query_id
                               if_delete_all = abap_true ).

    " delete the query head
    DELETE FROM zdbbr_queryh WHERE query_id = iv_query_id.

    COMMIT WORK.
  ENDMETHOD.


  METHOD fill_corresponding_data.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/04
*&---------------------------------------------------------------------*
*& Description: Fills corresponding data for query, i.e. join, tables, fields
*&---------------------------------------------------------------------*
    " 1) read join?
    IF cs_query_data-ref_join_id IS NOT INITIAL.
      cs_query_data-join_def = NEW zcl_dbbr_join_factory( )->read_join( iv_join_id = cs_query_data-ref_join_id ).

      IF cs_query_data-primary_table_alias IS INITIAL.
        cs_query_data-primary_table_alias = cs_query_data-join_def-primary_table_alias.
      ENDIF.

      IF cs_query_data-entity_type IS INITIAL.
        SELECT SINGLE type
          FROM zdbbr_i_databaseentity( p_language = @sy-langu )
          WHERE entity = @cs_query_data-primary_table
        INTO @cs_query_data-entity_type.
      ENDIF.
    ENDIF.

    " 2) read tables
    SELECT * FROM zdbbr_queryt INTO CORRESPONDING FIELDS OF TABLE cs_query_data-tables
      WHERE ref_query_id = cs_query_data-query_id
      ORDER BY selection_order ASCENDING.

    " 3) read table fields
    SELECT * FROM zdbbr_tabf INTO CORRESPONDING FIELDS OF TABLE cs_query_data-fields
      WHERE ref_id = cs_query_data-query_id.

    " read existing parameters
    SELECT * FROM zdbbr_queryp INTO CORRESPONDING FIELDS OF TABLE cs_query_data-parameters
      WHERE ref_query_id = cs_query_data-query_id.

    " 4) read existing variants
    IF if_load_variants = abap_true.
      cs_query_data-variants = zcl_dbbr_variant_factory=>find_variants_for_query( cs_query_data-query_id ).
    ENDIF.

    " 6) load jump fields
    IF if_load_jump_destinations = abap_true AND cs_query_data-has_jump_fields = abap_true.
      cs_query_data-jump_fields = CORRESPONDING #(
        NEW zcl_dbbr_jump_destination_f( )->get_jump_destinations( cs_query_data-query_id )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD find_queries.
*&---------------------------------------------------------------------*
*& Description: Finds querys for given name / primary_table
*& V001 - 2016/11/27: All querys are selected, regardless of filled
*&                    parameters
*&---------------------------------------------------------------------*
    DATA(lt_query_name_selopt) = COND zdbbr_selopt_itab(
      WHEN iv_query_name IS NOT INITIAL AND iv_query_name CS '*' THEN
        VALUE #( ( sign = 'I' option = 'CP' low = iv_query_name ) )
      WHEN iv_query_name IS NOT INITIAL AND iv_query_name NS '*' THEN
        VALUE #( ( sign = 'I' option = 'EQ' low = iv_query_name ) )
    ).

    SELECT * INTO TABLE @DATA(lt_query)
      FROM zdbbr_queryh
      WHERE query_name IN @lt_query_name_selopt
      ORDER BY query_name,
               primary_table.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " remove skripts that are not global and are not created by the user
    DELETE lt_query WHERE is_global = abap_false AND created_by <> sy-uname.
    et_queries = CORRESPONDING zdbbr_query_info_itab( lt_query ).

  ENDMETHOD.


  METHOD get_query.
*&---------------------------------------------------------------------*
*& Description: Returns complete query data for a given table name
*&---------------------------------------------------------------------*
    " 1) find query header
    SELECT SINGLE * FROM zdbbr_queryh INTO CORRESPONDING FIELDS OF rs_query
      WHERE query_name = iv_query_name.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF if_load_completely = abap_true.
      fill_corresponding_data( CHANGING cs_query_data = rs_query ).
    ENDIF.
  ENDMETHOD.


  METHOD get_query_by_id.
*&---------------------------------------------------------------------*
*& Description: Returns complete query data for a given query id
*&---------------------------------------------------------------------*
    " 1) find query header
    SELECT SINGLE * FROM zdbbr_queryh INTO CORRESPONDING FIELDS OF rs_query
      WHERE query_id = iv_query_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF if_load_completely = abap_true.
      fill_corresponding_data(
        EXPORTING if_load_variants          = if_load_variants
                  if_load_formulas          = if_load_formulas
                  if_load_jump_destinations = if_load_jump_destinations
        CHANGING  cs_query_data            = rs_query ).
    ENDIF.
  ENDMETHOD.


  METHOD get_query_id.
    SELECT SINGLE query_id FROM zdbbr_queryh INTO rv_query_id
      WHERE query_name = iv_query_name.
  ENDMETHOD.


  METHOD save_query.
    DATA: lt_query_fields     TYPE STANDARD TABLE OF zdbbr_tabf,
          lt_query_parameters TYPE TABLE OF zdbbr_queryp.

    DATA(ls_join_def) = is_query-join_def.
    DATA(lt_query_tables) = is_query-tables.
    lt_query_parameters = CORRESPONDING #( is_query-parameters ).
    lt_query_fields = CORRESPONDING #( is_query-fields ).
    DATA(ls_query) = CORRESPONDING zdbbr_query_info( is_query ).

    ls_query-has_jump_fields = xsdbool( is_query-jump_fields IS NOT INITIAL ).

    DATA(lr_join_factory) = NEW zcl_dbbr_join_factory( ).

*.. 1) is there already existing data for this query?
*.. possible invalidation of data of existing query variants !!!!
    IF ls_query-query_id IS NOT INITIAL.
      delete_related_by_query_id( iv_query_id = ls_query-query_id if_delete_all = abap_true ).
    ELSE.
      ls_query-query_id = zcl_dbbr_system_helper=>create_guid_22( ).
      CLEAR ls_join_def-join_id.
    ENDIF.

*.. delete existing join
    IF ls_query-ref_join_id IS NOT INITIAL.
      lr_join_factory->delete_join( ls_query-ref_join_id ).

      CLEAR: ls_query-ref_join_id,
             ls_join_def-join_id.
    ENDIF.

*.. save new data
*.. save join
    IF ls_join_def-tables IS NOT INITIAL.
      ls_query-ref_join_id = lr_join_factory->save_join( ls_join_def ).
    ENDIF.

*.. save query
    MODIFY zdbbr_queryh FROM ls_query.
    IF sy-subrc = 0.
      rv_query_id = ls_query-query_id.
    ENDIF.

*.. > save query tables
    LOOP AT lt_query_tables ASSIGNING FIELD-SYMBOL(<ls_query_table>).
      <ls_query_table>-ref_query_id = ls_query-query_id.
      <ls_query_table>-query_table_id = zcl_dbbr_system_helper=>create_guid_22( ).
    ENDLOOP.

    INSERT zdbbr_queryt FROM TABLE lt_query_tables.

*.. > save query fields
    LOOP AT lt_query_fields ASSIGNING FIELD-SYMBOL(<ls_query_field>).
      <ls_query_field>-table_field_id = zcl_dbbr_system_helper=>create_guid_22( ).
      <ls_query_field>-ref_id = ls_query-query_id.
    ENDLOOP.

    INSERT zdbbr_tabf FROM TABLE lt_query_fields.

*.. > Save query parameters
    LOOP AT lt_query_parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
      <ls_param>-ref_query_id = ls_query-query_id.
    ENDLOOP.


    INSERT zdbbr_queryp FROM TABLE lt_query_parameters.

    COMMIT WORK.

    IF is_query-variants IS NOT INITIAL.
      zcl_dbbr_variant_factory=>save_variants( is_query-variants ).
    ENDIF.

    IF is_query-jump_fields IS NOT INITIAL.
      DATA(lt_jump_destinations) = CORRESPONDING zdbbr_jumpdest_data_ui_itab( DEEP is_query-jump_fields ).
      NEW zcl_dbbr_jump_destination_f( )->save_jump_destinations(
        EXPORTING iv_query_id         = ls_query-query_id
        CHANGING  ct_jump_destinations = lt_jump_destinations
      ).
    ENDIF.

  ENDMETHOD.


  METHOD query_exists.
*&---------------------------------------------------------------------*
*& Description: Returns abap_true if the query exists
*&---------------------------------------------------------------------*
    SELECT COUNT( * ) INTO @DATA(lv_count)
      FROM zdbbr_queryh
      WHERE query_name  = @iv_query_name.
    rf_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD update_query_flags.
    IF if_has_jump_fields IS NOT SUPPLIED AND
       if_has_output_fields IS NOT SUPPLIED AND
       if_has_sort_fields IS NOT SUPPLIED.
      RETURN.
    ENDIF.

    " find query with id
    SELECT SINGLE * FROM zdbbr_queryh INTO @DATA(ls_query_head)
      WHERE query_id = @iv_query_id.

    IF if_has_jump_fields IS SUPPLIED.
      ls_query_head-has_jump_fields = if_has_jump_fields.
    ENDIF.

    IF if_has_sort_fields IS SUPPLIED.
      ls_query_head-has_sort_fields = if_has_sort_fields.
    ENDIF.

    IF if_has_output_fields IS SUPPLIED.
      ls_query_head-has_output_fields = if_has_output_fields.
    ENDIF.

    MODIFY zdbbr_queryh FROM ls_query_head.

    COMMIT WORK.
  ENDMETHOD.

  METHOD get_tables_of_query.
    SELECT SINGLE query_id, primary_table AS tabname
      FROM zdbbr_queryh
      WHERE query_name = @iv_query_name
    INTO @DATA(ls_query).

    SELECT tabname
     FROM zdbbr_queryt
    WHERE ref_query_id = @ls_query-query_id
    INTO TABLE @DATA(lt_join_tables).

    DATA(lt_range) = VALUE zif_dbbr_global_types=>ty_tabname_range(
      ( sign = 'I' option = 'EQ' low = ls_query-tabname )
      ( LINES OF VALUE #( FOR table IN lt_join_tables ( sign = 'I' option = 'EQ' low = table-tabname ) ) )
    ).

    CHECK lt_range IS NOT INITIAL.

    rt_entities = zcl_dbbr_dictionary_helper=>get_entity_by_range( EXPORTING it_entity_range = lt_range ).
  ENDMETHOD.

ENDCLASS.
