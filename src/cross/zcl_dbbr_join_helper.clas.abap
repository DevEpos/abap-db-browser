"! <p class="shorttext synchronized" lang="en">Helper for table joins</p>
CLASS zcl_dbbr_join_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Join Condition Type</p>
      "! Internal type for Join Condition which is used for building the
      "! FROM clause
      BEGIN OF ty_join_condition,
        join_operator   TYPE string,
        open_bracket    TYPE string,
        value           TYPE string,
        closing_bracket TYPE string,
      END OF ty_join_condition .
    TYPES:
      tt_join_conditions TYPE STANDARD TABLE OF ty_join_condition WITH EMPTY KEY .

    "! <p class="shorttext synchronized" lang="en">Builds FROM SQL clause for join definition</p>
    CLASS-METHODS build_from_clause_for_join_def
      IMPORTING
        !is_join_def          TYPE zdbbr_join_def OPTIONAL
        if_use_ddl_for_select TYPE abap_bool OPTIONAL
        !it_table_alias_map   TYPE zdbbr_table_to_alias_map_itab OPTIONAL
      RETURNING
        VALUE(rt_from_clause) TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Builds alias map for list of tables</p>
    CLASS-METHODS build_table_to_alias_map
      IMPORTING
        !is_join_def              TYPE zdbbr_join_def OPTIONAL
        !ir_tabfields             TYPE REF TO zcl_dbbr_tabfield_list OPTIONAL
      RETURNING
        VALUE(rt_table_alias_map) TYPE zdbbr_table_to_alias_map_itab .
    "! <p class="shorttext synchronized" lang="en">Builds where clause for given conditions</p>
    CLASS-METHODS build_where_for_conditions
      IMPORTING
        it_conditions   TYPE tt_join_conditions
      RETURNING
        VALUE(rt_where) TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Prefill cds view parameters</p>
    "!
    CLASS-METHODS prefill_parameters
      IMPORTING
        io_tabfields TYPE REF TO zcl_dbbr_tabfield_list
      CHANGING
        cs_join      TYPE zdbbr_join_def.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_join_table,
        join_type  TYPE string,
        table      TYPE string,
        conditions TYPE tt_join_conditions,
      END OF ty_join_table .
    TYPES:
      tt_join_table TYPE STANDARD TABLE OF ty_join_table WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_join,
        primary_table TYPE string,
        tables        TYPE tt_join_table,
      END OF ty_join .

    "! <p class="shorttext synchronized" lang="en">Fills missing operators, etc.</p>
    CLASS-METHODS repair_join_definition
      IMPORTING
        !is_join_def          TYPE zdbbr_join_def
        if_use_ddl_for_select TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_join_def)    TYPE zdbbr_join_def .
    "! <p class="shorttext synchronized" lang="en">Prepares join table definitions for building FROM Strings</p>
    CLASS-METHODS prepare_tables
      IMPORTING
        !is_join_def           TYPE zdbbr_join_def
        !it_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab
      RETURNING
        VALUE(rs_join)         TYPE ty_join .

    "! <p class="shorttext synchronized" lang="en">Fill parameters for a certain entity in the join</p>
    "!
    CLASS-METHODS fill_entity_params
      IMPORTING
        iv_entity       TYPE zdbbr_entity_id
        iv_entity_alias TYPE zdbbr_entity_alias
      CHANGING
        cs_join         TYPE zdbbr_join_def.
ENDCLASS.



CLASS zcl_dbbr_join_helper IMPLEMENTATION.



  METHOD build_from_clause_for_join_def.
    DATA(lt_table_to_alias_map) = it_table_alias_map.
    IF lt_table_to_alias_map IS INITIAL.
      lt_table_to_alias_map = build_table_to_alias_map(
         is_join_def = is_join_def
     ).
    ENDIF.

    DATA(ls_join_def) = repair_join_definition(
       if_use_ddl_for_select = if_use_ddl_for_select
       is_join_def           = is_join_def
    ).

*.. parse join definition for building the FROM Clause string
    DATA(ls_join_enriched) = prepare_tables(
        is_join_def           = ls_join_def
        it_table_to_alias_map = lt_table_to_alias_map
    ).

    rt_from_clause = VALUE #(
      ( ls_join_enriched-primary_table )
    ).

    LOOP AT ls_join_enriched-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      rt_from_clause = VALUE #( BASE rt_from_clause
        ( |{ <ls_table>-join_type }{ <ls_table>-table }| )
      ).

      LOOP AT <ls_table>-conditions ASSIGNING FIELD-SYMBOL(<ls_cond>).
        rt_from_clause = VALUE #( BASE rt_from_clause
          ( |{ <ls_cond>-join_operator }{ <ls_cond>-open_bracket }{ <ls_cond>-value }{ <ls_cond>-closing_bracket }| )
        ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_table_to_alias_map.
    DATA: lv_counter TYPE i.

    IF is_join_def IS NOT INITIAL.
      lv_counter = 1.

      INSERT VALUE #(
        tabname = is_join_def-primary_table
        alias   = zcl_dbbr_alias_map=>get_alias( lv_counter )
      ) INTO TABLE rt_table_alias_map.

      ADD 1 TO lv_counter.

      LOOP AT is_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table_ui>).
        INSERT VALUE #(
            tabname = <ls_join_table_ui>-add_table
            alias   = zcl_dbbr_alias_map=>get_alias( lv_counter )
        ) INTO TABLE rt_table_alias_map.
        ADD 1 TO lv_counter.
      ENDLOOP.
    ELSEIF ir_tabfields IS BOUND.
      LOOP AT ir_tabfields->get_table_list( ) ASSIGNING FIELD-SYMBOL(<ls_table>).
        rt_table_alias_map = VALUE #( BASE rt_table_alias_map
          ( tabname = <ls_table>-tabname_alias alias = <ls_table>-alias )
        ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD build_where_for_conditions.
    LOOP AT it_conditions ASSIGNING FIELD-SYMBOL(<ls_cond>).
      rt_where = VALUE #( BASE rt_where
        ( |{ <ls_cond>-join_operator }{ <ls_cond>-open_bracket }{ <ls_cond>-value }{ <ls_cond>-closing_bracket }| )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD prepare_tables.
    DATA: lv_on_string          TYPE string,
          lv_and_string         TYPE string,
          lv_or_string          TYPE string,

          lf_parenthesis_opened TYPE abap_bool,
          lv_parenthesis_open   TYPE string,
          lv_parenthesis_closed TYPE string,

          lv_reference_alias    TYPE string,

          ls_new_condition      TYPE ty_join_condition,
          lv_index              TYPE sy-tabix,
          lv_previous_join_cond TYPE string.

    FIELD-SYMBOLS: <ls_last_condition> TYPE ty_join_condition.

*... prefill some needed join condition operator strings
    lv_on_string = |  { zif_dbbr_c_selection_condition=>on  ALIGN = RIGHT WIDTH = 5 } |.
    lv_and_string = |  { zif_dbbr_c_selection_condition=>and  ALIGN = RIGHT WIDTH = 5 } |.
    lv_or_string = |  { zif_dbbr_c_selection_condition=>or  ALIGN = RIGHT WIDTH = 5 } |.

    rs_join-primary_table = is_join_def-primary_table.

    IF is_join_def-parameters IS NOT INITIAL.
      zcl_dbbr_selection_helper=>append_params_to_table_string(
        EXPORTING it_parameters = is_join_def-parameters
        CHANGING  cv_table_part = rs_join-primary_table
      ).
    ENDIF.

    rs_join-primary_table = rs_join-primary_table && | AS | &&
                            COND #( WHEN is_join_def-primary_table_alias IS NOT INITIAL THEN
                                        is_join_def-primary_table_alias
                                    ELSE
                                        |{ it_table_to_alias_map[ tabname = is_join_def-primary_table ]-alias }|
                                  ).

*... build joins for defined join tables
    LOOP AT is_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE is_virtual = abap_false.

      DATA(lv_join_table_alias) = COND #( WHEN <ls_join_table>-add_table_alias IS NOT INITIAL THEN
                                            <ls_join_table>-add_table_alias
                                          ELSE
                                            it_table_to_alias_map[ tabname = <ls_join_table>-add_table ]-alias ).

      DATA(lv_join_type_text) = SWITCH string(
        <ls_join_table>-join_type
        WHEN zif_dbbr_c_join_types=>inner_join THEN |INNER JOIN |
        WHEN zif_dbbr_c_join_types=>left_outer_join THEN |LEFT OUTER JOIN |
        WHEN zif_dbbr_c_join_types=>right_outer_join THEN |RIGHT OUTER JOIN |
      ).

      DATA(ls_parsed_table) = VALUE ty_join_table(
          join_type  = lv_join_type_text
          table      = |{ <ls_join_table>-add_table }|
      ).

      IF <ls_join_table>-parameters IS NOT INITIAL.
        zcl_dbbr_selection_helper=>append_params_to_table_string(
          EXPORTING it_parameters = <ls_join_table>-parameters
          CHANGING  cv_table_part = ls_parsed_table-table
        ).
      ENDIF.

      ls_parsed_table-table = |{ ls_parsed_table-table } AS { lv_join_table_alias }|.

*.... parse the conditions
      LOOP AT <ls_join_table>-conditions INTO DATA(ls_cond).
        lv_index = sy-tabix.

*...... Determine the alias of the reference field
        IF ls_cond-type = zif_dbbr_c_join_cond_type=>field.
          lv_reference_alias = COND #( WHEN ls_cond-ref_table_alias IS NOT INITIAL THEN
                                         ls_cond-ref_table_alias
                                       WHEN ls_cond-ref_table = is_join_def-primary_table THEN
                                         is_join_def-primary_table_alias
                                       ELSE
                                         is_join_def-tables[ add_table = ls_cond-ref_table ]-add_table_alias ).
        ELSE.

          lv_reference_alias = COND #( WHEN ls_cond-tabname_alias IS NOT INITIAL THEN
                                         ls_cond-tabname_alias
                                       WHEN ls_cond-tabname = is_join_def-primary_table THEN
                                         is_join_def-primary_table_alias
                                       WHEN ls_cond-tabname IS NOT INITIAL THEN
                                         is_join_def-tables[ add_table = ls_cond-tabname ]-add_table_alias ).
          IF lv_reference_alias IS INITIAL.
            lv_reference_alias = it_table_to_alias_map[ tabname = ls_cond-tabname ]-alias.
          ENDIF.
        ENDIF.

*...... check for need of possible open parenthesis
        IF ls_cond-and_or = zif_dbbr_c_selection_condition=>or.
          IF lf_parenthesis_opened = abap_false.
            lv_parenthesis_open = |( |.
            lf_parenthesis_opened = abap_true.
          ENDIF.
        ELSE.
          IF lf_parenthesis_opened = abap_true.
            lv_parenthesis_closed = | )|.
            lf_parenthesis_opened = abap_false.
          ENDIF.
        ENDIF.

        IF ls_cond-type = zif_dbbr_c_join_cond_type=>field.
          ls_parsed_table-conditions = VALUE #( BASE ls_parsed_table-conditions
            ( open_bracket   = lv_parenthesis_open
              join_operator  = COND #( WHEN lv_index = 1 THEN lv_on_string ELSE lv_previous_join_cond )
              value          = |{ lv_join_table_alias }~{ ls_cond-field } { ls_cond-operator } | &&
                               |{ lv_reference_alias }~{ ls_cond-ref_field }|
              closing_bracket = lv_parenthesis_closed )
          ).
        ELSE.
          DATA(lv_value1) = |{ ls_cond-value }|.
          DATA(lv_value2) = ||.

*........ handle LIKE and NOT LIKE operator
          IF  ( ls_cond-operator = zif_dbbr_c_operator=>like OR
                ls_cond-operator = zif_dbbr_c_operator=>not_like ) AND
              ls_cond-value_type <> zif_dbbr_c_join_cond_val_type=>system_value_input.
***          ls_filter_cond-value = replace( val = ls_filter_cond-value sub = '*' with = '%' occ = 0 ).
*.......... Use proper SAP to SQL conversion for pattern
            zcl_dbbr_like_pattern_convrter=>conv_sap_to_sql_pattern(
              EXPORTING  iv_sap_pattern   = lv_value1
              IMPORTING  ev_sql_pattern   = lv_value1
                         ef_escape_needed = DATA(lf_escape_needed)
              EXCEPTIONS OTHERS = 1
            ).
            lv_value1 = cl_abap_dyn_prg=>quote( lv_value1 ).
            IF lf_escape_needed = abap_true.
              lv_value1 = |{ lv_value1 } ESCAPE '#' |.
            ENDIF.

            CLEAR ls_cond-value2.
          ELSE.
            IF ls_cond-value_type = zif_dbbr_c_join_cond_val_type=>system_value_input.
              IF lv_value1 = 'SY-LANGU'.
                lv_value1 = cl_abap_dyn_prg=>quote( zcl_dbbr_system_helper=>get_system_language( ) ).
              ELSE.
                lv_value1 = |@{ lv_value1 }|.
              ENDIF.
            ELSE.
              lv_value1 = cl_abap_dyn_prg=>quote( lv_value1 ).
            ENDIF.
          ENDIF.

          IF ls_cond-operator = 'BETWEEN'.
            lv_value2 = COND #(
              WHEN ls_cond-value_type = zif_dbbr_c_join_cond_val_type=>system_value_input THEN
                |@{ ls_cond-value2 }|
              ELSE
                cl_abap_dyn_prg=>quote( ls_cond-value2 )
            ).
            lv_value2 = | { zif_dbbr_c_selection_condition=>and } { lv_value2 }|.
          ENDIF.

          ls_parsed_table-conditions = VALUE #( BASE ls_parsed_table-conditions
            ( open_bracket    = lv_parenthesis_open
              join_operator   = COND #( WHEN lv_index = 1 THEN lv_on_string ELSE lv_previous_join_cond )
              value           = |{ lv_reference_alias }~{ ls_cond-field }| &&
                                | { ls_cond-operator } | &&
                                |{ lv_value1 }{ lv_value2 }|
              closing_bracket = lv_parenthesis_closed )
          ).
        ENDIF.

        lv_previous_join_cond = COND #( WHEN ls_cond-and_or = zif_dbbr_c_selection_condition=>or THEN lv_or_string ELSE lv_and_string ).
        CLEAR: lv_parenthesis_closed,
               lv_parenthesis_open.

        IF lf_parenthesis_opened = abap_true.
          lv_parenthesis_open = |  |.
        ENDIF.
      ENDLOOP.

      rs_join-tables = VALUE #( BASE rs_join-tables ( ls_parsed_table ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD prefill_parameters.
    DATA: lo_cds_view TYPE REF TO zcl_dbbr_cds_view.

*.. Read parameters for primary and join entities
    DATA(lt_tables) = io_tabfields->get_table_list( ).

    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      CHECK <ls_table>-has_params = abap_true.

      fill_entity_params(
        EXPORTING iv_entity       = <ls_table>-tabname
                  iv_entity_alias = <ls_table>-tabname_alias
        CHANGING  cs_join         = cs_join
      ).

    ENDLOOP.
  ENDMETHOD.


  METHOD repair_join_definition.
    FIELD-SYMBOLS: <ls_condition> TYPE zdbbr_join_condition_data.

    rs_join_def = is_join_def.
    IF is_join_def-primary_table_entity_type = zif_dbbr_c_entity_type=>cds_view AND
       ( if_use_ddl_for_select = abap_true OR sy-saprl < 750 ).
      rs_join_def-primary_table = zcl_dbbr_cds_view_factory=>read_ddl_ddic_view(
        zcl_dbbr_cds_view_factory=>get_ddl_for_entity_name( iv_cds_view_name = is_join_def-primary_table )
      ).
    ENDIF.

    LOOP AT rs_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
*.... fill unified condition table
      IF <ls_table>-conditions IS INITIAL.
*....... First the field conditions are added
        LOOP AT <ls_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
          APPEND INITIAL LINE TO <ls_table>-conditions ASSIGNING <ls_condition>.
          <ls_condition> = CORRESPONDING #( <ls_field_cond> ).
          <ls_condition>-type = zif_dbbr_c_join_cond_type=>field.
          <ls_condition>-and_or = zif_dbbr_c_selection_condition=>and.
        ENDLOOP.
*........ and then the filter conditions
        LOOP AT <ls_table>-filter_conditions ASSIGNING FIELD-SYMBOL(<ls_filter_cond>).
          APPEND INITIAL LINE TO <ls_table>-conditions ASSIGNING <ls_condition>.
          <ls_condition> = CORRESPONDING #( <ls_filter_cond> MAPPING field = fieldname ).
          <ls_condition>-type = zif_dbbr_c_join_cond_type=>filter.
        ENDLOOP.
*...... remove the last condition operator
        <ls_table>-conditions[ lines( <ls_table>-conditions ) ]-and_or = space.
      ENDIF.
      IF <ls_table>-entity_type = zif_dbbr_c_entity_type=>cds_view AND
         ( if_use_ddl_for_select = abap_true OR sy-saprl < 750 ).
        <ls_table>-add_table = zcl_dbbr_cds_view_factory=>read_ddl_ddic_view(
          zcl_dbbr_cds_view_factory=>get_ddl_for_entity_name( iv_cds_view_name = <ls_table>-add_table )
        ).
      ENDIF.

      IF <ls_table>-join_type IS INITIAL.
        <ls_table>-join_type = zif_dbbr_c_join_types=>inner_join.
      ENDIF.

      LOOP AT <ls_table>-conditions ASSIGNING <ls_condition> WHERE operator IS INITIAL OR
                                                                   ( tabname IS INITIAL AND tabname_alias IS INITIAL ).
        IF <ls_condition>-operator IS INITIAL.
          <ls_condition>-operator = zif_dbbr_c_operator=>equals.
        ENDIF.
*...... Not quite sure if this can't still result in an erroneous select
        IF <ls_condition>-tabname IS INITIAL AND <ls_condition>-tabname_alias IS INITIAL.
          <ls_condition>-tabname = <ls_table>-table_name.
        ENDIF.

        IF <ls_condition>-value_type IS INITIAL.
          <ls_condition>-value_type = zif_dbbr_c_join_cond_val_type=>typed_input.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.



  METHOD fill_entity_params.
    DATA: lr_params TYPE REF TO zdbbr_table_parameter_t.
*.. Get the correct entity in the join to fill the parameter names

    TRY.
        IF cs_join-primary_table_alias = iv_entity_alias.
          lr_params = REF #( cs_join-parameters ).
        ELSE.
          lr_params = REF #( cs_join-tables[ add_table_alias = iv_entity_alias ]-parameters ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF lr_params IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(lo_cds_view) = zcl_dbbr_cds_view_factory=>read_cds_view( iv_entity ).
        lr_params->* = VALUE #(
          FOR <cds_param> IN lo_cds_view->get_parameters( )
          ( param_name = <cds_param>-parametername )
        ).
      CATCH zcx_dbbr_data_read_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
