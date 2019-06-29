CLASS zcl_dbbr_custom_f4_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS fetch_f4_values
      IMPORTING
        !iv_entered_value        TYPE dynfieldvalue OPTIONAL
        !is_custom_f4_definition TYPE zdbbr_f4_data
      EXPORTING
        !er_dynamic_table        TYPE REF TO data
        !ev_key_field            TYPE fieldname
        !ef_cancelled            TYPE boolean .
    CLASS-METHODS select_custom_f4_help
      IMPORTING
        !it_f4_helps                TYPE zdbbr_f4_data_itab
      RETURNING
        VALUE(rv_index_of_selected) TYPE sy-tabix .
    CLASS-METHODS call_custom_f4
      IMPORTING
        !if_multiple_select TYPE boolean OPTIONAL
        !iv_selfield_name   TYPE dynfnam OPTIONAL
        !iv_selvalue        TYPE dynfieldvalue OPTIONAL
        !it_f4_definition   TYPE zdbbr_f4_data_itab
        !iv_current_line    LIKE sy-tabix OPTIONAL
        !if_low             TYPE boolean
      CHANGING
        !cs_selfield        TYPE zdbbr_selfield
        !ct_selfield        TYPE zdbbr_selfield_itab OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_table_alias_map,
             tabname   TYPE string,
             alias     TYPE string,
             alv_alias TYPE string,
           END OF ty_table_alias_map.
    CLASS-DATA mt_table_alias_map TYPE STANDARD TABLE OF ty_table_alias_map.

    CLASS-METHODS build_f4_range_table
      IMPORTING
        !iv_key_field          TYPE fieldname
        !iv_table_of_key_field TYPE tabname
        !iv_entered_value      TYPE dynfieldvalue
      CHANGING
        !ct_selected_ranges    TYPE rsds_trange .
    CLASS-METHODS filtering_is_possible
      IMPORTING
        !it_fields         TYPE zdbbr_f4_field_itab
      RETURNING
        VALUE(rf_possible) TYPE boolean .
    CLASS-METHODS filter_values
      IMPORTING
        !iv_entered_value      TYPE dynfieldvalue OPTIONAL
        !iv_key_field          TYPE fieldname
        !iv_table_of_key_field TYPE tabname
        !it_fields             TYPE zdbbr_f4_field_itab
        !is_join_def           TYPE zdbbr_join_def OPTIONAL
      EXPORTING
        !ef_cancelled          TYPE boolean
        !et_where              TYPE STANDARD TABLE .
    CLASS-METHODS build_table_alias_map
      IMPORTING
        is_join_def TYPE zdbbr_join_def.
ENDCLASS.



CLASS zcl_dbbr_custom_f4_helper IMPLEMENTATION.


  METHOD build_f4_range_table.
    " does the entered value contain a wildcard character
    DATA(lv_option) = COND option( WHEN contains( val = iv_entered_value sub = '*' ) THEN 'CP' ELSE 'EQ' ).

    ct_selected_ranges = VALUE #(
      ( tablename  = iv_table_of_key_field
        frange_t = VALUE #(
           ( fieldname = iv_key_field
             selopt_t  = VALUE #(
               ( low = iv_entered_value
                 sign = 'I'
                 option = lv_option )
             )
           )
        )
      )
    ).
  ENDMETHOD.


  METHOD call_custom_f4.
*&---------------------------------------------------------------------*
*& Description: Calls custom f4 help
*&---------------------------------------------------------------------*
    DATA: ls_f4_definition TYPE zdbbr_f4_data.

    IF lines( it_f4_definition ) > 1.
      DATA(lt_f4_helps) = VALUE zdbbr_f4_data_itab( FOR f4_def IN it_f4_definition ( CORRESPONDING #( f4_def ) ) ).
      DATA(lv_index) = select_custom_f4_help( it_f4_helps = lt_f4_helps ).
      IF lv_index <= 0.
        " no values was chosen
        RETURN.
      ELSE.
        ls_f4_definition = it_f4_definition[ lv_index ].
      ENDIF.
    ELSE.
      ls_f4_definition = it_f4_definition[ 1 ].
    ENDIF.

    " is this f4 help a built in f4 help ?
    IF ls_f4_definition-is_built_in = abap_true.
      DATA(lv_tablename) = ls_f4_definition-fields[ 1 ]-search_table.
      DATA(lv_fieldname) = ls_f4_definition-fields[ 1 ]-search_field.

      IF if_multiple_select = abap_true.
        zcl_dbbr_f4_helper=>call_built_in_f4_multi(
          EXPORTING
            if_low          = if_low
            iv_tablename    = lv_tablename
            iv_fieldname    = lv_fieldname
            iv_current_line = iv_current_line
          CHANGING
            cs_selfield     = cs_selfield
            ct_selfield     = ct_selfield
        ).
      ELSE.
        zcl_dbbr_f4_helper=>call_built_in_selfield_f4(
          EXPORTING
            if_low           = if_low
            iv_selfield_name = iv_selfield_name
            iv_selvalue      = iv_selvalue
            iv_tablename     = lv_tablename
            iv_fieldname     = lv_fieldname
          CHANGING
            cs_selfields     = cs_selfield
        ).
      ENDIF.
    ELSE.
      " 2) get values for custom f4 call
      fetch_f4_values(
        EXPORTING
          is_custom_f4_definition = ls_f4_definition
          iv_entered_value        = iv_selvalue
        IMPORTING
          er_dynamic_table        = DATA(lr_dynamic_value_tab)
          ev_key_field            = DATA(lv_key_field_name)
          ef_cancelled            = DATA(lf_f4_cancelled)
      ).

      IF lf_f4_cancelled = abap_true.
        RETURN.
      ENDIF.

      FIELD-SYMBOLS: <lt_value_tab> TYPE STANDARD TABLE.
      ASSIGN lr_dynamic_value_tab->* TO <lt_value_tab>.

      DATA: lt_return TYPE TABLE OF ddshretval.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = lv_key_field_name
          window_title    = ls_f4_definition-description
          value_org       = 'S'
          display         = 'F'
          multiple_choice = if_multiple_select
        TABLES
          value_tab       = <lt_value_tab>
          return_tab      = lt_return
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      " Transfer selected value result to dynprofield
      IF sy-subrc = 0 AND NOT lt_return IS INITIAL.
        IF if_low = abap_true.
          ASSIGN cs_selfield-low TO FIELD-SYMBOL(<lv_selvalue>).
        ELSE.
          ASSIGN cs_selfield-high TO <lv_selvalue>.
        ENDIF.

        IF if_multiple_select = abap_true.
          DATA(lv_current_line) = iv_current_line.
          LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
            DATA(lv_tabix) = sy-tabix.

            IF  ls_f4_definition-perform_alpha_conversion = abap_true OR
                ls_f4_definition-perform_alpha_conv_assgmt = abap_true.
              zcl_dbbr_data_converter=>perform_alpha_conversion_input(
                EXPORTING
                  iv_tabname   = ls_f4_definition-fields[ is_search_key = abap_true ]-search_table
                  iv_fieldname = ls_f4_definition-fields[ is_search_key = abap_true ]-search_field
                  iv_value     = <ls_return>-fieldval
                IMPORTING
                  ev_output    = <ls_return>-fieldval
              ).
            ENDIF.
            <lv_selvalue> = <ls_return>-fieldval.
            IF <lv_selvalue> <> space AND <lv_selvalue> <> '#'.
              IF cs_selfield-lowercase <> abap_true.
                TRANSLATE cs_selfield-low TO UPPER CASE. "#EC TRANSLANG
              ENDIF.

              zcl_dbbr_data_converter=>convert_selopt_to_int_format(
                EXPORTING iv_tabname   = cs_selfield-tabname
                          iv_fieldname = cs_selfield-fieldname
                CHANGING  cv_value1    = <lv_selvalue>
              ).

            ENDIF.

            IF lv_tabix = 1 OR if_low = abap_false.
              MODIFY ct_selfield FROM cs_selfield INDEX lv_current_line.
            ELSE.
              INSERT cs_selfield INTO ct_selfield INDEX lv_current_line.
            ENDIF.
            ADD 1 TO lv_current_line.
          ENDLOOP.
        ELSE.
          DATA(lv_returnval) = lt_return[ 1 ]-fieldval.
          IF  ls_f4_definition-perform_alpha_conversion = abap_true OR
              ls_f4_definition-perform_alpha_conv_assgmt = abap_true.
            zcl_dbbr_data_converter=>perform_alpha_conversion_input(
              EXPORTING
                iv_tabname   = ls_f4_definition-fields[ is_search_key = abap_true ]-search_table
                iv_fieldname = ls_f4_definition-fields[ is_search_key = abap_true ]-search_field
                iv_value     = lv_returnval
              IMPORTING
                ev_output    = lv_returnval
            ).
          ENDIF.
          <lv_selvalue> = lv_returnval.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD fetch_f4_values.
*&---------------------------------------------------------------------*
*& Description: Creates dynamic table and selects values for f4 call
*&---------------------------------------------------------------------*
    DATA: lt_select          TYPE TABLE OF string,
          lt_tables          TYPE HASHED TABLE OF tabname WITH UNIQUE KEY table_line,
          lt_component_table TYPE abap_component_tab,
          lt_where           TYPE TABLE OF se16n_where_132,
          lt_from_clause     TYPE zdbbr_string_t,
          lt_fieldcat        TYPE lvc_t_fcat,
          lt_sort            TYPE TABLE OF string
          .

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    CHECK is_custom_f4_definition-fields IS NOT INITIAL.

    DATA(lt_f4_fields) = is_custom_f4_definition-fields.

    " determine the key field
    TRY.
        DATA(ls_search_field) = lt_f4_fields[ is_search_key = abap_true ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    " 1) check if there are any joins needed for this search help
    DATA(lf_joins_needed) = xsdbool( is_custom_f4_definition-join_def-tables IS NOT INITIAL ).

    IF lf_joins_needed = abap_true.
      lt_from_clause = zcl_dbbr_join_helper=>build_from_clause_for_join_def(
        is_join_def        = is_custom_f4_definition-join_def
      ).
      build_table_alias_map( is_custom_f4_definition-join_def ).
      ev_key_field = mt_table_alias_map[ tabname = ls_search_field-search_table ]-alv_alias && '_' && ls_search_field-search_field.
    ELSE. " fields are all in one table
      APPEND ls_search_field-search_table TO lt_from_clause.
      ev_key_field = ls_search_field-search_field.
    ENDIF.

    " 2) filter values
    IF filtering_is_possible( lt_f4_fields ).
      filter_values( EXPORTING it_fields          = lt_f4_fields
                               iv_entered_value   = iv_entered_value
                               iv_key_field       = ls_search_field-search_field
                               iv_table_of_key_field = ls_search_field-search_table
                               is_join_def        = is_custom_f4_definition-join_def
                     IMPORTING et_where           = lt_where
                               ef_cancelled       = ef_cancelled ).
      IF ef_cancelled = abap_true.
        RETURN.
      ENDIF.
    ELSE.
      " build where clause for entered key field value
      IF iv_entered_value IS NOT INITIAL.
        DATA(lt_entered_value_selopt) = VALUE zdbbr_selopt_itab(
          ( low    = iv_entered_value
            option = COND option( WHEN contains( val = iv_entered_value sub = '*' ) THEN 'CP' ELSE 'EQ' )
            sign   = 'I' )
        ).
        DATA(lv_entered_value_low) = COND string(
          WHEN lf_joins_needed = abap_true AND
               ls_search_field-search_table_alias IS NOT INITIAL THEN
            ls_search_field-search_table_alias && `~` && ls_search_field-search_field
          WHEN lf_joins_needed = abap_true AND
               ls_search_field-search_table_alias IS INITIAL THEN
            mt_table_alias_map[ tabname = ls_search_field-search_table ]-alias && `~` && ls_search_field-search_field
          ELSE
            ls_search_field-search_field
        ).

        lt_where = VALUE #( ( |{ lv_entered_value_low } IN @lt_entered_value_selopt| ) ).
      ENDIF.
    ENDIF.

    DATA(lv_field_count) = lines( lt_f4_fields ).

    " 3) Get column information for those tables, build the field catalog , build the select clause
    LOOP AT lt_f4_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      DATA(lv_tabix) = sy-tabix.

      DATA(lv_sql_name) = ``.
      DATA(lv_alias) = ``.
      DATA(lv_alias_name) = ``.
      IF lf_joins_needed = abap_true.

        IF <ls_field>-search_table_alias IS NOT INITIAL.
          lv_alias = mt_table_alias_map[ alias = <ls_field>-search_table_alias ]-alv_alias.
          lv_sql_name = mt_table_alias_map[ alias = <ls_field>-search_table_alias ]-alias && '~' && <ls_field>-search_field.
        ELSE.
          lv_alias = mt_table_alias_map[ tabname = <ls_field>-search_table ]-alv_alias.
          lv_sql_name = mt_table_alias_map[ tabname = <ls_field>-search_table ]-alias && '~' && <ls_field>-search_field.
        ENDIF.
        lv_alias_name = lv_alias && '_' && <ls_field>-search_field.
      ELSE.
        lv_alias_name =
        lv_sql_name = <ls_field>-search_field.
      ENDIF.

      APPEND VALUE abap_componentdescr(
          name       = lv_alias_name
          type       = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( <ls_field>-search_table && '-' && <ls_field>-search_field ) )
      ) TO lt_component_table.

      " add field to select table
      DATA(lv_select_field) = COND #(
        WHEN lf_joins_needed = abap_true THEN
          lv_sql_name && ` AS ` && lv_alias_name
        ELSE
          lv_sql_name
      ).
      IF lv_tabix <> lv_field_count.
        lv_select_field = lv_select_field && ','.
      ENDIF.

      APPEND lv_select_field TO lt_select.

      " does this field support sorting?
      IF <ls_field>-sort_active = abap_true.
        DATA(lv_separator) = COND #( WHEN lt_sort IS NOT INITIAL THEN `, ` ).
        lt_sort = VALUE #( BASE lt_sort ( |{ lv_separator }{ lv_sql_name } ASCENDING| ) ).
      ENDIF.
    ENDLOOP.

    " Create the structure type
    DATA(lr_new_type) = cl_abap_structdescr=>create( p_components = lt_component_table
                                                     p_strict = abap_false ).
    " create the table type for the structure type
    DATA(lr_new_tab) = cl_abap_tabledescr=>create(
      p_line_type  = lr_new_type
      p_table_kind = cl_abap_tabledescr=>tablekind_std
      p_unique     = abap_false
    ).

    CREATE DATA er_dynamic_table TYPE HANDLE lr_new_tab.
    ASSIGN er_dynamic_table->* TO <lt_table>.

    TRY.
        " select necessary data
        SELECT (lt_select) FROM (lt_from_clause) INTO CORRESPONDING FIELDS OF TABLE @<lt_table>
          UP TO 500 ROWS
        WHERE (lt_where)
        ORDER BY (lt_sort).
      CATCH cx_sy_open_sql_db.
        MESSAGE 'Error during SQL' TYPE 'S'.
      CATCH cx_sy_dynamic_osql_semantics INTO DATA(lr_osql_sem_exc).
        MESSAGE 'Error during parsing of dynamic SQL' TYPE 'S'.
      CATCH cx_sy_dynamic_osql_syntax.
        MESSAGE 'Error during parsing of dynamic SQL' TYPE 'S'.
    ENDTRY.

  ENDMETHOD.


  METHOD filtering_is_possible.
*& Description: Check if field value restriction (filtering) is allowed
*&---------------------------------------------------------------------*
    IF line_exists( it_fields[ allow_restriction = abap_true ] ).
      rf_possible = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD filter_values.
*&---------------------------------------------------------------------*
*& Description: Gets where clauses for filter fields from
*& free selection dialog FREE_SELECTIONS_DIALOG
*&---------------------------------------------------------------------*
    DATA: lv_selection_id       TYPE rsdynsel-selid,
          lt_selection_fields   TYPE TABLE OF rsdsfields,
          lt_selected_ranges    TYPE rsds_trange,
          lt_free_sel_where     TYPE rsds_twhere,
          lt_where              TYPE TABLE OF se16n_where_132,
          lv_filter_field_count TYPE i.

    """ create selection fields for free selection
    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_filter_field>) WHERE allow_restriction = abap_true.
      APPEND VALUE #( tablename = <ls_filter_field>-search_table
                      fieldname = <ls_filter_field>-search_field ) TO lt_selection_fields ASSIGNING FIELD-SYMBOL(<ls_filter_value>).
      " was a value supplied for the key field
      IF <ls_filter_field>-search_field = iv_key_field AND
         <ls_filter_field>-search_table = iv_table_of_key_field AND
          iv_entered_value IS NOT INITIAL.
        build_f4_range_table( EXPORTING iv_key_field          = iv_key_field
                                        iv_table_of_key_field = iv_table_of_key_field
                                        iv_entered_value      = iv_entered_value
                              CHANGING  ct_selected_ranges    = lt_selected_ranges ).
      ENDIF.
    ENDLOOP.

    """ create free selections dialog
    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind         = 'F'
      IMPORTING
        selection_id = lv_selection_id
      TABLES
        fields_tab   = lt_selection_fields
      EXCEPTIONS
        OTHERS       = 1.


    """ display free selections dialog
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id            = lv_selection_id
        title                   = 'Value Restriction'
        frame_text              = 'Selection'
        status                  = 1
        as_window               = 'X'
        tree_visible            = abap_false
        no_intervals            = abap_true
      IMPORTING
        field_ranges            = lt_selected_ranges
        number_of_active_fields = lv_filter_field_count
      TABLES
        fields_tab              = lt_selection_fields
      EXCEPTIONS
        no_action               = 1
        OTHERS                  = 2.

    IF sy-subrc = 1.
      ef_cancelled = abap_true.
      RETURN.
    ENDIF.

    """ no filtering occurred
    IF lv_filter_field_count = 0.
      RETURN.
    ENDIF.

    DATA(lf_join_is_active) = xsdbool( is_join_def IS NOT INITIAL ).

    IF lf_join_is_active = abap_true.
      """ enrich tablefields with table prefix - if necessary
      LOOP AT lt_selected_ranges ASSIGNING FIELD-SYMBOL(<ls_range>).
        """ determine table alias
        DATA(lv_alias) = mt_table_alias_map[ tabname = <ls_range>-tablename ]-alias.
        LOOP AT <ls_range>-frange_t ASSIGNING FIELD-SYMBOL(<ls_range_single_line>).
          <ls_range_single_line>-fieldname = lv_alias && '~' && <ls_range_single_line>-fieldname.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    """ get where conditions for ranges
    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = lt_selected_ranges
      IMPORTING
        where_clauses = lt_free_sel_where.

    """ create where for select
    LOOP AT lt_free_sel_where ASSIGNING FIELD-SYMBOL(<ls_where>).
      DATA(lv_tabix) = sy-tabix.
      IF lv_tabix <> 1.
        APPEND 'AND' TO et_where.
      ENDIF.

      APPEND LINES OF <ls_where>-where_tab TO et_where.
    ENDLOOP.


  ENDMETHOD.


  METHOD select_custom_f4_help.
*&---------------------------------------------------------------------*
*& Description: Display all available search helps, that the user
*& can choose from.
*&---------------------------------------------------------------------*
    DATA: lt_fieldcat TYPE lvc_t_fcat,
          ls_selfield TYPE slis_selfield,
          lf_exit     TYPE abap_bool.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_buffer_active        = ' '
        i_structure_name       = 'ZDBBR_F4_OVERVIEW'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    " only display the description of the search help
    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      IF <ls_fieldcat>-fieldname <> 'DESCRIPTION'.
        <ls_fieldcat>-no_out = abap_true.
      ENDIF.
    ENDLOOP.

    DATA(lt_f4_info) = CORRESPONDING zdbbr_f4_overview_itab( it_f4_helps ).

    " Show popup with the options and give one back
    CALL FUNCTION 'LVC_SINGLE_ITEM_SELECTION'
      EXPORTING
        i_title         = |{ 'Choose Value Help'(001) }|
        it_fieldcatalog = lt_fieldcat
      IMPORTING
        es_selfield     = ls_selfield
        e_exit          = lf_exit
      TABLES
        t_outtab        = lt_f4_info.

    IF lf_exit <> abap_true.
      rv_index_of_selected = ls_selfield-tabindex.
    ENDIF.

  ENDMETHOD.

  METHOD build_table_alias_map.
    mt_table_alias_map = VALUE #( ( alias     = is_join_def-primary_table_alias
                                    alv_alias = is_join_def-primary_table_alias_alv
                                    tabname   = is_join_def-primary_table  ) ).

    LOOP AT is_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>).
      mt_table_alias_map = VALUE #( BASE mt_table_alias_map
        ( alias     = <ls_join_table>-add_table_alias
          alv_alias = <ls_join_table>-add_table_alias_alv
          tabname   = <ls_join_table>-add_table  )
      ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
