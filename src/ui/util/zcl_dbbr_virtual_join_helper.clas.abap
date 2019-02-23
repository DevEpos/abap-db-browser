"! <p class="shorttext synchronized" lang="en">Helper for virtual joins</p>
CLASS zcl_dbbr_virtual_join_helper DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Creates new instance of virtual join helper</p>
    "!
    CLASS-METHODS create
      IMPORTING
        !is_join_def   TYPE zdbbr_join_def
        !ir_fields     TYPE REF TO zcl_dbbr_tabfield_list
        !ir_fields_all TYPE REF TO zcl_dbbr_tabfield_list
      RETURNING
        VALUE(result)  TYPE REF TO zcl_dbbr_virtual_join_helper .
    "! <p class="shorttext synchronized" lang="en">Fills the cache tables with values</p>
    "!
    METHODS fill_cache_tables
      IMPORTING
        !it_table TYPE STANDARD TABLE
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Checks if post SQL Join exists</p>
    "!
    METHODS post_join_exists
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Process the the virtual joins for each line</p>
    "!
    METHODS process_table
      IMPORTING
        !ir_table     TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_post_join_data TYPE zdbbr_post_join_data_itab .
    DATA mr_fields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mr_fields_all TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mt_conditional_fields TYPE zdbbr_tabfield_info_ui_itab .
    DATA mv_primary_table TYPE zdbbr_join_data_ui-primary_table .
    DATA mv_and_string TYPE string .
    DATA mv_or_string TYPE string .

    "! <p class="shorttext synchronized" lang="en">Builds where clause for all entries cache select</p>
    "!
    METHODS build_for_all_where_clause
      IMPORTING
        !is_post_join   TYPE zdbbr_post_join_data
      RETURNING
        VALUE(rt_where) TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Builds where clause for cache select</p>
    "!
    METHODS build_where_clause
      IMPORTING
        !is_post_join   TYPE zdbbr_post_join_data
      RETURNING
        VALUE(rt_where) TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Builds where condition out of join and current output line</p>
    "!
    METHODS build_where_for_cache_query
      IMPORTING
        !is_virtual_join TYPE zdbbr_post_join_data
        !is_line         TYPE any
      RETURNING
        VALUE(rt_where)  TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        !is_join_def   TYPE zdbbr_join_def
        !ir_fields     TYPE REF TO zcl_dbbr_tabfield_list
        !ir_fields_all TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Creates all needed cache tables</p>
    "!
    METHODS create_cache_tables .
    "! <p class="shorttext synchronized" lang="en">Fills cache with FOR ALL ENTRIES Select</p>
    "!
    METHODS fill_cache_for_all_select
      IMPORTING
        !it_table     TYPE STANDARD TABLE
      CHANGING
        !cs_post_join TYPE zdbbr_post_join_data
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Fills cache with normal SELECT due to needed conversion</p>
    "!
    METHODS fill_cache_single_select
      IMPORTING
        !it_table     TYPE STANDARD TABLE
      CHANGING
        !cs_post_join TYPE zdbbr_post_join_data
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Fills a single cache table with values</p>
    "!
    METHODS fill_cache_table
      IMPORTING
        !it_table     TYPE STANDARD TABLE
      CHANGING
        !cs_post_join TYPE zdbbr_post_join_data
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Fills all the virtual output fields for a single line</p>
    "!
    "!
    METHODS fill_output_fields
      IMPORTING
        !is_virtual_join TYPE zdbbr_post_join_data
        !it_cache        TYPE ANY TABLE
      CHANGING
        !cs_line         TYPE any
        !cr_t_new_lines  TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Get correct value for condition for WHERE Clause</p>
    "!
    METHODS get_value_for_where
      IMPORTING
        !iv_value      TYPE any
        !is_field_cond TYPE zdbbr_join_field_cond_ui
      RETURNING
        VALUE(result)  TYPE string .
    "! <p class="shorttext synchronized" lang="en">Parse filter conditions for virtual join table</p>
    "!
    METHODS parse_virtjtab_filter_cond
      IMPORTING
        !it_filter_cond      TYPE zdbbr_join_filter_cond_ui_t
      RETURNING
        VALUE(rt_conditions) TYPE zcl_dbbr_join_helper=>tt_join_conditions .
    "! <p class="shorttext synchronized" lang="en">Parse field conditions for virtual join table</p>
    "!
    METHODS parst_virtjtab_field_cond
      IMPORTING
        !it_field_cond       TYPE zdbbr_join_field_cond_ui_t
        !is_line             TYPE any
      RETURNING
        VALUE(rt_conditions) TYPE zcl_dbbr_join_helper=>tt_join_conditions .
    "! <p class="shorttext synchronized" lang="en">Processes a single line in the output table</p>
    "!
    METHODS process_line
      CHANGING
        !cs_line        TYPE any
        !cr_t_new_lines TYPE REF TO data .
ENDCLASS.



CLASS zcl_dbbr_virtual_join_helper IMPLEMENTATION.


  METHOD build_for_all_where_clause.

    DATA: lt_conditions TYPE zcl_dbbr_join_helper=>tt_join_conditions.

*.. parse field conditions
    LOOP AT is_post_join-field_cond ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
      DATA(lv_index) = sy-tabix.

      DATA(lv_offset) = CONV int2( <ls_field_cond>-off_offset ).
      DATA(lv_length) = CONV int2( <ls_field_cond>-off_length ).

      DATA(lv_length_suffix) = COND string(
        WHEN lv_offset IS NOT INITIAL AND lv_length IS NOT INITIAL THEN
            |+{ lv_offset }({ lv_length })|
        WHEN lv_length IS NOT INITIAL THEN
            |({ lv_length })|
      ).

      lt_conditions = VALUE #( BASE lt_conditions
        ( join_operator = COND #( WHEN lv_index = 1 THEN | | ELSE mv_and_string )
          value         = |{ <ls_field_cond>-field } { <ls_field_cond>-operator } | &&
                          |it_table-{ <ls_field_cond>-ref_field_alv }{ lv_length_suffix }| )
      ).
    ENDLOOP.

*.. parse the filter conditions for the given virtual join table
    lt_conditions = VALUE #( BASE lt_conditions
      ( LINES OF parse_virtjtab_filter_cond( is_post_join-filter_cond ) )
    ).

*.. finally create the where string table for the select
    rt_where = zcl_dbbr_join_helper=>build_where_for_conditions( lt_conditions ).

  ENDMETHOD.


  METHOD build_where_clause.
    DATA: lt_conditions TYPE zcl_dbbr_join_helper=>tt_join_conditions.

*.. parse field conditions
    LOOP AT is_post_join-field_cond ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
      DATA(lv_index) = sy-tabix.

      lt_conditions = VALUE #( BASE lt_conditions
        ( join_operator = COND #( WHEN lv_index = 1 THEN | | ELSE mv_and_string )
          value         = |{ <ls_field_cond>-field } { <ls_field_cond>-operator } | &&
                          |<ls_data>-{ <ls_field_cond>-ref_field_alv }| )
      ).
    ENDLOOP.

*.. parse the filter conditions for the given virtual join table
    lt_conditions = VALUE #( BASE lt_conditions
      ( LINES OF parse_virtjtab_filter_cond( is_post_join-filter_cond ) )
    ).

*.. finally create the where string table for the select
    rt_where = zcl_dbbr_join_helper=>build_where_for_conditions( lt_conditions ).

  ENDMETHOD.


  METHOD build_where_for_cache_query.
    DATA: lt_conditions TYPE zcl_dbbr_join_helper=>tt_join_conditions.

*.. parse field conditions
    LOOP AT is_virtual_join-field_cond ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
      DATA(lv_index) = sy-tabix.

      ASSIGN COMPONENT <ls_field_cond>-ref_field_alv OF STRUCTURE is_line TO FIELD-SYMBOL(<lv_ref_value>).
      CHECK sy-subrc = 0.

      DATA(lv_value) = cl_abap_dyn_prg=>quote(
        get_value_for_where( iv_value = <lv_ref_value> is_field_cond = <ls_field_cond> )
      ).

      lt_conditions = VALUE #( BASE lt_conditions
        ( join_operator = COND #( WHEN lv_index = 1 THEN | | ELSE mv_and_string )
          value         = |{ <ls_field_cond>-field } { <ls_field_cond>-operator } | &&
                          |{ lv_value }| )
      ).
    ENDLOOP.

**.. parse the filter conditions for the given virtual join table
*    lt_conditions = VALUE #( BASE lt_conditions
*      ( LINES OF parse_virtjtab_filter_cond( is_post_join-filter_cond ) )
*    ).

*.. finally create the where string table for the select
    LOOP AT lt_conditions ASSIGNING FIELD-SYMBOL(<ls_cond>).
      rt_where = VALUE #( BASE rt_where
        ( |{ <ls_cond>-join_operator }{ <ls_cond>-open_bracket }{ <ls_cond>-value }{ <ls_cond>-closing_bracket }| )
      ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    mr_fields = ir_fields.
    mr_fields_all = ir_fields_all.
    mv_primary_table = is_join_def-primary_table.

    LOOP AT is_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE is_virtual = abap_true.
      DATA(ls_post_join) = VALUE zdbbr_post_join_data(
          join_table = <ls_join_table>-add_table
          join_type  = <ls_join_table>-join_type
      ).

      LOOP AT <ls_join_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_join_field>).
        TRY.
            DATA(lr_field) =  mr_fields_all->get_field_ref( iv_tabname_alias    = <ls_join_table>-add_table
                                                            iv_fieldname = <ls_join_field>-field ).
          CATCH cx_sy_itab_line_not_found.
            RETURN.
        ENDTRY.

        DATA(ls_join_field) = CORRESPONDING zdbbr_join_field_cond_ui( <ls_join_field> ).
        ls_join_field-field_inttype = lr_field->inttype.

        TRY.
            DATA(lr_ref_field) = mr_fields_all->get_field_ref(
              iv_tabname_alias   = <ls_join_field>-ref_table
              iv_fieldname = <ls_join_field>-ref_field
            ).
          CATCH cx_sy_itab_line_not_found.
            RETURN.
        ENDTRY.

        ls_join_field-ref_field_alv = lr_ref_field->alv_fieldname.
        ls_join_field-ref_field_length = lr_ref_field->length.
        ls_join_field-ref_field_inttype = lr_ref_field->inttype.

        IF ls_join_field-field_inttype <> ls_join_field-ref_field_inttype.
          DATA(lf_conversion_needed) = abap_true.
          ls_join_field-is_conversion_needed = abap_true.
          ls_join_field-field_type_ref = CAST #( cl_abap_typedescr=>describe_by_name( |{ <ls_join_table>-add_table }-{ ls_join_field-field }| ) ).
        ENDIF.

        ls_post_join-field_cond = VALUE #(
           BASE ls_post_join-field_cond
           ( ls_join_field  )
        ).
      ENDLOOP.

*.... collect filter conditions as well
      LOOP AT <ls_join_table>-filter_conditions ASSIGNING FIELD-SYMBOL(<ls_filter_cond>).
        DATA(ls_filter_cond) = CORRESPONDING zdbbr_join_filter_cond_ui( <ls_filter_cond> ).
        IF <ls_filter_cond>-tabname = <ls_join_table>-add_table.
          ls_filter_cond-is_for_add_table = abap_true.
        ELSE.
          TRY.
              lr_field = mr_fields_all->get_field_ref(
                  iv_tabname_alias   = <ls_filter_cond>-tabname
                  iv_fieldname = <ls_filter_cond>-fieldname
              ).
              ls_filter_cond-alv_fieldname = lr_field->alv_fieldname.
            CATCH cx_sy_itab_line_not_found.
              CONTINUE.
          ENDTRY.
        ENDIF.

        ls_post_join-filter_cond = VALUE #( BASE ls_post_join-filter_cond ( ls_filter_cond ) ).
      ENDLOOP.

      ls_post_join-is_conversion_needed = lf_conversion_needed.
      CLEAR lf_conversion_needed.

      mt_post_join_data = VALUE #( BASE mt_post_join_data ( ls_post_join ) ).
    ENDLOOP.

    mt_conditional_fields = mr_fields->get_conditional_fields( ).

    mv_and_string = |  { 'AND'  ALIGN = RIGHT WIDTH = 5 } |.
    mv_or_string = |  { 'OR'  ALIGN = RIGHT WIDTH = 5 } |.
  ENDMETHOD.


  METHOD create.
    result = NEW #( ir_fields   = ir_fields
                    ir_fields_all = ir_fields_all
                    is_join_def = is_join_def ).
    result->create_cache_tables( ).
  ENDMETHOD.


  METHOD create_cache_tables.
    DATA: lt_fields TYPE zdbbr_dfies_itab.

    LOOP AT mt_post_join_data ASSIGNING FIELD-SYMBOL(<ls_post_join>).
      " collect key fields from join definition
      lt_fields = VALUE #(
        FOR <ls_key_field> IN <ls_post_join>-field_cond
        ( fieldname = <ls_key_field>-field
          tabname   = <ls_post_join>-join_table
          keyflag   = abap_true )
      ).

      " get output fields for join table
      lt_fields = VALUE zdbbr_dfies_itab(
        BASE lt_fields
        FOR <ls_field> IN mt_conditional_fields
        WHERE ( output_active = abap_true AND
                tabname       = <ls_post_join>-join_table )
        ( fieldname = <ls_field>-fieldname
          tabname   = <ls_field>-tabname )
      ).

      SORT lt_fields BY tabname fieldname keyflag DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_fields COMPARING tabname fieldname.

      CHECK lt_fields IS NOT INITIAL.

      <ls_post_join>-data_cache = zcl_dbbr_dictionary_helper=>build_dynamic_sorted_table(
        it_fields     = lt_fields
        if_unique_key = <ls_post_join>-is_conversion_needed
      ).

      <ls_post_join>-cache_fields = lt_fields.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_cache_for_all_select.
    DATA: lt_select TYPE TABLE OF string
*          lt_where  TYPE TABLE OF string
          .

    FIELD-SYMBOLS: <lt_cache> TYPE ANY TABLE.
    lt_select = VALUE #(
      FOR <ls_field> IN cs_post_join-cache_fields
      ( |{ <ls_field>-fieldname }| )
    ).

    DATA(lt_where) = build_for_all_where_clause( cs_post_join ).

    ASSIGN cs_post_join-data_cache->* TO <lt_cache>.

    TRY.
        SELECT DISTINCT (lt_select) INTO CORRESPONDING FIELDS OF TABLE <lt_cache>
         FROM (cs_post_join-join_table)
         FOR ALL ENTRIES IN it_table
         WHERE (lt_where).
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_dbbr_selection_common
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD fill_cache_single_select.
    DATA: lt_select     TYPE TABLE OF string,
          lt_conditions TYPE zcl_dbbr_join_helper=>tt_join_conditions,
          lt_sort_key   TYPE abap_sortorder_tab,
          lr_cache_copy TYPE REF TO data.

    FIELD-SYMBOLS: <lt_cache>      TYPE ANY TABLE,
                   <lt_cache_copy> TYPE ANY TABLE.


    LOOP AT cs_post_join-cache_fields ASSIGNING FIELD-SYMBOL(<ls_cache_field>).
      lt_select = VALUE #( BASE lt_select ( |{ <ls_cache_field>-fieldname }| ) ).
    ENDLOOP.

*.. pre-build the conditions for the filters as they do not change
    DATA(lt_filter_cond) = parse_virtjtab_filter_cond( it_filter_cond = cs_post_join-filter_cond ).

    ASSIGN cs_post_join-data_cache->* TO <lt_cache>.

    DATA(lr_cache_table_temp) = zcl_dbbr_dictionary_helper=>build_dynamic_std_table( EXPORTING it_fields = cs_post_join-cache_fields ).
    ASSIGN lr_cache_table_temp->* TO <lt_cache_copy>.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_data>).
*.... create where clause for the current line and the already created
*.... filter conditions
      DATA(lt_where) = zcl_dbbr_join_helper=>build_where_for_conditions(
        VALUE #(
          ( LINES OF parst_virtjtab_field_cond(
                        is_line       = <ls_data>
                        it_field_cond = cs_post_join-field_cond ) )
          ( LINES OF lt_filter_cond )
        )
      ).

      TRY.
          SELECT (lt_select) APPENDING CORRESPONDING FIELDS OF TABLE <lt_cache_copy>
           FROM (cs_post_join-join_table)
           WHERE (lt_where).
        CATCH cx_root INTO DATA(lx_root).
          RAISE EXCEPTION TYPE zcx_dbbr_selection_common
            EXPORTING
              previous = lx_root.
      ENDTRY.

    ENDLOOP.

    LOOP AT <lt_cache_copy> ASSIGNING FIELD-SYMBOL(<ls_cache_non_unique>).
      INSERT <ls_cache_non_unique> INTO TABLE <lt_cache>.
    ENDLOOP.

    CLEAR <lt_cache_copy>.

  ENDMETHOD.


  METHOD fill_cache_table.


    IF cs_post_join-is_conversion_needed = abap_true.
      fill_cache_single_select( EXPORTING it_table     = it_table
                                CHANGING  cs_post_join = cs_post_join ).
    ELSE.
      fill_cache_for_all_select( EXPORTING it_table     = it_table
                                 CHANGING  cs_post_join = cs_post_join ).
    ENDIF.


  ENDMETHOD.


  METHOD fill_cache_tables.
    LOOP AT mt_post_join_data ASSIGNING FIELD-SYMBOL(<ls_post_join>).
      fill_cache_table( EXPORTING it_table     = it_table
                        CHANGING  cs_post_join = <ls_post_join> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_output_fields.
    DATA: lr_t_temp TYPE REF TO data.

    FIELD-SYMBOLS: <lt_existing>  TYPE STANDARD TABLE,
                   <lt_new_lines> TYPE STANDARD TABLE.

    ASSIGN cr_t_new_lines->* TO <lt_existing>.

    CREATE DATA lr_t_temp LIKE <lt_existing>.
    ASSIGN lr_t_temp->* TO <lt_new_lines>.

*.. build where clause for current line and current virtual join table
    DATA(lt_where) = build_where_for_cache_query(
        is_virtual_join = is_virtual_join
        is_line         = cs_line
    ).

    IF <lt_existing> IS INITIAL.
      <lt_existing> = VALUE #( ( cs_line ) ).
    ENDIF.

*.. where condition was built, now select the corresponding value from the cache
    LOOP AT it_cache ASSIGNING FIELD-SYMBOL(<ls_cache_line>) WHERE (lt_where).

*.... fill output columns with found cache values
      LOOP AT <lt_existing> ASSIGNING FIELD-SYMBOL(<ls_new_line>).
        LOOP AT mt_conditional_fields ASSIGNING FIELD-SYMBOL(<ls_cond_field>) WHERE tabname = is_virtual_join-join_table.
          ASSIGN COMPONENT <ls_cond_field>-alv_fieldname OF STRUCTURE <ls_new_line> TO FIELD-SYMBOL(<lv_output_field_value>).
          ASSIGN COMPONENT <ls_cond_field>-fieldname OF STRUCTURE <ls_cache_line> TO FIELD-SYMBOL(<lv_cache_value>).
          IF <lv_output_field_value> IS ASSIGNED AND
             <lv_cache_value> IS ASSIGNED.
            <lv_output_field_value> = <lv_cache_value>.
          ENDIF.

          UNASSIGN: <lv_output_field_value>,
                    <lv_cache_value>.

        ENDLOOP.

        <lt_new_lines> = VALUE #( BASE <lt_new_lines> ( <ls_new_line> ) ).
      ENDLOOP.
    ENDLOOP.

    IF sy-subrc <> 0 AND is_virtual_join-join_type = zif_dbbr_c_join_types=>inner_join.
      CLEAR: <lt_existing>.
    ELSEIF <lt_new_lines> IS NOT INITIAL.
      <lt_existing> = <lt_new_lines>.
    ENDIF.

  ENDMETHOD.


  METHOD get_value_for_where.
    DATA:  lr_v_converted TYPE REF TO data.

*.. try type conversion if needed
    IF is_field_cond-is_conversion_needed = abap_true.
      CREATE DATA lr_v_converted TYPE HANDLE is_field_cond-field_type_ref.
      ASSIGN lr_v_converted->* TO FIELD-SYMBOL(<lv_converted>).
*.... Convert to field value
      <lv_converted> = iv_value.
*.... Condense the value if the type is c
      IF is_field_cond-field_inttype = cl_abap_typedescr=>typekind_char OR
         is_field_cond-field_inttype = cl_abap_typedescr=>typekind_clike OR
         is_field_cond-field_inttype = cl_abap_typedescr=>typekind_string.
        CONDENSE <lv_converted>.
      ENDIF.

      result = <lv_converted>.
    ELSE.
      result = iv_value.
    ENDIF.
  ENDMETHOD.


  METHOD parse_virtjtab_filter_cond.
    DATA: lv_value2             TYPE string,

          lf_parenthesis_opened TYPE abap_bool,
          lv_parenthesis_open   TYPE string,
          lv_parenthesis_closed TYPE string,

          ls_new_condition      TYPE zcl_dbbr_join_helper=>ty_join_condition,
          lv_previous_join_cond TYPE string.

    FIELD-SYMBOLS: <ls_last_condition> TYPE zcl_dbbr_join_helper=>ty_join_condition.

*.. parse the filter conditions
    LOOP AT it_filter_cond INTO DATA(ls_filter_cond) WHERE is_for_add_table = abap_true.
      DATA(lv_index) = sy-tabix.
      CLEAR: lv_value2.

*.... check for need of possible open parenthesis
      IF ls_filter_cond-and_or = 'OR'.
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

*.... handle LIKE and NOT LIKE operator
      IF ls_filter_cond-operator = zif_dbbr_c_operator=>like OR
         ls_filter_cond-operator = zif_dbbr_c_operator=>not_like.
        ls_filter_cond-value = replace( val = ls_filter_cond-value sub = '*' with = '%' occ = 0 ).

        IF ls_filter_cond-value2 IS NOT INITIAL.
          ls_filter_cond-value2 = replace( val = ls_filter_cond-value2 sub = '*' with = '%' occ = 0 ).
        ENDIF.
      ENDIF.

      DATA(lv_value1) = COND #(
        WHEN ls_filter_cond-value_type = zif_dbbr_c_join_cond_val_type=>system_value_input THEN
          |@{ ls_filter_cond-value }|
        ELSE
          cl_abap_dyn_prg=>quote( ls_filter_cond-value )
      ).

      IF ls_filter_cond-operator = 'BETWEEN'.
        lv_value2 = COND #(
          WHEN ls_filter_cond-value_type = zif_dbbr_c_join_cond_val_type=>system_value_input THEN
            |@{ ls_filter_cond-value2 }|
          ELSE
            cl_abap_dyn_prg=>quote( ls_filter_cond-value2 )
        ).
        lv_value2 = | AND { lv_value2 }|.
      ENDIF.

      rt_conditions = VALUE #( BASE rt_conditions
        ( open_bracket    = lv_parenthesis_open
          join_operator   = COND #( WHEN lv_index = 1 THEN
                                      mv_and_string
                                    ELSE
                                      lv_previous_join_cond )
          value           = |{ ls_filter_cond-fieldname }| &&
                            | { ls_filter_cond-operator } | &&
                            |{ lv_value1 }{ lv_value2 }|
          closing_bracket = lv_parenthesis_closed )
      ).

      lv_previous_join_cond = COND #( WHEN ls_filter_cond-and_or = zif_dbbr_c_selection_condition=>or THEN
                                        mv_or_string
                                      ELSE
                                        mv_and_string ).
      CLEAR: lv_parenthesis_closed,
             lv_parenthesis_open.

      IF lf_parenthesis_opened = abap_true.
        lv_parenthesis_open = |  |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD parst_virtjtab_field_cond.

*.. parse field conditions
    LOOP AT it_field_cond ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
      DATA(lv_index) = sy-tabix.

      ASSIGN COMPONENT <ls_field_cond>-ref_field_alv OF STRUCTURE is_line TO FIELD-SYMBOL(<lv_ref_value>).
      CHECK sy-subrc = 0.

      DATA(lv_value) = cl_abap_dyn_prg=>quote(
        get_value_for_where( iv_value = <lv_ref_value> is_field_cond = <ls_field_cond> )
      ).

      rt_conditions = VALUE #( BASE rt_conditions
        ( join_operator = COND #( WHEN lv_index = 1 THEN | | ELSE mv_and_string )
          value         = |{ <ls_field_cond>-field } { <ls_field_cond>-operator } | &&
                          |{ lv_value }| )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD post_join_exists.
    result = xsdbool( mt_post_join_data IS NOT INITIAL ).
  ENDMETHOD.


  METHOD process_line.
    DATA: lr_t_temp TYPE REF TO data.

    FIELD-SYMBOLS: <lt_cache>     TYPE ANY TABLE,
                   <lt_temp>      TYPE STANDARD TABLE,
                   <lt_new_lines> TYPE STANDARD TABLE.

    ASSIGN cr_t_new_lines->* TO <lt_new_lines>.

    CREATE DATA lr_t_temp LIKE <lt_new_lines>.
    ASSIGN lr_t_temp->* TO <lt_temp>.

    LOOP AT mt_post_join_data ASSIGNING FIELD-SYMBOL(<ls_post_join>).

      ASSIGN <ls_post_join>-data_cache->* TO <lt_cache>.
      fill_output_fields( EXPORTING is_virtual_join = <ls_post_join>
                                    it_cache        = <lt_cache>
                          CHANGING  cs_line         = cs_line
                                    cr_t_new_lines  = lr_t_temp ).

      " if no new lines were created, all following joins can be
      " ignored
      IF <lt_temp> IS INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.

    <lt_new_lines> = VALUE #( BASE <lt_new_lines> ( LINES OF <lt_temp> ) ).

  ENDMETHOD.


  METHOD process_table.
    DATA: lr_t_temp TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE,
                   <lt_temp> TYPE STANDARD TABLE.

    zcl_dbbr_screen_helper=>show_progress(
        iv_progress = 50
        iv_text     = 'Virtual Joins are processed...'
    ).

    ASSIGN ir_table->* TO <lt_data>.

    CREATE DATA lr_t_temp LIKE <lt_data>.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      process_line(
        CHANGING cs_line        = <ls_data>
                 cr_t_new_lines = lr_t_temp
      ).
      DELETE <lt_data>.
    ENDLOOP.

    ASSIGN lr_t_temp->* TO <lt_temp>.
    <lt_data> = <lt_temp>.

    result = xsdbool( <lt_data> IS NOT INITIAL ).

    IF result = abap_false.
      " did virtual join result in empty result?
      MESSAGE i047(zdbbr_info) WITH mv_primary_table.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
