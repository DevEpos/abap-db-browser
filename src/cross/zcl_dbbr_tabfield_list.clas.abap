"! <p class="shorttext synchronized" lang="en">Wrapper for list of table fields</p>
CLASS zcl_dbbr_tabfield_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_data_ref_list .

    ALIASES add
      FOR zif_uitb_data_ref_list~add .

    "! <p class="shorttext synchronized" lang="en">Create list from serialized data</p>
    CLASS-METHODS create_from_serialized
      IMPORTING
        !is_serialized_tabfields TYPE zdbbr_tabfield_list_data
      RETURNING
        VALUE(rr_list)           TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS active_field_exists
      IMPORTING
        !iv_tabname       TYPE tabname
        !iv_fieldname     TYPE fieldname
        !if_is_text_field TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rf_exists)  TYPE boolean .
    METHODS add_fields
      IMPORTING
        !ir_field_list TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Adds table to table list</p>
    METHODS add_table
      IMPORTING
        !is_entity_info TYPE zdbbr_entity_info .
    METHODS append_tabfield_info
      IMPORTING
        !is_tabfield TYPE zdbbr_tabfield_info_ui .
    METHODS build_complete_fieldnames .
    METHODS build_table_to_alias_map
      IMPORTING
        !if_update_tables            TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_table_to_alias_map) TYPE zdbbr_table_to_alias_map_itab .
    METHODS checked_field_exists
      IMPORTING
        !iv_specific_table TYPE tabname OPTIONAL
      RETURNING
        VALUE(rf_exists)   TYPE boolean .
    METHODS clear .
    METHODS clear_active_flag
      IMPORTING
        !iv_tablename       TYPE tabname OPTIONAL
        !if_clear_output    TYPE boolean OPTIONAL
        !if_clear_selection TYPE boolean OPTIONAL
        !if_clear_sort      TYPE boolean OPTIONAL .
    METHODS clear_alias_names .
    METHODS clear_calculation_flag .
    METHODS constructor
      IMPORTING
        !it_fields      TYPE zdbbr_tabfield_info_ui_itab OPTIONAL
        !it_tables      TYPE zdbbr_entity_info_t OPTIONAL
        !iv_mode        TYPE zdbbr_field_chooser_mode DEFAULT zif_dbbr_global_consts=>gc_field_chooser_modes-output
        !iv_entity_type TYPE zdbbr_entity_type DEFAULT zif_dbbr_c_entity_type=>table .
    "! <p class="shorttext synchronized" lang="en">Converts list to deep structure</p>
    METHODS convert_to_structure
      RETURNING
        VALUE(result) TYPE zdbbr_tabfield_list_data .
    METHODS copy
      RETURNING
        VALUE(rr_copy) TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS create_order_from_active .
    METHODS custom_order_exists
      RETURNING
        VALUE(rf_exists) TYPE boolean .
    METHODS delete_field
      IMPORTING
        !is_field TYPE zdbbr_tabfield_info_ui .
    METHODS delete_fields
      IMPORTING
        !it_fields_selopt TYPE zdbbr_selopt_itab .
    METHODS delete_fields_by_alv_fieldname
      IMPORTING
        !it_fields_selopt TYPE zdbbr_selopt_itab .
    METHODS delete_formula_fields .
    METHODS delete_inactive_fields .
    METHODS delete_text_fields .
    "! <p class="shorttext synchronized" lang="en">Deletion of tables/fields by custom criteria</p>
    "!
    METHODS delete_custom
      IMPORTING
        it_tabname_alias_range TYPE zdbbr_tabname_range_itab OPTIONAL
        if_keep_primary        TYPE abap_bool DEFAULT abap_true
        if_delete_params       TYPE abap_bool OPTIONAL
        if_delete_formfields   TYPE abap_bool OPTIONAL.
    METHODS delete_where_in_tablist
      IMPORTING
        !it_tabselopt TYPE zdbbr_tabname_range_itab .
    METHODS delete_where_not_in_tablist
      IMPORTING
        !it_tabselopt TYPE zdbbr_tabname_range_itab .
    METHODS extract_fields
      IMPORTING
        !it_tabname_selopt TYPE zdbbr_tabname_range_itab
      RETURNING
        VALUE(rr_fields)   TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS field_exists
      IMPORTING
        !iv_tabname      TYPE tabname
        !iv_fieldname    TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE boolean .
    METHODS field_is_active
      IMPORTING
        !is_field_info      TYPE zdbbr_tabfield_info_ui
      RETURNING
        VALUE(rf_is_active) TYPE boolean .
    METHODS get_conditional_fields
      RETURNING
        VALUE(result) TYPE zdbbr_tabfield_info_ui_itab .
    METHODS get_field
      IMPORTING
        !iv_tabname        TYPE tabname OPTIONAL
        !iv_fieldname      TYPE fieldname
      RETURNING
        VALUE(rs_tabfield) TYPE zdbbr_tabfield_info_ui .
    METHODS get_fields
      IMPORTING
        !if_include_only_checked TYPE boolean OPTIONAL
        !if_consider_all         TYPE boolean OPTIONAL
        !if_consider_output      TYPE boolean OPTIONAL
        !if_consider_sorted      TYPE boolean OPTIONAL
        !if_consider_selected    TYPE boolean OPTIONAL
      EXPORTING
        !et_fields               TYPE zdbbr_tabfield_info_ui_itab .
    METHODS get_fields_ref
      RETURNING
        VALUE(rr_fields_ref) TYPE REF TO zdbbr_tabfield_info_ui_itab .
    METHODS get_field_by_sql_name
      IMPORTING
        !iv_sql_fieldname  TYPE zdbbr_fieldname_with_alias
      RETURNING
        VALUE(rs_tabfield) TYPE zdbbr_tabfield_info_ui
      RAISING
        cx_sy_itab_line_not_found .
    METHODS get_field_count
      RETURNING
        VALUE(rv_field_count) TYPE sy-tabix .
    METHODS get_field_ref
      IMPORTING
        !iv_tabname_alias  TYPE tabname OPTIONAL
        !iv_fieldname      TYPE fieldname
        !if_is_text_field  TYPE boolean OPTIONAL
      RETURNING
        VALUE(rr_tabfield) TYPE REF TO zdbbr_tabfield_info_ui
      RAISING
        cx_sy_itab_line_not_found .
    METHODS get_field_ref_by_alv_name
      IMPORTING
        !iv_alv_fieldname  TYPE fieldname
      RETURNING
        VALUE(rr_tabfield) TYPE REF TO zdbbr_tabfield_info_ui
      RAISING
        cx_sy_itab_line_not_found .
    "! <p class="shorttext synchronized" lang="en">Get iterator instance for list</p>
    METHODS get_iterator
      IMPORTING
        !if_for_active     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rr_iterator) TYPE REF TO zif_uitb_data_ref_iterator .
    METHODS get_mode
      RETURNING
        VALUE(rv_mode) TYPE zdbbr_field_chooser_mode .
    METHODS get_next_entry
      RETURNING
        VALUE(rr_next_line) TYPE REF TO zdbbr_tabfield_info_ui .
    "! <p class="shorttext synchronized" lang="en">Get alias name for internal table name</p>
    "!
    "! @parameter iv_tabname | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_alias_name | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_table_alias_name
      IMPORTING
        iv_tabname           TYPE tabname
      RETURNING
        VALUE(rv_alias_name) TYPE tabname.
    METHODS get_table_list
      IMPORTING
        !if_include_only_active       TYPE abap_bool OPTIONAL
        !if_exclude_parameters        TYPE abap_bool OPTIONAL
        !if_exclude_fields_not_loaded TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_tables)              TYPE zdbbr_entity_info_t .
    "! <p class="shorttext synchronized" lang="en">Retrieve reference to all tables in the list</p>
    "!
    METHODS get_tables_ref
      RETURNING
        VALUE(rr_tables) TYPE REF TO zdbbr_entity_info_t.
    METHODS get_where_for_active_check
      RETURNING
        VALUE(rv_where) TYPE string .
    METHODS has_more_lines
      RETURNING
        VALUE(rf_more_lines) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Checks if the given table exists in the list</p>
    "!
    METHODS has_table
      IMPORTING
        iv_tabname_alias TYPE zdbbr_entity_alias
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Checks if this list has multiple tables</p>
    "!
    METHODS has_multiple_tables
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS initialize_iterator
      IMPORTING
        !if_for_active TYPE boolean OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Load fields for table</p>
    METHODS load_fields
      IMPORTING
        !ir_fields        TYPE REF TO zcl_dbbr_tabfield_list
        !iv_tabname_alias TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Overwrite current data with data from passed list</p>
    METHODS overwrite
      IMPORTING
        !ir_source TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS serialize_list
      RETURNING
        VALUE(rs_list_data) TYPE zdbbr_tabfield_list_data .
    METHODS set_all_text_fields
      IMPORTING
        !iv_tabname      TYPE tabname
        !iv_fieldname    TYPE fieldname
        !if_active       TYPE abap_bool
        !iv_output_order TYPE numc3 OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Sets the entity type attribute</p>
    METHODS set_entity_type
      IMPORTING
        !value TYPE zdbbr_entity_type .
    METHODS set_table_list
      IMPORTING
        !it_tables TYPE zdbbr_entity_info_t .
    METHODS sort .
    METHODS sort_in_custom_order .
    METHODS sort_in_ddic_order .
    "! <p class="shorttext synchronized" lang="en">Sort tables by active selection</p>
    METHODS sort_tables_by_active .
    METHODS switch_mode
      IMPORTING
        !iv_mode TYPE zdbbr_field_chooser_mode .
    "! <p class="shorttext synchronized" lang="en">Replace table alias and update fields</p>
    "!
    METHODS replace_table_alias
      IMPORTING
        iv_old_alias TYPE zdbbr_entity_alias
        iv_new_alias TYPE zdbbr_entity_alias.
    METHODS update_alias_names.
    "! <p class="shorttext synchronized" lang="en">Updates the table list (Aliases and indexes)</p>
    "! @parameter if_force_update | <p class="shorttext synchronized" lang="en">Forces the alias/index update even if not needed</p>
    METHODS update_tables
      IMPORTING
        if_force_update TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Update the active selection/selection order for a table</p>
    METHODS update_table_active_selection
      IMPORTING
        !iv_tabname_alias    TYPE tabname
        !if_active_selection TYPE abap_bool
        !iv_selection_order  TYPE tabfdpos .
    METHODS update_text_field_status .
    METHODS update_virtual_join_for_table
      IMPORTING
        !iv_table_name   TYPE tabname
        !if_virtual_join TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Set the grouping to active</p>
    METHODS set_multi_table_mode
      IMPORTING
        if_active_grouping TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      mty_fieldname_itab TYPE STANDARD TABLE OF fieldname .

    CONSTANTS mc_or TYPE string VALUE ` OR ` ##NO_TEXT.
    CONSTANTS:
      BEGIN OF mc_dynamic_where,
        output_order       TYPE string VALUE `OUTPUT_ORDER  > 0 and OUTPUT_ORDER < ` ##no_text,
        selection_order    TYPE string VALUE `SELECTION_ORDER > 0 AND SELECTION_ORDER < ` ##no_text,
        sort_order         TYPE string VALUE `SORT_ORDER > 0 AND SORT_ORDER < ` ##no_text,
        output_active      TYPE string VALUE 'OUTPUT_ACTIVE = abap_true' ##no_text,
        output_inactive    TYPE string VALUE 'OUTPUT_ACTIVE = abap_false' ##no_text,
        selection_active   TYPE string VALUE 'SELECTION_ACTIVE = abap_true' ##no_text,
        selection_inactive TYPE string VALUE 'SELECTION_ACTIVE = abap_false' ##no_text,
        sort_active        TYPE string VALUE 'SORT_ACTIVE = abap_true' ##no_text,
        sort_inactive      TYPE string VALUE 'SORT_ACTIVE = abap_false' ##no_text,
      END OF mc_dynamic_where .
    DATA mt_fields TYPE zdbbr_tabfield_info_ui_itab .
    "! <p class="shorttext synchronized" lang="en">List of Entity infos</p>
    DATA mt_tables TYPE zdbbr_entity_info_t.
    DATA mv_mode TYPE zdbbr_field_chooser_mode .
    DATA:
      BEGIN OF ms_where,
        custom_order_exists TYPE string,
        field_is_active     TYPE string,
        field_is_inactive   TYPE string,
      END OF ms_where .
    DATA:
      BEGIN OF ms_fieldnames,
        active_field TYPE string,
        order_field  TYPE string,
      END OF ms_fieldnames .
    DATA mv_iterator_index TYPE sy-tabix .
    DATA mf_iterator_for_active TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Type of Entity</p>
    DATA mv_entity_type TYPE zdbbr_entity_type .
    DATA: mf_multi_table_mode TYPE abap_bool.

    METHODS fill_full_fieldnames
      CHANGING
        !cs_field TYPE zdbbr_tabfield_info_ui .
    METHODS fill_full_txt_fieldnames
      CHANGING
        !cs_field            TYPE zdbbr_tabfield_info_ui
        !ct_unique_txt_names TYPE mty_fieldname_itab .
    METHODS get_where_for_specific_table
      IMPORTING
        !iv_table       TYPE tabname
      RETURNING
        VALUE(rv_where) TYPE string .
    METHODS update_mode .
    METHODS is_multi_table_list
      RETURNING
        VALUE(rf_multi_tables) TYPE abap_bool.
ENDCLASS.



CLASS zcl_dbbr_tabfield_list IMPLEMENTATION.


  METHOD active_field_exists.
    CASE mv_mode.
      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-output.
        rf_exists = xsdbool( line_exists( mt_fields[ KEY unique tabname_alias    = iv_tabname
                                                                fieldname        = iv_fieldname
                                                                output_active    = abap_true
                                                                is_text_field    = if_is_text_field ] ) ).
      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-sort.
        rf_exists = xsdbool( line_exists( mt_fields[ KEY unique tabname_alias    = iv_tabname
                                                                fieldname        = iv_fieldname
                                                                sort_active      = abap_true ] ) ).
      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-selection.
        rf_exists = xsdbool( line_exists( mt_fields[ KEY unique tabname_alias    = iv_tabname
                                                                fieldname        = iv_fieldname
                                                                selection_active = abap_true ] ) ).
    ENDCASE.
  ENDMETHOD.


  METHOD add_fields.
    ir_field_list->get_fields( IMPORTING et_fields = DATA(lt_fields) ).
    DATA(lt_tables) = ir_field_list->get_table_list( ).

    mt_fields = VALUE #( BASE mt_fields ( LINES OF lt_fields ) ).
    mt_tables = VALUE #( BASE mt_tables ( LINES OF lt_tables ) ).
  ENDMETHOD.


  METHOD add_table.
    DATA(ls_entity_info) = is_entity_info.

    IF ls_entity_info-index IS INITIAL.
      ls_entity_info-index = lines( mt_tables ) + 1.
    ENDIF.

    IF ls_entity_info-tabname_alias IS INITIAL.
      ls_entity_info-tabname_alias = ls_entity_info-tabname.
    ENDIF.

    IF ls_entity_info-selection_order IS INITIAL.
      ls_entity_info-selection_order = lines( mt_tables ) + 1.
    ENDIF.

    IF ls_entity_info-tabname = zif_dbbr_global_consts=>c_parameter_dummy_table OR
       ls_entity_info-tabname = zif_dbbr_global_consts=>gc_formula_dummy_table.
      ls_entity_info-is_custom = abap_true.
      DATA(lf_insert_first) = abap_true.

      IF ls_entity_info-tabname = zif_dbbr_global_consts=>gc_formula_dummy_table.
        ls_entity_info-alias = zif_dbbr_global_consts=>c_formula_alias.
      ENDIF.
    ENDIF.

    TRY.
        IF lf_insert_first = abap_true.
          INSERT ls_entity_info INTO mt_tables INDEX 1.
        ELSE.
          mt_tables = VALUE #( BASE mt_tables ( ls_entity_info ) ).
        ENDIF.
      CATCH cx_sy_itab_duplicate_key.
    ENDTRY.

  ENDMETHOD.


  METHOD append_tabfield_info.
    APPEND is_tabfield TO mt_fields.
  ENDMETHOD.


  METHOD build_complete_fieldnames.
    DATA: lt_unique_text_names TYPE STANDARD TABLE OF fieldname.

    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF <ls_field>-is_text_field = abap_true.
        fill_full_txt_fieldnames( CHANGING cs_field = <ls_field>
                                           ct_unique_txt_names = lt_unique_text_names ).
      ELSE.
        fill_full_fieldnames( CHANGING cs_field = <ls_field> ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_table_to_alias_map.
    FIELD-SYMBOLS: <ls_table> TYPE zdbbr_entity_info.

    IF if_update_tables = abap_true.
      update_tables( ).
      rt_table_to_alias_map = VALUE #(
        FOR table IN mt_tables
        ( alias   = table-alias
          tabname = table-tabname_alias )
      ).
      RETURN.
    ENDIF.

    LOOP AT mt_tables ASSIGNING <ls_table> WHERE alias <> space.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      rt_table_to_alias_map = VALUE #(
        FOR tab IN mt_tables
        WHERE ( alias <> space )
        ( alias   = tab-alias
          tabname = tab-tabname_alias )
      ).
    ELSE.
      DATA(lv_table_counter) = 1.

      LOOP AT mt_tables ASSIGNING <ls_table> WHERE tabname <> zif_dbbr_global_consts=>gc_formula_dummy_table
                                               AND tabname <> zif_dbbr_global_consts=>c_parameter_dummy_table.
        INSERT VALUE #(
            tabname = <ls_table>-tabname_alias
            alias   = zcl_dbbr_alias_map=>get_alias( lv_table_counter )
        ) INTO TABLE rt_table_to_alias_map.
        ADD 1 TO lv_table_counter.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD checked_field_exists.
    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE (ms_where-field_is_active).
      rf_exists = abap_true.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD clear.
    CLEAR: mt_fields,
           mt_tables,
           mv_mode,
           mf_iterator_for_active,
           mv_iterator_index.
  ENDMETHOD.


  METHOD clear_active_flag.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/11/25
*&---------------------------------------------------------------------*
*& Description: description
*&---------------------------------------------------------------------*

    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF iv_tablename IS NOT INITIAL AND
         iv_tablename <> <ls_field>-tabname_alias.
        CONTINUE.
      ENDIF.

      " if no specific flag was supplied, the active flag of the current mode is cleared
      IF if_clear_output = abap_false AND if_clear_sort = abap_false AND if_clear_selection = abap_false.
        ASSIGN COMPONENT ms_fieldnames-active_field OF STRUCTURE <ls_field> TO FIELD-SYMBOL(<lv_active_flag>).
        ASSIGN COMPONENT ms_fieldnames-order_field OF STRUCTURE <ls_field> TO FIELD-SYMBOL(<lv_order_field>).

        CLEAR: <lv_active_flag>,
               <lv_order_field>.
        CONTINUE.
      ENDIF.

      IF if_clear_output = abap_true.
        CLEAR: <ls_field>-output_active,
               <ls_field>-output_order.
      ENDIF.
      IF if_clear_selection = abap_true.
        CLEAR: <ls_field>-selection_active,
               <ls_field>-selection_order.
      ENDIF.
      IF if_clear_sort = abap_true.
        CLEAR: <ls_field>-sort_active,
               <ls_field>-sort_order,
               <ls_field>-sort_direction.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD clear_alias_names.
    LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      CLEAR <ls_table>-alias.
*.... Reset table alias to table itself
      <ls_table>-tabname_alias = <ls_table>-tabname.
    ENDLOOP.

    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      CLEAR <ls_field>-alias.
      <ls_field>-tabname_alias = <ls_field>-tabname.
      fill_full_fieldnames( CHANGING cs_field = <ls_field> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD clear_calculation_flag.
    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      CLEAR: <ls_field>-is_calculation_field.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    mt_fields = it_fields.
    mt_tables = it_tables.
    mv_mode = iv_mode.
    mv_entity_type = iv_entity_type.
    update_mode( ).
  ENDMETHOD.


  METHOD convert_to_structure.
    result = VALUE zdbbr_tabfield_list_data(
        fields      = mt_fields
        tables      = mt_tables
        entity_type = mv_entity_type
    ).
  ENDMETHOD.


  METHOD copy.
    rr_copy = NEW #( it_fields      = mt_fields
                     it_tables      = mt_tables
                     iv_mode        = mv_mode
                     iv_entity_type = mv_entity_type ).
  ENDMETHOD.


  METHOD create_from_serialized.
    rr_list = NEW zcl_dbbr_tabfield_list(
        it_fields      = is_serialized_tabfields-fields
        it_tables      = is_serialized_tabfields-tables
        iv_entity_type = is_serialized_tabfields-entity_type
    ).
  ENDMETHOD.


  METHOD create_order_from_active.

    DATA(lv_index) = 1.
    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      ASSIGN COMPONENT ms_fieldnames-active_field OF STRUCTURE <ls_field> TO FIELD-SYMBOL(<lv_active_flag>).
      ASSIGN COMPONENT ms_fieldnames-order_field OF STRUCTURE <ls_field> TO FIELD-SYMBOL(<lv_order_field>).

      <lv_order_field> = COND #( WHEN <lv_active_flag> = abap_true THEN
                                   lv_index
                                 ELSE
                                   0 ).

      ADD 1 TO lv_index.
    ENDLOOP.

    sort_in_custom_order( ).
  ENDMETHOD.


  METHOD custom_order_exists.
    DATA(lv_where) = ms_where-custom_order_exists && |{ lines( mt_fields )  + 1 }|.

    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE (lv_where).
      rf_exists = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD delete_field.
    DELETE mt_fields WHERE fieldname = is_field-fieldname
                       AND tabname   = is_field-tabname.
  ENDMETHOD.


  METHOD delete_fields.
    CHECK it_fields_selopt IS NOT INITIAL.

    DELETE mt_fields WHERE fieldname IN it_fields_selopt.
  ENDMETHOD.


  METHOD delete_fields_by_alv_fieldname.
    CHECK it_fields_selopt IS NOT INITIAL.

    DELETE mt_fields WHERE alv_fieldname IN it_fields_selopt.
  ENDMETHOD.


  METHOD delete_formula_fields.
    DELETE mt_fields WHERE is_formula_field = abap_true.
    DELETE mt_tables WHERE tabname = zif_dbbr_global_consts=>gc_formula_dummy_table.
  ENDMETHOD.


  METHOD delete_inactive_fields.
    DELETE mt_fields WHERE (ms_where-field_is_inactive).
  ENDMETHOD.


  METHOD delete_text_fields.
    DELETE mt_fields WHERE is_text_field = abap_true.
  ENDMETHOD.

  METHOD delete_custom.
    DATA(lt_tabname_range) = it_tabname_alias_range.


    IF if_delete_formfields = abap_true.
      lt_tabname_range = VALUE #( BASE lt_tabname_range ( sign = 'I' option = 'EQ' low = zif_dbbr_global_consts=>gc_formula_dummy_table ) ).
    ENDIF.

    IF if_delete_params = abap_true.
      lt_tabname_range = VALUE #( BASE lt_tabname_range ( sign = 'I' option = 'EQ' low = zif_dbbr_global_consts=>c_parameter_dummy_table ) ).
    ELSE.
*.. Exclude parameters from being deleted until it is clear that no entity exists any more that needs them
      lt_tabname_range = VALUE #( BASE lt_tabname_range ( sign = 'E' option = 'EQ' low = zif_dbbr_global_consts=>c_parameter_dummy_table ) ).
    ENDIF.

    IF if_keep_primary = abap_true AND line_exists( mt_tables[ is_primary = abap_true ] ).
      lt_tabname_range = VALUE #( BASE lt_tabname_range ( sign = 'E' option = 'EQ' low = mt_tables[ is_primary = abap_true ]-tabname_alias ) ).
    ENDIF.

    IF lt_tabname_range IS NOT INITIAL.
      DELETE mt_fields WHERE tabname_alias IN lt_tabname_range.
      DELETE mt_tables WHERE tabname_alias IN lt_tabname_range.
    ENDIF.

    IF if_delete_params = abap_false AND
       NOT line_exists( mt_tables[ has_params = abap_true ] ).
      DELETE mt_fields WHERE tabname_alias = zif_dbbr_global_consts=>c_parameter_dummy_table.
      DELETE mt_tables WHERE tabname_alias = zif_dbbr_global_consts=>c_parameter_dummy_table.
    ENDIF.
  ENDMETHOD.

  METHOD delete_where_in_tablist.
    DELETE mt_fields WHERE tabname_alias IN it_tabselopt.
    DELETE mt_tables WHERE tabname_alias IN it_tabselopt.
  ENDMETHOD.


  METHOD delete_where_not_in_tablist.
    DELETE mt_fields WHERE tabname_alias NOT IN it_tabselopt.
    DELETE mt_tables WHERE tabname_alias NOT IN it_tabselopt.
  ENDMETHOD.


  METHOD extract_fields.
    DATA: lt_fields LIKE mt_fields,
          lt_tables LIKE mt_tables.

    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE tabname_alias IN it_tabname_selopt.
      lt_fields = VALUE #( BASE lt_fields ( <ls_field> ) ).
      DELETE mt_fields.
    ENDLOOP.

    LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>) WHERE tabname_alias IN it_tabname_selopt.
      lt_tables = VALUE #( BASE lt_tables ( <ls_table> ) ).
      DELETE mt_tables.
    ENDLOOP.

    rr_fields = NEW zcl_dbbr_tabfield_list(
        it_fields = lt_fields
        it_tables = lt_tables
        iv_mode   = mv_mode
    ).

  ENDMETHOD.


  METHOD field_exists.
    rf_exists = xsdbool( line_exists( mt_fields[ tabname   = iv_tabname
                                                 fieldname = iv_fieldname ] ) ).
  ENDMETHOD.


  METHOD field_is_active.
    ASSIGN COMPONENT ms_fieldnames-active_field OF STRUCTURE is_field_info TO FIELD-SYMBOL(<lv_active_field>).
    rf_is_active = xsdbool( <lv_active_field> = abap_true ).
  ENDMETHOD.

  METHOD is_multi_table_list.
    DATA: lv_tab_count TYPE i.

    LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>) WHERE is_custom = abap_false.
      ADD 1 TO lv_tab_count.
    ENDLOOP.

    rf_multi_tables = xsdbool( lv_tab_count > 1 ).
  ENDMETHOD.


  METHOD fill_full_fieldnames.

    cs_field-sql_fieldname_long = COND #( WHEN ( is_multi_table_list( ) OR
                                                 mf_multi_table_mode = abap_true ) AND
                                               cs_field-tabname_alias IS NOT INITIAL THEN
                                       cs_field-tabname_alias && '~' && cs_field-fieldname_raw
                                     ELSE
                                       cs_field-fieldname_raw ).

    cs_field-sql_fieldname = COND #( WHEN cs_field-alias IS NOT INITIAL THEN
                                       cs_field-alias && '~' && cs_field-fieldname_raw
                                     ELSE
                                       cs_field-fieldname_raw ).
    " possible crop for alv name
    cs_field-alv_fieldname = COND #( WHEN cs_field-alias IS NOT INITIAL THEN
                                       cs_field-alias && '_' && cs_field-fieldname
                                     ELSE
                                       cs_field-fieldname ).
  ENDMETHOD.


  METHOD fill_full_txt_fieldnames.
    DATA: lv_simple_text_fieldname TYPE char24.

    CLEAR: cs_field-sql_fieldname,
           cs_field-alv_fieldname.

    DATA(lv_text_fieldname) = |{ cs_field-fieldname_raw(24) }_TXT|.

    cs_field-alv_fieldname = COND #( WHEN cs_field-alias IS NOT INITIAL THEN cs_field-alias && '_' ) && lv_text_fieldname.
    cs_field-sql_fieldname = COND #( WHEN cs_field-alias IS NOT INITIAL THEN cs_field-alias && '~' ) && lv_text_fieldname.
    cs_field-sql_fieldname_long = COND #( WHEN cs_field-alias IS NOT INITIAL THEN cs_field-alias && '~' ) && lv_text_fieldname.

    APPEND lv_text_fieldname TO ct_unique_txt_names.
  ENDMETHOD.


  METHOD get_conditional_fields.
    result = VALUE #( FOR <ls_field> IN mt_fields WHERE ( is_virtual_join_field = abap_true ) ( <ls_field> ) ).
  ENDMETHOD.


  METHOD get_field.
    IF iv_tabname IS INITIAL.
      rs_tabfield = VALUE #( mt_fields[ fieldname = iv_fieldname ] OPTIONAL ).
    ELSE.
      rs_tabfield = VALUE #( mt_fields[ tabname_alias = iv_tabname fieldname = iv_fieldname ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD get_fields.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/11/28
*&---------------------------------------------------------------------*
*& Description: Returns fields of this list
*&---------------------------------------------------------------------*
    DATA: lt_where TYPE STANDARD TABLE OF string.

    IF if_include_only_checked = abap_true.
      IF if_consider_all = abap_true. " consider all active fields
        lt_where = VALUE #( ( mc_dynamic_where-output_active ) ( mc_or )
                            ( mc_dynamic_where-sort_active ) ( mc_or )
                            ( mc_dynamic_where-selection_active )  ).
      ELSE.
        IF if_consider_output = abap_false AND
           if_consider_selected = abap_false AND
           if_consider_sorted = abap_false.
          " consider only checked fields of current mode
          lt_where = VALUE #( ( ms_where-field_is_active ) ).
        ELSE.
          IF if_consider_output = abap_true.
            APPEND mc_dynamic_where-output_active TO lt_where.
          ENDIF.
          IF if_consider_sorted = abap_true.
            IF lt_where IS NOT INITIAL.
              APPEND mc_or TO lt_where.
            ENDIF.
            APPEND mc_dynamic_where-sort_active TO lt_where.
          ENDIF.
          IF if_consider_selected = abap_true.
            IF lt_where IS NOT INITIAL.
              APPEND mc_or TO lt_where.
            ENDIF.
            APPEND mc_dynamic_where-selection_active TO lt_where.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR et_fields.

      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE (lt_where).
        APPEND <ls_field> TO et_fields.
      ENDLOOP.

    ELSE.
      et_fields = mt_fields.
    ENDIF.

  ENDMETHOD.


  METHOD get_fields_ref.
    rr_fields_ref = REF #( mt_fields ).
  ENDMETHOD.


  METHOD get_field_by_sql_name.
    rs_tabfield = mt_fields[ sql_fieldname = iv_sql_fieldname ].
  ENDMETHOD.


  METHOD get_field_count.
    rv_field_count = lines( mt_fields ).
  ENDMETHOD.


  METHOD get_field_ref.
    IF iv_tabname_alias IS INITIAL.
      rr_tabfield = REF #( mt_fields[ fieldname     = iv_fieldname
                                      is_text_field = if_is_text_field ] ).
    ELSE.
      rr_tabfield = REF #( mt_fields[ KEY unique tabname_alias = iv_tabname_alias
                                                 fieldname     = iv_fieldname
                                                 is_text_field = if_is_text_field ] ).
    ENDIF.
  ENDMETHOD.


  METHOD get_field_ref_by_alv_name.
    rr_tabfield = REF #( mt_fields[ KEY alv_fieldname alv_fieldname = iv_alv_fieldname ] ).
  ENDMETHOD.


  METHOD get_iterator.
    rr_iterator = zcl_uitb_data_ref_iterator=>create(
        ir_list  = me
        iv_where = COND #( WHEN if_for_active = abap_true THEN ms_where-field_is_active )
    ).
  ENDMETHOD.


  METHOD get_mode.
    rv_mode = mv_mode.
  ENDMETHOD.


  METHOD get_next_entry.
    IF mf_iterator_for_active = abap_true.
      DATA(lv_where) = ms_where-field_is_active.
    ENDIF.

    LOOP AT mt_fields FROM mv_iterator_index REFERENCE INTO DATA(lr_field)
       WHERE (lv_where).
      " cache current index
      mv_iterator_index = sy-tabix + 1.
      rr_next_line = lr_field.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_table_alias_name.
    rv_alias_name = VALUE #( mt_tables[ tabname = iv_tabname ]-tabname_alias DEFAULT iv_tabname ).
  ENDMETHOD.


  METHOD get_table_list.
    rt_tables = mt_tables.

    IF if_include_only_active = abap_true.
      DELETE rt_tables WHERE active_selection = abap_false.
    ENDIF.

    IF if_exclude_parameters = abap_true.
      DELETE rt_tables WHERE tabname = zif_dbbr_global_consts=>c_parameter_dummy_table.
    ENDIF.

    IF if_exclude_fields_not_loaded = abap_true.
      DELETE rt_tables WHERE fields_are_loaded = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_tables_ref.
    rr_tables = REF #( mt_tables ).
  ENDMETHOD.

  METHOD get_where_for_active_check.
    rv_where = ms_where-field_is_active.
  ENDMETHOD.


  METHOD get_where_for_specific_table.
    IF iv_table IS NOT INITIAL.
      rv_where = | AND TABNAME = { iv_table }|.
    ENDIF.
  ENDMETHOD.


  METHOD has_more_lines.
    IF mf_iterator_for_active = abap_true.
      DATA(lv_where) = ms_where-field_is_active.
    ENDIF.

    LOOP AT mt_fields FROM mv_iterator_index ASSIGNING FIELD-SYMBOL(<ls_field>)
       WHERE (lv_where).
      rf_more_lines = abap_true.
      RETURN.
    ENDLOOP.

  ENDMETHOD.

  METHOD has_table.
    rf_exists = xsdbool( line_exists( mt_tables[ tabname_alias = iv_tabname_alias ] ) ).
  ENDMETHOD.

  METHOD has_multiple_tables.
    result = xsdbool( lines( mt_tables ) > 1 ).
  ENDMETHOD.


  METHOD initialize_iterator.
    mv_iterator_index = 1.
    mf_iterator_for_active = if_for_active.
  ENDMETHOD.


  METHOD load_fields.
    DATA(lr_s_table) = REF #( mt_tables[ tabname_alias = iv_tabname_alias ] OPTIONAL ).
    CHECK lr_s_table IS BOUND.
    CHECK lr_s_table->fields_are_loaded = abap_false.

    ir_fields->get_fields( IMPORTING et_fields = DATA(lt_fields) ).

    mt_fields = VALUE #( BASE mt_fields ( LINES OF lt_fields ) ).

    lr_s_table->fields_are_loaded = abap_true.
  ENDMETHOD.


  METHOD overwrite.
    CLEAR mv_iterator_index.

    mt_fields = ir_source->mt_fields.
    mt_tables = ir_source->mt_tables.
  ENDMETHOD.


  METHOD serialize_list.
    rs_list_data-fields = mt_fields.
    rs_list_data-tables = mt_tables.
  ENDMETHOD.


  METHOD set_all_text_fields.
    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_text_field>) USING KEY unique WHERE tabname_alias = iv_tabname
                                                                                 AND fieldname     = iv_fieldname
                                                                                 AND is_text_field = abap_true.

      <ls_text_field>-output_active = if_active.
      IF iv_output_order IS NOT INITIAL.
        <ls_text_field>-output_order = iv_output_order.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_multi_table_mode.
    mf_multi_table_mode = if_active_grouping.
  ENDMETHOD.

  METHOD set_entity_type.
    mv_entity_type = value.
  ENDMETHOD.


  METHOD set_table_list.
    mt_tables = it_tables.

    LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      IF <ls_table>-tabname_alias IS INITIAL.
        <ls_table>-tabname_alias = <ls_table>-tabname.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD sort.
*& Description: Sorts list of fields according to current state
*&---------------------------------------------------------------------*
    IF checked_field_exists( ).
      IF custom_order_exists( ).
        sort_in_custom_order( ).
      ELSE.
        sort_in_ddic_order( ).
        create_order_from_active( ).
        sort_in_custom_order( ).
      ENDIF.
    ELSE.
      sort_in_ddic_order( ).
    ENDIF.
  ENDMETHOD.


  METHOD sort_in_custom_order.
    CASE mv_mode.
      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-output.
        SORT mt_fields BY output_active DESCENDING output_order ASCENDING.

      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-selection.
        SORT mt_fields BY alias selection_active DESCENDING selection_order ASCENDING.

      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-sort.
        SORT mt_fields BY sort_active DESCENDING sort_order ASCENDING.
    ENDCASE.
  ENDMETHOD.


  METHOD sort_in_ddic_order.
    SORT mt_fields BY alias ddic_order is_text_field ASCENDING.
  ENDMETHOD.


  METHOD sort_tables_by_active.
    SORT mt_tables BY active_selection DESCENDING selection_order ASCENDING.

    DATA(lv_param_table_index) = line_index( mt_tables[ tabname = zif_dbbr_global_consts=>c_parameter_dummy_table ] ).
    IF lv_param_table_index > 0.
      DATA(ls_param_table) = mt_tables[ lv_param_table_index ].
      DELETE mt_tables INDEX lv_param_table_index.
      INSERT ls_param_table INTO mt_tables INDEX 1.
    ENDIF.
  ENDMETHOD.


  METHOD switch_mode.
    IF iv_mode <> mv_mode.
      mv_mode = iv_mode.

      update_mode( ).
    ENDIF.
  ENDMETHOD.

  METHOD replace_table_alias.
    ASSIGN mt_tables[ tabname_alias = iv_old_alias ] TO FIELD-SYMBOL(<ls_table>).
    CHECK sy-subrc = 0.

    <ls_table>-tabname_alias = iv_new_alias.

*.. Update the fields as well
    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE tabname_alias = iv_old_alias.
      <ls_field>-tabname_alias = iv_new_alias.
    ENDLOOP.
  ENDMETHOD.


  METHOD update_alias_names.

    LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).

      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE tabname_alias = <ls_table>-tabname_alias.
        IF <ls_field>-is_formula_field = abap_true.
          " nothing to do.
          <ls_field>-alias = zif_dbbr_global_consts=>c_formula_alias.
        ELSE.
          <ls_field>-alias = <ls_table>-alias.
        ENDIF.
        fill_full_fieldnames( CHANGING cs_field = <ls_field> ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_mode.

    CASE mv_mode.
      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-output.
        ms_where-custom_order_exists = mc_dynamic_where-output_order.
        ms_where-field_is_active = mc_dynamic_where-output_active.
        ms_where-field_is_inactive = mc_dynamic_where-output_inactive.
        ms_fieldnames-active_field = 'OUTPUT_ACTIVE'.
        ms_fieldnames-order_field = 'OUTPUT_ORDER'.

      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-selection.
        ms_where-custom_order_exists = mc_dynamic_where-selection_order.
        ms_where-field_is_active = mc_dynamic_where-selection_active.
        ms_where-field_is_inactive = mc_dynamic_where-selection_inactive.
        ms_fieldnames-active_field = 'SELECTION_ACTIVE'.
        ms_fieldnames-order_field = 'SELECTION_ORDER'.

      WHEN zif_dbbr_global_consts=>gc_field_chooser_modes-sort.
        ms_where-custom_order_exists = mc_dynamic_where-sort_order.
        ms_where-field_is_active = mc_dynamic_where-sort_active.
        ms_where-field_is_inactive = mc_dynamic_where-sort_inactive.
        ms_fieldnames-active_field = 'SORT_ACTIVE'.
        ms_fieldnames-order_field = 'SORT_ORDER'.
    ENDCASE.

  ENDMETHOD.


  METHOD update_tables.
    SORT mt_tables BY selection_order.

    DATA(lv_index) = 1.

    IF lines( mt_tables ) > 1 AND
       NOT line_exists( mt_tables[ alias = space ] ) AND
       if_force_update = abap_false.
      RETURN.
    ENDIF.

    LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>) WHERE tabname <> zif_dbbr_global_consts=>c_parameter_dummy_table.
      <ls_table>-index = sy-tabix.
      IF <ls_table>-tabname = zif_dbbr_global_consts=>gc_formula_dummy_table.
        <ls_table>-alias = zif_dbbr_global_consts=>c_formula_alias.
      ELSE.
        IF lines( mt_tables ) > 1.
          <ls_table>-alias = zcl_dbbr_alias_map=>get_alias( lv_index ).
          ADD 1 TO lv_index.
        ELSE.
          CLEAR <ls_table>-alias.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD update_table_active_selection.
    ASSIGN mt_tables[ tabname_alias = iv_tabname_alias ] TO FIELD-SYMBOL(<ls_table>).
    CHECK sy-subrc = 0.

    <ls_table>-active_selection = if_active_selection.
    <ls_table>-selection_order = iv_selection_order.
  ENDMETHOD.


  METHOD update_text_field_status.
*&---------------------------------------------------------------------*
*& Description: Checks if text field for certain key field will be shown
*& in output list
*&---------------------------------------------------------------------*
    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE has_text_field = abap_true.

      DATA(lv_active_text_field_count) = 0.

      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_text_field>) USING KEY unique WHERE tabname_alias = <ls_field>-tabname_alias
                                                                                   AND fieldname     = <ls_field>-fieldname
                                                                                   AND is_text_field = abap_true.

        IF <ls_text_field>-output_active = abap_true.
          ADD 1 TO lv_active_text_field_count.
        ENDIF.

        " update the key field name for the text field
        <ls_text_field>-reference_alv_fieldname = <ls_field>-alv_fieldname.
      ENDLOOP.

      IF lv_active_text_field_count >= 1.
        <ls_field>-has_active_text_field = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_virtual_join_for_table.
    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE tabname = iv_table_name.
      <ls_field>-is_virtual_join_field = if_virtual_join.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~add.
    TRY.
        DATA(lr_s_tabfield) = CAST zdbbr_tabfield_info_ui( ir_s_element ).

        IF lr_s_tabfield->tabname_alias IS INITIAL.
          lr_s_tabfield->tabname_alias = lr_s_tabfield->tabname.
        ENDIF.

        IF lr_s_tabfield->fieldname_raw IS INITIAL.
          lr_s_tabfield->fieldname_raw = lr_s_tabfield->fieldname.
        ENDIF.

        IF NOT line_exists( mt_fields[ KEY unique tabname_alias = lr_s_tabfield->tabname_alias
                                                  fieldname     = lr_s_tabfield->fieldname
                                                  is_text_field = lr_s_tabfield->is_text_field ] ).
          APPEND lr_s_tabfield->* TO mt_fields REFERENCE INTO rr_new_element.
        ENDIF.
      CATCH cx_sy_move_cast_error.
      CATCH cx_sy_itab_duplicate_key.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~clear.
    CLEAR: mt_fields,
           mt_tables.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~create_new_line.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~get_all.
    rr_t_data = REF #( mt_fields ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~get_element.
    IF iv_where IS NOT INITIAL.
      LOOP AT mt_fields REFERENCE INTO DATA(lr_field) FROM iv_index
                                                      WHERE (iv_where).
        DATA(lv_current_index) = sy-tabix.
        EXIT.
      ENDLOOP.

      IF lr_field IS BOUND.
        ev_index_of_found = lv_current_index.
        rr_element = lr_field.
      ELSE.
        RAISE EXCEPTION TYPE zcx_uitb_element_not_found
          EXPORTING
            textid = zcx_uitb_element_not_found=>condition_access
            index  = iv_index
            cond   = iv_where.
      ENDIF.

    ELSE.
      TRY.
          rr_element = REF #( mt_fields[ iv_index ] ).
        CATCH cx_sy_itab_line_not_found.
          RAISE EXCEPTION TYPE zcx_uitb_element_not_found
            EXPORTING
              textid = zcx_uitb_element_not_found=>index_access
              index  = iv_index.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~get_iterator.
    rr_iterator = zcl_uitb_data_ref_iterator=>create(
        ir_list  = me
        iv_where = iv_where
    ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~has_component ##needed.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~is_empty.
    rf_is_empty = xsdbool( mt_fields IS INITIAL ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~size.
    rv_size = lines( mt_fields ).
  ENDMETHOD.
ENDCLASS.
