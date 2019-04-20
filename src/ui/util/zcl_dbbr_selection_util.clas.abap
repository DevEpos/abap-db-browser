"! <p class="shorttext synchronized" lang="en">Utility methods for Selection Controller</p>
CLASS zcl_dbbr_selection_util DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_dbbr_selection_controller .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_screen_util.

    ALIASES get_deactivated_functions
      FOR zif_dbbr_screen_util~get_deactivated_functions .
    ALIASES handle_pbo
      FOR zif_dbbr_screen_util~handle_pbo .
    ALIASES handle_ui_function
      FOR zif_dbbr_screen_util~handle_ui_function .

    TYPES:
      BEGIN OF ty_s_selection_data,
        entity_type          TYPE zdbbr_entity_type,
        entity_id            TYPE zdbbr_entity_id,
        query_string         TYPE string,
        do_for_all_select    TYPE abap_bool,
        for_all_entries_data TYPE REF TO data,
        association_target   TYPE zdbbr_cds_association,
        selection_fields     TYPE zdbbr_selfield_itab,
        technical_infos      TYPE zdbbr_tech_info,
        no_grouping          TYPE abap_bool,
        multi_or             TYPE zdbbr_or_seltab_itab,
        edit_mode            TYPE abap_bool,
        delete_mode_active   TYPE abap_bool,
        selfields_multi      TYPE zdbbr_selfield_itab,
        tabfields            TYPE REF TO zcl_dbbr_tabfield_list,
        tabfields_all        TYPE REF TO zcl_dbbr_tabfield_list,
        table_to_alias_map   TYPE zdbbr_table_to_alias_map_itab,
        join_definition      TYPE zdbbr_join_data,
        join_def             TYPE zdbbr_join_def,
        exclude_function     TYPE ui_functions,
        formula              TYPE REF TO zcl_dbbr_formula,
        nav_breadcrumbs      TYPE zdbbr_string_t,
        navigation_count     TYPE i,
      END OF ty_s_selection_data.

    "! <p class="shorttext synchronized" lang="en">Selection did finish</p>
    EVENTS selection_finished
      EXPORTING
        VALUE(ef_first_select) TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">No data was found</p>
    EVENTS no_data .

    "! <p class="shorttext synchronized" lang="en">Create typed utility for selection</p>
    CLASS-METHODS create_util
      IMPORTING
        is_selection_data TYPE ty_s_selection_data
      RETURNING
        VALUE(result)     TYPE REF TO zcl_dbbr_selection_util .
    "! <p class="shorttext synchronized" lang="en">Builds simple alv title</p>
    METHODS build_simple_alv_title
      RETURNING
        VALUE(result) TYPE lvc_title .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        is_selection_data TYPE ty_s_selection_data.
    "! <p class="shorttext synchronized" lang="en">Executes the selection for the entity</p>
    METHODS execute_selection
        ABSTRACT .
    "! <p class="shorttext synchronized" lang="en">Fills the dynamic document header on top of the alv output</p>
    METHODS fill_header
      CHANGING
        !cr_dd_doc TYPE REF TO cl_dd_document .
    "! <p class="shorttext synchronized" lang="en">Retrieves information about entity</p>
    METHODS get_entity_name
          ABSTRACT
      RETURNING
        VALUE(result) TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Handle custom context menu request for alv</p>
    METHODS handle_alv_ctx_menu_request
          ABSTRACT
      IMPORTING
        !if_selected_cols  TYPE abap_bool
        !if_selected_rows  TYPE abap_bool
        !if_selected_cells TYPE abap_bool
      CHANGING
        !ct_menu_entries   TYPE sctx_entrytab .
    "! <p class="shorttext synchronized" lang="en">Initialization of util</p>
    METHODS init
        ABSTRACT .
    "! <p class="shorttext synchronized" lang="en">Checks if there is an active table join</p>
    METHODS is_join_active
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Refresh the selection</p>
    METHODS refresh_selection
        ABSTRACT .
    "! <p class="shorttext synchronized" lang="en">Update where condition from ALV Filter</p>
    METHODS update_selection_for_filter .
    "! <p class="shorttext synchronized" lang="en">Retrieve ALV Util instance</p>
    METHODS get_alv_util
      RETURNING
        VALUE(rr_alv_util) TYPE REF TO zcl_dbbr_output_alv_util .
    "! <p class="shorttext synchronized" lang="en">Transform Criteria to ALV Filter</p>
    METHODS get_alv_filter_from_criteria
      RETURNING
        VALUE(rt_filter) TYPE lvc_t_filt .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF t_compare_field,
        fieldname  TYPE fieldname,
        is_numeric TYPE boolean,
      END OF t_compare_field .
    TYPES:
      tt_compare_field TYPE STANDARD TABLE OF t_compare_field .
    TYPES:
      BEGIN OF ty_group_tab_map,
        sp_group TYPE lvc_spgrp,
        tabname  TYPE tabname,
      END OF ty_group_tab_map .

    CONSTANTS c_col_group_prefix TYPE char3 VALUE 'SPG' ##NO_TEXT.
    DATA mt_column_groups TYPE lvc_t_sgrp .
    DATA:
      mt_group_tab_map TYPE HASHED TABLE OF ty_group_tab_map WITH UNIQUE KEY sp_group .
    DATA mf_count_lines TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">ID of an DB Browser entity</p>
    DATA mv_entity_id TYPE zdbbr_entity_id .
    "! <p class="shorttext synchronized" lang="en">Type of Entity</p>
    DATA mv_entity_type TYPE zdbbr_entity_type .
    DATA mr_s_global_data TYPE REF TO zdbbr_global_data.
    DATA mf_do_for_all_select TYPE abap_bool .
    DATA ms_association_target TYPE zdbbr_cds_association .
    DATA mr_t_for_all_data TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Creates subroutine pool program for selection data from db</p>
    DATA mr_select_program TYPE REF TO zcl_dbbr_select_prog_creator .
    DATA mr_t_data TYPE REF TO data .
    DATA mr_t_temp_data TYPE REF TO data .
    DATA mf_join_is_active TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Field Catalog for List Viewer Control</p>
    DATA mt_fieldcat TYPE lvc_t_fcat .
    DATA mt_jumpdest TYPE zdbbr_jumpdest_data_ui_itab .
    DATA mt_selection_fields TYPE zdbbr_selfield_itab .
    DATA mt_param_values TYPE zdbbr_selfield_itab .
    DATA mt_multi_or TYPE zdbbr_or_seltab_itab .
    DATA mt_selfields_multi TYPE zdbbr_selfield_itab .
    DATA mo_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_tabfields_all TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mt_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab .
    DATA mf_custom_query_active TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">List of FROM clauses</p>
    DATA mt_from TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">List of SELECT clauses</p>
    DATA mt_select TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">List of WHERE clauses</p>
    DATA mt_where TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">List of GROUP BY clauses</p>
    DATA mt_group_by TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">List of ORDER BY clauses</p>
    DATA mt_order_by TYPE zdbbr_string_t .
    DATA mt_add_texts TYPE zdbbr_additional_text_itab .
    DATA mf_group_by TYPE boolean .
    DATA mt_or TYPE zdbbr_or_seltab_sql_t .
    DATA mf_aggregation TYPE boolean .
    DATA mv_max_lines_existing TYPE sy-tabix .
    DATA mt_sort_alv TYPE lvc_t_sort .
    DATA ms_line_index TYPE dfies .
    DATA:
      BEGIN OF ms_control_info,
        primary_table          TYPE tabname,
        primary_table_name     TYPE ddtext,
        primary_table_tabclass TYPE tabclass,
        alv_varianttext        TYPE slis_varbz,
        edit                   TYPE abap_bool, " is table editable?
        client_dependent       TYPE abap_bool, "table itself is client dependent
        number                 TYPE zdbbr_no_of_lines,
        delete_mode            TYPE sap_bool,
      END OF ms_control_info .
    DATA ms_technical_info TYPE zdbbr_tech_info .
    DATA mo_tabfields_original TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_tabfields_all_original TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_post_join_helper TYPE REF TO zcl_dbbr_virtual_join_helper .
    DATA mt_exclude_function TYPE ucomm_it .
    DATA mo_formula TYPE REF TO zcl_dbbr_formula .
    DATA mo_formula_calculator TYPE REF TO zcl_dbbr_formula_calculator .
    DATA mt_dyntab_components TYPE zdbbr_abap_comp_type_itab .
    DATA:
      mt_virtual_join_table_range TYPE RANGE OF tabname .
    DATA mf_case_insensitive_search TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">List of Strings</p>
    DATA mt_nav_breadcrumbs TYPE zdbbr_string_t .
    DATA mv_navigation_count TYPE i .
    "! <p class="shorttext synchronized" lang="en">Dynamic Documents: Document</p>
    DATA mr_breadcrumbs_dd TYPE REF TO cl_dd_document .
    "! <p class="shorttext synchronized" lang="en">ALV Output</p>
    DATA mr_alv_grid TYPE REF TO zcl_dbbr_output_grid .
    "! <p class="shorttext synchronized" lang="en">ALV tree control: Index list</p>
    DATA mt_selected_rows TYPE lvc_t_indx .
    "! <p class="shorttext synchronized" lang="en">Definition of a Join</p>
    DATA ms_join_def TYPE zdbbr_join_def .
    DATA mt_current_live_filter TYPE lvc_t_filt .
    "! <p class="shorttext synchronized" lang="en">Abstract ALV Filter for Output Grid</p>
    DATA mr_alv_util TYPE REF TO zcl_dbbr_output_alv_util .
    DATA mv_query_string TYPE string.


    "! <p class="shorttext synchronized" lang="en">Adds column for hiding rows</p>
    METHODS add_hide_flag_column .
    "! <p class="shorttext synchronized" lang="en">Adds Column for a line index / group count</p>
    METHODS add_line_index_column .
    "! <p class="shorttext synchronized" lang="en">Adds a field catalog entry for a text field</p>
    METHODS add_text_fieldcat_entry
      IMPORTING
        !ir_selfield TYPE REF TO zdbbr_tabfield_info_ui .
    "! <p class="shorttext synchronized" lang="en">Build full field names for selection and output (i.e. alias)</p>
    METHODS build_full_fieldnames .
    "! <p class="shorttext synchronized" lang="en">Clears all aggregation info from fields</p>
    METHODS clear_aggregation_fields
      CHANGING
        !ct_selection_fields TYPE zdbbr_selfield_itab .
    "! <p class="shorttext synchronized" lang="en">Only count the lines for the given filter criteria</p>
    METHODS count_lines .
    "! <p class="shorttext synchronized" lang="en">Count lines without aggregation active</p>
    METHODS count_lines_simple .
    "! <p class="shorttext synchronized" lang="en">Count lines with active aggregation fields</p>
    METHODS count_lines_with_aggregation .
    "! <p class="shorttext synchronized" lang="en">Creates default SQL where clause from criteria</p>
    METHODS create_default_where_clause .
    "! <p class="shorttext synchronized" lang="en">Creates the dynamic internal table to hold the selected data</p>
    METHODS create_dynamic_table
      IMPORTING
        !if_cache_existing_data TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Creates the field catalog for the output</p>
    "!
    METHODS create_field_catalog .
    "! <p class="shorttext synchronized" lang="en">Creates FOR ALL ENTRIES where clause</p>
    "!
    METHODS create_for_all_where_clause .
    "! <p class="shorttext synchronized" lang="en">Creates the from clause for the select</p>
    "!
    METHODS create_from_clause .
    "! <p class="shorttext synchronized" lang="en">Creates the group by clause for the select</p>
    "!
    METHODS create_group_by_clause .
    "! <p class="shorttext synchronized" lang="en">Creates the order by clause for the select</p>
    "!
    METHODS create_order_by_clause .
    "! <p class="shorttext synchronized" lang="en">Creates the projection fields for the select</p>
    "!
    METHODS create_select_clause .

    "! <p class="shorttext synchronized" lang="en">Creates the where condition</p>
    "!
    METHODS create_where_clause .
    "! <p class="shorttext synchronized" lang="en">Determines if there is an aggregation field in the selection</p>
    "!
    METHODS determine_aggregation_state .
    "! <p class="shorttext synchronized" lang="en">Determines if the field should be displayed as a checkbox</p>
    "!
    METHODS determine_checkbox_output
      IMPORTING
        !ir_field     TYPE REF TO zdbbr_tabfield_info_ui
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Determines if a group by fields exists in the selection</p>
    "!
    METHODS determine_group_by_state .
    "! <p class="shorttext synchronized" lang="en">Exectue formulas for selected lines</p>
    "!
    METHODS execute_formula_for_lines .
    "! <p class="shorttext synchronized" lang="en">Fills the quantity/currency field for another field</p>
    "!
    METHODS fill_fcat_quan_curr_field
      IMPORTING
        !ir_tabfield       TYPE REF TO zdbbr_tabfield_info_ui
        !ir_tabfields      TYPE REF TO zcl_dbbr_tabfield_list
      CHANGING
        !cv_quantity_field TYPE lvc_s_fcat-qfieldname
        !cv_currency_field TYPE lvc_s_fcat-cfieldname .
    "! <p class="shorttext synchronized" lang="en">Retrieves the short texts for the given table fields</p>
    "!
    METHODS get_short_texts
      IMPORTING
        !ir_tabfield        TYPE REF TO zdbbr_tabfield_info_ui
      EXPORTING
        !ef_alt_text_exists TYPE abap_bool
        !ev_ddtext          TYPE ddtext
        !ev_reptext         TYPE reptext
        !ev_scrtext_l       TYPE scrtext_l
        !ev_scrtext_m       TYPE scrtext_m
        !ev_scrtext_s       TYPE scrtext_s .
    "! <p class="shorttext synchronized" lang="en">Handles a possible Jump Field</p>
    "!
    METHODS handle_possible_jumpfield
      IMPORTING
        !ir_current_field TYPE REF TO zdbbr_tabfield_info_ui
      CHANGING
        !cs_field         TYPE lvc_s_fcat .
    "! <p class="shorttext synchronized" lang="en">Initialize the navigation breadcrumbs</p>
    "!
    METHODS init_navigation_breadcrumbs
      IMPORTING
        !ir_container TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Read information about all entities in the selection</p>
    "!
    METHODS read_entity_infos.
    "! <p class="shorttext synchronized" lang="en">Select the actual data</p>
    "!
    METHODS select_data
      IMPORTING
        !if_count_lines   TYPE abap_bool OPTIONAL
        !if_refresh_only  TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rf_success) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets the column texts for the given field catalog entry</p>
    "!
    METHODS set_fieldcat_coltexts
      IMPORTING
        !ir_field    TYPE REF TO zdbbr_tabfield_info_ui
      CHANGING
        !cs_fieldcat TYPE lvc_s_fcat .
    "! <p class="shorttext synchronized" lang="en">Set Texts for line index column</p>
    "!
    METHODS set_line_index_column_texts
      CHANGING
        !cs_field TYPE lvc_s_fcat .
    "! <p class="shorttext synchronized" lang="en">Set miscellaneous information for selected rows</p>
    "!
    "!
    METHODS set_miscinfo_for_selected_data .
    "! <p class="shorttext synchronized" lang="en">Set ALV Filters from where condition</p>
    "!
    "!
    METHODS set_where_as_alv_filter .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_selection_util IMPLEMENTATION.


  METHOD add_hide_flag_column.
    DATA(ls_field) = VALUE lvc_s_fcat(
       fieldname = zif_dbbr_c_special_out_columns=>hide_flag
       ref_table = 'ZDBBR_ALV_SPECIAL_CELLS'
       ref_field = 'HIDE_FLAG'
       tech      = abap_true
   ).

    IF ms_technical_info-tech_names = abap_true.
      ls_field-reptext   =
      ls_field-scrtext_s =
      ls_field-scrtext_m =
      ls_field-scrtext_l =
      ls_field-coltext   = ls_field-fieldname.
    ENDIF.

    APPEND ls_field TO mt_fieldcat.
  ENDMETHOD.


  METHOD add_line_index_column.

    DATA(ls_field) = VALUE lvc_s_fcat(
       fieldname = zif_dbbr_c_special_out_columns=>line_index
    ).

    DATA(lv_type) = COND string(
     WHEN mf_group_by = abap_true THEN
       |ZDBBR_NO_OF_LINES|
     ELSE
       |SYST-TABIX|
    ).

    DATA(lr_number_of_lines) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_name( lv_type )
    ).
    ms_line_index = lr_number_of_lines->get_ddic_field(
        p_langu = zcl_dbbr_appl_util=>get_description_language( )
    ).

    ls_field = CORRESPONDING #( BASE ( ls_field )
      ms_line_index
      EXCEPT fieldname
             tabname
    ).

    IF mf_group_by = abap_false.
      ls_field-tech      = abap_true.
      ls_field-no_out    = abap_true.
    ENDIF.

    set_line_index_column_texts( CHANGING cs_field = ls_field ).

    APPEND ls_field TO mt_fieldcat.

  ENDMETHOD.


  METHOD add_text_fieldcat_entry.
    TYPES: BEGIN OF ty_text_field,
             selection_type TYPE char16,
             table          TYPE tabname,
             field          TYPE fieldname,
           END OF ty_text_field.

    DATA: lv_ref_field   TYPE fieldname,
          lv_ref_table   TYPE tabname,
          lt_text_fields TYPE SORTED TABLE OF ty_text_field WITH NON-UNIQUE KEY selection_type table field.

    " get all text field definitions to get the corresponding text field
    lt_text_fields = VALUE #(
       FOR <ls_field> IN mt_add_texts
       WHERE ( id_table = ir_selfield->tabname AND
               id_field = ir_selfield->fieldname )
       ( selection_type = <ls_field>-selection_type
         table          = <ls_field>-text_table
         field          = <ls_field>-text_field )
    ).

    IF lt_text_fields IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_text_field) = lt_text_fields[ 1 ].

    IF ls_text_field-selection_type = zif_dbbr_c_text_selection_type=>domain_value OR
       lines( lt_text_fields ) > 1.
      lv_ref_field = 'DDTEXT'.
      lv_ref_table = 'DD07T'.
    ELSE.
      lv_ref_field = ls_text_field-field.
      lv_ref_table = ls_text_field-table.
    ENDIF.

    DATA(ls_add_text_field) = VALUE lvc_s_fcat(
        fieldname      = ir_selfield->alv_fieldname
        emphasize      = COND #( WHEN ms_technical_info-emphasize_text_fields = abap_true THEN
                                   zif_dbbr_global_consts=>gc_alv_emphasize-text_field_color  )
        lowercase      = abap_true
        ref_field      = lv_ref_field
        ref_table      = lv_ref_table
    ).

    ls_add_text_field-sp_group = VALUE #( mt_group_tab_map[ tabname = ir_selfield->tabname ]-sp_group OPTIONAL ).
    ls_add_text_field-parameter2 = 'T'.

    set_fieldcat_coltexts(
      EXPORTING ir_field    = ir_selfield
      CHANGING  cs_fieldcat = ls_add_text_field
    ).

    APPEND ls_add_text_field TO mt_fieldcat.
  ENDMETHOD.


  METHOD build_full_fieldnames.
    IF mf_aggregation = abap_true OR
       mf_group_by = abap_true.
      mo_tabfields->set_multi_table_mode( ).
    ENDIF.
    mo_tabfields->build_complete_fieldnames( ).
    mo_tabfields_original->build_complete_fieldnames( ).
    mo_tabfields_all->build_complete_fieldnames( ).
    mo_tabfields_all_original->build_complete_fieldnames( ).
  ENDMETHOD.


  METHOD build_simple_alv_title.
    DATA: lv_title TYPE lvc_title.

    IF is_join_active( ).
      lv_title = `Join of ` && ms_control_info-primary_table && `(` && ms_join_def-primary_table_alias_alv && `)`.

      LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>).
        DATA(lv_current_index) = sy-tabix.
        DATA(lv_tabname) = <ls_join_table>-add_table && `(` && <ls_join_table>-add_table_alias_alv && `)`.

        IF lv_current_index = lines( ms_join_def-tables ).
          lv_title = lv_title && ` and ` && lv_tabname.
        ELSE.
          lv_title = lv_title && ` ` && lv_tabname.
        ENDIF.
      ENDLOOP.
    ELSE.
      lv_title = |Table - { ms_control_info-primary_table } - { ms_control_info-primary_table_name }|.
    ENDIF.

    " check max length of string
    IF strlen( lv_title ) >= 70.
      result = lv_title.
      result+67(3) = '...'.
    ELSE.
      result = lv_title.
    ENDIF.
  ENDMETHOD.


  METHOD clear_aggregation_fields.
    LOOP AT ct_selection_fields ASSIGNING FIELD-SYMBOL(<ls_selection_field>).
      CLEAR: <ls_selection_field>-group_by,
             <ls_selection_field>-aggregation.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    " set control data
    ms_control_info = VALUE #(
      primary_table   = COND #( WHEN is_selection_data-entity_type = zif_dbbr_c_entity_type=>table THEN is_selection_data-entity_id )
      edit            = is_selection_data-edit_mode
      delete_mode     = is_selection_data-delete_mode_active
    ).

    mt_nav_breadcrumbs  = is_selection_data-nav_breadcrumbs.
    mv_navigation_count = is_selection_data-navigation_count.

    mo_formula            = is_selection_data-formula.
    mv_query_string       = is_selection_data-query_string.
    ms_join_def           = is_selection_data-join_def.
    mv_entity_id          = is_selection_data-entity_id.
    mv_entity_type        = is_selection_data-entity_type.
    mf_do_for_all_select  = is_selection_data-do_for_all_select.
    mr_t_for_all_data     = is_selection_data-for_all_entries_data.
    ms_association_target = is_selection_data-association_target.
    ms_technical_info     = is_selection_data-technical_infos.
    mf_case_insensitive_search = is_selection_data-technical_infos-search_ignore_case.
    mt_selection_fields   = VALUE #( FOR selfield IN is_selection_data-selection_fields WHERE ( is_parameter = abap_false ) ( selfield ) ).
    mt_param_values       = VALUE #( ( LINES OF VALUE #( FOR param IN is_selection_data-selection_fields WHERE ( is_parameter = abap_true AND
                                                                                                  low IS NOT INITIAL OR
                                                                                                  high IS NOT INITIAL OR
                                                                                                  option IS NOT INITIAL )
                                                                                                ( param ) ) )
                                     ( LINES OF VALUE #( FOR multi_param IN is_selection_data-selfields_multi
                                                         WHERE ( is_parameter = abap_true AND
                                                                 low IS NOT INITIAL OR
                                                                 high IS NOT INITIAL OR
                                                                 option IS NOT INITIAL )
                                                         ( multi_param ) ) ) ).
    mt_selfields_multi    = is_selection_data-selfields_multi.
    mt_multi_or           = is_selection_data-multi_or.

    mo_tabfields          = COND #( WHEN is_selection_data-tabfields IS BOUND THEN is_selection_data-tabfields->copy( ) ELSE NEW #( ) ).
    mo_tabfields_original = COND #( WHEN is_selection_data-tabfields IS BOUND THEN is_selection_data-tabfields ELSE NEW #( ) ).
    mo_tabfields_all      = COND #( WHEN is_selection_data-tabfields_all IS BOUND THEN is_selection_data-tabfields_all->copy( ) ELSE NEW #( ) ).
    mo_tabfields_all_original = COND #( WHEN is_selection_data-tabfields_all IS BOUND THEN is_selection_data-tabfields_all ELSE NEW #( ) ).

    mt_table_to_alias_map = mo_tabfields_all->build_table_to_alias_map( ).
    mt_exclude_function   = is_selection_data-exclude_function.

    mr_s_global_data = CAST zdbbr_global_data( zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main )->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).

    IF ms_join_def-tables IS NOT INITIAL.
      mf_join_is_active = abap_true.
    ENDIF.

    IF is_selection_data-no_grouping = abap_true.
      clear_aggregation_fields( CHANGING ct_selection_fields = mt_selection_fields ).
    ENDIF.
  ENDMETHOD.


  METHOD count_lines.
    determine_group_by_state( ).
    determine_aggregation_state( ).
    build_full_fieldnames( ).

    IF mf_group_by = abap_true OR mf_aggregation = abap_true.
      count_lines_with_aggregation( ).
    ELSE.
      count_lines_simple( ).
    ENDIF.
  ENDMETHOD.


  METHOD count_lines_simple.
    create_where_clause( ).

    create_from_clause( ).

    CHECK select_data( if_count_lines = abap_true ).

    " if no selection occurred, prevent screen visibility
    IF ms_control_info-number <= 0.
      MESSAGE i060(zdbbr_info).
    ELSE.
      DATA(lv_number_of_lines) = |{ ms_control_info-number NUMBER = USER }|.
      MESSAGE i024(zdbbr_info) WITH lv_number_of_lines.
    ENDIF.
  ENDMETHOD.


  METHOD count_lines_with_aggregation.
    create_select_clause( ).
    create_from_clause( ).
    create_group_by_clause( ).
    create_where_clause( ).
    create_dynamic_table( ).

    CHECK select_data( if_count_lines = abap_true ).

    IF ms_control_info-number <= 0.
      MESSAGE i060(zdbbr_info).
    ELSE.
      DATA(lv_number_of_lines) = |{ ms_control_info-number NUMBER = USER }|.
      MESSAGE i024(zdbbr_info) WITH lv_number_of_lines.
    ENDIF.
  ENDMETHOD.


  METHOD create_default_where_clause.
    FIELD-SYMBOLS: <ls_selfield> TYPE zdbbr_selfield.

    DATA: lt_selfields     TYPE zdbbr_seltab_t,
          lt_selfields_and LIKE lt_selfields.

    CLEAR: mt_where,
           mt_or.

    LOOP AT mt_selection_fields ASSIGNING <ls_selfield>
       WHERE virtual_join_field = abap_false AND
             is_parameter       = abap_false AND
             ( low IS NOT INITIAL OR
               high IS NOT INITIAL OR
               option IS NOT INITIAL ).

*..... get table field to get alias name
      TRY.
          DATA(lr_selection_field) = mo_tabfields_all->get_field_ref(
              iv_tabname_alias = <ls_selfield>-tabname_alias
              iv_fieldname     = <ls_selfield>-fieldname
          ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      DATA(lv_fieldname_sql) = lr_selection_field->sql_fieldname_long.
      DATA(lv_fieldname) = lr_selection_field->fieldname.

      IF lr_selection_field->inttype = cl_abap_typedescr=>typekind_hex.
        DATA(lf_is_raw) = abap_true.
        DATA(lv_initial_value) = zcl_dbbr_dictionary_helper=>create_initial_raw_value( lr_selection_field->length ).
      ENDIF.

      APPEND VALUE #(
        sqlfieldname = lv_fieldname_sql
        field        = lv_fieldname
        low          = <ls_selfield>-low
        high         = <ls_selfield>-high
        sign         = <ls_selfield>-sign
        option       = <ls_selfield>-option
      ) TO lt_selfields ASSIGNING FIELD-SYMBOL(<ls_selfield_where>).

      IF lf_is_raw = abap_true.
        IF <ls_selfield_where>-low IS INITIAL.
          <ls_selfield_where>-low =
          <ls_selfield>-low = lv_initial_value.
        ENDIF.
      ENDIF.

*.... handle special case for Date-type fields with only option filled
      IF <ls_selfield>-datatype = 'DATS' AND
         <ls_selfield_where>-option   <> space AND
         <ls_selfield_where>-low      = space  AND
         <ls_selfield_where>-high     = space.
*...... fill date with initial date
        <ls_selfield_where>-low = VALUE sy-datum( ).
      ENDIF.

      " Search for multiple input
      LOOP AT mt_selfields_multi ASSIGNING FIELD-SYMBOL(<ls_selfield_multi>)
          WHERE fieldname     = <ls_selfield>-fieldname AND
                tabname_alias = <ls_selfield>-tabname_alias AND
                ( low  IS NOT INITIAL OR
                  high IS NOT INITIAL OR
                  option IS NOT INITIAL ).

        IF <ls_selfield_multi>-low = '#' AND <ls_selfield_multi>-option <> '#'.
          APPEND VALUE #(
            sqlfieldname = lv_fieldname_sql
            field        = lv_fieldname
            low          = COND #( WHEN lf_is_raw = abap_true AND <ls_selfield_multi>-low IS INITIAL THEN lv_initial_value ELSE space )
            high         = space
            sign         = <ls_selfield_multi>-sign
            option       = <ls_selfield_multi>-option
          ) TO lt_selfields.
        ELSE.
          APPEND VALUE #(
            sqlfieldname = lv_fieldname_sql
            field        = lv_fieldname
            low          = COND #( WHEN lf_is_raw = abap_true AND <ls_selfield_multi>-low IS INITIAL THEN lv_initial_value ELSE <ls_selfield_multi>-low )
            high         = <ls_selfield_multi>-high
            sign         = <ls_selfield_multi>-sign
            option       = <ls_selfield_multi>-option
          ) TO lt_selfields.
        ENDIF.
      ENDLOOP.

      APPEND LINES OF lt_selfields TO lt_selfields_and.
      CLEAR lt_selfields.
    ENDLOOP.

    " AND-selection
    APPEND VALUE #(
        pos    = 0
        values = lt_selfields_and
    ) TO mt_or.

    " append values from or tuple selections
    LOOP AT mt_multi_or ASSIGNING FIELD-SYMBOL(<ls_or_tuple>).
      APPEND VALUE #( pos = <ls_or_tuple>-pos ) TO mt_or ASSIGNING FIELD-SYMBOL(<ls_or_selfields>).

      LOOP AT <ls_or_tuple>-values ASSIGNING FIELD-SYMBOL(<ls_or_tuple_value>).
        IF mt_virtual_join_table_range IS NOT INITIAL AND
           <ls_or_tuple_value>-tabname IN mt_virtual_join_table_range.
          CONTINUE.
        ENDIF.

        APPEND CORRESPONDING zdbbr_seltab( <ls_or_tuple_value> ) TO <ls_or_selfields>-values ASSIGNING FIELD-SYMBOL(<ls_or_selfield>).

*...... get table field to get alias name
        TRY.
            lr_selection_field = mo_tabfields_all->get_field_ref(
                iv_tabname_alias = <ls_or_tuple_value>-tabname_alias
                iv_fieldname     = <ls_or_tuple_value>-fieldname
            ).
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.

        <ls_or_selfield>-field = lr_selection_field->sql_fieldname_long.

        " include multi values
        LOOP AT <ls_or_tuple_value>-multi_values ASSIGNING FIELD-SYMBOL(<ls_or_tuple_multi_value>).
          DATA(ls_multi_value) = CORRESPONDING zdbbr_seltab( <ls_or_tuple_multi_value> ).
          ls_multi_value-sqlfieldname = <ls_or_selfield>-sqlfieldname.
          ls_multi_value-field = <ls_or_selfield>-field.
          APPEND ls_multi_value TO <ls_or_selfields>-values.
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    " build 'where'-clause for AND fields
    mt_where = zcl_dbbr_where_clause_builder=>create_or_condition(
      it_or_seltab = mt_or
    ).

  ENDMETHOD.


  METHOD create_dynamic_table.
    FIELD-SYMBOLS: <lt_table>      TYPE STANDARD TABLE,
                   <lt_table_temp> TYPE STANDARD TABLE.

    IF mr_t_data IS BOUND.

      IF if_cache_existing_data = abap_true.
        mr_t_temp_data = mr_t_data.
      ELSE.
        CLEAR: mr_t_data,
               mr_t_temp_data.
      ENDIF.

    ENDIF.


    " create components for dynamic table
    mt_dyntab_components = CORRESPONDING #( zcl_dbbr_output_tab_builder=>create_dyn_comp_tab(
        ir_tabfields       = mo_tabfields
        if_active_grouping = mf_group_by
        it_add_texts       = mt_add_texts
        is_tech_info       = ms_technical_info )
    ).

    DATA(lr_struct_descr) = cl_abap_structdescr=>create(
      p_components = CORRESPONDING #( mt_dyntab_components )
      p_strict     = abap_false
    ).
    TRY.
        " create table type
        DATA(lr_table_descr) = cl_abap_tabledescr=>create( p_line_type = lr_struct_descr ).
        " create table
        CREATE DATA mr_t_data TYPE HANDLE lr_table_descr.
        " create another table with the same definition for comparing table entries
        IF if_cache_existing_data = abap_true AND mr_t_temp_data IS BOUND.
          ASSIGN mr_t_data->* TO <lt_table>.
          ASSIGN mr_t_temp_data->* TO <lt_table_temp>.
          <lt_table> = CORRESPONDING #( <lt_table_temp> ).
        ENDIF.

        CLEAR mr_t_temp_data.
        CREATE DATA mr_t_temp_data TYPE HANDLE lr_table_descr.
      CATCH cx_sy_table_creation INTO DATA(lr_table_creation_exc).
        MESSAGE lr_table_creation_exc TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD create_field_catalog.
    DATA: lf_create_fieldcat_entry TYPE abap_bool,
          lf_use_num_conversion    TYPE abap_bool.

    zcl_dbbr_screen_helper=>show_progress( |{ TEXT-001 }| ).

    DATA(lt_domain2convexit) = zcl_dbbr_dictionary_helper=>get_domain_to_convexit_entries( ).

*...create fieldcatalog / `select`-part from output fields
*...1) update mode of tabfield list to `output`.
    mo_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).
    mo_tabfields->sort( ).

    mo_tabfields->initialize_iterator( ).

    WHILE mo_tabfields->has_more_lines( ).
      CLEAR: lf_create_fieldcat_entry,
             lf_use_num_conversion.

      DATA(lr_current_field) = mo_tabfields->get_next_entry( ).

*...exclude some fields right away from the field catalog
      CHECK lr_current_field->is_parameter = abap_false.

      " handle text field
      IF lr_current_field->is_text_field = abap_true.
        IF lr_current_field->output_active = abap_true.
**...handle additional fields text output
          add_text_fieldcat_entry( lr_current_field ).
        ENDIF.
        CONTINUE.
      ENDIF.

      IF ms_technical_info-use_reduced_memory = abap_true.
        IF lr_current_field->output_active = abap_true OR
**...key fields will always be included in output table
           lr_current_field->is_key = abap_true.
          lf_create_fieldcat_entry = abap_true.
        ENDIF.
      ELSE.
        IF lr_current_field->is_formula_field = abap_true AND
           lr_current_field->output_active = abap_false.
          CONTINUE.
        ENDIF.

        lf_create_fieldcat_entry = abap_true.
      ENDIF.

      IF lf_create_fieldcat_entry = abap_true.
        DATA(lv_tabix) = sy-tabix.
        DATA(ls_field) = VALUE lvc_s_fcat( ).

        IF lr_current_field->is_formula_field = abap_false.
          DATA(ls_dtel_infos) = zcl_dbbr_dictionary_helper=>get_table_field_info( iv_tablename = lr_current_field->tabname
                                                                                   iv_fieldname = lr_current_field->fieldname ).
          ls_field-convexit = ls_dtel_infos-convexit.
        ENDIF.

        IF lr_current_field->is_key = abap_true.
          ls_field-parameter2 = 'K'.
          IF lr_current_field->tabname = ms_control_info-primary_table.
            ls_field-fix_column = xsdbool( ms_technical_info-key_cols_not_fixed = abap_false ).
            ls_field-key_sel = abap_true.
          ELSE.
            ls_field-emphasize = zif_dbbr_global_consts=>gc_alv_emphasize-key_color.
          ENDIF.
        ENDIF.

        IF lr_current_field->is_formula_field = abap_true.
          ls_field-emphasize = COND #( WHEN ms_technical_info-color_formula_fields = abap_true THEN
                                                zif_dbbr_global_consts=>gc_alv_colors-light_yellow ).
          ls_field-parameter2 = 'F'.
          ls_field-icon = zcl_dbbr_formula_helper=>is_icon_field( lr_current_field ).
        ENDIF.

*...... Special settings for numerical fields
        IF lr_current_field->is_numeric = abap_true.
          ls_field-no_zero = ms_technical_info-zero_val_as_blank.
        ENDIF.

**......check conversion exit setting
        IF ms_technical_info-no_convexit = abap_true.
          IF ls_field-convexit <> space.
            ls_field-no_convext = abap_true.
            CLEAR ls_field-edit_mask.
            ls_field-convexit = 'EMPTY'.
          ENDIF.
        ELSE. " use conversion exit
          IF  ls_field-convexit = space.
*.......... 1) try to find mapping entry for the current domain
            ls_field-convexit = VALUE #( lt_domain2convexit[ domname = lr_current_field->domname ]-convexit OPTIONAL ).
          ENDIF.

          IF ls_field-convexit = space AND
             lr_current_field->is_numeric = abap_true.
            IF ms_technical_info-no_trailing_sign = abap_true.
              ls_field-convexit = 'NUM'.

              lf_use_num_conversion = abap_true.
            ENDIF.
          ENDIF.

        ENDIF.

        IF lr_current_field->is_formula_field = abap_false.
          ls_field-ref_field = lr_current_field->fieldname.
          ls_field-ref_table = lr_current_field->tabname.
        ENDIF.

        ls_field-fieldname = lr_current_field->alv_fieldname.
        ls_field-lowercase = lr_current_field->is_lowercase.
        ls_field-outputlen = lr_current_field->outputlen.
        ls_field-intlen = lr_current_field->length.

        IF lf_use_num_conversion = abap_false.
          fill_fcat_quan_curr_field(
            EXPORTING ir_tabfield       = lr_current_field
                      ir_tabfields      = mo_tabfields
            CHANGING  cv_quantity_field = ls_field-qfieldname
                      cv_currency_field = ls_field-cfieldname
          ).
        ENDIF.

        set_fieldcat_coltexts(
          EXPORTING ir_field    = lr_current_field
          CHANGING  cs_fieldcat = ls_field
        ).

        handle_possible_jumpfield(
          EXPORTING ir_current_field = lr_current_field
          CHANGING  cs_field         = ls_field
        ).

        ls_field-no_out = xsdbool( lr_current_field->output_active = abap_false ).

**...set output mode to checkbox if there is at least one domain fix value with value 'X'
        ls_field-checkbox = determine_checkbox_output( EXPORTING ir_field = lr_current_field ).

        ls_field-inttype = lr_current_field->inttype.
        ls_field-decimals = lr_current_field->decimals.
        ls_field-datatype = lr_current_field->datatype.
        ls_field-rollname = lr_current_field->rollname.
        ls_field-domname = lr_current_field->domname.
        ls_field-dd_roll = lr_current_field->rollname.

        ls_field-f4availabl = lr_current_field->f4_available.

        ls_field-sp_group = VALUE #( mt_group_tab_map[ tabname = lr_current_field->tabname ]-sp_group OPTIONAL ).
        APPEND ls_field TO mt_fieldcat.
      ENDIF.
    ENDWHILE.

    add_line_index_column( ).
    add_hide_flag_column( ).

  ENDMETHOD.


  METHOD create_for_all_where_clause.
    CONSTANTS: c_dyn_for_all_table TYPE string VALUE zif_dbbr_global_consts=>c_for_all_entries_select_table.

    LOOP AT ms_association_target-fields ASSIGNING FIELD-SYMBOL(<ls_assoc_field>).
      mt_where = VALUE #( BASE mt_where
        (
          |{ <ls_assoc_field>-name } { <ls_assoc_field>-operator } | &&
          |@{ c_dyn_for_all_table }-{ <ls_assoc_field>-name } | &&
          | { <ls_assoc_field>-and_or }|
        )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD create_from_clause.
    mt_from = VALUE #( ( |{ ms_control_info-primary_table }| ) ).
  ENDMETHOD.


  METHOD create_group_by_clause.
    DATA(lt_virtual_join_tab_range) = mt_virtual_join_table_range.
    IF lt_virtual_join_tab_range IS INITIAL.
      lt_virtual_join_tab_range = VALUE #( ( sign = 'I' option = 'EQ' ) ).
    ENDIF.

    DATA(lt_group_by_fields) = VALUE zdbbr_selfield_itab(
       FOR selfield IN mt_selection_fields
       WHERE ( group_by = abap_true AND
               tabname  NOT IN lt_virtual_join_tab_range
               )
       ( selfield )
    ).


    LOOP AT lt_group_by_fields ASSIGNING FIELD-SYMBOL(<ls_group_by>).
      IF mf_join_is_active = abap_true.
*.... retrieve correct table field to get alias
        TRY.
            DATA(lr_group_by_field) = mo_tabfields->get_field_ref(
                iv_tabname_alias = <ls_group_by>-tabname_alias
                iv_fieldname     = <ls_group_by>-fieldname
            ).
            mt_group_by = VALUE #( BASE mt_group_by
              ( |{ lr_group_by_field->sql_fieldname_long }| )
            ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ELSE.
        mt_group_by = VALUE #( BASE mt_group_by
          ( |{ <ls_group_by>-fieldname  }| )
        ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_group_by ASSIGNING FIELD-SYMBOL(<lv_group_by_field>).
      IF sy-tabix <> lines( mt_group_by ).
        <lv_group_by_field> = <lv_group_by_field> && ','.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_order_by_clause.
    """ create fieldcatalog / `order by`-part from output fields
    " 1) update mode of tabfield list to `sort`.
    mo_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-sort ).

    " check if there is at least one `sort`-field
    CHECK mo_tabfields->checked_field_exists( ).

    mo_tabfields->sort( ).
    mo_tabfields->initialize_iterator( if_for_active = abap_true ).

    WHILE mo_tabfields->has_more_lines( ).
      DATA(lr_field) = mo_tabfields->get_next_entry( ).

      CHECK lr_field->is_virtual_join_field = abap_false.

      DATA(lv_order_by_type) = SWITCH string(
        lr_field->sort_direction
        WHEN zif_dbbr_global_consts=>gc_sort_direction-ascending THEN 'ASCENDING'
        WHEN zif_dbbr_global_consts=>gc_sort_direction-descending THEN 'DESCENDING'
      ).
      mt_order_by = VALUE #( BASE mt_order_by ( |{ lr_field->sql_fieldname_long } { lv_order_by_type }, | ) ).

      " if field will not be displayed prevent sorting in ALV -> not possible as this field does not exist in the fieldcatalog
      CHECK lr_field->output_active = abap_true.

      mt_sort_alv = VALUE #(
        BASE mt_sort_alv
        ( spos       = lr_field->sort_order
          fieldname  = lr_field->alv_fieldname
          up         = xsdbool( lr_field->sort_direction = zif_dbbr_global_consts=>gc_order_by_type-ascending )
          down       = xsdbool( lr_field->sort_direction = zif_dbbr_global_consts=>gc_order_by_type-descending ) )
      ).
    ENDWHILE.

    IF mt_order_by IS NOT INITIAL.
      ASSIGN mt_order_by[ lines( mt_order_by ) ] TO FIELD-SYMBOL(<lv_last_field>).
      <lv_last_field> = replace( val = <lv_last_field> regex = ',.*' with = space ).
    ENDIF.

  ENDMETHOD.


  METHOD create_select_clause.
    CLEAR mt_select.

    IF mf_group_by = abap_false AND
       mf_aggregation = abap_false AND
       mf_join_is_active = abap_false.

      mt_select = VALUE #( ( `*` ) ).
      RETURN.
    ENDIF.

    mo_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).

    mo_tabfields->initialize_iterator( ).

    WHILE mo_tabfields->has_more_lines( ).
      DATA(lr_current_entry) = mo_tabfields->get_next_entry( ).

*...ignore formula fields - they have to be calculated
*...also to be ignored are text fields and parameters
      CHECK lr_current_entry->is_formula_field = abap_false.
      CHECK lr_current_entry->is_text_field = abap_false.
      CHECK lr_current_entry->is_virtual_join_field = abap_false.
      CHECK lr_current_entry->is_parameter = abap_false.

      IF ms_technical_info-use_reduced_memory = abap_true.
        IF lr_current_entry->output_active = abap_false AND
           lr_current_entry->has_active_text_field = abap_false AND
           lr_current_entry->is_key = abap_false AND
           lr_current_entry->needed_for_virtual_join = abap_false.
          CONTINUE.
        ENDIF.
      ENDIF.

      TRY.
          DATA(lr_selfield) = REF #( mt_selection_fields[ tabname   = lr_current_entry->tabname
                                                          fieldname = lr_current_entry->fieldname ] ).
          IF lr_selfield->aggregation <> space.
            APPEND |{ lr_selfield->aggregation }( { lr_current_entry->sql_fieldname_long } ) AS { lr_current_entry->alv_fieldname }, | TO mt_select.
          ELSE.
            APPEND |{ lr_current_entry->sql_fieldname_long } AS { lr_current_entry->alv_fieldname }, | TO mt_select.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
*... safety check: is this field an output field?
          APPEND |{ lr_current_entry->sql_fieldname_long } AS { lr_current_entry->alv_fieldname }, | TO mt_select.
      ENDTRY.
    ENDWHILE.

    IF mf_group_by = abap_true.
      APPEND |COUNT( * ) AS { zif_dbbr_c_special_out_columns=>line_index }| TO mt_select.
    ELSE.
      IF mt_select IS NOT INITIAL.
        ASSIGN mt_select[ lines( mt_select ) ] TO FIELD-SYMBOL(<lv_last_field>).
        <lv_last_field> = replace( val = <lv_last_field> regex = ',.*' with = space ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD create_util.
    DATA: lv_typename  TYPE seoclsname,
          lf_sql_query TYPE abap_bool.

    IF is_selection_data-join_def-tables IS NOT INITIAL AND
       is_selection_data-entity_type <> zif_dbbr_c_selscreen_mode=>query.
      lv_typename = 'ZCL_DBBR_JOIN_SELECTION_UTIL'.
    ELSE.
      CASE is_selection_data-entity_type.

        WHEN zif_dbbr_c_selscreen_mode=>table.
          lv_typename = 'ZCL_DBBR_TABLE_SELECTION_UTIL'.

        WHEN zif_dbbr_c_selscreen_mode=>cds_view.
          lv_typename = 'ZCL_DBBR_CDS_SELECTION_UTIL'.

        WHEN zif_dbbr_c_selscreen_mode=>query.
          IF is_selection_data-query_string IS NOT INITIAL AND
             is_selection_data-entity_id IS INITIAL.
            lf_sql_query = abap_true.
          ELSE.
*.......... SQL queries need a different kind of processing
            SELECT SINGLE @abap_true
              FROM zdbbr_queryh
              WHERE query_name  = @is_selection_data-entity_id
                AND is_sql_query = @abap_true
            INTO @lf_sql_query.
          ENDIF.

          IF lf_sql_query = abap_true.
            lv_typename = 'ZCL_DBBR_SQL_QUERY_SELCTN_UTIL'.
          ELSE.
            lv_typename = 'ZCL_DBBR_QUERY_SELECTION_UTIL'.
          ENDIF.
      ENDCASE.
    ENDIF.


    IF lv_typename IS NOT INITIAL.
      CREATE OBJECT result TYPE (lv_typename)
        EXPORTING
          is_selection_data = is_selection_data.
*... perform initialization for util
      result->init( ).
    ENDIF.
  ENDMETHOD.


  METHOD create_where_clause.
    IF mf_do_for_all_select = abap_true.
      create_for_all_where_clause( ).
    ELSE.
      create_default_where_clause( ).
    ENDIF.
  ENDMETHOD.


  METHOD determine_aggregation_state.
    " determine if single aggregation is active
    LOOP AT mt_selection_fields ASSIGNING FIELD-SYMBOL(<ls_selfield>) WHERE aggregation <> space.
      mf_aggregation = abap_true.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD determine_checkbox_output.
    CHECK: ir_field->is_numeric = abap_false,
          ir_field->rollname IS NOT INITIAL.

    DATA(lr_elem_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( ir_field->rollname ) ).

    lr_elem_descr->get_ddic_fixed_values(
      RECEIVING
        p_fixed_values = DATA(lt_fix_values)
      EXCEPTIONS
        not_found      = 1
        no_ddic_type   = 2
        OTHERS         = 3
    ).
    IF sy-subrc = 0.
      result = xsdbool( lines( lt_fix_values ) = 2 AND
                        line_exists( lt_fix_values[ low = 'X' ] ) AND
                        line_exists( lt_fix_values[ option = 'EQ' low = space ] ) ).
    ENDIF.
  ENDMETHOD.


  METHOD determine_group_by_state.
    mf_group_by = xsdbool( line_exists( mt_selection_fields[ group_by = abap_true ] ) ).
  ENDMETHOD.


  METHOD execute_formula_for_lines.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    CHECK mo_formula_calculator IS NOT INITIAL.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ TEXT-005 }| iv_progress = 25 ).

    ASSIGN mr_t_data->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_row>).
      DATA(lr_row) = REF data( <ls_row> ).

      mo_formula_calculator->calculate_row( CHANGING cr_row = lr_row ).

    ENDLOOP.
  ENDMETHOD.


  METHOD fill_fcat_quan_curr_field.
    CHECK: ir_tabfield->is_text_field = abap_false,
       ir_tabfield->ref_tab IS NOT INITIAL,
       ir_tabfield->ref_field IS NOT INITIAL,
       ( ir_tabfield->datatype = 'QUAN' OR
         ir_tabfield->datatype = 'CURR' ).

    " find the corresponding reference field
    TRY.
        DATA(lr_reference_field) = ir_tabfields->get_field_ref(
            iv_tabname_alias   = ir_tabfield->ref_tab
            iv_fieldname = ir_tabfield->ref_field
        ).

        CASE ir_tabfield->datatype.

          WHEN 'QUAN'.
            cv_quantity_field = lr_reference_field->alv_fieldname.

          WHEN 'CURR'.
            cv_currency_field = lr_reference_field->alv_fieldname.
        ENDCASE.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD fill_header.
    cr_dd_doc->add_table(
      EXPORTING no_of_columns = 2
                border        = '0'
                width         = '60%'
      IMPORTING table         = DATA(lr_table)
    ).

    lr_table->add_column( IMPORTING column = DATA(lr_col1) ).
    lr_table->add_column( IMPORTING column = DATA(lr_col2) ).
    IF is_join_active( ).
      lr_table->add_column( IMPORTING column = DATA(lr_col3) ).
    ENDIF.

*.. add table content
    lr_table->span_columns( col_start_span = lr_col1 no_of_cols = 2 ).
    lr_table->new_row( ).
    lr_col1->add_text(
        text          = |{ 'Primary Entity' }|
        sap_emphasis  = cl_dd_document=>strong
    ).
    lr_table->new_row( ).

    DATA(lt_tables) = mo_tabfields->get_table_list( ).
    DATA(ls_primary_table) = lt_tables[ is_primary = abap_true ].
    DELETE lt_tables WHERE is_primary = abap_true
                        OR tabname = zif_dbbr_global_consts=>gc_formula_dummy_table
                        OR tabname = zif_dbbr_global_consts=>c_parameter_dummy_table.

*.. add primary table and number of lines
    IF is_join_active( ).
      DATA(lv_primtab_name) = '(' && ls_primary_table-alias  && `) ` &&
                              ls_primary_table-tabname_alias.
      lr_col2->add_text( text = |{ ls_primary_table-tabname }| ).
      lr_col3->add_text( text = |{ ls_primary_table-description }| ).
    ELSE.
      lv_primtab_name = ls_primary_table-tabname.
      lr_col2->add_text( text = |{ ls_primary_table-description }| ).
    ENDIF.

    lr_col1->add_text( text = |{ lv_primtab_name }| ).

    " create header lines for secondary tables
    IF is_join_active( ).
      lr_table->new_row( ).
      lr_table->span_columns( col_start_span = lr_col1 no_of_cols = 2 ).
      lr_col1->add_text( text = 'Secondary Tables' sap_emphasis  = cl_dd_document=>strong ).
      lr_table->new_row( ).

      " print secondary table if available
      LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
        DATA(lv_tabname) = '(' && <ls_table>-alias  && `) ` &&
                            <ls_table>-tabname_alias.

        lr_col1->add_text( text = |{ lv_tabname }| ).
        lr_col2->add_text( text = |{ <ls_table>-tabname }| ).
        lr_col3->add_text( text = |{ <ls_table>-description }| ).

        lr_table->new_row( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_alv_filter_from_criteria.
    CHECK ms_technical_info-activate_alv_live_filter = abap_true.

    rt_filter = get_alv_util( )->get_filter_from_criteria(
        it_criteria       = mt_selection_fields
        it_criteria_multi = mt_selfields_multi
    ).

  ENDMETHOD.


  METHOD get_alv_util.
    IF mr_alv_util IS INITIAL.
      mr_alv_util = NEW zcl_dbbr_output_alv_util(
          if_use_live_filter = ms_technical_info-activate_alv_live_filter
          ir_alv_grid        = mr_alv_grid
          ir_t_data          = mr_t_data
          ir_tabfields       = mo_tabfields_all
          ir_t_fieldcat      = REF #( mt_fieldcat )
      ).
    ENDIF.

    rr_alv_util = mr_alv_util.
  ENDMETHOD.


  METHOD get_short_texts.
    CLEAR ef_alt_text_exists.

    IF ir_tabfield->alt_long_text <> space OR ir_tabfield->alt_medium_text <> space.
      ef_alt_text_exists = abap_true.
    ENDIF.

    ev_reptext = ir_tabfield->header_text.
    ev_scrtext_s = ir_tabfield->std_short_text.


    ev_scrtext_m = COND #( WHEN ir_tabfield->alt_medium_text IS NOT INITIAL THEN
                             ir_tabfield->alt_medium_text
                           ELSE
                             ir_tabfield->std_medium_text ).
    ev_scrtext_l = COND #( WHEN ir_tabfield->alt_long_text IS NOT INITIAL THEN
                             ir_tabfield->alt_long_text
                           ELSE
                             ir_tabfield->std_long_text ).

    ev_ddtext = ir_tabfield->field_ddtext.
  ENDMETHOD.


  METHOD handle_possible_jumpfield.
    TRY .
        DATA(lr_jump_dest) = REF #( mt_jumpdest[ jump_source_field = ir_current_field->fieldname
                                                 jump_source_table = ir_current_field->tabname ] ).
        cs_field-hotspot = xsdbool( lr_jump_dest->is_hotspot = abap_true ).
        cs_field-parameter1 = 'J'.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD init_navigation_breadcrumbs.
    CHECK ir_container IS BOUND.
    CHECK mr_breadcrumbs_dd IS INITIAL.

    mr_breadcrumbs_dd = NEW cl_dd_document( ).

    mr_breadcrumbs_dd->initialize_document( background_color = cl_dd_area=>col_background_level2 ).

    mr_breadcrumbs_dd->add_table(
      EXPORTING no_of_columns = 1
                border        = '0'
                width         = '100%'
      IMPORTING table         = DATA(lr_table)
    ).

    lr_table->add_column( IMPORTING column = DATA(lr_col1) ).
    lr_table->new_row( ).
    lr_col1->add_text(
        text          = 'Navigation History'
        sap_emphasis  = cl_dd_document=>strong
    ).
    lr_table->new_row( ).

*... fill navigation breadcrumbs
    LOOP AT mt_nav_breadcrumbs ASSIGNING FIELD-SYMBOL(<lv_breadcrumb>).
      IF sy-tabix > 1.
        lr_col1->add_text(
          text = ' => '
          sap_emphasis = cl_dd_area=>strong
        ).
      ENDIF.
      lr_col1->add_text(
        text = |{ <lv_breadcrumb> }|
      ).
    ENDLOOP.
    mr_breadcrumbs_dd->merge_document( ).

    mr_breadcrumbs_dd->display_document( parent = ir_container reuse_control = abap_true ).
  ENDMETHOD.


  METHOD is_join_active.
    result = mf_join_is_active.
  ENDMETHOD.

  METHOD read_entity_infos.
    RETURN.
  ENDMETHOD.

  METHOD select_data.
    FIELD-SYMBOLS: <lt_table>     TYPE table,
                   <lt_selection> TYPE table.

    rf_success = abap_true.

    TRY.
*...... generate the program to perform the data selection
        IF ms_technical_info-activate_alv_live_filter = abap_true OR
           if_refresh_only = abap_false.
          mr_select_program = zcl_dbbr_select_prog_creator=>create_program(
              if_only_create_count_logic = if_count_lines
              if_create_for_all          = mf_do_for_all_select
              is_association_target      = ms_association_target
              it_select                  = mt_select
              it_from                    = mt_from
              it_where                   = mt_where
              it_order_by                = mt_order_by
              it_group_by                = mt_group_by
              iv_max_size                = ms_technical_info-max_lines
          ).
        ELSE.
          mr_select_program->set_max_rows( ms_technical_info-max_lines ).
        ENDIF.

*...... either select the data or, only count the lines for the where clause
        ASSIGN mr_t_data->* TO <lt_table>.
        IF if_count_lines = abap_false.

          zcl_dbbr_screen_helper=>show_progress( iv_text = |{ TEXT-001 }| iv_progress = 1 ).

          mr_select_program->select_data(
            EXPORTING ir_t_for_all = mr_t_for_all_data
            IMPORTING et_data      = <lt_table>
          ).

          ms_control_info-number = lines( <lt_table> ).

**....... also determine the maximum number of lines if no join is active
          IF NOT is_join_active( ) AND
             ms_control_info-number = ms_technical_info-max_lines.

            zcl_dbbr_screen_helper=>show_progress( iv_text = |{ TEXT-007 }| iv_progress = 25 ).

            IF mf_group_by = abap_true OR mf_aggregation = abap_true.
              mv_max_lines_existing = mr_select_program->determine_size_for_group_by( mr_t_temp_data ).
            ELSE.
              mv_max_lines_existing = mr_select_program->determine_size( mr_t_for_all_data ).
            ENDIF.
          ELSE.
            mv_max_lines_existing = ms_control_info-number.
          ENDIF.
        ELSE.
*........ only count the number of lines that exist for the condition
          zcl_dbbr_screen_helper=>show_progress( iv_text = |{ TEXT-007 }| iv_progress = 25 ).

          IF mf_group_by = abap_true OR mf_aggregation = abap_true.
            ms_control_info-number = mr_select_program->determine_size_for_group_by( mr_t_temp_data ).
          ELSE.
            ms_control_info-number = mr_select_program->determine_size( ).
          ENDIF.
        ENDIF.

      CATCH zcx_dbbr_dyn_prog_generation INTO DATA(lx_dyn_generation).
        CLEAR rf_success.
        MESSAGE lx_dyn_generation->zif_dbbr_exception_message~get_message( ) TYPE 'I' DISPLAY LIKE 'E'.
      CATCH zcx_dbbr_selection_common INTO DATA(lx_sql_error).
        CLEAR rf_success.
        MESSAGE lx_sql_error->zif_dbbr_exception_message~get_message( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD set_fieldcat_coltexts.
    get_short_texts( EXPORTING ir_tabfield        = ir_field
                     IMPORTING ev_reptext         = DATA(lv_reptext)
                               ev_scrtext_s       = DATA(lv_scrtext_s)
                               ev_scrtext_m       = DATA(lv_scrtext_m)
                               ev_scrtext_l       = DATA(lv_scrtext_l)
                               ev_ddtext          = DATA(lv_ddtext)
                               ef_alt_text_exists = DATA(lf_alt_text_exists) ).

    cs_fieldcat-tooltip = ir_field->sql_fieldname.
    cs_fieldcat-scrtext_m = lv_scrtext_m.
    cs_fieldcat-scrtext_s = lv_scrtext_s.
    cs_fieldcat-reptext   = lv_reptext.

    DATA(lv_textfield_suffix) = COND #( WHEN ir_field->is_text_field = abap_true THEN ` (T)` ).
    DATA(lv_alias_prefix) = COND #( WHEN ir_field->alias IS NOT INITIAL THEN ir_field->alias && '~' ).

    cs_fieldcat-scrtext_l = COND #( WHEN lv_scrtext_m IS NOT INITIAL THEN
                                    lv_alias_prefix && lv_scrtext_m && lv_textfield_suffix
                                  WHEN lv_scrtext_l IS NOT INITIAL THEN
                                    lv_alias_prefix && lv_scrtext_l && lv_textfield_suffix
                                  WHEN lv_ddtext IS NOT INITIAL THEN
                                    lv_alias_prefix && lv_ddtext && lv_textfield_suffix
                                  ELSE
                                    ir_field->sql_fieldname ).

    IF lv_alias_prefix IS NOT INITIAL OR lv_textfield_suffix IS NOT INITIAL.
      CLEAR: cs_fieldcat-scrtext_m,
             cs_fieldcat-scrtext_s,
             cs_fieldcat-reptext.
    ENDIF.

    " set technical names if it is requested (for active join, technical names will always be used
    IF ms_technical_info-tech_names = abap_true.
      cs_fieldcat-reptext   =
      cs_fieldcat-scrtext_s =
      cs_fieldcat-scrtext_m =
      cs_fieldcat-scrtext_l =
      cs_fieldcat-coltext   = ir_field->sql_fieldname.
      cs_fieldcat-tooltip   = COND #( WHEN lv_scrtext_l IS NOT INITIAL THEN
                                        lv_scrtext_l
                                      WHEN lv_scrtext_m IS NOT INITIAL THEN
                                        lv_scrtext_m
                                      ELSE
                                        lv_ddtext ) && lv_textfield_suffix.
    ENDIF.
  ENDMETHOD.


  METHOD set_line_index_column_texts.
    IF ms_technical_info-tech_names = abap_true.
      cs_field-reptext   =
      cs_field-scrtext_s =
      cs_field-scrtext_m =
      cs_field-scrtext_l =
      cs_field-coltext   = cs_field-fieldname.
      cs_field-tooltip   = COND #( WHEN ms_line_index-scrtext_l IS NOT INITIAL THEN
                                        ms_line_index-scrtext_l
                                      WHEN ms_line_index-scrtext_m IS NOT INITIAL THEN
                                        ms_line_index-scrtext_m
                                      ELSE
                                        ms_line_index-fieldtext ).
    ELSE.
      cs_field-reptext = ms_line_index-reptext.
      cs_field-scrtext_s = ms_line_index-scrtext_s.
      cs_field-scrtext_m = ms_line_index-scrtext_m.
      cs_field-scrtext_l = ms_line_index-scrtext_l.
      cs_field-coltext = ms_line_index-reptext.
      cs_field-tooltip = ms_line_index-fieldtext.
    ENDIF.
  ENDMETHOD.


  METHOD set_miscinfo_for_selected_data.
    DATA: lt_where  TYPE TABLE OF string,
          lt_select TYPE TABLE OF string.

    FIELD-SYMBOLS: <lt_table>      TYPE STANDARD TABLE,
                   <lt_text_cache> TYPE SORTED TABLE.

    IF mt_add_texts IS INITIAL.
      RETURN.
    ENDIF.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ TEXT-004 }| iv_progress = 75 ).

    ASSIGN mr_t_data->* TO <lt_table>.

    " fill cache tables for text table selections
    LOOP AT mt_add_texts ASSIGNING FIELD-SYMBOL(<ls_add_text>).
      CLEAR: lt_select, lt_where.

      CHECK <ls_add_text>-selection_type <> zif_dbbr_c_text_selection_type=>domain_value.

      ASSIGN <ls_add_text>-table_cache->* TO <lt_text_cache>.
      CHECK sy-subrc = 0.

      " build select clause
      lt_select = VALUE #(
        BASE lt_select
        ( |{ <ls_add_text>-key_field } { <ls_add_text>-key_field2 } { <ls_add_text>-text_field }| )
      ).

      " include possible language field in where clause
      IF <ls_add_text>-language_field IS NOT INITIAL.
        lt_where = VALUE #( BASE lt_where
          ( |{ <ls_add_text>-language_field } = { cl_abap_dyn_prg=>quote( zcl_dbbr_appl_util=>get_description_language( ) ) }| )
        ).
      ENDIF.

      TRY .
          SELECT (lt_select) FROM (<ls_add_text>-text_table) INTO CORRESPONDING FIELDS OF TABLE <lt_text_cache>
            WHERE (lt_where).
        CATCH cx_sy_open_sql_error.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_row>).

      LOOP AT mt_add_texts ASSIGNING <ls_add_text>.
        " get domain key value from selection table
        ASSIGN COMPONENT <ls_add_text>-id_field_alv_int OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_text_key_value>).
        CHECK sy-subrc = 0.

        IF <ls_add_text>-id_field2_alv_int IS NOT INITIAL.
          ASSIGN COMPONENT <ls_add_text>-id_field2_alv_int OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_text_key2_value>).
          CHECK sy-subrc = 0.
        ENDIF.

        " is this a condition field?
        IF <ls_add_text>-condition_field IS NOT INITIAL.
          ASSIGN COMPONENT <ls_add_text>-condition_field_alv_int OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_cond_key_value>).
          IF sy-subrc <> 0.
            CONTINUE.
          ELSE. " check the condition value
            CASE <ls_add_text>-condition_op.
              WHEN 'EQ'.
                CHECK <lv_cond_key_value> EQ <ls_add_text>-condition_value.
              WHEN 'CP'.
                CHECK <lv_cond_key_value> CP <ls_add_text>-condition_value.
              WHEN 'CO'.
                CHECK <lv_cond_key_value> CO <ls_add_text>-condition_value.
              WHEN 'CA'.
                CHECK <lv_cond_key_value> CA <ls_add_text>-condition_value.
            ENDCASE.
          ENDIF.
        ENDIF.
        " get reference to text column in selection table
        ASSIGN COMPONENT <ls_add_text>-text_field_alv_int OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_text_value>).
        CHECK sy-subrc = 0.

        " read corresponding text value
        IF <ls_add_text>-selection_type = zif_dbbr_c_text_selection_type=>domain_value.
          DATA(lv_domain_fix_value) = condense( val = CONV domvalue_l( <lv_text_key_value> ) ).
          DATA(ls_text_value) = VALUE #( <ls_add_text>-domain_fix_values[ low = lv_domain_fix_value ] OPTIONAL ).
          <lv_text_value> = ls_text_value-ddtext.
        ELSE.
          ASSIGN <ls_add_text>-table_cache->* TO <lt_text_cache>.

          IF <lv_text_key2_value> IS ASSIGNED.
            READ TABLE <lt_text_cache> WITH TABLE KEY (<ls_add_text>-key_field) = <lv_text_key_value>
                                                      (<ls_add_text>-key_field2) = <lv_text_key2_value> ASSIGNING FIELD-SYMBOL(<ls_text_value>).
          ELSE.
            READ TABLE <lt_text_cache> WITH TABLE KEY (<ls_add_text>-key_field) = <lv_text_key_value> ASSIGNING <ls_text_value>.
          ENDIF.
          CHECK sy-subrc = 0.

          " extract text value from found text line
          zcl_dbbr_addtext_helper=>fill_text_field_value(
              iv_text_field     = <ls_add_text>-text_field
              iv_alv_text_field = <ls_add_text>-text_field_alv_int
              is_text_cache_row = <ls_text_value>
              ir_row            = REF #( <ls_row> )
              it_fix_values     = <ls_add_text>-domain_fix_values
          ).

        ENDIF.

        UNASSIGN: <lv_cond_key_value>,
                  <lv_text_key2_value>,
                  <lv_text_key_value>,
                  <lv_text_value>,
                  <ls_text_value>.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD set_where_as_alv_filter.
  ENDMETHOD.


  METHOD update_selection_for_filter.
    mr_alv_util->build_selection_criteria(
      IMPORTING
        et_criteria       = mt_selection_fields
        et_criteria_multi = mt_selfields_multi
    ).

    create_where_clause( ).
    refresh_selection( ).

  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = mt_exclude_function.
    result = VALUE #( BASE result
      ( zif_dbbr_c_selection_functions=>change_cds_parameters )
      ( zif_dbbr_c_selection_functions=>edit_data )
    ).

    IF mf_aggregation = abap_true OR
       mf_group_by    = abap_true.

      result = VALUE #(
       BASE result
       ( zif_dbbr_c_selection_functions=>toggle_entity_info_header )
       ( zif_dbbr_c_selection_functions=>group_by_selected_columns     )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_ui_function.

    CASE cv_function.

      WHEN zif_dbbr_c_selection_functions=>change_max_row_count.
        CLEAR cv_function.

        DATA(lv_max_rows) = zcl_dbbr_appl_util=>popup_get_value(
          EXPORTING
            is_field  = VALUE #( fieldname = 'TABIX' tabname = 'SYST' fieldtext = |{ 'Max Entries'(009) }| value = ms_technical_info-max_lines )
            iv_title  = |{ 'Set Max Number of Rows'(008) }|
        ).

        DATA(lv_max_rows_number) = CONV sy-tabix( lv_max_rows ).
        IF lv_max_rows_number = ms_technical_info-max_lines.
          RETURN.
        ENDIF.

*...... Update max row count and trigger reselection
        ms_technical_info-max_lines = lv_max_rows_number.
        zcl_uitb_screen_util=>set_function_code( zif_dbbr_c_selection_functions=>refresh ).

      WHEN zif_dbbr_c_selection_functions=>show_sql_of_select.
        CLEAR cv_function.

        IF mr_select_program IS BOUND.
          DATA(lv_sql) = mr_select_program->get_select_sql( ).
          IF lv_sql IS NOT INITIAL.
            zcl_uitb_abap_code_viewer=>show_code(
                iv_title  = 'SQL for Current Select'
                iv_code   = lv_sql
                iv_width  = 900
                iv_height = 500
                iv_theme  = ms_technical_info-code_viewer_theme
            ).
          ENDIF.
        ENDIF.

      WHEN zif_dbbr_c_selection_functions=>show_users_settings.
        CLEAR cv_function.

        zcl_dbbr_app_starter=>show_user_settings(
          EXPORTING if_disable_save = abap_true
                    iv_start_dynnr  = zif_dbbr_screen_ids=>c_user_settings-output_tab
                    iv_start_tab    = zcl_dbbr_user_settings_sc=>c_tab_ids-output_tab
          CHANGING  cs_settings     = ms_technical_info-settings
        ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
