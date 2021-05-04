"! <p class="shorttext synchronized" lang="en">Abstract ALV Filter for Output Grid</p>
CLASS zcl_dbbr_output_alv_util DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mf_use_live_filter TYPE abap_bool READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Hides empty columns</p>
    METHODS hide_empty_columns
      EXPORTING
        ef_no_empty_cols TYPE abap_bool
        ev_hidden_cols   TYPE i
      CHANGING
        ct_fieldcat      TYPE lvc_t_fcat.

    "! <p class="shorttext synchronized" lang="en">Builds selection criteria from db filters</p>
    METHODS build_selection_criteria
      EXPORTING
        !et_criteria       TYPE zdbbr_selfield_itab
        !et_criteria_multi TYPE zdbbr_selfield_itab .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !if_use_live_filter    TYPE abap_bool
        !ir_alv_grid           TYPE REF TO zcl_dbbr_output_grid
        !ir_t_data             TYPE REF TO data
        !if_aggregation_active TYPE abap_bool OPTIONAL
        ir_t_fieldcat          TYPE REF TO lvc_t_fcat
        !ir_tabfields          TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Delete filters from selected columns</p>
    METHODS del_filter_from_selected_cols
      RETURNING
        VALUE(rf_filter_removed) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Group data by selected columns</p>
    METHODS execute_column_grouping .
    "! <p class="shorttext synchronized" lang="en">Execute a quick filter for the selected cells</p>
    METHODS execute_quick_filter
      IMPORTING
        !if_exclusion_mode TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Retrieve the count of filtered entries</p>
    METHODS get_filtered_count
      RETURNING
        VALUE(result) TYPE sy-tabix .
    "! <p class="shorttext synchronized" lang="en">Retrieve all active filters</p>
    METHODS get_filters
      RETURNING
        VALUE(result) TYPE lvc_t_filt .
    "! <p class="shorttext synchronized" lang="en">Get Filter from Selection Criteria</p>
    METHODS get_filter_from_criteria
      IMPORTING
        !it_criteria       TYPE zdbbr_selfield_itab
        !it_criteria_multi TYPE zdbbr_selfield_itab
      RETURNING
        VALUE(rt_filter)   TYPE lvc_t_filt .
    "! <p class="shorttext synchronized" lang="en">Have the DB Filters changed?</p>
    METHODS have_db_filters_changed
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Have the UI Filters changed?</p>
    METHODS have_ui_filters_changed
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Checks if column grouping is active</p>
    METHODS is_column_grouping_active
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Remove Grouping from Columns</p>
    METHODS remove_column_grouping .
    "! <p class="shorttext synchronized" lang="en">Update the filters from ALV Grid</p>
    METHODS update_filters .
    "! <p class="shorttext synchronized" lang="en">Sets the db filter</p>
    METHODS set_db_filter
      IMPORTING
        !it_filter TYPE lvc_t_filt .
    "! <p class="shorttext synchronized" lang="en">Updates the table reference</p>
    METHODS update_result_table_ref
      IMPORTING
        ir_t_data TYPE REF TO data.
  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">ALV Output</p>
    DATA mo_alv TYPE REF TO zcl_dbbr_output_grid .
    DATA:
      BEGIN OF ms_new_filters,
        ui_filter TYPE lvc_t_filt,
        db_filter TYPE lvc_t_filt,
      END OF ms_new_filters .
    DATA mr_t_data TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">ALV control: Table of filter conditions</p>
    DATA mt_ui_filter TYPE lvc_t_filt .
    "! <p class="shorttext synchronized" lang="en">ALV control: Table of filter conditions</p>
    DATA mt_db_filter TYPE lvc_t_filt .
    DATA mf_aggregation_active TYPE abap_bool .
    DATA mf_col_grouping_active TYPE abap_bool .
    DATA mr_t_fieldcat TYPE REF TO lvc_t_fcat.
    "! <p class="shorttext synchronized" lang="en">Wrapper for list of table fields</p>
    DATA mo_tabfields TYPE REF TO zcl_dbbr_tabfield_list .

    "! <p class="shorttext synchronized" lang="en">Check fi the db filters changed</p>
    METHODS check_db_filter_changed .
    "! <p class="shorttext synchronized" lang="en">Check if the given filter tables differ</p>
    METHODS check_filter_changed
      CHANGING
        !ct_filter_old           TYPE lvc_t_filt
        !ct_filter_new           TYPE lvc_t_filt
      RETURNING
        VALUE(rf_filter_changed) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Check if the ui filters changed</p>
    METHODS check_ui_filter_changed .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_compare_field,
        fieldname  TYPE fieldname,
        is_numeric TYPE boolean,
      END OF t_compare_field .
    TYPES:
      tt_compare_field TYPE STANDARD TABLE OF t_compare_field .
    CONSTANTS: c_max_filter_length TYPE i VALUE 40,
               c_string_inttype    TYPE string VALUE 'Cg' ##NO_TEXT.

    DATA mf_db_filters_changed TYPE abap_bool .
    DATA mf_ui_filters_changed TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Check if two rows differ in their values</p>
    METHODS rows_differ
      IMPORTING
        is_left_row           TYPE any
        it_compare_by         TYPE tt_compare_field
        is_right_row          TYPE any
      RETURNING
        VALUE(rf_rows_differ) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Handles String/LCHR columns</p>
    "!
    "! @parameter cs_filter | <p class="shorttext synchronized" lang="en">The current ALV Filter value</p>
    METHODS handle_lob_filter
      CHANGING
        cs_filter TYPE lvc_s_filt.
ENDCLASS.



CLASS zcl_dbbr_output_alv_util IMPLEMENTATION.


  METHOD build_selection_criteria.

*.. Check if a filter update is needed
    CHECK mf_db_filters_changed = abap_true.

    CLEAR: et_criteria,
           et_criteria_multi.

    mt_db_filter = ms_new_filters-db_filter.

*.. Transform filter values to selection criteria
    LOOP AT mt_db_filter ASSIGNING FIELD-SYMBOL(<ls_filter>)
       GROUP BY ( fieldname = <ls_filter>-fieldname )
       ASSIGNING FIELD-SYMBOL(<ls_filter_group>).

      TRY.
          DATA(lr_fieldname) = mo_tabfields->get_field_ref_by_alv_name( <ls_filter_group>-fieldname ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      DATA(lf_first_index) = abap_true.

      LOOP AT GROUP <ls_filter_group> ASSIGNING FIELD-SYMBOL(<ls_filter_group_entry>).
        IF lf_first_index = abap_true.
          et_criteria = VALUE #( BASE et_criteria
            (  tabname_alias = lr_fieldname->tabname_alias
               fieldname     = lr_fieldname->fieldname
               low           = <ls_filter_group_entry>-low
               high          = <ls_filter_group_entry>-high
               option        = <ls_filter_group_entry>-option
               sign          = <ls_filter_group_entry>-sign )
          ).
          CLEAR lf_first_index.
        ELSE.
          et_criteria_multi = VALUE #( BASE et_criteria_multi
            (  tabname_alias = lr_fieldname->tabname_alias
               fieldname     = lr_fieldname->fieldname
               low           = <ls_filter_group_entry>-low
               high          = <ls_filter_group_entry>-high
               option        = <ls_filter_group_entry>-option
               sign          = <ls_filter_group_entry>-sign )
          ).
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_db_filter_changed.
    mf_db_filters_changed = check_filter_changed(
      CHANGING ct_filter_new = ms_new_filters-db_filter
               ct_filter_old = mt_db_filter
    ).
  ENDMETHOD.


  METHOD check_filter_changed.
    rf_filter_changed = abap_false.

    CHECK NOT ( ct_filter_new IS INITIAL AND
                ct_filter_old IS INITIAL ).

    SORT ct_filter_old BY fieldname low high sign option.
    SORT ct_filter_new BY fieldname low high sign option.

    IF lines( ct_filter_new ) = lines( ct_filter_old ).

      DO lines( ct_filter_new ) TIMES.
        DATA(lr_filter_old) = REF #( ct_filter_old[ sy-index ] ).
        DATA(lr_filter_new) = REF #( ct_filter_new[ sy-index ] ).

        IF lr_filter_old->fieldname <> lr_filter_new->fieldname OR
           lr_filter_old->low <> lr_filter_new->low OR
           lr_filter_old->high <> lr_filter_new->high OR
           lr_filter_old->option <> lr_filter_new->option OR
           lr_filter_old->sign <> lr_filter_new->sign.

          rf_filter_changed = abap_true.
          EXIT.
        ENDIF.
      ENDDO.

    ELSE.
      rf_filter_changed = abap_true.
    ENDIF.

    ct_filter_old = ct_filter_new.
  ENDMETHOD.


  METHOD check_ui_filter_changed.
    mf_ui_filters_changed = check_filter_changed(
      CHANGING ct_filter_new = ms_new_filters-ui_filter
               ct_filter_old = mt_ui_filter
    ).
  ENDMETHOD.


  METHOD constructor.
    mf_use_live_filter = if_use_live_filter.
    mr_t_data = ir_t_data.
    mr_t_fieldcat = ir_t_fieldcat.
    mo_tabfields = ir_tabfields.
    mo_alv = ir_alv_grid.
    mf_aggregation_active = if_aggregation_active.
  ENDMETHOD.


  METHOD del_filter_from_selected_cols.
    DATA: lt_fields_range TYPE RANGE OF fieldname.

    mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(lt_selected_cols) ).
    CHECK lt_selected_cols IS NOT INITIAL.

    lt_fields_range = VALUE #( FOR col IN lt_selected_cols ( sign = 'I' option = 'EQ' low = col-fieldname ) ).

    mo_alv->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    DELETE lt_filter WHERE fieldname IN lt_fields_range.

    rf_filter_removed = xsdbool( sy-subrc = 0 ).
    CHECK rf_filter_removed = abap_true.

    mo_alv->set_filter_criteria( lt_filter ).
  ENDMETHOD.


  METHOD execute_column_grouping.
    FIELD-SYMBOLS: <lt_table>              TYPE STANDARD TABLE,
                   <lt_table_temp>         TYPE STANDARD TABLE,
                   <lv_field_value_unique> TYPE any,
                   <lv_field_value>        TYPE any,
                   <ls_unique_row>         TYPE any.

    DATA: lt_sort_key          TYPE abap_sortorder_tab,
          lr_t_temp            TYPE REF TO data,
          lv_group_count       TYPE sy-tabix,
          lt_alv_sort_criteria TYPE lvc_t_sort,
          lt_field_compare     TYPE tt_compare_field.

    mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(lt_columns) ).

    IF lt_columns IS INITIAL.
      MESSAGE s062(zdbbr_info).
      RETURN.
    ENDIF.

    DATA(lt_col_selopt) = VALUE zif_sat_ty_global=>ty_t_selopt(
      FOR col IN lt_columns
      ( sign = 'I'
        option = 'EQ'
        low = col-fieldname )
    ).

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF <ls_field>-fieldname NOT IN lt_col_selopt.
        <ls_field>-no_out = abap_true.
      ELSE. " this is a sorting column
        DATA(lf_is_numeric) = zcl_dbbr_ddic_util=>is_type_numeric( <ls_field>-inttype ).
*...... Reset numeric type to non numeric if domain is timestamp
        IF lf_is_numeric = abap_true AND
           ( <ls_field>-domname = zif_dbbr_c_global=>c_domain_names-timestamp OR
             <ls_field>-domname = zif_dbbr_c_global=>c_domain_names-timestamp_long ).
          lf_is_numeric = abap_false.
        ENDIF.

        IF lf_is_numeric = abap_false.
          lt_sort_key = VALUE #( BASE lt_sort_key ( name = |{ <ls_field>-fieldname }| ) ).

*........ fill alv sort table
          lt_alv_sort_criteria = VALUE #( BASE lt_alv_sort_criteria
            ( fieldname  = <ls_field>-fieldname
              up         = abap_true )
          ).
          <ls_field>-emphasize = zif_dbbr_c_global=>c_alv_colors-light_green.
        ENDIF.
        lt_field_compare = VALUE #( BASE lt_field_compare
          ( fieldname  = <ls_field>-fieldname
            is_numeric = lf_is_numeric )
        ).
      ENDIF.
    ENDLOOP.

    ASSIGN mr_t_data->* TO <lt_table>.
    CREATE DATA lr_t_temp LIKE <lt_table>.
    ASSIGN lr_t_temp->* TO <lt_table_temp>.

    CLEAR <lt_table_temp>.

*.. perform grouping operation for selected columns
    SORT <lt_table> BY (lt_sort_key).

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_row>).

      IF <ls_unique_row> IS NOT ASSIGNED OR
         rows_differ( is_left_row   = <ls_row>
                      is_right_row  = <ls_unique_row>
                      it_compare_by = lt_field_compare ).

*...... complete the count of previous group
        IF <ls_unique_row> IS ASSIGNED.
          ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>line_index OF STRUCTURE <ls_unique_row> TO FIELD-SYMBOL(<lv_group_count>).
          <lv_group_count> = lv_group_count.
        ENDIF.

        APPEND INITIAL LINE TO <lt_table_temp> ASSIGNING <ls_unique_row>.
        LOOP AT lt_field_compare ASSIGNING FIELD-SYMBOL(<ls_group_field>).
          ASSIGN COMPONENT <ls_group_field>-fieldname OF STRUCTURE <ls_unique_row> TO <lv_field_value_unique>.
          ASSIGN COMPONENT <ls_group_field>-fieldname OF STRUCTURE <ls_row> TO <lv_field_value>.

          <lv_field_value_unique> = <lv_field_value>.
        ENDLOOP.

*...... reset count of found group to 1
        lv_group_count = 1.
      ELSE. " aggregate entry
        ADD 1 TO lv_group_count.

*...... aggregate fields with numeric values
        LOOP AT lt_field_compare ASSIGNING <ls_group_field> WHERE is_numeric = abap_true.
          ASSIGN COMPONENT <ls_group_field>-fieldname OF STRUCTURE <ls_unique_row> TO <lv_field_value_unique>.
          ASSIGN COMPONENT <ls_group_field>-fieldname OF STRUCTURE <ls_row> TO <lv_field_value>.

          TRY.
              ADD <lv_field_value> TO <lv_field_value_unique>.
            CATCH cx_sy_arithmetic_overflow.
              MESSAGE s060(zdbbr_exception) DISPLAY LIKE 'E'.
              EXIT.
          ENDTRY.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

*.. update the group count of the last entry
    IF <ls_unique_row> IS ASSIGNED.
      ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>line_index OF STRUCTURE <ls_unique_row> TO <lv_group_count>.
      <lv_group_count> = lv_group_count.
    ENDIF.

    <lt_table> = <lt_table_temp>.

*.. show line_index color to display the grouping count
    ASSIGN lt_fieldcat[ fieldname = zif_dbbr_c_special_out_columns=>line_index ] TO FIELD-SYMBOL(<ls_line_index_col>).

    <ls_line_index_col>-no_out = abap_false.
    <ls_line_index_col>-tech = abap_false.


    mo_alv->set_sort_criteria( lt_alv_sort_criteria ).
    mo_alv->set_filter_criteria( VALUE #( ) ).
    mo_alv->set_frontend_fieldcatalog( lt_fieldcat ).
    mo_alv->refresh_table_display( ).
    mo_alv->optimize_columns( ).

    mf_col_grouping_active = abap_true.

    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
  ENDMETHOD.


  METHOD execute_quick_filter.
    TYPES: BEGIN OF lty_selected_cell ,
             index     TYPE sy-tabix,
             fieldname TYPE lvc_fname,
           END OF lty_selected_cell.

    TYPES: ltt_selected_cells TYPE SORTED TABLE OF lty_selected_cell WITH NON-UNIQUE KEY index fieldname.

    DATA: lt_selected_cells_internal TYPE ltt_selected_cells.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

*.. get filter criteria for evaluating the fields, that need to be filled for
*.. quick filter
    mo_alv->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
    mo_alv->get_selected_cells( IMPORTING et_cell = DATA(lt_cells) ).

    " convert selected cells into internal structure
    lt_selected_cells_internal = VALUE #(
      FOR cell IN lt_cells
      ( index     = cell-row_id-index
        fieldname = cell-col_id-fieldname )
    ).

    ASSIGN mr_t_data->* TO <lt_table>.

*.. fill filter structure
    LOOP AT lt_selected_cells_internal ASSIGNING FIELD-SYMBOL(<ls_cell>).
      AT NEW index.
        ASSIGN <lt_table>[ <ls_cell>-index ] TO FIELD-SYMBOL(<ls_line>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDAT.

*.... get information from field catalog
      DATA(lr_fieldcat_entry) = REF #( lt_fieldcat[ fieldname = <ls_cell>-fieldname ] ).

*.... get value for fieldname and index in current data table
      ASSIGN COMPONENT lr_fieldcat_entry->fieldname OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_value>).

      APPEND VALUE lvc_s_filt(
          fieldname = <ls_cell>-fieldname
          seltext   = lr_fieldcat_entry->coltext
          lowercase = lr_fieldcat_entry->lowercase
          inttype   = lr_fieldcat_entry->inttype
          datatype  = lr_fieldcat_entry->datatype
          no_sign   = abap_true
          ref_field = lr_fieldcat_entry->ref_field
          ref_table = lr_fieldcat_entry->ref_table
          low       = <lv_value>
          sign      = COND #( WHEN if_exclusion_mode = abap_true THEN 'E' ELSE 'I' )
          option    = zif_sat_c_options=>equals
      ) TO lt_filter ASSIGNING FIELD-SYMBOL(<ls_new_filter>).
      <ls_new_filter>-low = condense( <ls_new_filter>-low ).
      IF lr_fieldcat_entry->inttype CA c_string_inttype.
        handle_lob_filter( CHANGING cs_filter = <ls_new_filter> ).
      ENDIF.
    ENDLOOP.

    SORT lt_filter BY fieldname low sign option.
    DELETE ADJACENT DUPLICATES FROM lt_filter COMPARING fieldname low sign option.
    mo_alv->set_filter_criteria( lt_filter ).

  ENDMETHOD.


  METHOD get_filtered_count.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    IF mf_use_live_filter = abap_true.
      ASSIGN mr_t_data->* TO <lt_data>.
      LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE (`HIDE_FLAG = abap_true`).
        ADD 1 TO result.
      ENDLOOP.
    ENDIF.

    mo_alv->get_filtered_entries( IMPORTING et_filtered_entries = DATA(lt_filtered) ).
    result = lines( lt_filtered ).
  ENDMETHOD.


  METHOD get_filters.
    IF mf_col_grouping_active = abap_false.
      result = mt_db_filter.
    ENDIF.

    result = VALUE #( BASE result ( LINES OF mt_ui_filter ) ).
  ENDMETHOD.


  METHOD get_filter_from_criteria.
    CHECK mf_use_live_filter = abap_true.

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
    IF lt_fieldcat IS INITIAL.
      lt_fieldcat = mr_t_fieldcat->*.
    ENDIF.

    LOOP AT it_criteria ASSIGNING FIELD-SYMBOL(<ls_selfield>)
       WHERE is_parameter       = abap_false AND
             ( low IS NOT INITIAL OR
               high IS NOT INITIAL OR
               option IS NOT INITIAL ).

      CHECK: <ls_selfield>-option <> zif_sat_c_options=>is_null,
             <ls_selfield>-option <> zif_sat_c_options=>is_not_null.

*..... get table field to get alias name
      TRY.
          DATA(lr_selection_field) = mo_tabfields->get_field_ref(
              iv_tabname_alias   = <ls_selfield>-tabname_alias
              iv_fieldname = <ls_selfield>-fieldname
          ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

*.... Check that field exists in fieldcatalog
      CHECK line_exists( lt_fieldcat[ fieldname = lr_selection_field->alv_fieldname ] ).

      DATA(lv_fieldname) = lr_selection_field->sql_fieldname.

      DATA(ls_filter) = VALUE lvc_s_filt(
          fieldname = lr_selection_field->alv_fieldname
          lowercase = lr_selection_field->is_lowercase
          inttype   = lr_selection_field->inttype
          no_sign   = abap_true
          ref_field = lr_selection_field->ref_field
          ref_table = lr_selection_field->ref_tab
          high      = <ls_selfield>-high
          low       = <ls_selfield>-low
          sign      = COND #(  WHEN <ls_selfield>-sign IS NOT INITIAL THEN <ls_selfield>-sign ELSE 'I' )
          option    = <ls_selfield>-option
      ).

      zcl_sat_where_clause_builder=>get_option(
        EXPORTING
          iv_sign        = ls_filter-sign
          iv_option      = ls_filter-option
          iv_high        = ls_filter-high
        CHANGING
          cv_option      = ls_filter-option
          cv_low         = ls_filter-low
      ).
      rt_filter = VALUE #( BASE rt_filter ( ls_filter ) ).

*.... Search for multiple input
      LOOP AT it_criteria_multi ASSIGNING FIELD-SYMBOL(<ls_selfield_multi>)
          WHERE fieldname = <ls_selfield>-fieldname AND
                tabname   = <ls_selfield>-tabname AND
                ( low  IS NOT INITIAL OR
                  high IS NOT INITIAL OR
                  option IS NOT INITIAL ).
        DATA(lv_low) = VALUE zsat_value( ).
        DATA(lv_high) = VALUE zsat_value( ).
        IF <ls_selfield_multi>-low = '#' AND <ls_selfield_multi>-option <> '#'.
          lv_low = space.
          lv_high = space.
        ELSE.
          lv_low = <ls_selfield_multi>-low.
          lv_high = <ls_selfield_multi>-high.
        ENDIF.

        ls_filter = VALUE #(
          fieldname = lr_selection_field->alv_fieldname
          lowercase = lr_selection_field->is_lowercase
          inttype   = lr_selection_field->inttype
          no_sign   = abap_true
          ref_field = lr_selection_field->ref_field
          ref_table = lr_selection_field->ref_tab
          high      = lv_high
          low       = lv_low
          sign      = COND #(  WHEN <ls_selfield_multi>-sign IS NOT INITIAL THEN <ls_selfield_multi>-sign ELSE 'I' )
          option    = <ls_selfield_multi>-option
        ).

        zcl_sat_where_clause_builder=>get_option(
          EXPORTING
            iv_sign        = ls_filter-sign
            iv_option      = ls_filter-option
            iv_high        = ls_filter-high
          CHANGING
            cv_option      = ls_filter-option
            cv_low         = ls_filter-low
        ).
        rt_filter = VALUE #( BASE rt_filter ( ls_filter ) ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD have_db_filters_changed.
    result = mf_db_filters_changed.
  ENDMETHOD.


  METHOD have_ui_filters_changed.
    result = mf_ui_filters_changed.
  ENDMETHOD.


  METHOD is_column_grouping_active.
    result = mf_col_grouping_active.
  ENDMETHOD.


  METHOD remove_column_grouping.
    mf_col_grouping_active = abap_false.
  ENDMETHOD.

  METHOD handle_lob_filter.
    CHECK cs_filter-low IS NOT INITIAL.

    DATA(lv_length) = numofchar( cs_filter-low ).
    IF lv_length > c_max_filter_length.
      lv_length = c_max_filter_length - 1.
      cs_filter-low = cs_filter-low(lv_length) && '*'.
      cs_filter-option = zif_sat_c_options=>contains_pattern.
    ENDIF.
  ENDMETHOD.

  METHOD rows_differ.
    LOOP AT it_compare_by ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE is_numeric = abap_false.
      ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE is_left_row TO FIELD-SYMBOL(<lv_left_value>).
      ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE is_right_row TO FIELD-SYMBOL(<lv_right_value>).

      IF <lv_left_value> <> <lv_right_value>.
        rf_rows_differ = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_db_filter.
    mt_db_filter = it_filter.
  ENDMETHOD.


  METHOD update_filters.
    FIELD-SYMBOLS: <lt_filters> TYPE lvc_t_filt.

    CLEAR: ms_new_filters,
           mf_db_filters_changed,
           mf_ui_filters_changed.

    mo_alv->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    SORT lt_filter BY fieldname low high option sign.
    DELETE ADJACENT DUPLICATES FROM lt_filter COMPARING fieldname low high option sign.

    IF mf_use_live_filter = abap_true AND mf_col_grouping_active = abap_false.
      LOOP AT lt_filter ASSIGNING FIELD-SYMBOL(<ls_filter>)
        GROUP BY ( fieldname = <ls_filter>-fieldname )
        ASSIGNING FIELD-SYMBOL(<ls_filter_group>).

        TRY.
            DATA(lr_field) = mo_tabfields->get_field_ref_by_alv_name( <ls_filter_group>-fieldname ).

            IF lr_field->is_text_field = abap_true OR
               lr_field->is_formula_field = abap_true OR
               lr_field->is_virtual_join_field = abap_true OR
               lr_field->is_virtual_element = abap_true OR
               lr_field->inttype = cl_abap_typedescr=>typekind_string OR
               lr_field->datatype = 'LCHR'.
              ASSIGN ms_new_filters-ui_filter TO <lt_filters>.
            ELSE.
              ASSIGN ms_new_filters-db_filter TO <lt_filters>.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            ASSIGN ms_new_filters-ui_filter TO <lt_filters>.
        ENDTRY.

        <lt_filters> = VALUE #( BASE <lt_filters> FOR filter IN GROUP <ls_filter_group> ( filter ) ).
      ENDLOOP.

      check_db_filter_changed( ).
    ELSE.
      ms_new_filters-ui_filter = lt_filter.
    ENDIF.

    check_ui_filter_changed( ).
  ENDMETHOD.

  METHOD update_result_table_ref.
    mr_t_data = ir_t_data.
  ENDMETHOD.

  METHOD hide_empty_columns.
    DATA: lt_cols_to_hide TYPE RANGE OF lvc_fname,
          lf_hide_col     TYPE abap_bool,
          lv_type         TYPE char1.

    FIELD-SYMBOLS: <lt_data> TYPE table.

    lt_cols_to_hide = VALUE #( FOR field IN ct_fieldcat WHERE ( no_out = abap_false AND tech = abap_false ) ( sign = 'I' option = 'EQ' low = field-fieldname ) ).

    ASSIGN mr_t_data->* TO <lt_data>.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      IF lt_cols_to_hide IS INITIAL.
        EXIT.
      ENDIF.

      LOOP AT lt_cols_to_hide ASSIGNING FIELD-SYMBOL(<ls_field>).
        CLEAR lf_hide_col.

        ASSIGN COMPONENT <ls_field>-low OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_cell>).
        DESCRIBE FIELD <lv_cell> TYPE lv_type.

        IF  lv_type = cl_abap_typedescr=>typekind_string OR
            lv_type = cl_abap_typedescr=>typekind_char OR
            lv_type = cl_abap_typedescr=>typekind_num.
          IF <lv_cell> CO '0.,' OR <lv_cell> IS INITIAL.
            lf_hide_col = abap_true.
          ENDIF.
        ELSE.
          IF <lv_cell> IS INITIAL.
            lf_hide_col = abap_true.
          ENDIF.
        ENDIF.

        IF lf_hide_col = abap_false.
          DELETE lt_cols_to_hide.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    IF lt_cols_to_hide IS INITIAL.
      ef_no_empty_cols = abap_true.
      RETURN.
    ENDIF.

    LOOP AT ct_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WHERE fieldname IN lt_cols_to_hide.
      <ls_fcat>-no_out = abap_true.
    ENDLOOP.

    ev_hidden_cols = lines( lt_cols_to_hide ).
  ENDMETHOD.

ENDCLASS.
