"! <p class="shorttext synchronized" lang="en">Table controller for data selection</p>
CLASS zcl_dbbr_selscreen_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_base_select_tc
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_page_scroller .
    INTERFACES zif_uitb_table .

    EVENTS aggregation_attr_changed .

    METHODS aggregation_is_active
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS clear .
    METHODS collapse_all_table_fields
      IMPORTING
        !ir_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS constructor
      IMPORTING
        !is_selscreen_settings TYPE zdbbr_selscreen_settings OPTIONAL .
    METHODS delete_aggregations .
    METHODS determine_loop_lines .
    METHODS display_lines .
    METHODS expand_all_hidden_fields .
    METHODS expand_all_table_fields
      IMPORTING
        !ir_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS expand_collapse_table_fields
      IMPORTING
        !ir_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS get_col_index
      IMPORTING
        !iv_name        TYPE screen-name OPTIONAL
        !iv_group1      TYPE screen-group1 OPTIONAL
        !iv_group2      TYPE screen-group2 OPTIONAL
        !iv_group3      TYPE screen-group3 OPTIONAL
        !iv_group4      TYPE screen-group4 OPTIONAL
      RETURNING
        VALUE(rv_index) TYPE sy-index .
    METHODS get_current_line
      IMPORTING
        !if_reset_index TYPE boolean DEFAULT abap_true
      RETURNING
        VALUE(rv_index) LIKE sy-tabix .
    METHODS get_data
      RETURNING
        VALUE(result) TYPE zdbbr_selfield_itab .
    METHODS get_loop_lines
      RETURNING
        VALUE(rv_looplines) TYPE sy-loopc .
    METHODS scroll_to_next_table
      IMPORTING
        !it_tablist TYPE zdbbr_entity_info_t .
    METHODS scroll_to_previous_table
      IMPORTING
        !it_tablist TYPE zdbbr_entity_info_t .
    METHODS search_value
      IMPORTING
        !iv_code                    LIKE sy-ucomm
      RETURNING
        VALUE(rf_search_successful) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Set LOWERCASE setting of current field</p>
    METHODS set_lowercase_setting .

  PROTECTED SECTION.


  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_fields,
        system_value_type TYPE string VALUE 'GS_SELFIELDS-SYSTEM_VALUE_TYPE' ##NO_TEXT,
        high_value        TYPE string VALUE 'GS_SELFIELDS-HIGH' ##NO_TEXT,
        low_value         TYPE string VALUE 'GS_SELFIELDS-LOW' ##NO_TEXT,
        group_by          TYPE string VALUE 'GS_SELFIELDS-GROUP_BY' ##NO_TEXT,
        aggregation       TYPE string VALUE 'GS_SELFIELDS-AGGREGATION' ##NO_TEXT,
        push              TYPE string VALUE 'PUSH' ##NO_TEXT,
        option            TYPE string VALUE 'OPTION' ##NO_TEXT,
      END OF c_fields .
    DATA mr_expand_button TYPE REF TO zdbbr_button .
    DATA mr_global_data TYPE REF TO zdbbr_global_data .
    DATA mr_option_icon TYPE REF TO zdbbr_button .
    DATA mr_push_icon TYPE REF TO zdbbr_button .
    DATA mr_selfields_multi TYPE REF TO zdbbr_selfield_itab .
    DATA mr_sel_init_table TYPE REF TO zdbbr_selopt_control_itab .
    DATA mr_subquery_icon TYPE REF TO zdbbr_button .
    DATA mr_table_data TYPE REF TO zdbbr_selfield_itab .
    DATA ms_selscreen_settings TYPE zdbbr_selscreen_settings .
    DATA mt_selection_fields_hidden TYPE zdbbr_selfield_itab .
    DATA mv_current_line LIKE sy-tabix .
    DATA mv_linecount LIKE sy-tabix .
    DATA mv_looplines TYPE sy-loopc .
    DATA mv_search TYPE string .
    DATA mr_tableview TYPE REF TO cxtab_control.

    METHODS check_aggregation_validity .
    METHODS check_entered_system_value .
    "! <p class="shorttext synchronized" lang="en">Delete values of current line</p>
    "!
    "! @parameter cs_selfields | <p class="shorttext synchronized" lang="en"></p>
    METHODS delete_line_values
      CHANGING
        !cs_selfields TYPE zdbbr_selfield .
    "! <p class="shorttext synchronized" lang="en">Handle fields in advanced mode</p>
    "!
    METHODS handle_advanced_mode .
    "! <p class="shorttext synchronized" lang="en">Optimize columns</p>
    "!
    METHODS handle_column_optimization .
    "! <p class="shorttext synchronized" lang="en">Expand/Collapse columns</p>
    "!
    METHODS handle_expand_column .
    "! <p class="shorttext synchronized" lang="en">Special handling of a table header row</p>
    "!
    METHODS handle_table_header_row .
    "! <p class="shorttext synchronized" lang="en">Handle option to display tech fields first</p>
    "!
    METHODS handle_tech_first_setting .
    "! <p class="shorttext synchronized" lang="en">Scroll to to first field in the table</p>
    "!
    "! @parameter iv_tabname | <p class="shorttext synchronized" lang="en"></p>
    METHODS scroll_to_first_field_of_table
      IMPORTING
        !iv_tabname TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Update status for input field</p>
    "!
    METHODS update_input_field_status .
ENDCLASS.



CLASS zcl_dbbr_selscreen_table IMPLEMENTATION.


  METHOD aggregation_is_active.
    CLEAR result.

    LOOP AT mr_table_data->* ASSIGNING FIELD-SYMBOL(<ls_selfield>) WHERE group_by = abap_true OR
                                                                         aggregation <> space.
      result = abap_true.
      RETURN.
    ENDLOOP.

    " check hidden fields
    LOOP AT mt_selection_fields_hidden ASSIGNING <ls_selfield> WHERE group_by = abap_true
                                                                  OR aggregation <> space.
      result = abap_true.
      RETURN.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_aggregation_validity.

    IF mr_selfield_line->group_by = abap_true AND
       mr_selfield_line->aggregation <> space.

      MESSAGE e019(zdbbr_info).
    ENDIF.

    IF ( mr_selfield_line->aggregation = zif_dbbr_c_aggregation=>average OR
         mr_selfield_line->aggregation = zif_dbbr_c_aggregation=>sum )
       AND mr_selfield_line->is_numeric = abap_false.

      MESSAGE e020(zdbbr_info) WITH zcl_dbbr_dictionary_helper=>get_domain_fix_value_text( mr_selfield_line->aggregation ).
    ENDIF.

  ENDMETHOD.


  METHOD check_entered_system_value.

    DATA(lv_inttype) = zcl_dbbr_dictionary_helper=>get_dtel_inttype( mr_selfield_line->rollname ).
    IF NOT zcl_dbbr_dictionary_helper=>check_for_possible_systype( iv_inttype           = lv_inttype
                                                                   iv_system_value_type = mr_selfield_line->system_value_type ).
      MESSAGE e071(zdbbr_info).
    ELSE.
      zcl_dbbr_system_helper=>get_system_value( EXPORTING iv_system_value_type = mr_selfield_line->system_value_type
                                                IMPORTING ev_system_value      = mr_selfield_line->low ).
    ENDIF.

  ENDMETHOD.


  METHOD clear.
    CLEAR mr_table_data->*.
    CLEAR mt_selection_fields_hidden.
  ENDMETHOD.


  METHOD collapse_all_table_fields.
    LOOP AT CAST zdbbr_selfield_itab( mr_table_data )->* ASSIGNING FIELD-SYMBOL(<ls_table_header>) WHERE tree_collapsed = abap_false
                                                                                                    AND is_table_header = abap_true.

      " remove rows for table
      mt_selection_fields_hidden = VALUE #(
        BASE mt_selection_fields_hidden
        FOR visible_field IN mr_table_data->*
        WHERE ( is_table_header = abap_false AND
                tabname         = <ls_table_header>-tabname )
        ( visible_field )
      ).

      DELETE mr_table_data->* WHERE is_table_header = abap_false
                                AND tabname         = <ls_table_header>-tabname.
      <ls_table_header>-tree_collapsed = abap_true.
    ENDLOOP.

    zif_uitb_page_scroller~scroll_page_top( ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    " init global data references from cache
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_tableview = CAST cxtab_control( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_selfields_tc ) ).
    mr_selfield_line = CAST zdbbr_selfield( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_selfields ) ).
    mr_table_data = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields ) ).
    mr_push_icon = CAST zdbbr_button( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_push ) ).
    mr_option_icon = CAST zdbbr_button( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_option ) ).
    mr_global_data = CAST zdbbr_global_data( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).
    mr_sel_init_table = CAST zdbbr_selopt_control_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_sel_init ) ).
    mr_selfields_multi = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields_multi ) ).
    mr_subquery_icon = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_subquery ) ).
    mr_expand_button = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_bt_expand ) ).

    ms_selscreen_settings = is_selscreen_settings.
  ENDMETHOD.


  METHOD delete_aggregations.
    LOOP AT mr_table_data->* ASSIGNING FIELD-SYMBOL(<ls_selfield>).
      CLEAR: <ls_selfield>-group_by,
             <ls_selfield>-aggregation.
    ENDLOOP.
  ENDMETHOD.


  METHOD delete_line_values.
    CLEAR: cs_selfields-low,
           cs_selfields-high,
           cs_selfields-sign,
           cs_selfields-push,
           cs_selfields-option,
           cs_selfields-system_value_type.

    IF cs_selfields-is_parameter = abap_true.
      IF cs_selfields-is_range_param = abap_false.
        cs_selfields-option = zif_dbbr_c_options=>equals.
      ENDIF.
      TRY.
          DATA(lr_tabfield) = get_util( )->mo_data->mo_tabfield_list->get_field_ref(
            iv_tabname_alias       = cs_selfields-tabname
            iv_fieldname     = cs_selfields-fieldname
          ).
          cs_selfields-low = lr_tabfield->default_low.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

    cs_selfields-sign = zif_dbbr_global_consts=>gc_options-i.
  ENDMETHOD.


  METHOD determine_loop_lines.
    mv_looplines = sy-loopc.
  ENDMETHOD.


  METHOD display_lines.
*&---------------------------------------------------------------------*
*& Description: Performs conversion to external format before entries
*& are displayed
*&---------------------------------------------------------------------*
    IF mr_selfield_line->is_parameter = abap_true.
      zcl_dbbr_data_converter=>convert_values_to_disp_format(
        EXPORTING
          iv_rollname            = mr_selfield_line->rollname
          iv_type                = mr_selfield_line->inttype
          iv_length              = CONV #( mr_selfield_line->intlen )
          iv_decimals            = CONV #( mr_selfield_line->decimals )
        CHANGING
          cv_value1              = mr_selfield_line->low
      ).
    ELSE.
      zcl_dbbr_data_converter=>convert_selopt_to_disp_format(
        EXPORTING iv_tabname   = mr_selfield_line->tabname
                  iv_fieldname = mr_selfield_line->fieldname
        CHANGING  cv_value1    = mr_selfield_line->low
                  cv_value2    = mr_selfield_line->high
      ).
    ENDIF.
  ENDMETHOD.


  METHOD expand_all_hidden_fields.
    LOOP AT mr_table_data->* ASSIGNING FIELD-SYMBOL(<ls_line>) WHERE tree_collapsed = abap_true.
      DATA(lv_index) = sy-tabix.

      " 1st solution, just add the hidden fields back to the selection screen
      INSERT LINES OF VALUE zdbbr_selfield_itab(
        FOR hidden_field IN mt_selection_fields_hidden
        WHERE ( tabname = <ls_line>-tabname )
        ( hidden_field )
      ) INTO mr_table_data->* INDEX lv_index.
      DELETE mt_selection_fields_hidden WHERE tabname = <ls_line>-tabname.
    ENDLOOP.
  ENDMETHOD.


  METHOD expand_all_table_fields.
    LOOP AT CAST zdbbr_selfield_itab( mr_table_data )->* ASSIGNING FIELD-SYMBOL(<ls_table_header>) WHERE tree_collapsed = abap_true
                                                                                                    AND is_table_header = abap_true.
      DATA(lv_tabix) = sy-tabix + 1.

      INSERT LINES OF VALUE zdbbr_selfield_itab(
        FOR hidden_field IN mt_selection_fields_hidden
        WHERE ( tabname = <ls_table_header>-tabname )
        ( hidden_field )
      ) INTO mr_table_data->* INDEX lv_tabix.

      DELETE mt_selection_fields_hidden WHERE tabname = <ls_table_header>-tabname.

      <ls_table_header>-tree_collapsed = abap_false.
    ENDLOOP.

    IF sy-subrc = 0.
      zif_uitb_page_scroller~scroll_page_top( ).
    ENDIF.
  ENDMETHOD.


  METHOD expand_collapse_table_fields.
    DATA(lr_current_line_ref) = zif_uitb_table~get_current_line_ref( ).

    CHECK lr_current_line_ref IS BOUND.

    DATA(lv_index) = zif_uitb_table~get_current_line_index( ) + 1.
    ASSIGN CAST zdbbr_selfield( lr_current_line_ref )->* TO FIELD-SYMBOL(<ls_current_line>).

    IF <ls_current_line>-tree_collapsed = abap_true.
      <ls_current_line>-tree_collapsed = abap_false.
      " 1st solution, just add the hidden fields back to the selection screen
      INSERT LINES OF VALUE zdbbr_selfield_itab(
        FOR hidden_field IN mt_selection_fields_hidden
        WHERE ( tabname = <ls_current_line>-tabname )
        ( hidden_field )
      ) INTO mr_table_data->* INDEX lv_index.
      DELETE mt_selection_fields_hidden WHERE tabname = <ls_current_line>-tabname.
    ELSE.
      " remove rows for table
      mt_selection_fields_hidden = VALUE #(
        BASE mt_selection_fields_hidden
        FOR visible_field IN mr_table_data->*
        WHERE ( is_table_header = abap_false AND
                tabname         = <ls_current_line>-tabname )
        ( visible_field )
      ).

      DELETE mr_table_data->* WHERE is_table_header = abap_false
                                AND tabname         = <ls_current_line>-tabname.
      <ls_current_line>-tree_collapsed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_col_index.

    LOOP AT mr_tableview->cols ASSIGNING FIELD-SYMBOL(<ls_table_column>).
      IF iv_name IS NOT INITIAL AND <ls_table_column>-screen-name = iv_name.
        rv_index = <ls_table_column>-index.
        RETURN.
      ENDIF.
      IF iv_group1 IS NOT INITIAL AND <ls_table_column>-screen-group1 = iv_group1.
        rv_index = <ls_table_column>-index.
        RETURN.
      ENDIF.
      IF iv_group2 IS NOT INITIAL AND <ls_table_column>-screen-group2 = iv_group2.
        rv_index = <ls_table_column>-index.
        RETURN.
      ENDIF.
      IF iv_group3 IS NOT INITIAL AND <ls_table_column>-screen-group3 = iv_group3.
        rv_index = <ls_table_column>-index.
        RETURN.
      ENDIF.
      IF iv_group4 IS NOT INITIAL AND <ls_table_column>-screen-group4 = iv_group4.
        rv_index = <ls_table_column>-index.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_current_line.
*&---------------------------------------------------------------------*
*& Description: Returns the current line index of this table
*&---------------------------------------------------------------------*
    IF if_reset_index = abap_true.
      zif_uitb_table~determine_current_line( ).
    ENDIF.

    rv_index = mv_current_line.
  ENDMETHOD.


  METHOD get_data.
    result = mr_table_data->*.
    DELETE result WHERE is_table_header = abap_true.

    result = VALUE #( BASE result ( LINES OF mt_selection_fields_hidden ) ).
  ENDMETHOD.


  METHOD get_loop_lines.
    rv_looplines = mv_looplines.
  ENDMETHOD.


  METHOD handle_advanced_mode.

    IF mr_selfield_line->has_subquery = abap_true.
      DATA(lv_push_icon_name) = CONV iconname( 'ICON_CHANGE_TEXT' ).
      DATA(lv_icon_text) = CONV text15( 'Edit' ).
      LOOP AT SCREEN INTO DATA(ls_screen).
        IF ls_screen-name = c_fields-low_value OR
           ls_screen-name = c_fields-high_value OR
           ls_screen-name = c_fields-system_value_type OR
           ls_screen-name = c_fields-option OR
           ls_screen-name = c_fields-push.
          ls_screen-input = 0.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ELSE.
      lv_push_icon_name = 'ICON_CREATE_TEXT'.
      lv_icon_text = 'Select'.
    ENDIF.

    zcl_dbbr_icon_handler=>create_icon(
      EXPORTING
        iv_icon_name = lv_push_icon_name
        iv_text      = lv_icon_text
        iv_info      = |Query { lv_icon_text }|
      IMPORTING
        ev_info_text = DATA(lv_icon_info_text)
        ev_push      = mr_subquery_icon->*
    ).

    " set new indexes for columns
    LOOP AT mr_tableview->cols ASSIGNING FIELD-SYMBOL(<ls_table_column>).
      CASE <ls_table_column>-screen-group3.
        WHEN 'ADV'.
          <ls_table_column>-invisible = COND #( WHEN mr_global_data->advanced_mode = abap_true THEN 0 ELSE 1 ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_column_optimization.
    FIELD-SYMBOLS: <ls_table_column> TYPE scxtab_column.

    " try to condense the fields to make more columns visible
    LOOP AT mr_tableview->cols ASSIGNING <ls_table_column>.
      CASE <ls_table_column>-screen-name.
        WHEN 'GS_SELFIELDS-DESCRIPTION'.
          <ls_table_column>-vislength = 25.
        WHEN c_fields-low_value.
          <ls_table_column>-vislength = 25.
        WHEN c_fields-high_value.
          <ls_table_column>-vislength = 25.
        WHEN 'GS_SELFIELDS-MARK'.
          <ls_table_column>-vislength = 8.
        WHEN 'GS_SELFIELDS-FIELDNAME'.
          <ls_table_column>-vislength = 20.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD handle_expand_column.
  ENDMETHOD.


  METHOD handle_table_header_row.

    DATA: ls_screen TYPE screen.

    " mark join table headers
    IF mr_selfield_line->is_table_header = abap_true.
      " create correct icon for expansion/compression of table node
      IF get_util( )->mo_data->is_multi_table_mode( ).
        IF mr_selfield_line->tree_collapsed = abap_false.
          DATA(lv_info) = CONV iconquick( 'Collapse Fields' ).
          DATA(lv_icon) = CONV iconname( 'ICON_COLLAPSE' ).
        ELSE.
          lv_info = 'Expand Fields'.
          lv_icon = 'ICON_EXPAND'.
        ENDIF.

        zcl_dbbr_icon_handler=>create_icon(
          EXPORTING
            iv_icon_name = lv_icon
            iv_info      = lv_info
          IMPORTING
            ev_push      = mr_expand_button->*
        ).
      ENDIF.

      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-group1 = 'INP' OR
           ls_screen-group3 = 'ICN' OR
           ls_screen-name   = c_fields-option.
          ls_screen-active = 0.
        ENDIF.

        IF ls_screen-group2 = 'TXT'.
          ls_screen-intensified = 1.
        ENDIF.
        MODIFY SCREEN FROM ls_screen.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-group1 = 'EXP'.
          ls_screen-active = 0.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD handle_tech_first_setting.
    DATA: lv_temp_index TYPE sy-index.

    " get current indexes of tech- and normal fieldname columns
    DATA(lv_tech_fieldname_index) = get_col_index( iv_group4 = 'TEC' ).
    DATA(lv_fieldname_index) = get_col_index( iv_group4 = 'FLD' ).

    IF mr_global_data->tech_first = abap_true.
      IF lv_tech_fieldname_index > lv_fieldname_index.
        lv_temp_index = lv_tech_fieldname_index.
        lv_tech_fieldname_index = lv_fieldname_index.
        lv_fieldname_index = lv_temp_index.
      ENDIF.
    ELSE.
      IF lv_fieldname_index > lv_tech_fieldname_index.
        lv_temp_index = lv_fieldname_index.
        lv_fieldname_index = lv_tech_fieldname_index.
        lv_tech_fieldname_index = lv_temp_index.
      ENDIF.
    ENDIF.

    " set new indexes for columns
    LOOP AT mr_tableview->cols ASSIGNING FIELD-SYMBOL(<ls_table_column>).
      CASE <ls_table_column>-screen-group4.
        WHEN 'TEC'.
          <ls_table_column>-index = lv_tech_fieldname_index.
        WHEN 'FLD'.
          <ls_table_column>-index = lv_fieldname_index.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD scroll_to_first_field_of_table.
    DATA(lt_selfields) = mr_table_data->*.

    ASSIGN mr_table_data->*[ tabname = iv_tabname ] TO FIELD-SYMBOL(<ls_selfield>).
    IF sy-subrc = 0.
      mr_tableview->top_line = sy-tabix.
    ENDIF.
  ENDMETHOD.


  METHOD scroll_to_next_table.
    CHECK mr_tableview->top_line > 0.

    DATA(lv_index_of_current_table) = line_index( it_tablist[ tabname = mr_table_data->*[ mr_tableview->top_line ]-tabname ] ).

    IF lv_index_of_current_table <> 0 AND lv_index_of_current_table <> lines( it_tablist ).
      scroll_to_first_field_of_table( it_tablist[ lv_index_of_current_table + 1 ]-tabname ).
    ENDIF.
  ENDMETHOD.


  METHOD scroll_to_previous_table.
    CHECK mr_tableview->top_line > 0.

    DATA(lv_index_of_current_table) = line_index( it_tablist[ tabname = mr_table_data->*[ mr_tableview->top_line ]-tabname ] ).

    IF lv_index_of_current_table <> 0 AND lv_index_of_current_table <> 1.
      scroll_to_first_field_of_table( it_tablist[ lv_index_of_current_table - 1 ]-tabname ).
    ENDIF.
  ENDMETHOD.


  METHOD search_value.
*&---------------------------------------------------------------------*
*& Description: Search for a value, that the user enters
*&---------------------------------------------------------------------*
    DATA: lv_rcode(1),
          lt_fields TYPE TABLE OF sval
          .

    IF iv_code = 'SEARCH'.
      lt_fields = VALUE #( ( tabname = 'SE16N_SELFIELDS' fieldname = 'FIELDNAME' ) ).

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          popup_title = 'Search'
        IMPORTING
          returncode  = lv_rcode
        TABLES
          fields      = lt_fields
        EXCEPTIONS
          OTHERS      = 1.
      CHECK: sy-subrc = 0.
      CHECK: lv_rcode = space.

      mv_search = to_upper( lt_fields[ 1 ]-value ).
      DATA(lv_current_line) = 1.
    ELSE. " continue search with previous value
      lv_current_line = get_current_line( ) + 1.
    ENDIF.

    IF mv_search IS INITIAL.
      RETURN.
    ENDIF.

    " use table searcher for actual search
    DATA(lr_table_searcher) = NEW zcl_uitb_table_func_executor( ir_table = mr_table_data  ).
    DATA(lv_index) = lr_table_searcher->search(
      iv_search_value  = mv_search
      it_search_fields = VALUE #(
        ( |FIELDNAME|   )
        ( |SCRTEXT_M|   )
        ( |SCRTEXT_L|   )
        ( |DESCRIPTION| )
      )
      iv_start_index   = lv_current_line
    ).

    IF lv_index > 0.
      mr_tableview->top_line = lv_index.
      rf_search_successful = abap_true.
    ELSE.
      MESSAGE mv_search && ` was not found` TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD set_lowercase_setting.
    DATA(lr_s_current_line) = CAST zdbbr_selfield( zif_uitb_table~get_current_line_ref( ) ).
    CHECK lr_s_current_line IS BOUND.

    IF lr_s_current_line->lowercase = abap_true .
      lr_s_current_line->lowercase = abap_false.
    ELSE.
      lr_s_current_line->lowercase = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD update_input_field_status.

*.. depending on the select option, not all fields are inputable
    DATA(ls_sel_init) = VALUE #( mr_sel_init_table->*[ option = mr_selfield_line->option ] OPTIONAL ).
    IF ls_sel_init IS NOT INITIAL.
*.... Deactivate all field input
      IF ls_sel_init-no_input = abap_true.
        LOOP AT SCREEN.
          IF screen-name = c_fields-low_value OR
             screen-name = c_fields-high_value OR
             screen-name = c_fields-push OR
             screen-name = c_fields-system_value_type OR
             screen-name = c_fields-group_by OR
             screen-name = c_fields-aggregation.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        IF ls_sel_init-high <> abap_true.
          LOOP AT SCREEN.
            IF screen-name = c_fields-high_value.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF ls_sel_init-low <> abap_true.
          LOOP AT SCREEN.
            IF screen-name = c_fields-low_value.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF ls_sel_init-no_multi = abap_true.
          LOOP AT SCREEN.
            IF screen-name = c_fields-push OR
               screen-name = c_fields-system_value_type.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF ls_sel_init-no_aggregation = abap_true.
          LOOP AT SCREEN.
            IF screen-name = c_fields-group_by OR
               screen-name = c_fields-aggregation.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    IF mr_selfield_line->is_parameter = abap_true.
      LOOP AT SCREEN.
        IF screen-name = c_fields-high_value OR
           screen-name = c_fields-push.
          IF mr_selfield_line->is_range_param = abap_false.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-name = c_fields-group_by OR
               screen-name = c_fields-aggregation.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-name = c_fields-low_value.
          IF mr_selfield_line->is_range_param = abap_false.
            screen-required = 2.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-name = c_fields-option.
          IF mr_selfield_line->is_range_param = abap_false.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_bottom.
    mr_tableview->top_line = mv_linecount - mv_looplines + 1.
    IF mr_tableview->top_line < 1.
      mr_tableview->top_line = 1.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_down.
    mr_tableview->top_line = mr_tableview->top_line + mv_looplines.
    IF mr_tableview->top_line >= mv_linecount.
      mr_tableview->top_line = mv_linecount - mv_looplines + 1.
      IF mr_tableview->top_line < 1.
        mr_tableview->top_line = 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_top.
    mr_tableview->top_line = 1.
  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_up.
    mr_tableview->top_line = mr_tableview->top_line - mv_looplines.
    IF mr_tableview->top_line < 1.
      mr_tableview->top_line = 1.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_table~add_line ##needed.
  ENDMETHOD.


  METHOD zif_uitb_table~delete_all.
    LOOP AT mr_table_data->* ASSIGNING FIELD-SYMBOL(<ls_selfield>).
      delete_line_values( CHANGING cs_selfields = <ls_selfield> ).
      " delete possible multi select data
      DELETE mr_selfields_multi->* WHERE tabname   = <ls_selfield>-tabname AND
                                         fieldname = <ls_selfield>-fieldname.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_table~delete_current_line.
    DATA(lv_current_line) = get_current_line( ).

    ASSIGN mr_table_data->*[ lv_current_line ] TO FIELD-SYMBOL(<ls_current_line>).

    IF <ls_current_line> IS ASSIGNED.
      delete_line_values( CHANGING cs_selfields = <ls_current_line> ).
      DELETE mr_selfields_multi->* WHERE tabname   = <ls_current_line>-tabname AND
                                         fieldname = <ls_current_line>-fieldname.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_table~determine_current_line.
    GET CURSOR LINE mv_current_line.
    mv_current_line = mv_current_line + mr_tableview->top_line - 1.
  ENDMETHOD.


  METHOD zif_uitb_table~determine_line_count.
    mv_linecount = lines( mr_table_data->* ).
    mr_tableview->lines = mv_linecount.
  ENDMETHOD.


  METHOD zif_uitb_table~get_current_line_index.
    rv_index = mv_current_line.
  ENDMETHOD.


  METHOD zif_uitb_table~get_current_line_ref.
    zif_uitb_table~determine_current_line( ).

    IF mv_current_line = 0.
      RETURN.
    ENDIF.

    rr_line = REF #( mr_table_data->*[ mv_current_line ] ).
  ENDMETHOD.


  METHOD zif_uitb_table~get_current_loop_line.
    rv_current_loop_line = mv_current_line - mr_tableview->top_line + 1.
  ENDMETHOD.

  METHOD zif_uitb_table~pbo.
    DATA: ls_screen TYPE screen.

    FIELD-SYMBOLS: <ls_table_column> TYPE scxtab_column.

    IF mr_selfield_line->inttype IS NOT INITIAL AND
       NOT zcl_dbbr_system_helper=>syst_value_allwd_for_inttyp( mr_selfield_line->inttype ).
      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-name = c_fields-system_value_type.
          ls_screen-active = 0.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF mr_selfield_line->push = abap_true.
      DATA(lv_push_icon_name) = CONV iconname( 'ICON_DISPLAY_MORE' ).
    ELSE.
      lv_push_icon_name = 'ICON_ENTER_MORE'.
    ENDIF.

    zcl_dbbr_icon_handler=>create_icon(
      EXPORTING
        iv_icon_name = lv_push_icon_name
      IMPORTING
        ev_info_text = DATA(lv_icon_text)
        ev_push      = mr_push_icon->*
    ).

    IF NOT mr_selfield_line->option IS INITIAL.
      DATA(lv_option_icon_name) = zcl_dbbr_icon_handler=>get_sign_icon_name(
          iv_sign      = mr_selfield_line->sign
          iv_option    = mr_selfield_line->option
      ).
    ELSE.
      lv_option_icon_name = 'ICON_SELECTION'.
    ENDIF.

    zcl_dbbr_icon_handler=>create_icon(
      EXPORTING
        iv_icon_name = lv_option_icon_name
      IMPORTING
        ev_push      = mr_option_icon->*
    ).

*.. update other field attributes

    IF mr_selfield_line->system_value_type <> space.
      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-group4 = 'CLD'.
          ls_screen-input = 0.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ENDIF.

    update_input_field_status( ).

    handle_table_header_row( ).

    " mark key fields as intensified
    IF mr_selfield_line->key = abap_true.
      LOOP AT SCREEN.
        IF screen-group2 = 'TXT'.
          screen-intensified = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.

    " .check for LCHR and do not allow input (more than 1333 character in datatype LCHR)
    IF mr_selfield_line->datatype = 'LCHR'.
      LOOP AT SCREEN.
        IF screen-group1 = 'INP'.
          screen-input = 0.
        ENDIF.
        IF screen-group3 = 'OPT' OR
           screen-group3 = 'ICN'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.

    " set field to outputlength of current table column field
    LOOP AT SCREEN.
      IF screen-name = c_fields-low_value OR
         screen-name = c_fields-high_value.
        screen-length = mr_selfield_line->outputlen.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

*.. IF table has NO LINES disable ALL INPUT
    IF mr_tableview->current_line > mv_linecount.
      LOOP AT SCREEN.
        screen-input = 0.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD zif_uitb_table~update_fields.
*&---------------------------------------------------------------------*
*& Description: Updates the internal table from the table control
*&---------------------------------------------------------------------*
    CHECK: iv_function_code <> 'DELETE_ALL',
           iv_function_code <> 'DELETE'.

    " get current line to get current field values
    DATA(ls_selfield_old) = mr_table_data->*[ mr_tableview->current_line ].

*.. Check if uppercase conversion should be performed
    DATA(lf_no_uppercase_conversion) = is_uppercase_conv_disabled( ).

    " handle calculated system values at first
    IF mr_selfield_line->system_value_type <> space.
      check_entered_system_value( ).
    ELSE. " normal field input

      DATA(lv_save_low) = mr_selfield_line->low.
      DATA(lv_save_high) = mr_selfield_line->high.

      precheck_selfield_value( EXPORTING is_selfield          = mr_selfield_line->*
                                         if_no_uppercase_conv = lf_no_uppercase_conversion
                               CHANGING  cv_value             = mr_selfield_line->low ).
      precheck_selfield_value( EXPORTING is_selfield          = mr_selfield_line->*
                                         if_no_uppercase_conv = lf_no_uppercase_conversion
                               CHANGING  cv_value             = mr_selfield_line->high ).

      TRY.
          conv_selfields_to_internal(
               if_no_uppercase_conversion = lf_no_uppercase_conversion ).

        CATCH zcx_dbbr_conversion_exc INTO DATA(lx_conv_error).
          lx_conv_error->zif_dbbr_exception_message~print( iv_msg_type = 'E' ).
          RETURN.
      ENDTRY.

      check_interval_validity(
        iv_save_low  = lv_save_low
        iv_save_high = lv_save_high ).

      check_aggregation_validity( ).

      """ check if state of `group by `/ `aggregation` changed since last input
      IF mr_selfield_line->group_by <> ls_selfield_old-group_by OR
         mr_selfield_line->aggregation <> ls_selfield_old-aggregation.
        RAISE EVENT aggregation_attr_changed.
      ENDIF.

      fill_selopt_sign( ).

    ENDIF.

    MODIFY mr_table_data->* FROM mr_selfield_line->* INDEX mr_tableview->current_line.
  ENDMETHOD.


  METHOD zif_uitb_table~update_screen_attributes.
    FIELD-SYMBOLS: <ls_table_column> TYPE scxtab_column.

*.. control visibility of table and table tool bar
    DATA(lf_no_table_data) = xsdbool( mr_table_data->* IS INITIAL ).
    mr_tableview->invisible = lf_no_table_data.
    DATA(lo_table_tb) = zcl_dbbr_toolbar_util=>get_selscreen_table_tb( ).
    IF lo_table_tb IS BOUND.
      lo_table_tb->set_visible( COND #( WHEN lf_no_table_data = abap_true THEN cl_gui_control=>visible_false ELSE cl_gui_control=>visible_true ) ).
    ENDIF.

    handle_column_optimization( ).

*.. if user wants the technical view, display more fields
    DATA(lv_techview_visibility) = COND #( WHEN mr_global_data->tech_view = abap_true THEN
                                             0
                                           ELSE
                                             1 ).
    LOOP AT mr_tableview->cols ASSIGNING <ls_table_column>.
      IF <ls_table_column>-screen-group3 = 'TEC'.
        <ls_table_column>-invisible = lv_techview_visibility.
      ENDIF.
    ENDLOOP.

*.. technical fieldname before or after more descriptive fieldname
    handle_tech_first_setting( ).

*.. disable aggrations if not asked for
    IF get_util( )->mo_data->mr_s_settings->disable_aggregations = abap_true.
      LOOP AT mr_tableview->cols ASSIGNING <ls_table_column>.
        IF <ls_table_column>-screen-group4 = 'GRP'.
          <ls_table_column>-invisible = 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

*.. disable intervals
    IF get_util( )->mo_data->mr_s_settings->disable_interval = abap_true.
      LOOP AT mr_tableview->cols ASSIGNING <ls_table_column>.
        IF <ls_table_column>-screen-group3 = 'IVA'.
          <ls_table_column>-invisible = 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    handle_advanced_mode( ).

    get_util( )->handle_table_pbo( ).

  ENDMETHOD.
ENDCLASS.
