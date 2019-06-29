"! <p class="shorttext synchronized" lang="en">Controller for data selection</p>
CLASS zcl_dbbr_selection_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    CONSTANTS:
      BEGIN OF c_function_texts,
        show_text_field_infos    TYPE gui_text VALUE 'Show Text Field Information',
        hide_lines               TYPE gui_text VALUE 'Hide selected Rows',
        keep_lines               TYPE gui_text VALUE 'Keep selected Rows',
        compare_lines            TYPE gui_text VALUE 'Compare selected Rows',
        dont_emphasize_columns   TYPE gui_text VALUE 'Remove color from selected Columns',
        dont_emphasize_rows      TYPE gui_text VALUE 'Remove color from selected Rows',
        dont_emphasize_cells     TYPE gui_text VALUE 'Remove color from selected Cells',
        quickfilter              TYPE gui_text VALUE 'Quickfilter for selected Cells',
        quickfilter_distinct     TYPE gui_text VALUE 'Quickfilter via distinct values',
        quickfilter_exclusion    TYPE gui_text VALUE 'Quickfilter for Selection (Exclude)',
        calc_sum_of_chosen_cells TYPE gui_text VALUE 'Calculate total for selected Cells',
        show_string_cell_content TYPE gui_text VALUE 'Show cell content in Editor',
        group_selected_columns   TYPE gui_text VALUE 'Group selected Columns',
        delete_filters_from_cols TYPE gui_text VALUE 'Remove Filter from Columns',
        emph_light_green         TYPE gui_text VALUE 'Light Green',
        emph_green               TYPE gui_text VALUE 'Green',
        emph_green_inverted      TYPE gui_text VALUE 'Green (inverted)',
        emph_light_orange        TYPE gui_text VALUE 'Light Orange',
        emph_orange              TYPE gui_text VALUE 'Orange',
        emph_light_yellow        TYPE gui_text VALUE 'Light Yellow',
        emph_yellow              TYPE gui_text VALUE 'Yellow',
        emph_light_blue          TYPE gui_text VALUE 'Light Blue',
        emph_blue                TYPE gui_text VALUE 'Blue',
        emph_light_grey_blue     TYPE gui_text VALUE 'Light Grey Blue',
        emph_grey_blue           TYPE gui_text VALUE 'Grey Blue',
        emph_light_red           TYPE gui_text VALUE 'Light Red',
        emph_red                 TYPE gui_text VALUE 'Red',
        emph_red_inverted        TYPE gui_text VALUE 'Red (Inverted)',
        hide_other_columns       TYPE gui_text VALUE 'Hide not selected columns',
      END OF c_function_texts .

    "! <p class="shorttext synchronized" lang="en">Global initializer for class</p>
    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">Creates the controller</p>
    CLASS-METHODS create_controller
      IMPORTING
        is_selection_data    TYPE zcl_dbbr_selection_util=>ty_s_selection_data
      RETURNING
        VALUE(rr_controller) TYPE REF TO zcl_dbbr_selection_controller .
    "! <p class="shorttext synchronized" lang="en">Create new controller instance from memory</p>
    CLASS-METHODS create_controller_from_data
      IMPORTING
        !is_controller_serialized TYPE zdbbr_sel_ctrl_serialized
        !ir_t_for_all_data        TYPE REF TO data
        !if_not_first_screen_call TYPE abap_bool
      RETURNING
        VALUE(rr_controller)      TYPE REF TO zcl_dbbr_selection_controller .
    "! <p class="shorttext synchronized" lang="en">Executes the selection</p>
    METHODS execute_selection
      IMPORTING
        !if_count_lines_only TYPE boolean OPTIONAL
      RETURNING
        VALUE(rf_no_data)    TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Returns the updated table fields</p>
    METHODS get_updated_tabfields
      RETURNING
        VALUE(rr_tabfields) TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Indicates if the layout was transferred</p>
    METHODS layout_was_transferred
      RETURNING
        VALUE(rf_transferred) TYPE sap_bool .
  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">Initializes the ALV Grid</p>
    METHODS init_grid_control .
  PRIVATE SECTION.

    ALIASES mf_first_call
      FOR zif_uitb_screen_controller~mf_first_call .
    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

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
    CLASS-DATA ss_hide_row_filter TYPE lvc_s_filt .
    DATA mt_column_groups TYPE lvc_t_sgrp .
    DATA:
      mt_group_tab_map TYPE HASHED TABLE OF ty_group_tab_map WITH UNIQUE KEY sp_group .
    DATA mf_layout_transferred TYPE sap_bool .
    CONSTANTS mc_container TYPE dynfnam VALUE 'GRID' ##NO_TEXT.
    DATA mf_first_cell_marked TYPE boolean .
    DATA mf_no_data TYPE abap_bool.
    DATA mo_alv_grid TYPE REF TO zcl_dbbr_output_grid .
    DATA mo_alv_header_doc TYPE REF TO cl_dd_document .
    DATA mo_header_dock TYPE REF TO cl_gui_docking_container .
    DATA mo_side_toolbar_dock TYPE REF TO cl_gui_docking_container .
    DATA mo_side_toolbar TYPE REF TO cl_gui_toolbar .
    DATA ms_alv_layout TYPE lvc_s_layo .
    DATA mv_max_lines_existing TYPE sy-tabix .
    DATA mf_default_alv_var_active TYPE abap_bool .
    DATA mo_util TYPE REF TO zcl_dbbr_selection_util .
    DATA mf_grouping_active TYPE abap_bool .
    DATA mf_has_parent TYPE abap_bool .
    DATA mo_splitter_container TYPE REF TO cl_gui_splitter_container .
    DATA mo_output_container TYPE REF TO cl_gui_container .
    DATA mf_selection_finished TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Indicates if the ALV header needs a refresh</p>
    METHODS alv_headers_needs_refresh
      IMPORTING
        !iv_function_code       TYPE sy-ucomm
      RETURNING
        VALUE(rf_needs_refresh) TYPE boolean .
    METHODS are_selected_cols_filtered
      IMPORTING
        !it_cols        TYPE lvc_t_col
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS build_accentuation_submenu
      RETURNING
        VALUE(rr_emph_menu) TYPE REF TO cl_ctmenu .
    METHODS calculate_sum_of_cells .
    METHODS call_jumpfield_transaction
      IMPORTING
        !iv_fieldname TYPE lvc_s_col-fieldname
        !iv_row_index TYPE lvc_s_row-index .
    METHODS compare_selected_lines .
    METHODS constructor
      IMPORTING
        is_selection_data TYPE zcl_dbbr_selection_util=>ty_s_selection_data.
    METHODS control_tech_view .
    "! <p class="shorttext synchronized" lang="en">Create the header for the ALV Grid</p>
    METHODS create_alv_header .
    "! <p class="shorttext synchronized" lang="en">Returns the current line count</p>
    METHODS current_line_count
      RETURNING
        VALUE(rv_current_line_count) TYPE sy-tabix .
    METHODS delete_coloring_of_columns .
    METHODS delete_coloring_of_rows .
    "! <p class="shorttext synchronized" lang="en">Deletes all hidden rows until next refresh</p>
    METHODS delete_hidden_lines .
    METHODS display_top_header .
    METHODS emphasize_negative_values .
    METHODS emphasize_selected
      IMPORTING
        !iv_chosen_color_code TYPE sy-ucomm OPTIONAL
        !if_clear_color       TYPE boolean OPTIONAL .
    METHODS emphasize_selected_cells
      IMPORTING
        !it_selected_cells    TYPE lvc_t_cell
        !iv_chosen_color_code TYPE sy-ucomm
        !if_clear_color       TYPE boolean .
    METHODS emphasize_selected_columns
      IMPORTING
        !it_selected_cols     TYPE lvc_t_col
        !iv_chosen_color_code TYPE sy-ucomm
        !if_clear_color       TYPE boolean .
    METHODS emphasize_selected_rows
      IMPORTING
        !it_selected_rows     TYPE lvc_t_row
        !iv_chosen_color_code TYPE sy-ucomm
        !if_clear_color       TYPE boolean .
    METHODS emphasize_sort_fields
      IMPORTING
        !it_sort     TYPE lvc_t_sort
      CHANGING
        !ct_fieldcat TYPE lvc_t_fcat .
    METHODS get_color_code_for_ucomm
      IMPORTING
        !iv_chosen_color_code TYPE sy-ucomm
      RETURNING
        VALUE(rs_color)       TYPE lvc_s_colo .
    METHODS group_by_selected_columns .
    "! <p class="shorttext synchronized" lang="en">Hides the selected rows from the table</p>
    METHODS hide_selected_rows .
    "! <p class="shorttext synchronized" lang="en">Hide not selected columns</p>
    METHODS keep_selected_columns .
    "! <p class="shorttext synchronized" lang="en">Discard all non selected rows</p>
    METHODS keep_selected_rows .
    METHODS on_after_user_command
          FOR EVENT after_user_command OF cl_gui_alv_grid
      IMPORTING
          !e_not_processed
          !e_saved
          !e_ucomm .
    METHODS on_before_user_command
          FOR EVENT before_user_command OF cl_gui_alv_grid
      IMPORTING
          !e_ucomm .
    METHODS on_context_menu_request
          FOR EVENT context_menu_request OF cl_gui_alv_grid
      IMPORTING
          !e_object .
    METHODS on_double_click
          FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
          !e_row
          !e_column
          !es_row_no .
    METHODS on_hotspot_click
          FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
          !e_column_id
          !e_row_id
          !es_row_no .
    METHODS on_no_data
          FOR EVENT no_data OF zcl_dbbr_selection_util
      IMPORTING
          ef_criteria_exist.
    METHODS on_selection_finish
          FOR EVENT selection_finished OF zcl_dbbr_selection_util
      IMPORTING
          !ef_first_select .
    METHODS on_toolbar_clicked
          FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
          !fcode .
    METHODS on_user_command
          FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
          !e_ucomm .
    "! <p class="shorttext synchronized" lang="en">Performs a quick filter</p>
    METHODS perform_quick_filter
      IMPORTING
        !if_exclude TYPE boolean OPTIONAL .
    METHODS perform_row_comparison
      IMPORTING
        !it_fieldcat             TYPE lvc_t_fcat
      RETURNING
        VALUE(rf_rows_identical) TYPE boolean .
    METHODS process_filter_change .
    "! <p class="shorttext synchronized" lang="en">Refreshes the data by performing a new select</p>
    METHODS refresh .
    METHODS remove_filt_from_selected_cols .
    METHODS remove_grouping .
    METHODS reset_alv_layout .
    METHODS rows_differ
      IMPORTING
        !is_left_row          TYPE any
        !it_compare_by        TYPE tt_compare_field
        !is_right_row         TYPE any
      RETURNING
        VALUE(rf_rows_differ) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Saves the current selection as Value help</p>
    METHODS save_as_f4 .
    METHODS scroll_to_column .
    "! <p class="shorttext synchronized" lang="en">Inserts all hidden rows back into the table area</p>
    METHODS show_hidden_rows
      IMPORTING
        !if_refresh TYPE boolean DEFAULT abap_true .

    "! <p class="shorttext synchronized" lang="en">Shows the detail view for a certain line</p>
    METHODS show_line_detail
      IMPORTING
        !is_line TYPE any .
    METHODS transfer_filter_values
      IMPORTING
        !if_leave_screen TYPE boolean DEFAULT abap_true .
    METHODS transfer_layout_info_to_selscr .
    "! <p class="shorttext synchronized" lang="en">Shows the cell content in a popup editor</p>
    "! This action makes only sense if the data type of the column is STRING
    METHODS show_string_cell_content.
    "! <p class="shorttext synchronized" lang="en">Hides all columns where no cell has any values</p>
    METHODS hide_cols_without_values.
ENDCLASS.



CLASS zcl_dbbr_selection_controller IMPLEMENTATION.


  METHOD alv_headers_needs_refresh.
    CASE iv_function_code.
      WHEN zif_dbbr_c_selection_functions=>clear_filter OR
           zif_dbbr_c_selection_functions=>define_variant OR
           zif_dbbr_c_selection_functions=>filter_lines OR
           zif_dbbr_c_selection_functions=>load_variant.
        rf_needs_refresh = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD are_selected_cols_filtered.
    DATA: lt_fields_range TYPE RANGE OF fieldname.

    r_result = abap_false.

    lt_fields_range = VALUE #( FOR col IN it_cols ( sign = 'I' option = 'EQ' low = col-fieldname ) ).

    mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).

    IF lt_filter IS NOT INITIAL.

      LOOP AT lt_filter ASSIGNING FIELD-SYMBOL(<ls_filter>) WHERE fieldname IN lt_fields_range.
        EXIT.
      ENDLOOP.

      r_result = xsdbool( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD build_accentuation_submenu.

    rr_emph_menu  = NEW cl_ctmenu( ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_light_green
        text  = c_function_texts-emph_light_green
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_green
        text  = c_function_texts-emph_green
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_green_inverted
        text  = c_function_texts-emph_green_inverted
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_light_orange
        text  = c_function_texts-emph_light_orange
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_orange
        text  = c_function_texts-emph_orange
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_light_yellow
        text  = c_function_texts-emph_light_yellow
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_yellow
        text  = c_function_texts-emph_yellow
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_light_blue
        text  = c_function_texts-emph_light_blue
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_blue
        text  = c_function_texts-emph_blue
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_light_grey_blue
        text  = c_function_texts-emph_light_grey_blue
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_light_red
        text  = c_function_texts-emph_light_red
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_red_inverted
        text  = c_function_texts-emph_red_inverted
    ).
    rr_emph_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emph_red
        text  = c_function_texts-emph_red
    ).

  ENDMETHOD.

  METHOD show_string_cell_content.
    FIELD-SYMBOLS: <lt_data> TYPE table.
    ASSIGN mo_util->mr_t_data->* TO <lt_data>.
    CHECK sy-subrc = 0.
    mo_alv_grid->get_selected_cells( IMPORTING et_cell = DATA(lt_cell) ).
    DATA(ls_cell) = lt_cell[ 1 ].

    ASSIGN <lt_data>[ ls_cell-row_id-index ] TO FIELD-SYMBOL(<ls_row>).
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT ls_cell-col_id-fieldname OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_value>).
    CHECK sy-subrc = 0.

    DATA(lo_string_editor) = NEW zcl_uitb_popup_editor(
      iv_text         = <lv_value>
      iv_editor_title = |Cell Content for { ls_cell-col_id-fieldname }|
      if_coding_font  = abap_true
    ).
    lo_string_editor->zif_uitb_view~show( ).
  ENDMETHOD.

  METHOD calculate_sum_of_cells.
    TYPES: BEGIN OF lty_cell,
             index  TYPE lvc_index,
             col_id TYPE lvc_fname,
             value  TYPE lvc_value,
           END OF lty_cell.

    DATA: lt_cells_internal TYPE STANDARD TABLE OF lty_cell,
          lv_sum            TYPE p LENGTH 16 DECIMALS 2.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.


    mo_alv_grid->get_selected_cells( IMPORTING et_cell = DATA(lt_cells) ).

    IF lt_cells IS INITIAL.
      RETURN.
    ENDIF.

    lt_cells_internal = VALUE #(
      FOR cell IN lt_cells
      ( index  = cell-row_id-index
        col_id = cell-col_id-fieldname
        value  = cell-value )
    ).

    CLEAR lt_cells.

    " get fieldcatalog to check that all selected cells are calculatable
    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    DATA(lt_cells_unique) = lt_cells_internal.
    SORT lt_cells_unique BY col_id.
    DELETE ADJACENT DUPLICATES FROM lt_cells_unique COMPARING col_id.

    " are only numeric cells selected
    LOOP AT lt_cells_unique ASSIGNING FIELD-SYMBOL(<ls_cell>).
      ASSIGN lt_fieldcat[ fieldname = <ls_cell>-col_id ] TO FIELD-SYMBOL(<ls_field_definition>).
      IF sy-subrc = 0.
        IF NOT zcl_dbbr_dictionary_helper=>is_type_numeric( <ls_field_definition>-inttype ).
          MESSAGE 'Selection contains non numerical Fields' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    " sum values
    LOOP AT lt_cells_internal ASSIGNING <ls_cell>.
      AT NEW index.
        ASSIGN <lt_table>[ <ls_cell>-index ] TO FIELD-SYMBOL(<ls_row>).
      ENDAT.


      ASSIGN COMPONENT <ls_cell>-col_id OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_value>).
      IF sy-subrc = 0.
        ADD <lv_value> TO lv_sum.
      ENDIF.
    ENDLOOP.

    MESSAGE |Total: { lv_sum NUMBER = USER ALIGN = RIGHT }| TYPE 'I'.
  ENDMETHOD.


  METHOD call_jumpfield_transaction.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                   <ls_line>  TYPE any.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    ASSIGN <lt_table>[ iv_row_index ] TO <ls_line>.
    " column is automatically a jump field
    " 1) get jumpfield definition
    DATA(ls_field) = mo_util->mt_fieldcat[ fieldname = iv_fieldname ].
    DATA(lr_tabfield) = mo_util->mo_tabfields->get_field_ref_by_alv_name( iv_fieldname ).

    LOOP AT mo_util->mt_jumpdest INTO DATA(ls_jumpfield) WHERE jump_source_field = lr_tabfield->fieldname
                                                  AND jump_source_table = lr_tabfield->tabname.
      " 2) check if conditions are met
      IF ls_jumpfield-crit_field IS NOT INITIAL.
        DATA(ls_crit_field) = mo_util->mo_tabfields->get_field(
            iv_tabname   = ls_jumpfield-crit_table
            iv_fieldname = ls_jumpfield-crit_field
        ).
        ASSIGN COMPONENT ls_crit_field-alv_fieldname OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_crit_val>).
        CASE ls_jumpfield-crit_operation.
          WHEN 'EQ'.
            IF NOT <lv_crit_val> EQ ls_jumpfield-crit_value.
              CONTINUE.
            ENDIF.
          WHEN 'CP'.
            IF NOT <lv_crit_val> CP ls_jumpfield-crit_value.
              CONTINUE.
            ENDIF.
          WHEN 'CA'.
            IF NOT <lv_crit_val> CA ls_jumpfield-crit_value.
              CONTINUE.
            ENDIF.
        ENDCASE.
      ENDIF.

      " set parameter values
      LOOP AT ls_jumpfield-parameters ASSIGNING FIELD-SYMBOL(<ls_param>) WHERE active = abap_true.
        IF <ls_param>-param_field IS NOT INITIAL.
          " read crit field
          TRY.
              DATA(lr_param_field) = mo_util->mo_tabfields->get_field_ref(
                  iv_tabname_alias = <ls_param>-param_table
                  iv_fieldname     = <ls_param>-param_field
              ).
            CATCH cx_sy_itab_line_not_found.
              MESSAGE |Parameter Field { <ls_param>-param_table }-{ <ls_param>-param_field } was not found. Jump was aborted!| TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
          ENDTRY.
          ASSIGN COMPONENT lr_param_field->alv_fieldname OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_param_val>).
          <ls_param>-param_value = <lv_param_val>.
        ENDIF.

        IF <ls_param>-obligatory = abap_true AND <ls_param>-param_value IS INITIAL.
          RETURN.
        ENDIF.
      ENDLOOP.

      DATA(lf_jump_field_found) = abap_true.
      EXIT.
    ENDLOOP.

    IF lf_jump_field_found = abap_false.
      RETURN.
    ENDIF.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |Transaction { ls_jumpfield-jump_target } is called| ).

    " 3) call transaction in new mode
    CASE ls_jumpfield-jump_target_type.
      WHEN zif_dbbr_global_consts=>gc_jump_call_types-normal.
        CALL FUNCTION 'ZDBBR_CALL_TRANSACTION' " STARTING NEW TASK 'NEWT'
*          DESTINATION 'NONE'
          EXPORTING
            is_jump_dest = ls_jumpfield.    " DBAnalyt.:.: UI-Daten eines Sprungfeldes
      WHEN zif_dbbr_global_consts=>gc_jump_call_types-start_new_mode.
        CALL FUNCTION 'ZDBBR_CALL_TRANSACTION' STARTING NEW TASK 'NEWT'
          DESTINATION 'NONE'
          EXPORTING
            is_jump_dest = ls_jumpfield.    " DBAnalyt.:.: UI-Daten eines Sprungfeldes
    ENDCASE.
  ENDMETHOD.


  METHOD class_constructor.
    ss_hide_row_filter = VALUE #(
       fieldname = zif_dbbr_c_special_out_columns=>hide_flag
       ref_field = zif_dbbr_c_special_out_columns=>hide_flag
       ref_table = 'ZDBBR_ALV_SPECIAL_CELLS'
       no_sign   = abap_true
       sign      = 'I'
       option    = 'EQ'
    ).
  ENDMETHOD.


  METHOD compare_selected_lines.
    FIELD-SYMBOLS: <lt_table>         TYPE STANDARD TABLE,
                   <lt_table_compare> TYPE STANDARD TABLE.

    mo_alv_grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows) ).

    IF lines( lt_index_rows ) < 2.
      MESSAGE s059(zdbbr_info).
      RETURN.
    ENDIF.

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    " get field count which need to be compared
    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE no_out = abap_false.
    ENDLOOP.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    ASSIGN mo_util->mr_t_temp_data->* TO <lt_table_compare>.

    " determine the selected rows
    CLEAR <lt_table_compare>.

    LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<ls_row_index>).
      APPEND <lt_table>[ <ls_row_index>-index ] TO <lt_table_compare>.
    ENDLOOP.

    perform_row_comparison(
        it_fieldcat = lt_fieldcat
    ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(lr_compare_alv)
          CHANGING
            t_table        = <lt_table_compare>
        ).

        lr_compare_alv->get_functions( )->set_default( ).
        lr_compare_alv->get_columns( )->set_color_column( CONV #( zif_dbbr_c_special_out_columns=>cell_col_row_color ) ).
        lr_compare_alv->get_display_settings( )->set_list_header( |DB Browser - Comparison of { lines( lt_index_rows ) } Entries| ).

        " transfer fieldcatalog definitions
        DATA(lt_columns) = lr_compare_alv->get_columns( )->get( ).

        LOOP AT lt_columns ASSIGNING FIELD-SYMBOL(<ls_column>).
          DATA(lr_column) = lr_compare_alv->get_columns( )->get_column( <ls_column>-columnname ).
          " get fieldcat definition
          DATA(ls_field) = VALUE #( lt_fieldcat[ fieldname = <ls_column>-columnname ] OPTIONAL ).
          IF ls_field IS INITIAL.
            CONTINUE.
          ENDIF.

          lr_column->set_short_text( space ).
          lr_column->set_medium_text( space ).
          lr_column->set_long_text( ls_field-seltext ).

          IF ls_field-no_out = abap_true OR ls_field-tech = abap_true.
            lr_column->set_visible( abap_false ).
            CONTINUE.
          ENDIF.

        ENDLOOP.

        lr_compare_alv->get_columns( )->set_optimize( ).
        lr_compare_alv->display( ).
      CATCH cx_salv_error.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    SET LANGUAGE 'EN'.

*... create util instance for handling selection stuff
    mo_util = zcl_dbbr_selection_util=>create_util( is_selection_data ).

    SET HANDLER:
      on_selection_finish FOR mo_util,
      on_no_data FOR mo_util.

    mf_first_call = abap_true.

  ENDMETHOD.


  METHOD control_tech_view.
    mo_util->ms_technical_info-tech_names = xsdbool( mo_util->ms_technical_info-tech_names = abap_false ).

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fcat) ).

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      CLEAR: <ls_fcat>-scrtext_s,
             <ls_fcat>-scrtext_m,
             <ls_fcat>-scrtext_l,
             <ls_fcat>-coltext,
             <ls_fcat>-reptext,
             <ls_fcat>-tooltip.
      TRY .
          DATA(lr_field) = mo_util->mo_tabfields_all->get_field_ref_by_alv_name( <ls_fcat>-fieldname ).
          mo_util->set_fieldcat_coltexts(
            EXPORTING ir_field    = lr_field
            CHANGING  cs_fieldcat = <ls_fcat>
          ).
        CATCH cx_sy_itab_line_not_found.
          IF <ls_fcat>-fieldname = zif_dbbr_c_special_out_columns=>line_index.
            mo_util->set_line_index_column_texts( CHANGING cs_field = <ls_fcat> ).
          ENDIF.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    mo_alv_grid->set_frontend_fieldcatalog( lt_fcat ).

    mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
    mo_alv_grid->optimize_columns( ).

  ENDMETHOD.


  METHOD create_alv_header.
    mo_alv_grid->set_gridtitle( i_gridtitle = mo_util->build_simple_alv_title( )  ).
  ENDMETHOD.


  METHOD create_controller.
    rr_controller = NEW zcl_dbbr_selection_controller( is_selection_data ).
  ENDMETHOD.


  METHOD create_controller_from_data.
*... first create table fields
    DATA(lr_tabfields) = zcl_dbbr_tabfield_list=>create_from_serialized( is_controller_serialized-tabfields_data ).
    DATA(lr_tabfields_all) = zcl_dbbr_tabfield_list=>create_from_serialized( is_controller_serialized-tabfields_all_data ).

    DATA(ls_tech_info) = is_controller_serialized-technical_info.
    IF ir_t_for_all_data IS BOUND.
      ls_tech_info-activate_alv_live_filter = abap_false.
    ENDIF.

    DATA(ls_selection_data) = CORRESPONDING zcl_dbbr_selection_util=>ty_s_selection_data(
       is_controller_serialized MAPPING association_target = navigation_info
                                        selfields_multi    = selection_fields_multi
                                        multi_or           = selection_fields_or
                                        nav_breadcrumbs    = navigation_breadcrumbs
    ).

    ls_selection_data-do_for_all_select = xsdbool( ir_t_for_all_data IS BOUND ).
    ls_selection_data-for_all_entries_data = ir_t_for_all_data.
    ls_selection_data-technical_infos = ls_tech_info.
    ls_selection_data-tabfields = lr_tabfields.
    ls_selection_data-tabfields_all = lr_tabfields_all.
    ls_selection_data-navigation_count = is_controller_serialized-navigation_count.

    rr_controller = NEW zcl_dbbr_selection_controller( ls_selection_data ).

*... manually reset first screen call if required
    IF if_not_first_screen_call = abap_true.
*      CLEAR: rr_controller->mf_first_call.
      rr_controller->mf_has_parent = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD current_line_count.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    IF mo_util->mr_t_data IS NOT INITIAL.
      ASSIGN mo_util->mr_t_data->* TO <lt_table>.
      rv_current_line_count = lines( <lt_table> ).
    ELSE.
      rv_current_line_count = mo_util->ms_control_info-number.
    ENDIF.
  ENDMETHOD.


  METHOD delete_coloring_of_columns.
    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WHERE key_sel = abap_false.
      " get field catalog row for selected column
      CLEAR <ls_fieldcat>-emphasize.
    ENDLOOP.

    mo_alv_grid->set_frontend_fieldcatalog( lt_fieldcat ).

    mo_alv_grid->refresh_table_display(
      is_stable      = VALUE #( row = abap_true col = abap_true )
      i_soft_refresh = abap_true
    ).
  ENDMETHOD.


  METHOD delete_coloring_of_rows.
    FIELD-SYMBOLS: <lt_table>       TYPE STANDARD TABLE.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_row>).
      ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>cell_col_row_color OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lt_colors>).
      IF sy-subrc = 0.
        CLEAR <lt_colors>.
      ENDIF.
    ENDLOOP.

    mo_alv_grid->refresh_table_display(
      is_stable      = VALUE #( row = abap_true col = abap_true )
      i_soft_refresh = abap_true
    ).
  ENDMETHOD.


  METHOD delete_hidden_lines.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2017/01/11
*&---------------------------------------------------------------------*
*& Description: Deletes the currently hidden rows from data table
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter_criteria) ).
    DELETE lt_filter_criteria WHERE fieldname = ss_hide_row_filter-fieldname.

    " if filter did not exist, no deletion and no refresh is needed
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    DELETE <lt_table> WHERE (`HIDE_FLAG EQ 'X'`).

    " update filter criteria
    mo_alv_grid->set_filter_criteria( lt_filter_criteria ).
  ENDMETHOD.


  METHOD display_top_header.
*& Description: Creates top of page header for ALV Grid with dynamic document
*&---------------------------------------------------------------------*
    IF mo_header_dock IS NOT INITIAL.
      mo_header_dock->free( ).
      cl_gui_cfw=>flush( ).
      CLEAR: mo_header_dock,
             mo_alv_header_doc.
      RETURN.
    ENDIF.

    IF mo_header_dock IS INITIAL.
      mo_header_dock = NEW cl_gui_docking_container(
          side  = cl_gui_docking_container=>dock_at_top
          lifetime = cl_gui_control=>lifetime_dynpro
          ratio = 15
      ).
    ENDIF.

    ">>> create dynamic document for header
    IF mo_alv_header_doc IS INITIAL.
      mo_alv_header_doc = NEW cl_dd_document( ).
    ENDIF.

    mo_alv_header_doc->initialize_document( background_color = cl_dd_area=>col_background_level2 ).

    mo_util->fill_header( CHANGING cr_dd_doc = mo_alv_header_doc ).

    mo_alv_header_doc->merge_document( ).

    mo_alv_header_doc->display_document( parent = mo_header_dock reuse_control = abap_true ).


  ENDMETHOD.


  METHOD emphasize_negative_values.
    DATA: lt_numeric_fields TYPE TABLE OF string.
    FIELD-SYMBOLS: <lt_table>  TYPE STANDARD TABLE,
                   <lt_colors> TYPE STANDARD TABLE.

    " first determine all numeric fields
    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_field>).
      " get dtel info
      IF zcl_dbbr_dictionary_helper=>is_type_numeric( <ls_field>-inttype ).
        APPEND <ls_field>-fieldname TO lt_numeric_fields.
      ENDIF.
    ENDLOOP.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_row>).
      ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>cell_col_row_color OF STRUCTURE <ls_row> TO <lt_colors>.
      DELETE <lt_colors> WHERE ('FNAME is not initial').

      LOOP AT lt_numeric_fields ASSIGNING FIELD-SYMBOL(<lv_numeric_field_name>).
        ASSIGN COMPONENT <lv_numeric_field_name> OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_numeric_field>).
        IF sy-subrc = 0 AND <lv_numeric_field> < 0.
          APPEND VALUE lvc_s_scol(
            fname    = <lv_numeric_field_name>
            color    = VALUE #( col = 6 inv = 1 )
            nokeycol = abap_true
          ) TO <lt_colors>.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    mo_alv_grid->refresh_table_display(
        is_stable = VALUE #( row = abap_true col = abap_true )
    ).
  ENDMETHOD.


  METHOD emphasize_selected.
    mo_alv_grid->get_selected_columns( IMPORTING et_index_columns = DATA(lt_index_cols) ).
    mo_alv_grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows) ).
    mo_alv_grid->get_selected_cells( IMPORTING et_cell = DATA(lt_cells) ).

    IF lt_index_cols IS NOT INITIAL.
      emphasize_selected_columns( it_selected_cols     = lt_index_cols
                                  iv_chosen_color_code = iv_chosen_color_code
                                  if_clear_color       = if_clear_color ).
    ELSEIF lt_index_rows IS NOT INITIAL.
      emphasize_selected_rows( it_selected_rows     = lt_index_rows
                               iv_chosen_color_code = iv_chosen_color_code
                               if_clear_color       = if_clear_color ).
    ELSEIF lt_cells IS NOT INITIAL.
      emphasize_selected_cells( it_selected_cells    = lt_cells
                                iv_chosen_color_code = iv_chosen_color_code
                                if_clear_color       = if_clear_color ).
    ENDIF.

    " refresh table
    mo_alv_grid->refresh_table_display(
      is_stable      = VALUE #( row = abap_true col = abap_true )
      i_soft_refresh = abap_true
    ).

  ENDMETHOD.


  METHOD emphasize_selected_cells.
    FIELD-SYMBOLS: <lt_table>  TYPE STANDARD TABLE,
                   <lt_colors> TYPE lvc_t_scol.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

*    mr_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    IF if_clear_color = abap_false.
      " get correct color code
      DATA(ls_color) = VALUE lvc_s_scol(
        color    = get_color_code_for_ucomm( iv_chosen_color_code )
        nokeycol = abap_true
      ).
    ENDIF.

    LOOP AT it_selected_cells ASSIGNING FIELD-SYMBOL(<ls_selected_cell>)
        GROUP BY ( row = <ls_selected_cell>-row_id )
        ASSIGNING FIELD-SYMBOL(<ls_selected_cell_group>).

      " determine current row
      ASSIGN <lt_table>[ <ls_selected_cell_group>-row ] TO FIELD-SYMBOL(<ls_current_row>).

      LOOP AT GROUP <ls_selected_cell_group> ASSIGNING FIELD-SYMBOL(<ls_cell_entry>).
        ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>cell_col_row_color OF STRUCTURE <ls_current_row> TO <lt_colors>.

        IF if_clear_color = abap_true.
          DELETE <lt_colors> WHERE fname = <ls_cell_entry>-col_id.
        ELSE.
          ASSIGN <lt_colors>[ fname = <ls_cell_entry>-col_id ] TO FIELD-SYMBOL(<ls_color_for_field>).
          IF sy-subrc <> 0.
            APPEND ls_color TO <lt_colors> ASSIGNING <ls_color_for_field>.
            <ls_color_for_field>-fname = <ls_cell_entry>-col_id.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.


  METHOD emphasize_selected_columns.

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
    DATA(ls_color) = get_color_code_for_ucomm( iv_chosen_color_code ).
    DATA(lv_color_string) = condense( val = |C{ ls_color-col }{ ls_color-int }{ ls_color-inv }| ).

    LOOP AT it_selected_cols ASSIGNING FIELD-SYMBOL(<ls_selected_column>).

      " get field catalog row for selected column
      DATA(lr_column_def) = REF #( lt_fieldcat[ fieldname = <ls_selected_column>-fieldname ] ).
      IF if_clear_color = abap_false.
        lr_column_def->emphasize = lv_color_string.
      ELSE.
        CLEAR lr_column_def->emphasize.
      ENDIF.
    ENDLOOP.

    mo_alv_grid->set_frontend_fieldcatalog( lt_fieldcat ).
    mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.


  METHOD emphasize_selected_rows.
    FIELD-SYMBOLS: <lt_table>       TYPE STANDARD TABLE,
                   <lt_color_struc> TYPE lvc_t_scol.

    IF if_clear_color = abap_false.
      " get correct color code
      DATA(lt_color_tab) = VALUE lvc_t_scol(
          (
            color    = get_color_code_for_ucomm( iv_chosen_color_code )
            nokeycol = abap_true
          )
      ).
    ENDIF.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    " set hide flag
    LOOP AT it_selected_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
      ASSIGN <lt_table>[ <ls_row>-index ] TO FIELD-SYMBOL(<ls_row_data>).

      IF sy-subrc = 0.
        ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>cell_col_row_color OF STRUCTURE <ls_row_data> TO <lt_color_struc>.
        IF sy-subrc = 0.
          <lt_color_struc> = lt_color_tab.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD emphasize_sort_fields.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/1
*&---------------------------------------------------------------------*
*& Description: Emphasizes the sort columns
*&---------------------------------------------------------------------*

    LOOP AT ct_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      CLEAR <ls_fieldcat>-emphasize.

      " is this a sorted field?
      TRY.
          DATA(ls_sorted) = it_sort[ fieldname = <ls_fieldcat>-fieldname ].
          <ls_fieldcat>-emphasize = zif_dbbr_global_consts=>gc_alv_colors-light_green.
        CATCH cx_sy_itab_line_not_found.
          " is this a formula field
          IF <ls_fieldcat>-parameter2 = 'F'.

            <ls_fieldcat>-emphasize = COND #( WHEN mo_util->ms_technical_info-color_formula_fields = abap_true THEN
                                                zif_dbbr_global_consts=>gc_alv_colors-light_yellow ).
            CONTINUE.
          ENDIF.

          IF <ls_fieldcat>-parameter2 = 'K'.
            <ls_fieldcat>-emphasize = zif_dbbr_global_consts=>gc_alv_emphasize-key_color.
            CONTINUE.
          ENDIF.

*          IF line_exists( mt_add_texts[ text_field_alv_int = <ls_fieldcat>-fieldname ] ).
          IF <ls_fieldcat>-parameter2 = 'T'.
            <ls_fieldcat>-emphasize = COND #( WHEN mo_util->ms_technical_info-emphasize_text_fields = abap_true THEN
                                                zif_dbbr_global_consts=>gc_alv_emphasize-text_field_color  ).
          ENDIF.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD execute_selection.
    CLEAR mf_no_data.

    IF if_count_lines_only = abap_true.
      mo_util->count_lines( ).
    ELSE.
      mo_util->execute_selection( ).
    ENDIF.

    rf_no_data = mf_no_data.
  ENDMETHOD.


  METHOD get_color_code_for_ucomm.
    CASE iv_chosen_color_code.
      WHEN zif_dbbr_c_selection_functions=>emph_light_green.
        rs_color = VALUE #( col = 5 ).
      WHEN zif_dbbr_c_selection_functions=>emph_green.
        rs_color = VALUE #( col = 5 int = 1 ).
      WHEN zif_dbbr_c_selection_functions=>emph_green_inverted.
        rs_color = VALUE #( col = 5 inv = 1 ).
      WHEN zif_dbbr_c_selection_functions=>emph_light_orange.
        rs_color = VALUE #( col = 7 ).
      WHEN zif_dbbr_c_selection_functions=>emph_orange.
        rs_color = VALUE #( col = 7 int = 1 ).
      WHEN zif_dbbr_c_selection_functions=>emph_light_yellow.
        rs_color = VALUE #( col = 3 ).
      WHEN zif_dbbr_c_selection_functions=>emph_yellow.
        rs_color = VALUE #( col = 3 int = 1 ).
      WHEN zif_dbbr_c_selection_functions=>emph_light_grey_blue.
        rs_color = VALUE #( col = 4 ).
      WHEN zif_dbbr_c_selection_functions=>emph_light_blue.
        rs_color = VALUE #( col = 1 ).
      WHEN zif_dbbr_c_selection_functions=>emph_blue.
        rs_color = VALUE #( col = 1 int = 1 ).
      WHEN zif_dbbr_c_selection_functions=>emph_light_red.
        rs_color = VALUE #( col = 6 ).
      WHEN zif_dbbr_c_selection_functions=>emph_red.
        rs_color = VALUE #( col = 6 int = 1 ).
      WHEN zif_dbbr_c_selection_functions=>emph_red_inverted.
        rs_color = VALUE #( col = 6 inv = 1 ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_updated_tabfields.
    rr_tabfields = mo_util->mo_tabfields_original.
  ENDMETHOD.


  METHOD group_by_selected_columns.
    mo_util->get_alv_util( )->execute_column_grouping( ).
  ENDMETHOD.


  METHOD hide_selected_rows.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2017/01/05
*&---------------------------------------------------------------------*
*& Description: Hide the selected rows
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    mo_alv_grid->get_selected_rows(
      IMPORTING
        et_index_rows = DATA(lt_index_rows)
        et_row_no     = DATA(lt_row_no)
    ).

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    " set hide flag
    LOOP AT lt_row_no ASSIGNING FIELD-SYMBOL(<ls_row>).
      ASSIGN <lt_table>[ <ls_row>-row_id ] TO FIELD-SYMBOL(<ls_row_data>).

      IF sy-subrc = 0.
        ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>hide_flag OF STRUCTURE <ls_row_data> TO FIELD-SYMBOL(<lv_hide_flag>).
        IF sy-subrc = 0.
          <lv_hide_flag> = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " add filter for hiding rows
    mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    IF NOT line_exists( lt_filter[ fieldname = zif_dbbr_c_special_out_columns=>hide_flag ] ).
      APPEND ss_hide_row_filter TO lt_filter.
    ENDIF.

    mo_alv_grid->set_filter_criteria( lt_filter ).

    " refresh table
    mo_alv_grid->refresh_table_display(
      is_stable      = VALUE #( row = abap_true col = abap_true )
    ).

  ENDMETHOD.


  METHOD init_grid_control.
    DATA: lr_alv_container TYPE REF TO cl_gui_container.

    FIELD-SYMBOLS: <lt_table> TYPE table.

    CHECK mo_output_container IS INITIAL.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    zcl_dbbr_screen_helper=>show_progress( iv_text = |{ TEXT-002 }| iv_progress = 75 ).

    mo_output_container = cl_gui_container=>default_screen.

    IF mf_has_parent = abap_true.
      mo_splitter_container = NEW #(
        parent  = mo_output_container
        rows    = 2
        columns = 1
      ).

      mo_splitter_container->set_row_mode( cl_gui_splitter_container=>mode_absolute ).
      mo_splitter_container->set_row_height( id = 1 height = 65 ).
*.,.. retrieve container for alv creation
      lr_alv_container = mo_splitter_container->get_container( column = 1 row = 2 ).
*..,. initialize the navigation bread crumbs
      mo_util->init_navigation_breadcrumbs(
        mo_splitter_container->get_container( column = 1 row = 1 )
      ).
    ELSE.
      lr_alv_container = mo_output_container.
    ENDIF.

    " create alv grid
    mo_alv_grid = NEW #( lr_alv_container ).
    mo_alv_grid->set_default_toolbar( ).
    mo_util->mo_alv_grid = mo_alv_grid.

    SET HANDLER:
       on_double_click FOR mo_alv_grid,
       on_user_command FOR mo_alv_grid,
       on_before_user_command FOR mo_alv_grid,
       on_after_user_command FOR mo_alv_grid,
       on_context_menu_request FOR mo_alv_grid,
       on_hotspot_click FOR mo_alv_grid.

    ms_alv_layout = VALUE #(
        sel_mode   = 'D'
        cwidth_opt = abap_true
        no_merging = mo_util->ms_technical_info-no_merging_on
        ctab_fname = zif_dbbr_c_special_out_columns=>cell_col_row_color
        grid_title = mo_util->build_simple_alv_title( )
        no_keyfix  = mo_util->ms_technical_info-key_cols_not_fixed

    ).

    mo_util->read_entity_infos( ).

    DATA(ls_variant) = VALUE disvariant(
        report     = zif_dbbr_c_report_id=>main && mo_util->get_entity_name( )
        handle     = abap_true
        username   = sy-uname
    ).

    " coloring of sort fields
    IF mo_util->ms_technical_info-color_sort_columns = abap_true.
      emphasize_sort_fields( EXPORTING it_sort     = mo_util->mt_sort_alv
                             CHANGING  ct_fieldcat = mo_util->mt_fieldcat ).
    ENDIF.

    DATA(lt_sort) = mo_util->mt_sort_alv.
    DATA(lt_filter) = mo_util->get_alv_filter_from_criteria( ).

    mo_alv_grid->set_table_for_first_display(
      EXPORTING is_layout         = ms_alv_layout
                i_save            = 'A'
                i_default         = mo_util->ms_technical_info-enable_alv_default_variant
                is_variant        = ls_variant
                it_special_groups = mt_column_groups
      CHANGING  it_fieldcatalog   = mo_util->mt_fieldcat
                it_outtab         = <lt_table>
                it_sort           = lt_sort
                it_filter         = lt_filter
    ).

*.. Filters in the ALV util need to updated to reflect the ALV filters
    mo_util->get_alv_util( )->update_filters( ).

    IF mo_util->ms_technical_info-enable_alv_default_variant = abap_true AND ls_variant-variant IS INITIAL.
      " check if a default variant exists
      mo_alv_grid->get_variant( IMPORTING es_variant = DATA(ls_alv_var_active) ).
      IF ls_alv_var_active-variant IS NOT INITIAL.
        mf_default_alv_var_active = abap_true.
        MESSAGE s052(zdbbr_info).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD keep_selected_columns.
    DATA: lt_fieldname_range TYPE RANGE OF lvc_fname.

    mo_alv_grid->get_selected_columns( IMPORTING et_index_columns = DATA(lt_cols) ).

    CHECK lt_cols IS NOT INITIAL.

    lt_fieldname_range = VALUE #(
      FOR col IN lt_cols ( sign = 'I' option = 'EQ' low = col-fieldname )
    ).

    CLEAR lt_cols.

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_field_cat) ).

    LOOP AT lt_field_cat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WHERE fieldname NOT IN lt_fieldname_range.
      <ls_fieldcat>-no_out = abap_true.
    ENDLOOP.

    CHECK sy-subrc = 0.

    mo_alv_grid->set_frontend_fieldcatalog( lt_field_cat ).


    mo_alv_grid->refresh_table_display(
      is_stable      = VALUE #( row = abap_true col = abap_true )
    ).

    mo_alv_grid->optimize_columns( ).
  ENDMETHOD.


  METHOD keep_selected_rows.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2017/01/05
*&---------------------------------------------------------------------*
*& Description: Hide non-selected rows
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    mo_alv_grid->get_selected_rows(
      IMPORTING
        et_row_no     = DATA(lt_row_no)
    ).

    " build selopt table for selected rows
    DATA(lt_selected_row_selopt) = VALUE zdbbr_selopt_itab(
      FOR row_no IN lt_row_no
      ( sign   = 'I'
        option = 'EQ'
        low    = row_no-row_id )
    ).

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_data_row>).
      CHECK sy-tabix NOT IN lt_selected_row_selopt.
      ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>hide_flag OF STRUCTURE <ls_data_row> TO FIELD-SYMBOL(<lv_hide_flag>).
      IF sy-subrc = 0.
        <lv_hide_flag> = abap_true.
      ENDIF.
    ENDLOOP.

    " add filter for hiding rows
    mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    IF NOT line_exists( lt_filter[ fieldname = zif_dbbr_c_special_out_columns=>hide_flag ] ).
      APPEND ss_hide_row_filter TO lt_filter.
    ENDIF.

    mo_alv_grid->set_filter_criteria( lt_filter ).

    " refresh table
    mo_alv_grid->refresh_table_display(
      is_stable      = VALUE #( row = abap_true col = abap_true )
    ).

  ENDMETHOD.


  METHOD layout_was_transferred.
    rf_transferred = mf_layout_transferred.
  ENDMETHOD.


  METHOD on_after_user_command.
    DATA: lf_emphasize_sort_fields TYPE abap_bool,
          lf_trigger_dummy_pai     TYPE abap_bool,
          lf_update_filter         TYPE abap_bool.

    CASE e_ucomm.

      WHEN cl_gui_alv_grid=>mc_fc_sort_asc OR
           cl_gui_alv_grid=>mc_fc_sort_dsc.
        lf_emphasize_sort_fields = abap_true.

      WHEN cl_gui_alv_grid=>mc_fc_current_variant OR
           cl_gui_alv_grid=>mc_fc_variant_admin OR
           cl_gui_alv_grid=>mc_fc_load_variant OR
           cl_gui_alv_grid=>mc_fc_maintain_variant.
        lf_emphasize_sort_fields = abap_true.
        lf_update_filter = abap_true.
        lf_trigger_dummy_pai = abap_true.

      WHEN cl_gui_alv_grid=>mc_fc_filter OR
           cl_gui_alv_grid=>mc_fc_delete_filter.
        lf_update_filter = abap_true.
        lf_trigger_dummy_pai = abap_true.

      WHEN zif_dbbr_c_selection_functions=>quick_filter OR
           zif_dbbr_c_selection_functions=>delete_filters_from_cols OR
           cl_gui_alv_grid=>mc_fc_sum OR
           zif_dbbr_c_selection_functions=>quick_filter_exclusion.
        lf_trigger_dummy_pai = abap_true.

      WHEN zif_dbbr_c_selection_functions=>compare_selected_lines OR
           zif_dbbr_c_selection_functions=>group_by_selected_columns OR
           zif_dbbr_c_selection_functions=>control_tech_view OR
           zif_dbbr_c_selection_functions=>show_users_settings OR
           zif_dbbr_c_selection_functions=>edit_data OR
           zif_dbbr_c_selection_functions=>change_max_row_count OR
           zif_dbbr_c_selection_functions=>change_cds_parameters OR
           zif_dbbr_c_selection_functions=>navigate_association OR
           zif_dbbr_c_selection_functions=>hide_other_columns.
        RETURN.
    ENDCASE.

    IF lf_emphasize_sort_fields = abap_true.
      IF mo_util->ms_technical_info-color_sort_columns = abap_true.
        mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
        mo_alv_grid->get_sort_criteria( IMPORTING et_sort = DATA(lt_sort) ).

        emphasize_sort_fields( EXPORTING it_sort    = lt_sort
                               CHANGING ct_fieldcat = lt_fieldcat ).

        mo_alv_grid->set_frontend_fieldcatalog( lt_fieldcat ).
      ENDIF.
    ENDIF.

    IF lf_update_filter = abap_true.
      process_filter_change( ).
    ENDIF.

    IF lf_trigger_dummy_pai = abap_true.
      cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
    ELSEIF e_ucomm NP '%*' AND
           e_ucomm NP '&*'.
      cl_gui_cfw=>set_new_ok_code( e_ucomm ).
    ENDIF.

  ENDMETHOD.


  METHOD on_before_user_command.
*&---------------------------------------------------------------------*
*& Author: stockbal     Date: 2017/01/11
*&---------------------------------------------------------------------*
    CASE e_ucomm.
      WHEN cl_gui_alv_grid=>mc_fc_delete_filter.
        mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter_criteria) ).
        IF line_exists( lt_filter_criteria[ fieldname = ss_hide_row_filter-fieldname ] ).
          " show hidden lines after filter is deleted
          show_hidden_rows( if_refresh = abap_false ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD on_context_menu_request.
*& Description: Context menu request event for alv grid
*&---------------------------------------------------------------------*
    DATA: ls_field             TYPE lvc_s_fcat,
          lt_serialized        TYPE sctx_serialize,
          lt_temp_menu_entries TYPE sctx_entrytab,
          lt_menu_flat         TYPE sctx_entrytab.

    e_object->if_ctxmnu_internal~serialize_menu( CHANGING menu = lt_serialized ).
    lt_menu_flat = VALUE #(
      FOR serialized_entry IN lt_serialized
      ( type        = serialized_entry-type
        fcode       = serialized_entry-fcode
        icon        = serialized_entry-icon
        text        = serialized_entry-text
        disabled    = serialized_entry-disabled
        checked     = serialized_entry-checked
        accelerator = serialized_entry-accelerator )
    ).

    mo_alv_grid->get_selected_rows(
      IMPORTING et_index_rows = DATA(lt_index_rows)
                et_row_no     = DATA(lt_row_no)
    ).

    mo_alv_grid->get_selected_columns( IMPORTING et_index_columns = DATA(lt_index_cols) ).
    mo_alv_grid->get_selected_cells( IMPORTING et_cell = DATA(lt_cells) ).

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    DATA(lr_emph_menu) = build_accentuation_submenu( ).

    mo_util->handle_alv_ctx_menu_request(
       EXPORTING if_selected_cells = xsdbool( lt_cells IS NOT INITIAL )
                 if_selected_rows  = xsdbool( lt_row_no IS NOT INITIAL )
                 if_selected_cols  = xsdbool( lt_index_cols IS NOT INITIAL )
       CHANGING  ct_menu_entries = lt_menu_flat
    ).

    IF lt_index_rows IS NOT INITIAL.
      lt_menu_flat = VALUE #( BASE lt_menu_flat ( type = sctx_c_type_separator ) ).
      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_function
          fcode = zif_dbbr_c_selection_functions=>hide_lines
          text  = c_function_texts-hide_lines
        )
      ).
      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_function
          fcode = zif_dbbr_c_selection_functions=>keep_lines
          text  = c_function_texts-keep_lines
        )
      ).
      lt_menu_flat = VALUE #( BASE lt_menu_flat ( type = sctx_c_type_separator ) ).
      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_submenu
          text  = 'Color selected Rows'
          menu  = lr_emph_menu
        )
      ).
      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_function
          fcode = zif_dbbr_c_selection_functions=>no_emphasize_lines
          text  = c_function_texts-dont_emphasize_rows
        )
      ).

      IF lines( lt_index_rows ) > 1.
        lt_menu_flat = VALUE #( BASE lt_menu_flat ( type = sctx_c_type_separator ) ).
        lt_menu_flat = VALUE #( BASE lt_menu_flat
          ( type  = sctx_c_type_function
            fcode = zif_dbbr_c_selection_functions=>compare_selected_lines
            text  = c_function_texts-compare_lines
          )
        ).

      ENDIF.
    ELSEIF lt_index_cols IS NOT INITIAL.
      IF lt_cells IS INITIAL.
        DATA(lv_hide_func_index) = line_index( lt_menu_flat[ fcode = zif_uitb_c_alv_functions=>column_invisible ] ).
        INSERT VALUE #(
            type  = sctx_c_type_function
            fcode = zif_dbbr_c_selection_functions=>hide_other_columns
            text  = c_function_texts-hide_other_columns
        ) INTO lt_menu_flat INDEX lv_hide_func_index + 1.
      ENDIF.

      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_submenu
          text  = 'Color selected Columns'
          menu  = lr_emph_menu
        )
      ).

      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_function
          fcode = zif_dbbr_c_selection_functions=>no_emphasize_lines
          text  = c_function_texts-dont_emphasize_columns
        )
      ).

      lt_menu_flat = VALUE #( BASE lt_menu_flat ( type = sctx_c_type_separator ) ).

      IF NOT ( mo_util->mf_group_by = abap_true OR
               mo_util->mf_aggregation = abap_true ).
        lt_menu_flat = VALUE #( BASE lt_menu_flat
          ( type  = sctx_c_type_function
            fcode = zif_dbbr_c_selection_functions=>group_by_selected_columns
            text  = c_function_texts-group_selected_columns
          )
        ).
      ENDIF.

      IF are_selected_cols_filtered( lt_index_cols ).
        DATA(lv_filter_func_index) = line_index( lt_menu_flat[ fcode = zif_uitb_c_alv_functions=>filter ] ).
        INSERT VALUE #( type  = sctx_c_type_function
            fcode = zif_dbbr_c_selection_functions=>delete_filters_from_cols
            text  = c_function_texts-delete_filters_from_cols
        ) INTO lt_menu_flat INDEX lv_filter_func_index + 1.
      ENDIF.
    ELSE.
      lt_menu_flat = VALUE #( BASE lt_menu_flat ( type = sctx_c_type_separator ) ).
      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_submenu
          text  = 'Color selected Cells'
          menu  = lr_emph_menu
        )
      ).
      lt_menu_flat = VALUE #( BASE lt_menu_flat
        ( type  = sctx_c_type_function
          fcode = zif_dbbr_c_selection_functions=>no_emphasize_cells
          text  = c_function_texts-dont_emphasize_cells
        )
      ).
      lv_filter_func_index = line_index( lt_menu_flat[ fcode = zif_uitb_c_alv_functions=>filter ] ).
      lt_temp_menu_entries = VALUE #(
        ( type  = sctx_c_type_function
          fcode = zif_dbbr_c_selection_functions=>quick_filter
          text  = c_function_texts-quickfilter )
        ( type  = sctx_c_type_function
          fcode = zif_dbbr_c_selection_functions=>quick_filter_exclusion
          text  = c_function_texts-quickfilter_exclusion )
      ).

      INSERT LINES OF lt_temp_menu_entries INTO lt_menu_flat INDEX lv_filter_func_index + 1.

      " add option to calculate certain selected cells
      IF lt_cells IS NOT INITIAL.
        lt_menu_flat = VALUE #( BASE lt_menu_flat ( type = sctx_c_type_separator ) ).
        IF lines( lt_cells ) = 1.
*......... Check if cell type is string
          ls_field = VALUE #( lt_fieldcat[ fieldname = lt_cells[ 1 ]-col_id-fieldname ] OPTIONAL ).
          IF ls_field IS NOT INITIAL AND ls_field-inttype = cl_abap_typedescr=>typekind_string.
            lt_menu_flat = VALUE #( BASE lt_menu_flat
              ( type  = sctx_c_type_function
                fcode = zif_dbbr_c_selection_functions=>show_string_cell_content
                text  = c_function_texts-show_string_cell_content )
            ).
          ENDIF.
        ENDIF.
        lt_menu_flat = VALUE #( BASE lt_menu_flat
          ( type  = sctx_c_type_function
            fcode = zif_dbbr_c_selection_functions=>calc_sum_of_chosen_cells
            text  = c_function_texts-calc_sum_of_chosen_cells
          )
        ).
        e_object->add_function(
            fcode = zif_dbbr_c_selection_functions=>calc_sum_of_chosen_cells
            text  = c_function_texts-calc_sum_of_chosen_cells
        ).
      ENDIF.
    ENDIF.

    e_object->clear( ).
    e_object->initialize_from_source( lt_menu_flat ).
  ENDMETHOD.


  METHOD on_double_click.
*&---------------------------------------------------------------------*
*& Description: Handles double click on ALV line
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF lty_selfield,
             fieldname         TYPE scrtext_l,
             value             TYPE se16n_value,
             value_unconverted TYPE se16n_value,
             tech_fieldname    TYPE fieldname,
           END OF lty_selfield.

    FIELD-SYMBOLS: <lt_table>        TYPE table.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    ASSIGN <lt_table>[ es_row_no-row_id ] TO FIELD-SYMBOL(<ls_current_line>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(ls_field) = VALUE #( mo_util->mt_fieldcat[ fieldname = e_column-fieldname ] OPTIONAL ).
    IF ls_field IS NOT INITIAL.

      IF ls_field-parameter1 = 'J'.
        on_hotspot_click(
            e_column_id = e_column
            e_row_id    = e_row
            es_row_no   = es_row_no
        ).
        RETURN.
      ENDIF.
    ENDIF.

    show_line_detail(
      is_line = <ls_current_line>
    ).

  ENDMETHOD.


  METHOD on_hotspot_click.
    call_jumpfield_transaction( iv_fieldname = e_column_id-fieldname
                                iv_row_index = e_row_id-index ).

  ENDMETHOD.


  METHOD on_no_data.
    mf_no_data = abap_true.

    IF mf_has_parent = abap_true.
*... export error message to memory
      IF ef_criteria_exist = abap_true.
        MESSAGE i101(zdbbr_info) INTO DATA(lv_message).
      ELSE.
        MESSAGE i060(zdbbr_info) INTO lv_message.
      ENDIF.

      DATA(lv_memid) = CONV char32( |{ zif_dbbr_c_report_id=>output }{ sy-uname }MSG| ).
      EXPORT
        message      = lv_message
        message_type = 'I'
      TO MEMORY ID lv_memid.
      zcl_dbbr_screen_helper=>leave_screen( ).
    ELSE.
      IF mo_util->ms_technical_info-activate_alv_live_filter = abap_true.
        IF mo_alv_grid IS INITIAL.
          IF ef_criteria_exist = abap_true.
            MESSAGE i101(zdbbr_info).
          ELSE.
            MESSAGE i060(zdbbr_info).
          ENDIF.
        ELSE.
          mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
        ENDIF.
      ELSE.
        IF ef_criteria_exist = abap_true.
          MESSAGE i101(zdbbr_info).
        ELSE.
          MESSAGE i060(zdbbr_info).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD on_selection_finish.
    IF ef_first_select = abap_true.
      zif_uitb_screen_controller~call_screen( ).
    ELSE.
      DATA(lr_alv_util) = mo_util->get_alv_util( ).

      IF lr_alv_util->mf_use_live_filter = abap_true.
        mo_alv_grid->set_filter_criteria( lr_alv_util->get_filters( ) ).
      ENDIF.

      mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).

    ENDIF.
  ENDMETHOD.


  METHOD on_toolbar_clicked.
    on_user_command( fcode ).
  ENDMETHOD.


  METHOD on_user_command.
    CASE e_ucomm.
      WHEN zif_dbbr_c_selection_functions=>quick_filter.
        perform_quick_filter( ).

      WHEN zif_dbbr_c_selection_functions=>quick_filter_exclusion.
        perform_quick_filter( if_exclude = abap_true ).

      WHEN zif_dbbr_c_selection_functions=>hide_lines.
        hide_selected_rows( ).

      WHEN zif_dbbr_c_selection_functions=>keep_lines.
        keep_selected_rows( ).

      WHEN zif_dbbr_c_selection_functions=>emph_light_green OR
           zif_dbbr_c_selection_functions=>emph_green OR
           zif_dbbr_c_selection_functions=>emph_green_inverted OR
           zif_dbbr_c_selection_functions=>emph_light_orange OR
           zif_dbbr_c_selection_functions=>emph_orange OR
           zif_dbbr_c_selection_functions=>emph_light_yellow OR
           zif_dbbr_c_selection_functions=>emph_yellow OR
           zif_dbbr_c_selection_functions=>emph_light_grey_blue OR
           zif_dbbr_c_selection_functions=>emph_grey_blue OR
           zif_dbbr_c_selection_functions=>emph_light_blue OR
           zif_dbbr_c_selection_functions=>emph_blue OR
           zif_dbbr_c_selection_functions=>emph_light_red OR
           zif_dbbr_c_selection_functions=>emph_red OR
           zif_dbbr_c_selection_functions=>emph_red_inverted.
        emphasize_selected( iv_chosen_color_code = e_ucomm ).

      WHEN zif_dbbr_c_selection_functions=>no_emphasize_lines.
        emphasize_selected( if_clear_color = abap_true ).

      WHEN zif_dbbr_c_selection_functions=>no_emphasize_cells.
        emphasize_selected( if_clear_color = abap_true ).

      WHEN zif_dbbr_c_selection_functions=>calc_sum_of_chosen_cells.
        calculate_sum_of_cells( ).

      WHEN zif_dbbr_c_selection_functions=>show_string_cell_content.
        show_string_cell_content( ).

      WHEN zif_dbbr_c_selection_functions=>compare_selected_lines.
        compare_selected_lines( ).

      WHEN zif_dbbr_c_selection_functions=>group_by_selected_columns.
        group_by_selected_columns( ).

      WHEN zif_dbbr_c_selection_functions=>delete_filters_from_cols.
        remove_filt_from_selected_cols( ).

      WHEN zif_dbbr_c_selection_functions=>hide_other_columns.
        keep_selected_columns( ).

      WHEN zif_dbbr_c_selection_functions=>control_tech_view.
        control_tech_view( ).

      WHEN OTHERS.
        mo_util->zif_dbbr_screen_util~handle_ui_function( CHANGING cv_function = e_ucomm ).
    ENDCASE.

  ENDMETHOD.


  METHOD perform_quick_filter.
    DATA(lr_alv_util) = mo_util->get_alv_util( ).
    lr_alv_util->execute_quick_filter( if_exclude ).

    process_filter_change( ).
  ENDMETHOD.


  METHOD perform_row_comparison.
*&---------------------------------------------------------------------*
*& Description: Compares rows across all columns
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lt_table>            TYPE STANDARD TABLE,
                   <lt_colors_left_row>  TYPE lvc_t_scol,
                   <lt_colors_right_row> TYPE lvc_t_scol.

    ASSIGN mo_util->mr_t_temp_data->* TO <lt_table>.

    rf_rows_identical = abap_true.

    DATA(lv_line_count) = lines( <lt_table> ).

    ASSIGN <lt_table>[ 1 ] TO FIELD-SYMBOL(<ls_left_row>).
    DATA(lv_index) = 2.

    WHILE lv_index <= lv_line_count.
      DATA(lf_row_differs) = abap_false.

      ASSIGN <lt_table>[ lv_index ] TO FIELD-SYMBOL(<ls_right_row>).

      ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>cell_col_row_color OF STRUCTURE <ls_left_row> TO <lt_colors_left_row>.
      ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>cell_col_row_color OF STRUCTURE <ls_right_row> TO <lt_colors_right_row>.

      LOOP AT it_fieldcat ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE no_out = abap_false
                                                               AND tech   = abap_false.
        ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE <ls_left_row> TO FIELD-SYMBOL(<lv_left_value>).
        ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE <ls_right_row> TO FIELD-SYMBOL(<lv_right_value>).
        IF <lv_left_value> IS NOT ASSIGNED OR <lv_right_value> IS NOT ASSIGNED.
          EXIT.
        ENDIF.

        IF <lv_left_value> <> <lv_right_value>.
          APPEND VALUE lvc_s_scol(
              fname    = <ls_field>-fieldname
              color    = VALUE lvc_s_colo(
                col = col_negative
                int = 0
                inv = 0
              )
          ) TO <lt_colors_right_row>.
          lf_row_differs = abap_true.
          rf_rows_identical = abap_false.
        ENDIF.

        UNASSIGN: <lv_left_value>, <lv_right_value>.
      ENDLOOP.

      IF lf_row_differs = abap_false.
        APPEND VALUE lvc_s_scol(
            color    = VALUE lvc_s_colo(
              col = col_positive
              int = 0
              inv = 0
            )
        ) TO <lt_colors_right_row>.
      ENDIF.

      ADD 1 TO lv_index.
    ENDWHILE.

  ENDMETHOD.


  METHOD process_filter_change.
    DATA(lr_alv_util) = mo_util->get_alv_util( ).
    lr_alv_util->update_filters( ).

    IF lr_alv_util->have_db_filters_changed( ).
      mo_util->update_selection_for_filter( ).
    ELSE.
      mo_alv_grid->set_filter_criteria( lr_alv_util->get_filters( ) ).
      mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
    ENDIF.
  ENDMETHOD.


  METHOD refresh.
    mo_util->refresh_selection( ).
  ENDMETHOD.


  METHOD remove_filt_from_selected_cols.
    mo_util->get_alv_util( )->del_filter_from_selected_cols( ).
    process_filter_change( ).
  ENDMETHOD.


  METHOD remove_grouping.
    mo_util->get_alv_util( )->remove_column_grouping( ).

    mo_alv_grid->set_frontend_fieldcatalog( mo_util->mt_fieldcat ).
    mo_alv_grid->set_sort_criteria( mo_util->mt_sort_alv ).
    mo_alv_grid->set_filter_criteria( VALUE #( ) ).

    refresh( ).

    mo_alv_grid->set_scroll_info_via_id(
        is_col_info = VALUE lvc_s_col( fieldname = mo_util->mt_fieldcat[ 2 ]-fieldname )
        is_row_no   = VALUE lvc_s_roid( row_id = 1 )
    ).

    mo_alv_grid->optimize_columns( ).

    cl_gui_control=>set_focus( mo_alv_grid ).
  ENDMETHOD.


  METHOD reset_alv_layout.
    mo_alv_grid->set_frontend_fieldcatalog( mo_util->mt_fieldcat ).
    mo_alv_grid->set_sort_criteria( VALUE #( ) ).
    mo_alv_grid->set_frontend_layout( ms_alv_layout ).

    mo_alv_grid->optimize_columns( ).
    mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
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


  METHOD save_as_f4.
*&---------------------------------------------------------------------*
*& Description: Saves current selection as custom f4 help for a database table
*&---------------------------------------------------------------------*
    DATA: lf_go_to_f4_manager TYPE abap_bool.

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
    DELETE lt_fieldcat WHERE no_out = abap_true OR tech = abap_true.
    SORT lt_fieldcat BY col_pos.

    DATA(lr_f4_screen_controller) = NEW zcl_dbbr_custom_f4_sc(
        it_fieldcat     = lt_fieldcat
        io_tabfields    = mo_util->mo_tabfields
        is_join_def     = mo_util->ms_join_def
    ).

    lr_f4_screen_controller->zif_uitb_screen_controller~call_screen( ).

  ENDMETHOD.


  METHOD scroll_to_column.
    DATA: lt_columns TYPE lty_t_col_selection.

*.. Build columns list from alv field catalog of visible columns
    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_field_catalog) ).
    DELETE lt_field_catalog WHERE tech = abap_true
                               OR no_out = abap_true.

    LOOP AT lt_field_catalog ASSIGNING FIELD-SYMBOL(<ls_field_cat>).
      DATA(lv_col_index) = sy-tabix.

      TRY.
          DATA(lr_field) = mo_util->mo_tabfields_all->get_field_ref_by_alv_name( <ls_field_cat>-fieldname ).
          lt_columns = VALUE #( BASE lt_columns
            ( idx            = lv_col_index
              tech_fieldname = lr_field->alv_fieldname
              fieldname      = lr_field->sql_fieldname
              description    = lr_field->field_ddtext )
          ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

*.. choose column via popup dialog
    DATA(lr_view) = NEW lcl_choose_col_view(
        it_col = lt_columns
    ).

    DATA(lv_chosen_column) = lr_view->get_chosen_column( ).
    CHECK lv_chosen_column IS NOT INITIAL.

    mo_alv_grid->get_current_cell(
      IMPORTING es_row_id = DATA(ls_row_id)
    ).

*... select found column
    mo_alv_grid->set_current_cell_via_id(
      EXPORTING
        is_column_id = VALUE lvc_s_col( fieldname = lv_chosen_column )
        is_row_no    = VALUE lvc_s_roid( row_id = ls_row_id-index )
    ).

    mo_alv_grid->set_selected_columns(
        it_col_table = VALUE #( ( fieldname = lv_chosen_column ) )
    ).

    cl_gui_control=>set_focus( mo_alv_grid ).
  ENDMETHOD.


  METHOD show_hidden_rows.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mo_util->mr_t_data->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_row>) WHERE (`HIDE_FLAG EQ 'X'`).
      ASSIGN COMPONENT zif_dbbr_c_special_out_columns=>hide_flag OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_hide_flag>).
      IF sy-subrc = 0.
        CLEAR: <lv_hide_flag>.
      ENDIF.
    ENDLOOP.

    " update filter
    mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    DELETE lt_filter WHERE fieldname = zif_dbbr_c_special_out_columns=>hide_flag.
    mo_alv_grid->set_filter_criteria( lt_filter ).

    " refresh table
    IF if_refresh = abap_true.
      mo_alv_grid->refresh_table_display(
        is_stable      = VALUE #( row = abap_true col = abap_true )
      ).
    ENDIF.

  ENDMETHOD.



  METHOD show_line_detail.
    data(lo_detail_viewer) = new lcl_detail_viewer(
      io_util           = mo_util
      it_fieldcat       = mo_util->mt_fieldcat
      io_tabfields_all  = mo_util->mo_tabfields_all
      is_technical_info = mo_util->ms_technical_info
    ).
    lo_detail_viewer->show_details( is_line ).
  ENDMETHOD.


  METHOD transfer_filter_values.
*&---------------------------------------------------------------------*
*& Description: Transfers filter values to selection screen mask
*&---------------------------------------------------------------------*
    DATA: lv_transferred_filter_vals TYPE sy-tabix.

    FIELD-SYMBOLS: <lt_selfields>       TYPE zdbbr_selfield_itab,
                   <lt_selfields_multi> TYPE zdbbr_selfield_itab.

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    DATA(lr_selfields) = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields ) ).
    DATA(lr_selfields_multi) = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields_multi ) ).

    ASSIGN lr_selfields->* TO <lt_selfields>.
    ASSIGN lr_selfields_multi->* TO <lt_selfields_multi>.

    " get the current filter values
    mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).

    LOOP AT lt_filter ASSIGNING FIELD-SYMBOL(<ls_filter_value>)
      GROUP BY ( fieldname = <ls_filter_value>-fieldname
                 ref_field = <ls_filter_value>-ref_field
                 ref_tab   = <ls_filter_value>-ref_table )
      ASSIGNING FIELD-SYMBOL(<ls_filter_value_group>).

      " check if this field exists in the selection screen table
      IF NOT line_exists( <lt_selfields>[ tabname   = <ls_filter_value_group>-ref_tab
                                          fieldname = <ls_filter_value_group>-ref_field ] ).
        CONTINUE.
      ENDIF.

      DATA(lf_first_iteration) = abap_true.
      LOOP AT GROUP <ls_filter_value_group> ASSIGNING FIELD-SYMBOL(<ls_filter_value_group_entry>).
        IF lf_first_iteration = abap_true.
          ASSIGN <lt_selfields>[ tabname   = <ls_filter_value_group>-ref_tab
                                 fieldname = <ls_filter_value_group>-ref_field ] TO FIELD-SYMBOL(<ls_selfield_for_filter>).

          ASSIGN <ls_selfield_for_filter> TO FIELD-SYMBOL(<ls_selfield>).
          lf_first_iteration = abap_false.
        ELSE.
          " use selfield line as pattern for the following multi select entries
          <ls_selfield_for_filter>-push = abap_true.

          DATA(ls_selfield_multi) = <ls_selfield_for_filter>.

          APPEND ls_selfield_multi TO <lt_selfields_multi> ASSIGNING <ls_selfield>.
        ENDIF.

        <ls_selfield>-low = condense( val = <ls_filter_value_group_entry>-low ).
        <ls_selfield>-high = condense( val = <ls_filter_value_group_entry>-high ).
        <ls_selfield>-sign = <ls_filter_value_group_entry>-sign.
        <ls_selfield>-option = <ls_filter_value_group_entry>-option.

        IF <ls_selfield>-low = space AND
           <ls_selfield>-high = space.

          <ls_selfield>-option = 'EQ'.
        ELSEIF <ls_selfield>-sign = 'I' AND <ls_selfield>-option = 'EQ'.
          CLEAR <ls_selfield>-option.
        ELSE.
          <ls_selfield>-option = <ls_filter_value_group_entry>-option.
        ENDIF.

        ADD 1 TO lv_transferred_filter_vals.
      ENDLOOP.
    ENDLOOP.

    SORT <lt_selfields_multi> BY tabname fieldname sign option low high.
    DELETE ADJACENT DUPLICATES FROM <lt_selfields_multi> COMPARING tabname fieldname sign option low high.

    IF if_leave_screen = abap_true.
      IF lv_transferred_filter_vals > 0.
        MESSAGE s033(zdbbr_info) WITH lv_transferred_filter_vals.
        zif_uitb_screen_controller~free_screen_resources( ).
        zcl_dbbr_screen_helper=>leave_screen( ).
      ELSE.
        MESSAGE w034(zdbbr_info).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD transfer_layout_info_to_selscr.
    " get fieldcatalog to determine all visible fields and the ordering
    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
    mo_alv_grid->get_sort_criteria( IMPORTING et_sort = DATA(lt_sort) ).

    " delete irrelevant fieldcatalog data
    SORT lt_fieldcat BY col_pos.
    DELETE lt_fieldcat WHERE tech = abap_true.

    " renumber column positions
    DATA(lv_pos) = 1.
    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      <ls_fieldcat>-col_pos = lv_pos.
      ADD 1 TO lv_pos.
    ENDLOOP.

    DATA(lr_original_tabfields) = mo_util->mo_tabfields_original.

    " fill information into original tabfield list
    lr_original_tabfields->clear_active_flag( if_clear_output = abap_true
                                              if_clear_sort   = abap_true ).
    lr_original_tabfields->initialize_iterator( ).

    WHILE lr_original_tabfields->has_more_lines( ).
      DATA(lr_tabfield) = lr_original_tabfields->get_next_entry( ).

      ASSIGN lt_fieldcat[ fieldname = lr_tabfield->alv_fieldname ] TO <ls_fieldcat>.
      IF sy-subrc = 0.
        lr_tabfield->output_active = xsdbool( <ls_fieldcat>-no_out = abap_false ).
        lr_tabfield->output_order = <ls_fieldcat>-col_pos.
      ENDIF.

      ASSIGN lt_sort[ fieldname = lr_tabfield->alv_fieldname ] TO FIELD-SYMBOL(<ls_sort_criteria>).
      IF sy-subrc = 0.
        lr_tabfield->sort_active = abap_true.
        lr_tabfield->sort_direction = COND #( WHEN <ls_sort_criteria>-down = abap_true THEN
                                                zif_dbbr_global_consts=>gc_sort_direction-descending
                                              WHEN <ls_sort_criteria>-up = abap_true THEN
                                                zif_dbbr_global_consts=>gc_sort_direction-ascending ).
        lr_tabfield->sort_order = <ls_sort_criteria>-spos.
      ENDIF.
    ENDWHILE.

    mf_layout_transferred = abap_true.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    CHECK mf_has_parent = abap_false.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_output_rep_var_ids=>c_r_controller
            global_ref    = me )
        )
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel ##needed.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>output.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_selection_output.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
    DATA(lv_function_code) = cv_function_code.
    CLEAR cv_function_code.

    CASE lv_function_code.

      WHEN zif_dbbr_c_selection_functions=>show_details.
        mo_alv_grid->get_current_cell( IMPORTING es_row_id = DATA(ls_row_id) es_row_no = DATA(ls_row_no) es_col_id = DATA(ls_col_id) ).
        on_double_click(
          EXPORTING
            e_row     = ls_row_id
            e_column  = ls_col_id
            es_row_no = ls_row_no
        ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>reset_alv_layout.
        reset_alv_layout( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>compare_selected_lines.
        compare_selected_lines( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>leave_screen_with_layout.
        transfer_layout_info_to_selscr( ).
        transfer_filter_values( if_leave_screen = abap_false ).
        zif_uitb_screen_controller~free_screen_resources( ).
        zcl_dbbr_screen_helper=>leave_screen( ).
        RETURN.

      WHEN zif_dbbr_global_consts=>gc_function_codes-leave_screen OR
           zif_dbbr_global_consts=>gc_function_codes-cancel_screen OR
           zif_dbbr_global_consts=>gc_function_codes-quit_program.

        IF lv_function_code = zif_dbbr_global_consts=>gc_function_codes-leave_screen AND
           mo_util->ms_technical_info-auto_layout_transfer = abap_true AND
           mo_util->mf_aggregation = abap_false AND
           mo_util->mf_group_by = abap_false.
          transfer_layout_info_to_selscr( ).
        ENDIF.
        zif_uitb_screen_controller~free_screen_resources( ).
        IF lv_function_code = zif_dbbr_global_consts=>gc_function_codes-quit_program.
          zcl_dbbr_screen_helper=>quit_program( ).
        ELSE.
          zcl_dbbr_screen_helper=>leave_screen( ).
        ENDIF.
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>refresh.
        refresh( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>save_selection_as_f4.
        save_as_f4( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>quick_filter.
        perform_quick_filter( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>quick_filter_exclusion.
        perform_quick_filter( if_exclude = abap_true ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>show_hidden_lines.
        show_hidden_rows( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>delete_hidden_lines.
        delete_hidden_lines( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>delete_colors_of_rows.
        delete_coloring_of_rows( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>delete_colors_of_columns.
        delete_coloring_of_columns( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>go_to_column.
        scroll_to_column( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>emphasize_negative_values.
        emphasize_negative_values( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>toggle_entity_info_header.
        display_top_header( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>hide_columns.
        mo_alv_grid->hide_selected_columns( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>transfer_filter_values.
        transfer_filter_values( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>group_by_selected_columns.
        group_by_selected_columns( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>remove_column_grouping.
        remove_grouping( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>display_db_browser_version.
        zcl_dbbr_version=>show_version( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>control_tech_view.
        control_tech_view( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>set_focus_to_list.
        cl_gui_control=>set_focus( mo_alv_grid ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>hide_cols_without_values.
        hide_cols_without_values( ).
        RETURN.

      WHEN zif_dbbr_c_selection_functions=>show_shortcuts.
        mo_alv_grid->show_active_default_shortcuts( ).
        RETURN.

      WHEN OTHERS.
        mo_util->handle_ui_function( CHANGING cv_function = lv_function_code ).

    ENDCASE.

    CHECK lv_function_code IS NOT INITIAL.

    " now handle alv grid function codes
    mo_alv_grid->set_function_code(
      CHANGING
        c_ucomm = lv_function_code
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    mo_util->zif_dbbr_screen_util~handle_pbo( if_first_call = mf_first_call ).

    IF mo_alv_grid IS NOT INITIAL.
*... mark first cell and focus on alv grid, after screen has been initially loaded
      IF mf_first_cell_marked = abap_false.
        mo_alv_grid->set_current_cell_via_id(
            is_row_no     = VALUE #( row_id = 1 )
        ).
        cl_gui_control=>set_focus( mo_alv_grid ).
        mf_first_cell_marked = abap_true.
      ENDIF.
    ELSE.
      init_grid_control( ).
      IF mf_first_cell_marked = abap_false.
        mo_alv_grid->set_current_cell_via_id(
            is_row_no     = VALUE #( row_id = 1 )
        ).
        cl_gui_control=>set_focus( mo_alv_grid ).
        mf_first_cell_marked = abap_true.
      ENDIF.
    ENDIF.

    zif_uitb_screen_controller~set_status( ).

*... reset flag to signal first screen call is over
    CLEAR mf_first_call.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    DATA: lt_excluding TYPE ui_functions.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    lt_excluding = mo_util->zif_dbbr_screen_util~get_deactivated_functions( ).

    IF mo_alv_grid IS NOT INITIAL.
      mo_alv_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
      IF lt_filter IS INITIAL.
        APPEND zif_dbbr_c_selection_functions=>clear_filter TO lt_excluding.
        APPEND zif_dbbr_c_selection_functions=>transfer_filter_values TO lt_excluding.
      ENDIF.
      mo_alv_grid->get_subtotals(
        IMPORTING
          ep_collect00 = DATA(lr_ref)
      ).
      ASSIGN lr_ref->* TO <lt_table>.
      IF sy-subrc = 0 AND <lt_table> IS INITIAL.
        APPEND zif_dbbr_c_selection_functions=>sum_column TO lt_excluding.
      ENDIF.
    ENDIF.

    IF mo_alv_grid IS INITIAL OR NOT mo_util->get_alv_util( )->is_column_grouping_active( ).
      lt_excluding = VALUE #( BASE lt_excluding ( zif_dbbr_c_selection_functions=>remove_column_grouping ) ).
    ENDIF.

    DATA(lv_select_type_text) = COND string( WHEN mo_util->is_join_active( ) THEN TEXT-t03 ELSE TEXT-t02 ).

    mo_alv_grid->set_excluded_functions( lt_excluding ).

    SET PF-STATUS 'OUTPUT_SELECTION' OF PROGRAM zif_dbbr_c_report_id=>output EXCLUDING lt_excluding.

    IF mo_alv_grid IS NOT INITIAL.
      DATA(lv_alv_filtered_entries) = mo_util->get_alv_util( )->get_filtered_count( ).
    ENDIF.

    DATA(lv_filtered_line_count) = COND #( WHEN lv_alv_filtered_entries = 0 THEN
                                             current_line_count( )
                                           ELSE
                                             current_line_count( ) - lv_alv_filtered_entries ).

    " differentiate headers of join and default/custom query selection
    IF mo_util->is_join_active( ) OR mo_util->mf_custom_query_active = abap_true.
      DATA(lv_selection_count_text) = |{ lv_filtered_line_count NUMBER = USER } Entries|.
    ELSE.
      lv_selection_count_text = |{ lv_filtered_line_count NUMBER = USER } of { mo_util->mv_max_lines_existing NUMBER = USER } Entries|.
    ENDIF.

    SET TITLEBAR 'OUTPUT_TITLE' OF PROGRAM zif_dbbr_c_report_id=>output WITH lv_select_type_text lv_selection_count_text.
  ENDMETHOD.


  METHOD hide_cols_without_values.
    DATA: lt_cols_to_hide TYPE RANGE OF lvc_fname,
          lf_hide_col     TYPE abap_bool,
          lv_type         TYPE char1.

    FIELD-SYMBOLS: <lt_data> TYPE table.

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    lt_cols_to_hide = VALUE #( FOR field IN lt_fieldcat WHERE ( no_out = abap_false AND tech = abap_false ) ( sign = 'I' option = 'EQ' low = field-fieldname ) ).

    ASSIGN mo_util->mr_t_data->* TO <lt_data>.

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
      MESSAGE s103(zdbbr_info).
      RETURN.
    ENDIF.

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WHERE fieldname IN lt_cols_to_hide.
      <ls_fcat>-no_out = abap_true.
    ENDLOOP.

    MESSAGE s102(zdbbr_info) WITH |{ lines( lt_cols_to_hide ) }|.

    mo_alv_grid->set_frontend_fieldcatalog( lt_fieldcat ).

    mo_alv_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.

ENDCLASS.
