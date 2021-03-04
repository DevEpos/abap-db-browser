"! <p class="shorttext synchronized" lang="en">ALV Output</p>
CLASS zcl_dbbr_output_grid DEFINITION
  PUBLIC
  INHERITING FROM cl_gui_alv_grid
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Sortierte Felder haben sich geändert</p>
    EVENTS sorted_fields_changed .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !ir_parent TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">CLASS CONSTRUCTOR</p>
    CLASS-METHODS class_constructor.
    METHODS execute_user_command
      IMPORTING
        !iv_function_code TYPE sy-ucomm .
    "! <p class="shorttext synchronized" lang="en">Retrieves all selected rows</p>
    METHODS get_all_selected_rows
      RETURNING
        VALUE(result) TYPE lvc_t_indx .
    "! <p class="shorttext synchronized" lang="en">Gibt 'X' zurück wenn mind. 1 Spalte selektiert ist</p>
    METHODS has_selected_columns
      RETURNING
        VALUE(rf_columns_selected) TYPE boolean .
    METHODS hide_selected_columns .
    "! <p class="shorttext synchronized" lang="en">Optimiert die Spaltenbreite</p>
    METHODS optimize_columns .
    METHODS set_column_names
      IMPORTING
        !if_tech_names TYPE boolean .
    METHODS dispatch
        REDEFINITION .
    METHODS get_variant_fieldcat
      RETURNING
        VALUE(rt_fieldcat) TYPE lvc_t_fcat.
    METHODS set_sort_criteria
        REDEFINITION .
    "! <p class="shorttext synchronized" lang="en">Sets the toolbar functions</p>
    METHODS set_toolbar_functions
      IMPORTING
        it_functions TYPE ttb_button
        it_menus     TYPE ttb_btnmnu OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Excludes the given functions from ALV</p>
    METHODS set_excluded_functions
      IMPORTING
        it_functions TYPE ui_functions.
    "! <p class="shorttext synchronized" lang="en">Sets the default toolbar buttons</p>
    METHODS set_default_toolbar.
    "! <p class="shorttext synchronized" lang="en">Shows the defined default GUI Status shortcuts</p>
    METHODS show_active_default_shortcuts.

    "! <p class="shorttext synchronized" lang="en">Changes the number of fixed rows</p>
    METHODS change_fixed_rows
      IMPORTING
        iv_rows           TYPE i OPTIONAL
        if_from_selection TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Copy table content as ABAP Value Statement</p>
    "!
    "! @parameter if_compact | <p class="shorttext synchronized" lang="en">Compact mode on/off</p>
    METHODS copy_as_value_statement
      IMPORTING
        if_compact TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Returns range of selected columns</p>
    METHODS get_selected_cols_range
      RETURNING
        VALUE(rt_col_range) TYPE zif_sat_ty_global=>ty_t_selopt.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_toolbar_buttons TYPE ttb_button.
    DATA mt_toolbar_menu TYPE ttb_btnmnu.
    DATA mt_excluded TYPE RANGE OF ui_func.

    TYPES: BEGIN OF ty_s_tb_button.
             INCLUDE TYPE stb_button.
             TYPES: fkey TYPE ui_func.
    TYPES:END OF ty_s_tb_button.

    CLASS-DATA gt_shortcuts_map TYPE zif_uitb_ty_gui_screen=>ty_t_fkey_map.
    CLASS-DATA gt_default_tb_buttons TYPE STANDARD TABLE OF ty_s_tb_button WITH EMPTY KEY.
    CLASS-DATA gt_default_tb_menu TYPE ttb_btnmnu.

    "! <p class="shorttext synchronized" lang="en">Retrieves the currently active toolbar buttons</p>
    METHODS get_active_toolbar_buttons
      RETURNING
        VALUE(rt_buttons) TYPE ttb_button.
    "! <p class="shorttext synchronized" lang="en">Handler for toolbar build event</p>
    METHODS on_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_interactive
        e_object.
    METHODS on_toolbar_button
        FOR EVENT toolbar_button_click OF cl_gui_alv_grid
      IMPORTING
        fcode.

ENDCLASS.



CLASS zcl_dbbr_output_grid IMPLEMENTATION.


  METHOD class_constructor.
*.. Create default toolbar buttons
    gt_default_tb_buttons = VALUE #(
      ( function  = zif_dbbr_c_selection_functions=>edit_data
        icon      = icon_change
        quickinfo = |{ 'Edit Data'(002) }|
        fkey = zif_uitb_c_gui_screen=>c_functions-shift_f6 )
      ( butn_type = cntb_btype_sep )
      ( function  = zif_dbbr_c_selection_functions=>toggle_entity_info_header
        icon      = icon_overview
        quickinfo = |{ 'Show / Hide Additional Info'(003) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-f7 )
      ( butn_type = cntb_btype_sep )
      ( function  = zif_dbbr_c_selection_functions=>refresh
        icon      = icon_refresh
        quickinfo = |{ 'Refresh'(004) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-f8 )
      ( function  = zif_dbbr_c_selection_functions=>change_max_row_count
        text      = |{ 'Max Entries'(001) }| quickinfo = |{ 'Change Max. Number of Entries'(005) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-f5 )
      ( function  = zif_dbbr_c_selection_functions=>change_cds_parameters
        icon      = icon_parameter_export
        quickinfo = |{ 'Change Parameters'(007) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-f6 )
      ( butn_type = cntb_btype_sep )
      ( function  = zif_dbbr_c_selection_functions=>show_users_settings
        icon      = icon_personal_settings
        quickinfo = |{ 'Show Settings'(008) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f11 )
      ( butn_type = cntb_btype_sep )
      ( function  = cl_gui_alv_grid=>mc_fc_sort_asc
        icon      = icon_sort_up
        quickinfo = |{ 'Sort Up'(009) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_f4 )
      ( function  = cl_gui_alv_grid=>mc_fc_sort_dsc
        icon      = icon_sort_down
        quickinfo = |{ 'Sort Down'(010) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f4 )
      ( butn_type = cntb_btype_sep )
      ( function  = cl_gui_alv_grid=>mc_fc_filter
        icon      = icon_filter
        quickinfo = |{ 'Set Filter'(011) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_f5 )
      ( function  = zif_dbbr_c_selection_functions=>quick_filter
        icon      = icon_select_with_condition  text = |{ 'Quick-Filter'(012) }|
        quickinfo = |{ 'Filter with chosen Table Cells'(013) }|
        butn_type = cntb_btype_dropdown
        fkey      = zif_uitb_c_gui_screen=>c_functions-f9 )
      ( function  = cl_gui_alv_grid=>mc_fc_delete_filter
        icon      = icon_filter_undo
        quickinfo = |{ 'Remove Filters'(014) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f2  )
      ( function  = zif_dbbr_c_selection_functions=>transfer_filter_values
        icon      = icon_filter
        text      = |{ 'Transfer'(016) }|
        quickinfo = |{ 'Transfer Filters to Sel.Scrn'(015) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-shift_f12 )
      ( butn_type = cntb_btype_sep )
      ( function  = cl_gui_alv_grid=>mc_fc_sum
        icon      = icon_sum
        quickinfo = |{ 'Total'(017) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_f6 )
      ( function  = cl_gui_alv_grid=>mc_fc_subtot
        icon      = icon_intermediate_sum
        quickinfo = |{ 'Sub Totals'(018) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f6 )
      ( butn_type = cntb_btype_sep )
      ( function  = zif_dbbr_c_selection_functions=>go_to_column
        icon      = icon_next_page
        text      = |{ 'to Column'(019) }|
        quickinfo = |{ 'Scroll to Column'(057) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-shift_f4 )
      ( function  = 'ROWS'
        icon      = icon_settings
        text      = |{ 'Rows'(020) }|
        quickinfo = |{ TEXT-020 }|
        butn_type = cntb_btype_menu )
      ( function  = 'COLS'
        icon      = icon_settings
        text      = |{ 'Columns'(021) }|
        quickinfo = |{ TEXT-021 }|
        butn_type = cntb_btype_menu )
      ( butn_type = cntb_btype_sep )
      ( function  = cl_gui_alv_grid=>mc_fc_call_xxl
        icon      = icon_xxl
        text      = |{ 'Export'(022) }|
        quickinfo = |{ 'Export data to Excel...'(023) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-shift_f8 )
      ( function  = cl_gui_alv_grid=>mc_fc_current_variant
        icon      = icon_alv_variants
        quickinfo = |{ 'Change Layout'(024) }|
        butn_type = cntb_btype_dropdown
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_f8 )
      ( function  = cl_gui_alv_grid=>mc_fc_load_variant
        icon      = icon_alv_variant_choose
        quickinfo = |{ 'Choose Layout'(025) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_f9 )
      ( function  = cl_gui_alv_grid=>mc_fc_save_variant
        icon      = icon_alv_variant_save
        quickinfo = |{ 'Save Layout'(026) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_f10 )
      ( butn_type = cntb_btype_sep )
      ( function  = zif_dbbr_c_selection_functions=>control_tech_view
        icon      = icon_active_inactive
        text      = |{ text-027 }|
        quickinfo = |{ 'Technical Column Names on/off'(028) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-shift_f7 )
      ( function  = zif_dbbr_c_selection_functions=>compare_selected_lines
        icon      = icon_compare
        quickinfo = |{ 'Compare selected Rows'(029) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f7 )
      ( function  = zif_dbbr_c_selection_functions=>remove_column_grouping
        icon      = icon_delete
        text      = |{ 'Remove Grouping'(030) }|
        quickinfo = |{ 'Remove Column Grouping'(031) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f5 )
      ( butn_type = cntb_btype_sep )
      ( function  = zif_dbbr_c_selection_functions=>navigate_association
        icon      = icon_workflow_fork
        quickinfo = |{ 'Navigate to Association'(033) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-shift_f5 )
      ( function  = zif_dbbr_c_selection_functions=>show_cds_source
        icon      = icon_biw_info_object
        text      = |{ 'Source' }|
        quickinfo = |{ 'Show CDS Source Code'(052) }|
        fkey      = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f10 )
      ( butn_type = cntb_btype_sep )
      ( function  = zif_dbbr_c_selection_functions=>open_in_sql_console
        icon      = icon_edit_file
        quickinfo = |{ 'Open in SQL Console'(056) }| )
      ( butn_type = cntb_btype_sep )
      ( function  = 'COPY_MENU'
        icon      = icon_system_copy
        butn_type = cntb_btype_menu
        text      = 'Copy as...' )
    ).

*.. Fill shortcut mapping from default toolbar buttons
    gt_shortcuts_map = VALUE #( FOR bt IN gt_default_tb_buttons
                                WHERE ( fkey IS NOT INITIAL )
                                ( fkey            = bt-fkey
                                  mapped_function = bt-function
                                  text            = COND #( WHEN bt-quickinfo IS NOT INITIAL THEN bt-quickinfo ELSE bt-text ) ) ).

*.. Add some additional shortcut mappings for which there are no buttons
    gt_shortcuts_map = VALUE #( BASE gt_shortcuts_map
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-f2
        text            = |{ 'Show Line Details'(046) }|  )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_f11
        mapped_function = zif_dbbr_c_selection_functions=>save_selection_as_f4
        text            = |{ 'Save as Value Help'(047) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f12
        mapped_function = zif_dbbr_c_selection_functions=>show_sql_of_select
        text            = |{ 'Show SQL of current Selection'(048) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f2
        text            = |{ 'Hide selected Columns'(049) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_f1
        mapped_function = zif_dbbr_c_selection_functions=>set_focus_to_list
        text            = |{ 'Set Focus to List'(050) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_f2
        mapped_function = zif_dbbr_c_selection_functions=>set_focus_to_assoc_list
        text            = |{ 'Set Focus to Associations'(051) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f9
        mapped_function = zif_dbbr_c_selection_functions=>delete_filters_from_cols
        text            = |{ 'Delete Filters from selected Columns'(053) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f8
        mapped_function = zif_dbbr_c_selection_functions=>copy_as_val_stmnts
        text            = |{ 'Copy Rows as Value statement'(055) }| )
    ).

*.. Create and fill button menus
    DATA(lo_quickfilt_menu) = NEW cl_ctmenu( ).
    lo_quickfilt_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>quick_filter_exclusion
        text  = |{ 'Quickfilter for Selection (Inverted)'(034) }|
    ).
    gt_shortcuts_map = VALUE #( BASE gt_shortcuts_map ( text            = TEXT-034
                                                        fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f9
                                                        mapped_function = zif_dbbr_c_selection_functions=>quick_filter_exclusion ) ).

    DATA(lo_rows_menu) = NEW cl_ctmenu( ).
    lo_rows_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>show_hidden_lines
        text  = |{ 'Show hidden Rows'(035) }|
    ).
    lo_rows_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>delete_hidden_lines
        text  = |{ 'Discard hidden Rows'(036) }|
    ).
    lo_rows_menu->add_separator( ).
    lo_rows_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>delete_colors_of_rows
        text  = |{ 'Remove all row colors'(037) }|
    ).
    lo_rows_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>emphasize_negative_values
        text  = |{ 'Mark negative values'(038) }|
    ).
    lo_rows_menu->add_separator( ).
    lo_rows_menu->add_function(
       fcode  = zif_dbbr_c_selection_functions=>remove_fixed_rows
       text   = |{ 'Remove fixed rows'(054) }|
    ).

    DATA(lo_cols_menu) = NEW cl_ctmenu( ).
    lo_cols_menu->add_function(
        fcode = cl_gui_alv_grid=>mc_fc_col_optimize
        text  = |{ 'Optimize Column Width' }|
    ).
    lo_cols_menu->add_separator( ).
    lo_cols_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>manage_text_fields
        text  = |{ 'Manage Text Fields' }|
    ).
    lo_cols_menu->add_separator( ).
    lo_cols_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>group_by_selected_columns
        text  = |{ 'Group selected Columns'(039) }|
    ).
    lo_cols_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>remove_column_grouping
        text  = |{ TEXT-030 }|
    ).
    lo_cols_menu->add_separator( ).
    lo_cols_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>disable_chkbox_col_style_all
        text  = |{ 'Disable checkbox cell style' }|
    ).
    lo_cols_menu->add_separator( ).
    lo_cols_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>delete_colors_of_columns
        text  = |{ 'Remove all column colors'(040) }|
    ).
    lo_cols_menu->add_separator( ).
    lo_cols_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>hide_cols_without_values
        text  = |{ 'Hide all Columns where no values exist'(041) }|
    ).
    lo_cols_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>show_all_columns
        text  = |{ 'Show all Columns'(058) }|
    ).

    DATA(lo_variant_menu) = NEW cl_ctmenu( ).
    lo_variant_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>reset_alv_layout
        text  = |{ 'Reset ALV layout'(042) }|
    ).
    lo_variant_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>leave_screen_with_layout
        text  = |{ 'Leave Screen (+ Transfer Layout)'(043) }|
    ).

    DATA(lo_copy_menu) = NEW cl_ctmenu( ).
    lo_copy_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>copy_as_val_stmnts
        text  = |{ 'Copy as VALUE Statement' }|
    ).
    lo_copy_menu->add_function(
        fcode = zif_dbbr_c_selection_functions=>copy_as_val_stmnt_compact
        text  = |{ 'Copy as VALUE Statement (Compact)' }|
    ).
    gt_shortcuts_map = VALUE #( BASE gt_shortcuts_map ( text            = TEXT-043
                                                        fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f3
                                                        mapped_function = zif_dbbr_c_selection_functions=>leave_screen_with_layout ) ).

    gt_default_tb_menu = VALUE #(
      ( function = zif_dbbr_c_selection_functions=>quick_filter ctmenu = lo_quickfilt_menu )
      ( function = cl_gui_alv_grid=>mc_fc_current_variant       ctmenu = lo_variant_menu )
      ( function = 'ROWS'                                       ctmenu = lo_rows_menu )
      ( function = 'COLS'                                       ctmenu = lo_cols_menu )
      ( function = 'COPY_MENU'                                  ctmenu = lo_copy_menu )
    ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor(
        i_parent        = ir_parent
        i_lifetime      = cl_gui_control=>lifetime_dynpro
        i_fcat_complete = abap_true
    ).

    SET HANDLER:
      on_toolbar FOR me,
      on_toolbar_button FOR me.
  ENDMETHOD.


  METHOD dispatch.
*& Description: Redefined because of additional event handling
*&---------------------------------------------------------------------*

    DATA: lv_action TYPE sy-ucomm.

    get_event_parameter( EXPORTING parameter_id = 0
                                   queue_only   = space
                         IMPORTING parameter    = lv_action ).

    super->dispatch(
      EXPORTING
        cargo             = cargo
        eventid           = eventid
        is_shellevent     = is_shellevent
        is_systemdispatch = is_systemdispatch
      EXCEPTIONS
        cntl_error        = 1
        OTHERS            = 2
    ).
    IF sy-subrc <> 0.
*.... Implement suitable error handling here
      RETURN.
    ENDIF.
*    ENDIF.

*.. handle specific function codes
    CASE lv_action.
      WHEN mc_mb_variant OR
           mc_fc_current_variant OR
           mc_fc_load_variant OR
           mc_fc_save_variant OR
           mc_fc_maintain_variant OR
           mc_fc_variant_admin OR
           mc_fc_sort OR
           mc_fc_sort_asc OR
           mc_fc_sort_dsc.
        IF eventid <> evt_toolbar_menubutton_click.
          RAISE EVENT sorted_fields_changed.

          " refresh the table
*          refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true )
*                                        i_soft_refresh = abap_true ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD execute_user_command.
    fcode_bouncer( ).
  ENDMETHOD.


  METHOD get_active_toolbar_buttons.
    rt_buttons = mt_toolbar_buttons.
*.. Remove exluded functions
    IF mt_excluded IS NOT INITIAL.
      DELETE rt_buttons WHERE function IN mt_excluded.
    ENDIF.
*.. Remove adjacent separators
    DATA(lf_sep_found) = abap_false.

    LOOP AT rt_buttons ASSIGNING FIELD-SYMBOL(<ls_button>).
      IF lf_sep_found = abap_true.
        IF <ls_button>-butn_type = cntb_btype_sep.
          DELETE rt_buttons.
        ELSE.
          CLEAR lf_sep_found.
        ENDIF.
      ELSE.
        lf_sep_found = xsdbool( <ls_button>-butn_type = cntb_btype_sep ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_all_selected_rows.
    get_selected_rows(
      IMPORTING
        et_index_rows = DATA(lt_rows)
    ).
    get_selected_cells(
      IMPORTING
        et_cell = DATA(lt_cells)
    ).

    result = VALUE #(
      FOR cell IN lt_cells
      ( cell-row_id-index  )
    ).

    result = VALUE #( BASE result
      FOR row IN lt_rows
      ( row-index )
    ).

    SORT result.
    DELETE ADJACENT DUPLICATES FROM result.
  ENDMETHOD.


  METHOD get_variant_fieldcat.
    get_internal_fieldcat( IMPORTING et_fieldcatalog = rt_fieldcat ).

    set_toolbar_buttons(
      EXPORTING
        toolbar_table = mt_toolbar_buttons
*      EXCEPTIONS
*        error         = 1
*        others        = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.


  METHOD has_selected_columns.
    get_selected_columns( IMPORTING et_index_columns = DATA(lt_col_index) ).

    rf_columns_selected = xsdbool( lt_col_index IS NOT INITIAL ).
  ENDMETHOD.


  METHOD hide_selected_columns.

    get_scroll_info_via_id( IMPORTING es_row_info = DATA(ls_row_id)
                                      es_col_info = DATA(ls_col_id) ).

    get_selected_columns( IMPORTING et_index_columns = DATA(lt_index_columns) ).

    IF me->is_cache_valid( ) NE abap_true OR www_active EQ abap_true.
      cl_gui_cfw=>flush( ).
    ENDIF.

    get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    IF lt_index_columns IS NOT INITIAL.
      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        IF line_exists( lt_index_columns[ fieldname = <ls_fieldcat>-fieldname ] ).
          <ls_fieldcat>-no_out = 'X'.
        ENDIF.
      ENDLOOP.

      " update fieldcatalog
      set_frontend_fieldcatalog( lt_fieldcat ).

      set_scroll_info_via_id(
          is_row_info = ls_row_id
          is_col_info = ls_col_id
      ).
      refresh_table_display( ).
    ELSE.
      MESSAGE s005(0k).
    ENDIF.
  ENDMETHOD.


  METHOD on_toolbar.
    e_object->mt_btnmnu =  mt_toolbar_menu.
    e_object->mt_toolbar = mt_toolbar_buttons.

*.. Remove exluded functions
    IF mt_excluded IS NOT INITIAL.
      DELETE e_object->mt_toolbar WHERE function IN mt_excluded.
    ENDIF.
*.. Remove adjacent separators
    DATA(lf_sep_found) = abap_false.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<ls_button>).
      IF lf_sep_found = abap_true.
        IF <ls_button>-butn_type = cntb_btype_sep.
          DELETE e_object->mt_toolbar.
        ELSE.
          CLEAR lf_sep_found.
        ENDIF.
      ELSE.
        lf_sep_found = xsdbool( <ls_button>-butn_type = cntb_btype_sep ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_toolbar_button.

  ENDMETHOD.


  METHOD optimize_columns.
    optimize_all_cols(
*      EXPORTING
*        include_header = 1    " Spaltenüberschriften berücksichtigen (0=Nein, 1=Ja)
*      EXCEPTIONS
*        error          = 1
*        others         = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD set_column_names.
    DATA: lv_tooltip TYPE lvc_tip,
          lv_coltext TYPE lvc_txtcol.
    get_internal_variant(
      IMPORTING
        es_variant = DATA(ls_variant)
    ).
    get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fcat) ).

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      DATA(ls_fcat) = <ls_fcat>.
      <ls_fcat>-tooltip = ls_fcat-coltext.
      <ls_fcat>-coltext = ls_fcat-tooltip.
    ENDLOOP.

    set_frontend_fieldcatalog( lt_fcat ).

    refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.


  METHOD set_default_toolbar.
    set_toolbar_functions(
        it_functions = CORRESPONDING #( gt_default_tb_buttons )
        it_menus     = gt_default_tb_menu
    ).
  ENDMETHOD.


  METHOD set_excluded_functions.
    mt_excluded = VALUE #( FOR function IN it_functions ( sign = 'I' option = 'EQ' low = function ) ).
    LOOP AT mt_excluded ASSIGNING FIELD-SYMBOL(<ls_excluded>).
      CASE <ls_excluded>-low.

        WHEN '&OUP'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_sort_asc.

        WHEN '&ODN'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_sort_dsc.

        WHEN '&OL0'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_variant_admin.

        WHEN '&OAD'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_load_variant.

        WHEN '&AVE'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_save_variant.

        WHEN '&ILT'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_filter.

        WHEN '&ILD'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_delete_filter.

        WHEN '&UMC'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_sum.

        WHEN '&SUM'.
          <ls_excluded>-low = cl_gui_alv_grid=>mc_fc_subtot.
      ENDCASE.
    ENDLOOP.

    set_toolbar_buttons(
      EXPORTING
        toolbar_table = get_active_toolbar_buttons( )
*      EXCEPTIONS
*        error         = 1
*        others        = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.


  METHOD set_sort_criteria.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/01
*&---------------------------------------------------------------------*
    super->set_sort_criteria( EXPORTING it_sort = it_sort ).

    RAISE EVENT sorted_fields_changed.
  ENDMETHOD.


  METHOD set_toolbar_functions.
    mt_toolbar_buttons = it_functions.
    mt_toolbar_menu = it_menus.
  ENDMETHOD.


  METHOD show_active_default_shortcuts.
    DATA: lt_active_buttons_range TYPE RANGE OF ui_func.

    DATA(lt_shortcuts) = gt_shortcuts_map.

    IF mt_excluded IS NOT INITIAL.
      DELETE lt_shortcuts WHERE mapped_function IN mt_excluded.
    ENDIF.

    zcl_uitb_app_shortcuts_viewer=>display_shortcuts( it_shortcuts = lt_shortcuts ).

  ENDMETHOD.

  METHOD change_fixed_rows.
    IF if_from_selection = abap_true.
      get_selected_rows( IMPORTING et_index_rows = DATA(lt_row) ).
      CHECK lt_row IS NOT INITIAL.
      set_fixed_rows( CONV #( lt_row[ lines( lt_row ) ]-index ) ).
    ELSE.
      set_fixed_rows( iv_rows ).
    ENDIF.
  ENDMETHOD.


  METHOD copy_as_value_statement.
    TYPES: lty_value_line TYPE string.

    DATA: lt_value_stmnt          TYPE TABLE OF lty_value_line,
          lv_value_stmnt          TYPE lty_value_line,
          lv_value                TYPE string,
          lr_clipboard_export_tab TYPE REF TO data,
          lv_rc                   TYPE i.

    FIELD-SYMBOLS: <lt_data> TYPE table.

    get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
    DELETE lt_fieldcat WHERE tech   = abap_true OR
                             no_out = abap_true.
    ASSIGN mt_outtab->* TO <lt_data>.

    lt_value_stmnt = VALUE #( ( |VALUE #(| ) ).

    get_filtered_entries( IMPORTING et_filtered_entries = DATA(lt_filtered_entries) ).

    DATA(lv_copied_rows) = 0.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_line>).
      IF lt_filtered_entries IS NOT INITIAL.
        CHECK NOT line_exists( lt_filtered_entries[ table_line = sy-tabix ] ).
      ENDIF.


      lv_value_stmnt = |  (|.
      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_cell_value>).
        CHECK sy-subrc = 0.
        lv_value = |'{ <lv_cell_value> }'|.
        IF if_compact = abap_true.
          lv_value_stmnt = |{ lv_value_stmnt } { <ls_fieldcat>-fieldname  } = { lv_value }|.
        ELSE.
          lv_value_stmnt = |{ lv_value_stmnt } { <ls_fieldcat>-fieldname  } = { lv_value WIDTH = <ls_fieldcat>-outputlen + 2 }#|.
        ENDIF.
      ENDLOOP.

      lv_value_stmnt = lv_value_stmnt && | )|.
      REPLACE ALL OCCURRENCES OF '#' IN lv_value_stmnt WITH space.
      lt_value_stmnt = VALUE #( BASE lt_value_stmnt ( lv_value_stmnt ) ).
      ADD 1 TO lv_copied_rows.
    ENDLOOP.

    lt_value_stmnt = VALUE #( BASE lt_value_stmnt ( |).| ) ).

*.. Determine line with maximum length
    DATA(lv_max_char) = 0.
    LOOP AT lt_value_stmnt ASSIGNING FIELD-SYMBOL(<lv_value_line>).
      DATA(lv_length) = strlen( <lv_value_line> ).
      IF lv_length > lv_max_char.
        lv_max_char = lv_length.
      ENDIF.
    ENDLOOP.

    DATA(lo_line_type_descr) = cl_abap_elemdescr=>get_c( lv_max_char ).
    DATA(lo_table_descr) = cl_abap_tabledescr=>get(
        p_line_type  = lo_line_type_descr
    ).

    CREATE DATA lr_clipboard_export_tab TYPE HANDLE lo_table_descr.
    ASSIGN lr_clipboard_export_tab->* TO FIELD-SYMBOL(<lt_clipboard_data>).

    IF sy-subrc = 0.
      MOVE-CORRESPONDING lt_value_stmnt TO <lt_clipboard_data>.
    ENDIF.

    CHECK <lt_clipboard_data> IS NOT INITIAL.

    cl_gui_frontend_services=>clipboard_export(
*      EXPORTING
*        no_auth_check        = space
      IMPORTING
        data                 = <lt_clipboard_data>
      CHANGING
        rc                   = lv_rc
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE |Copied VALUE #( ) Statement of { lv_copied_rows } lines to Clipboard| TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD get_selected_cols_range.
    get_selected_columns( IMPORTING et_index_columns = DATA(lt_cols) ).
    rt_col_range = VALUE #( FOR col IN lt_cols ( sign = 'I' option = 'EQ' low = col-fieldname ) ).
  ENDMETHOD.

ENDCLASS.
