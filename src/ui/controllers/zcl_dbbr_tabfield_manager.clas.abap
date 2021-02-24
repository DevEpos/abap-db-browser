CLASS zcl_dbbr_tabfield_manager DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    CONSTANTS:
      BEGIN OF c_tabs,
        all_fields_tab    TYPE dynfnam VALUE 'ALL_FIELDS',
        filter_fields_tab TYPE dynfnam VALUE 'FILTER_FIELDS',
      END OF c_tabs .
    CONSTANTS:
      BEGIN OF c_screens,
        main_screen          TYPE sy-dynnr VALUE '0800',
        all_fields_screen    TYPE sy-dynnr VALUE '0801',
        filter_fields_screen TYPE sy-dynnr VALUE '0802',
      END OF c_screens .
    CONSTANTS:
      BEGIN OF c_fcode,
        select_fields        TYPE sy-ucomm VALUE 'SEL_ALL',
        select_key_fields    TYPE sy-ucomm VALUE 'SEL_KEY',
        deselect_fields      TYPE sy-ucomm VALUE 'DE_SEL_ALL',
        delete_selected      TYPE sy-ucomm VALUE 'DEL_FIELD',
        sort_in_ddic         TYPE sy-ucomm VALUE 'SORT_DDIC',
        move_node_up         TYPE sy-ucomm VALUE 'MOVE_UP',
        move_node_down       TYPE sy-ucomm VALUE 'MOVE_DOWN',
        move_node_top        TYPE sy-ucomm VALUE 'MOVE_TOP',
        move_node_bottom     TYPE sy-ucomm VALUE 'MOVE_BOTTM',
        page_down            TYPE sy-ucomm VALUE 'PAGE_DOWN',
        page_up              TYPE sy-ucomm VALUE 'PAGE_UP',
        page_top             TYPE sy-ucomm VALUE 'PAGE_TOP',
        page_bottom          TYPE sy-ucomm VALUE 'PAGE_BOTTM',
        select_text_fields   TYPE sy-ucomm VALUE 'SEL_T_ALL',
        deselect_text_fields TYPE sy-ucomm VALUE 'DESEL_T_AL',
        insert_association   TYPE sy-ucomm VALUE 'ADDASSOC',
      END OF c_fcode .

    METHODS constructor
      IMPORTING
        !io_fields            TYPE REF TO zcl_dbbr_tabfield_list
        !io_original_fields   TYPE REF TO zcl_dbbr_tabfield_list
        !if_field_aggregation TYPE boolean OPTIONAL
        !io_cds_view          TYPE REF TO zcl_sat_cds_view OPTIONAL
        !iv_mode              TYPE zdbbr_field_chooser_mode
        !iv_entity_type       TYPE zsat_entity_type
        !is_join_def          TYPE zdbbr_join_def OPTIONAL .
    METHODS show_output_fields .
    METHODS show_available_fields .
    METHODS data_should_be_transferred
      RETURNING
        VALUE(rf_transfer_data) TYPE boolean .
    METHODS retrieve_current_data
      RETURNING
        VALUE(rr_current_fields) TYPE REF TO zcl_dbbr_tabfield_list .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    TYPES:
      BEGIN OF ty_table_overview.
        INCLUDE TYPE zdbbr_entity_info.
        TYPES: style TYPE  lvc_t_styl.
    TYPES: x_color  TYPE  char4.
    TYPES: END OF ty_table_overview .

    DATA mr_overview_dock TYPE REF TO cl_gui_docking_container .
    DATA mr_dd_alv_list TYPE REF TO cl_alv_dd_listbox .
    DATA:
      mt_tableoverview TYPE STANDARD TABLE OF ty_table_overview .
    DATA mt_fieldcat TYPE lvc_t_fcat .
    DATA mo_select_field_tree TYPE REF TO zcl_dbbr_field_select_tree .
    DATA mo_output_field_tree TYPE REF TO zcl_dbbr_field_output_tree .
    DATA mo_fields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_original_fields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_fields_cache TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mv_mode TYPE zdbbr_field_chooser_mode .
    DATA mf_output_tree_updated TYPE boolean .
    DATA mf_select_tree_updated TYPE boolean .
    DATA mf_ignore_pai TYPE boolean .
    DATA mf_field_aggregation TYPE boolean .
    DATA mf_transfer_data TYPE boolean .
    DATA mv_current_table TYPE tabname .
    DATA mv_entity_type TYPE zsat_entity_type .
    DATA mf_single_table_mode TYPE boolean .
    DATA mr_global_data TYPE REF TO zdbbr_global_data .
    DATA mr_active_tab TYPE REF TO syst_ucomm .
    DATA mr_tree_tab_control TYPE REF TO cxtab_tabstrip .
    DATA mr_tree_screen TYPE REF TO syst_dynnr .
    DATA mr_filter_fields_tab TYPE REF TO zdbbr_button .
    DATA mo_cds_view TYPE REF TO zcl_sat_cds_view .

    METHODS create_alv_grid .
    METHODS display_alv_grid .
    METHODS create_table_overview .
    METHODS filter_fields_for_table .
    METHODS create_table_overview_data .
    METHODS get_current_tab_controller
      RETURNING
        VALUE(rr_controller) TYPE REF TO zif_uitb_page_scroller .
    METHODS get_current_content_searcher
      RETURNING
        VALUE(rr_content_searcher) TYPE REF TO zif_uitb_content_searcher .
    METHODS on_output_tree_updated
        FOR EVENT tree_data_updated OF zcl_dbbr_field_output_tree .
    METHODS on_select_tree_updated
        FOR EVENT tree_data_updated OF zcl_dbbr_field_select_tree .
    METHODS on_table_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        !es_row_no
        !e_column_id
        !e_row_id .
    METHODS on_table_data_changed_finished
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        !et_good_cells
        !e_modified .
ENDCLASS.



CLASS zcl_dbbr_tabfield_manager IMPLEMENTATION.


  METHOD constructor.

*... retrieve some references to global data
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_global_data = CAST zdbbr_global_data( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).
    mr_active_tab = CAST syst_ucomm( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_active_tab ) ).
    mr_tree_tab_control = CAST cxtab_tabstrip( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_tree_tab ) ).
    mr_tree_screen = CAST syst_dynnr( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_tree_screen ) ).
    mr_filter_fields_tab = CAST zdbbr_button( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_bt_filter_fields ) ).
    mv_entity_type = iv_entity_type.

    mv_mode = iv_mode.
    mo_cds_view = io_cds_view.
    mf_field_aggregation = if_field_aggregation.

    mo_fields = io_fields.
    mo_original_fields = io_original_fields.

*... set starting parameters before screen is initially called
    IF mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-output AND
       mf_field_aggregation = abap_true.
      mr_active_tab->* =
      mr_tree_tab_control->activetab = zcl_dbbr_tabfield_manager=>c_tabs-filter_fields_tab.
      mr_tree_screen->* = zcl_dbbr_tabfield_manager=>c_screens-filter_fields_screen.
    ELSE.
      mr_active_tab->* =
      mr_tree_tab_control->activetab = zcl_dbbr_tabfield_manager=>c_tabs-all_fields_tab.
      mr_tree_screen->* = zcl_dbbr_tabfield_manager=>c_screens-all_fields_screen.
    ENDIF.

    IF mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-selection AND
       is_join_def-tables IS NOT INITIAL.

      mf_single_table_mode = abap_true.
*... set current table to primary table as default mode
      DATA(lt_tables) = mo_fields->get_table_list( ).
      mv_current_table = lt_tables[ is_primary = abap_true ]-tabname_alias.

      CLEAR mo_fields_cache.

*... create table overview list befor extraction
      create_table_overview_data( ).
      mo_fields_cache = mo_fields->extract_fields( VALUE #( ( sign = 'E' option = 'EQ' low = mv_current_table ) ) ).
    ENDIF.

    mo_output_field_tree = COND #(
       WHEN mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-selection THEN
         NEW zcl_dbbr_field_output_tree(
            io_fields            = mo_fields
            io_original_fields   = mo_original_fields
            if_field_aggregation = if_field_aggregation
            if_single_table_mode = mf_single_table_mode
            iv_mode              = mv_mode
            iv_current_table     = mv_current_table
            iv_entity_type       = mv_entity_type
         )
       ELSE
         NEW zcl_dbbr_fld_outp_tree_out(
            io_fields            = mo_fields
            io_original_fields   = mo_original_fields
            if_field_aggregation = if_field_aggregation
            if_single_table_mode = mf_single_table_mode
            iv_mode              = mv_mode
            iv_current_table     = mv_current_table
            iv_entity_type       = mv_entity_type
         )
    ).

    mo_select_field_tree = NEW #(
      io_fields            = mo_fields
      iv_mode              = mv_mode
      if_single_table_mode = mf_single_table_mode
      iv_current_table     = mv_current_table
      iv_entity_type       = mv_entity_type
    ).

*... register event handlers for tree update
    SET HANDLER:
        on_output_tree_updated FOR mo_output_field_tree,
        on_select_tree_updated FOR mo_select_field_tree.

  ENDMETHOD.


  METHOD create_alv_grid.

    mr_dd_alv_list = NEW cl_alv_dd_listbox( i_parent = mr_overview_dock ).
    mr_dd_alv_list->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    " set event handlers
    SET HANDLER:
      on_table_hotspot_click FOR mr_dd_alv_list,
      on_table_data_changed_finished FOR mr_dd_alv_list.

*... create fieldcatalog
    mt_fieldcat = zcl_uitb_alv_fieldcat_util=>create_field_catalog( REF #( mt_tableoverview ) ).

    LOOP AT mt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      CASE <ls_fieldcat>-fieldname.

        WHEN 'ACTIVE_SELECTION'.
          <ls_fieldcat>-checkbox = abap_true.
          <ls_fieldcat>-edit = abap_true.
          <ls_fieldcat>-outputlen = 7.
          <ls_fieldcat>-coltext = 'Output'.

        WHEN 'TABNAME_ALIAS'.
          <ls_fieldcat>-hotspot = abap_true.
          <ls_fieldcat>-outputlen = 20.

        WHEN 'DESCRIPTION'.
          <ls_fieldcat>-outputlen = 40.

        WHEN OTHERS.
          <ls_fieldcat>-no_out = abap_true.
      ENDCASE.
    ENDLOOP.


  ENDMETHOD.


  METHOD create_table_overview.

    CHECK: mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-selection,
           mf_single_table_mode = abap_true,
           mr_overview_dock IS INITIAL.

    mr_overview_dock = NEW cl_gui_docking_container(
        side       = cl_gui_docking_container=>dock_at_top
        ratio      = 25
    ).

    create_alv_grid( ).
    display_alv_grid( ).


  ENDMETHOD.


  METHOD create_table_overview_data.

    mt_tableoverview = CORRESPONDING #(
      mo_fields->get_table_list( if_exclude_parameters        = abap_true
                                 if_exclude_fields_not_loaded = abap_true )
    ).

    SORT mt_tableoverview BY selection_order.

    LOOP AT mt_tableoverview ASSIGNING FIELD-SYMBOL(<ls_table>).
      IF <ls_table>-is_primary = abap_true.
        <ls_table>-x_color = zif_dbbr_c_global=>c_alv_colors-light_green.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD data_should_be_transferred.

    rf_transfer_data = mf_transfer_data.

  ENDMETHOD.


  METHOD display_alv_grid.


    DATA(ls_layout) = VALUE lvc_s_layo(
        sel_mode     = 'C'
        stylefname   = 'STYLE'
        no_rowmark   = abap_true
        info_fname   = zif_dbbr_c_special_out_columns=>line_color
    ).

    mr_dd_alv_list->set_table_for_first_display(
      EXPORTING is_layout       = ls_layout
      CHANGING  it_outtab       = mt_tableoverview
                it_fieldcatalog = mt_fieldcat
    ).

  ENDMETHOD.


  METHOD filter_fields_for_table.

    mo_select_field_tree->update_current_table( mv_current_table ).
    mo_select_field_tree->update_nodes( ).

    mo_output_field_tree->update_current_table( mv_current_table ).
    mo_output_field_tree->reset_update_ability( ).
    mo_output_field_tree->fill_tree( ).

  ENDMETHOD.


  METHOD get_current_content_searcher.

    rr_content_searcher = COND #(
        WHEN mr_active_tab->* = c_tabs-all_fields_tab THEN mo_select_field_tree
        WHEN mr_active_tab->* = c_tabs-filter_fields_tab THEN mo_output_field_tree
    ).

  ENDMETHOD.


  METHOD get_current_tab_controller.

    IF mr_active_tab->* = c_tabs-all_fields_tab.
      rr_controller = mo_select_field_tree.
    ELSE.
      rr_controller = mo_output_field_tree.
    ENDIF.

  ENDMETHOD.


  METHOD on_output_tree_updated.

    mf_output_tree_updated = abap_true.

  ENDMETHOD.


  METHOD on_select_tree_updated.

    mf_select_tree_updated = abap_true.

  ENDMETHOD.


  METHOD on_table_data_changed_finished.

    IF et_good_cells IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lr_s_entity_info) = REF #( mt_tableoverview[ et_good_cells[ 1 ]-row_id ] ).

    DATA(lf_checked_value) = CONV boolean( et_good_cells[ 1 ]-value ).

    IF lf_checked_value = abap_true.
      RETURN. " nothing to do
    ENDIF.

    IF lf_checked_value = abap_false.

      IF lr_s_entity_info->tabname_alias = mv_current_table.
        CLEAR lr_s_entity_info->x_color.

*... clear possible set active flags
        mo_fields->clear_active_flag( ).
*... set primary table as active table
        LOOP AT mt_tableoverview ASSIGNING FIELD-SYMBOL(<ls_overview_table>).
          IF to_upper( <ls_overview_table>-tabname_alias ) = mr_global_data->primary_table.
            <ls_overview_table>-x_color = zif_dbbr_c_global=>c_alv_colors-light_green.
          ENDIF.
        ENDLOOP.

        mv_current_table = mt_tableoverview[ is_primary = abap_true ]-tabname_alias.
        mo_fields->add_fields( mo_fields_cache ).
        mo_fields_cache->clear( ).

        mo_fields_cache = mo_fields->extract_fields( VALUE #( ( sign = 'E' option = 'EQ' low = mv_current_table ) ) ).

        filter_fields_for_table( ).
        mr_dd_alv_list->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
      ELSE.
*... flag was removed from table in the cache
        IF mo_fields_cache IS NOT INITIAL.
          mo_fields_cache->clear_active_flag( iv_tablename = lr_s_entity_info->tabname_alias ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD on_table_hotspot_click.

    " get current line
    IF line_exists( mt_tableoverview[ e_row_id-index ] ).
      DATA(lr_current_line) = REF #( mt_tableoverview[ e_row_id-index ] ).

      IF lr_current_line->active_selection = abap_false.
        RETURN.
      ENDIF.

      IF mv_current_table <> lr_current_line->tabname_alias.
        CLEAR mt_tableoverview[ tabname_alias = mv_current_table ]-x_color.
        lr_current_line->x_color = zif_dbbr_c_global=>c_alv_colors-light_green.

        mv_current_table = lr_current_line->tabname_alias.
        mo_fields->add_fields( mo_fields_cache ).
        mo_fields_cache->clear( ).

        mo_fields_cache = mo_fields->extract_fields( VALUE #( ( sign = 'E' option = 'EQ' low = mv_current_table ) ) ).
        filter_fields_for_table( ).

        mr_dd_alv_list->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD retrieve_current_data.
    " check if there any cached fields
    IF mo_fields_cache IS NOT INITIAL.
      mo_fields->add_fields( mo_fields_cache ).
      mo_fields_cache->clear( ).
      CLEAR mo_fields_cache.
    ENDIF.

    " update table list of tabfield list
    IF mt_tableoverview IS NOT INITIAL.
      LOOP AT mt_tableoverview ASSIGNING FIELD-SYMBOL(<ls_table>).
        <ls_table>-selection_order = sy-tabix.
      ENDLOOP.
      mo_fields->set_table_list( CORRESPONDING #( mt_tableoverview ) ).
    ENDIF.

    " update sort fields -> sorting will be removed if field is not longer an output field
    IF mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-output.
      mo_fields->initialize_iterator( ).
      WHILE mo_fields->has_more_lines( ).
        DATA(lr_current_field) = mo_fields->get_next_entry( ).
        IF lr_current_field->output_active = abap_false.
          CLEAR: lr_current_field->sort_active,
                 lr_current_field->sort_direction,
                 lr_current_field->sort_order.
        ENDIF.
      ENDWHILE.
    ENDIF.

    rr_current_fields = mo_fields.

  ENDMETHOD.


  METHOD show_available_fields.

    mo_select_field_tree->create_tree( ).

    " check if an update is needed
    IF mf_output_tree_updated = abap_true.

      mo_select_field_tree->refresh_from_model( ).
      CLEAR mf_output_tree_updated.
    ENDIF.

  ENDMETHOD.


  METHOD show_output_fields.

    IF mf_select_tree_updated = abap_true.

      mo_output_field_tree->reset_update_ability( ).
    ENDIF.

    mo_output_field_tree->fill_tree( ).
    CLEAR mf_select_tree_updated.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    DATA: lv_end_line TYPE i,
          lv_end_col  TYPE i.

    IF mv_mode <> zif_dbbr_c_global=>c_field_chooser_modes-output.
      lv_end_line = COND #( WHEN mf_single_table_mode = abap_true THEN  31 ).
      lv_end_col = 110.
    ENDIF.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_tabfield_manager
            global_ref    = me )
        )
        iv_start_line    = 2
        iv_start_column  = 10
        iv_end_line      = lv_end_line
        iv_end_column    = lv_end_col
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.

    CASE iv_function_code.
      WHEN zif_dbbr_c_global=>c_function_codes-leave_screen OR
           zif_dbbr_c_global=>c_function_codes-quit_program OR
           zif_dbbr_c_global=>c_function_codes-cancel_screen.

        zcl_dbbr_screen_helper=>leave_screen( ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.

    mo_output_field_tree->free( ).
    mo_select_field_tree->free( ).
    IF mr_overview_dock IS BOUND.
      mr_overview_dock->free( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_choose_table_fields.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    cl_gui_cfw=>dispatch( IMPORTING return_code = DATA(lv_function) ).
    IF lv_function <> cl_gui_cfw=>rc_noevent.
      " a control event occured => exit PAI
      RETURN.
    ENDIF.

    IF sy-dynnr = c_screens-main_screen.
      CASE cv_function_code.
        WHEN zif_dbbr_c_global=>c_function_codes-cancel.
          zcl_dbbr_screen_helper=>leave_screen( ).

        WHEN 'FINISH'.
          " set flag that data transfer from screen should be triggered
          mf_transfer_data = abap_true.
          zcl_dbbr_screen_helper=>leave_screen( ).
      ENDCASE.
    ENDIF.

    CASE cv_function_code.

      WHEN c_tabs-all_fields_tab.
        mr_active_tab->* =
        mr_tree_tab_control->activetab = cv_function_code.
        mr_tree_screen->* = c_screens-all_fields_screen.

      WHEN c_tabs-filter_fields_tab.
        mr_active_tab->* =
        mr_tree_tab_control->activetab = cv_function_code.
        mr_tree_screen->* = c_screens-filter_fields_screen.

      WHEN c_fcode-sort_in_ddic.
        mo_output_field_tree->sort_fields_in_ddic_order( ).

      WHEN c_fcode-select_fields.
        mo_select_field_tree->select_all_fields( ).

      WHEN c_fcode-deselect_fields.
        mo_select_field_tree->deselect_all_fields( ).

      WHEN c_fcode-select_text_fields.
        mo_select_field_tree->select_all_text_fields( ).

      WHEN c_fcode-deselect_text_fields.
        mo_select_field_tree->deselect_all_text_fields( ).

      WHEN c_fcode-select_key_fields.
        mo_select_field_tree->select_key_fields( ).

      WHEN c_fcode-delete_selected.
        mo_output_field_tree->delete_selected_nodes( ).

      WHEN c_fcode-move_node_top.
        mo_output_field_tree->move_selected_nodes( zcl_dbbr_field_output_tree=>c_node_move_types-top ).

      WHEN c_fcode-move_node_bottom.
        mo_output_field_tree->move_selected_nodes( zcl_dbbr_field_output_tree=>c_node_move_types-bottom ).

      WHEN c_fcode-move_node_up.
        mo_output_field_tree->move_selected_nodes( zcl_dbbr_field_output_tree=>c_node_move_types-up ).

      WHEN c_fcode-move_node_down.
        mo_output_field_tree->move_selected_nodes( zcl_dbbr_field_output_tree=>c_node_move_types-down ).

      WHEN c_fcode-page_up.
        get_current_tab_controller( )->scroll_page_up( ).

      WHEN c_fcode-page_down.
        get_current_tab_controller( )->scroll_page_down( ).

      WHEN c_fcode-page_top.
        get_current_tab_controller( )->scroll_page_top( ).

      WHEN c_fcode-page_bottom.
        get_current_tab_controller( )->scroll_page_bottom( ).

      WHEN zif_dbbr_c_global=>c_function_codes-search.
        get_current_content_searcher( )->search( ).

      WHEN zif_dbbr_c_global=>c_function_codes-search_further.
        get_current_content_searcher( )->search_next( ).

    ENDCASE.

    CLEAR cv_function_code.


  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    zif_uitb_screen_controller~set_status( ).

    IF sy-dynnr = c_screens-main_screen.
      create_table_overview( ).
    ENDIF.

    " init tree data
    IF sy-dynnr = c_screens-all_fields_screen. " screen for defining output/selection fields
      show_available_fields( ).
    ELSEIF sy-dynnr = c_screens-filter_fields_screen. " screen for sorting checked output/selection fields
      show_output_fields( ).
    ELSE.
      LOOP AT SCREEN INTO DATA(ls_screen).
        IF ls_screen-name = c_tabs-all_fields_tab.
          IF mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-output AND
             mf_field_aggregation = abap_true.
            ls_screen-invisible = 1.
            MODIFY SCREEN FROM ls_screen.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    DATA: lt_exclude_tab TYPE TABLE OF sy-ucomm.

    IF sy-dynnr = c_screens-main_screen.

      IF mr_active_tab->* = c_tabs-all_fields_tab.
        lt_exclude_tab = VALUE #( ( c_fcode-delete_selected )
                                  ( c_fcode-sort_in_ddic )
                                  ( c_fcode-move_node_up )
                                  ( c_fcode-move_node_down )
                                  ( c_fcode-move_node_top )
                                  ( c_fcode-move_node_bottom ) ).
      ELSE.
        lt_exclude_tab = VALUE #( ( c_fcode-select_fields )
                                  ( c_fcode-select_key_fields )
                                  ( c_fcode-deselect_fields )
                                  ( c_fcode-select_text_fields )
                                  ( c_fcode-deselect_text_fields )
                                  ( c_fcode-insert_association ) ).
*        IF mv_mode = zif_dbbr_c_global=>gc_field_chooser_modes-output AND
*           mf_field_aggregation = abap_true.
*          lt_exclude_tab = VALUE #( BASE lt_exclude_tab
*            ( mc_functions-delete_selected )
*          ).
*        ENDIF.
      ENDIF.

      CASE mv_mode.
        WHEN zif_dbbr_c_global=>c_field_chooser_modes-selection.
          APPEND c_fcode-select_text_fields TO lt_exclude_tab.
          APPEND c_fcode-deselect_text_fields TO lt_exclude_tab.
      ENDCASE.

      SET PF-STATUS '0800' OF PROGRAM zif_dbbr_c_report_id=>main  EXCLUDING lt_exclude_tab.
      DATA(lv_mode_text) = COND string( WHEN mv_mode = zif_dbbr_c_global=>c_field_chooser_modes-output THEN
                                          'Output'
                                        ELSE
                                          'Selection' ).
      mr_filter_fields_tab->* = lv_mode_text && ' Fields'.

      SET TITLEBAR 'FIELD_CHOOSER_TITLE' OF PROGRAM zif_dbbr_c_report_id=>main WITH lv_mode_text.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
