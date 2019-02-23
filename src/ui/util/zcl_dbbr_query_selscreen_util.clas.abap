CLASS zcl_dbbr_query_selscreen_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_table_selscreen_util
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ir_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data .
    METHODS check_primary_entity
        REDEFINITION .
    METHODS clear
        REDEFINITION .
    METHODS get_entity_type_for_join
        REDEFINITION.
    METHODS get_entity_information
        REDEFINITION .
    METHODS get_title
        REDEFINITION .
    METHODS load_entity
        REDEFINITION .
    METHODS set_custom_functions
        REDEFINITION .
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION .
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION .
  PROTECTED SECTION.
    METHODS fill_primary_entity
        REDEFINITION.
    METHODS create_table_header
        REDEFINITION.
    METHODS finish_loading
        REDEFINITION.
  PRIVATE SECTION.

    DATA mv_query_name TYPE zdbbr_query_name .
    DATA mv_primary_entity_type TYPE zdbbr_entity_type.
    DATA mo_query_f TYPE REF TO zcl_dbbr_query_factory .
    DATA mt_parameters TYPE zdbbr_query_data-parameters.
    DATA mf_sql_query TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Delete the query</p>
    "!
    METHODS delete_query .
    "! <p class="shorttext synchronized" lang="en">Copies the query</p>
    "!
    METHODS copy_query .
    "! <p class="shorttext synchronized" lang="en">Maintain jump definitions of query</p>
    "!
    METHODS maintain_jumps .
    "! <p class="shorttext synchronized" lang="en">Export query to file</p>
    "!
    METHODS download_query .
    "! <p class="shorttext synchronized" lang="en">Loads a standard query</p>
    "!
    METHODS load_standard_query
      IMPORTING
        is_query_data TYPE zdbbr_query_data.
    "! <p class="shorttext synchronized" lang="en">Loads a custom query</p>
    "!
    METHODS load_custom_query
      IMPORTING
        is_query_data TYPE zdbbr_query_data.

    "! <p class="shorttext synchronized" lang="en">Create fields for query parameters in tabfield list</p>
    "!
    METHODS fill_query_parameters.
    "! <p class="shorttext synchronized" lang="en">Edit the current sql query</p>
    "!
    METHODS edit_sql_query.
    "! <p class="shorttext synchronized" lang="en">Create new SQL Query</p>
    "!
    METHODS create_sql_query.
ENDCLASS.



CLASS zcl_dbbr_query_selscreen_util IMPLEMENTATION.


  METHOD check_primary_entity.
    rf_success = abap_true.
  ENDMETHOD.

  METHOD create_table_header.
    " check if table is special parameter table
    IF iv_tabname = zif_dbbr_global_consts=>c_parameter_dummy_table.
      rs_table_header = VALUE zdbbr_selfield(
          tabname              = iv_tabname
          is_table_header      = abap_true
          description          = 'Parameters'(007)
          fieldname_raw        = iv_tabname
          ddic_order           = 0
      ).
    ELSE.
      rs_table_header = super->create_table_header(
          iv_tabname       = iv_tabname
          iv_tabname_alias = iv_tabname_alias
          iv_typename      = iv_typename
      ).
    ENDIF.
  ENDMETHOD.



  METHOD clear.
    super->clear( ).

    CLEAR: mv_query_name.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
      ir_selscreen_data = ir_selscreen_data
      iv_entity_type    = zif_dbbr_c_entity_type=>query
    ).
    mo_query_f = NEW zcl_dbbr_query_factory( ).
    mv_query_name = mo_data->mr_s_global_data->query_name.
    mo_data->mr_v_selmask_entity_type->* = 'Query Name'(006).

*.. fill custom menu with functions
    mo_custom_menu = NEW #( ).
    mo_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>edit_jump_fields
        text  = |{ 'Define Jumps'(002) }|
    ).
    mo_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>copy_query
        text  = |{ 'Copy query'(003) }|
    ).
    mo_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>export_query
        text  = |{ 'Download query to PC'(004) }|
    ).
    mo_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>delete_query
        text  = |{ 'Delete query'(005) }|
    ).

    DATA(lo_toolbar) = fill_toolbar(
      if_create_extended_search = abap_true
      it_custom_buttons = VALUE #(
        ( function  = zif_dbbr_c_selscreen_functions=>edit_sql_query
          icon      = icon_change
          quickinfo = |{ 'Change Custom Query'(008) }| )
        ( function  = zif_dbbr_c_selscreen_functions=>create_sql_query
          icon      = icon_create
          quickinfo = |{ 'Create Custom Query'(009) }| )
      )
    ).

    lo_toolbar->set_button_visible( fcode = zif_dbbr_c_selscreen_functions=>edit_sql_query visible = abap_false ).
  ENDMETHOD.


  METHOD copy_query.
    DATA(lr_query_copier_view) = NEW zcl_dbbr_copy_query_ctrl(
      is_query_info = CORRESPONDING #( DEEP mo_data->mr_s_query_info->* )
    ).

    lr_query_copier_view->show( ).

    CHECK lr_query_copier_view->was_copied( ).

*.. clear current selection mask entries
    clear( ).

**.. load the new query
    DATA(ls_copied_query) = lr_query_copier_view->get_new_query( ).

*.. update history
    zcl_dbbr_selscreen_history=>add_new_entry(
        iv_entity      = ls_copied_query-query_name
        iv_type        = zif_dbbr_c_entity_type=>query
        iv_description = ls_copied_query-description
    ).
    zcl_dbbr_selscreen_history=>navigate_to_current( ).
  ENDMETHOD.


  METHOD delete_query.
    CHECK zcl_dbbr_appl_util=>popup_to_confirm(
          iv_title                 = 'Delete query?'
          iv_query                 = |Are you sure you want to delete the query { mv_query_name } ?|
          iv_icon_type             = 'ICON_MESSAGE_QUESTION'
    ) = '1'.

*.. remove any navigation history entries for this query
    zcl_dbbr_selscreen_history=>delete_entity_from_history(
      iv_entity_id   = mv_entity_id
      iv_entity_type = mv_entity_type
    ).

    DATA(lv_entity_id) = mv_entity_id.
    DATA(lv_entity_type) = mv_entity_type.


*.. delete the query
    mo_query_f->delete_query_by_id( iv_query_id = mo_data->mr_s_query_info->query_id ).
    MESSAGE s023(zdbbr_info) WITH mv_query_name.

*.. clear any remaining data in the selection screen for the old entity
    clear( ).

*.. show the current entry in the history
    IF NOT zcl_dbbr_selscreen_history=>navigate_to_current( ).
      CLEAR: mo_data->mr_s_entity_info->entity_id.
    ENDIF.

*.. Reload the favorite/history tree
    notify_of_deleted_entity(
        iv_entity_id   = lv_entity_id
        iv_entity_type = lv_entity_type
    ).
  ENDMETHOD.


  METHOD download_query.
    DATA(lr_query_exporter) = NEW zcl_dbbr_query_exporter(
      it_query_info = VALUE #( ( query_id   = mo_data->mr_s_query_info->query_id
                                  query_name = mv_query_name ) )
      iv_file_name   = |Query-{ mv_query_name }|
    ).

    lr_query_exporter->export_data( ).
  ENDMETHOD.

  METHOD get_entity_type_for_join.
    rv_entity_type = mo_data->mr_s_query_info->entity_type.
  ENDMETHOD.


  METHOD get_entity_information.
    super->get_entity_information(
      IMPORTING
        ev_description = ev_description
    ).
    ev_type = zif_dbbr_c_favmenu_type=>query.
    ev_entity =
    ev_entity_raw = mv_query_name.
  ENDMETHOD.


  METHOD get_title.
    IF has_content( ).
      result = |Query - { mv_query_name }|.
    ELSE.
      result = |Query Mode|.
    ENDIF.
  ENDMETHOD.


  METHOD load_entity.
    CHECK mv_query_name IS NOT INITIAL.

    zcl_dbbr_screen_helper=>show_progress( iv_text = `Selection Mask for query ` && mv_query_name && ` is loading...`
                                           iv_progress = 1 ).

    clear_edit_flags( ).

    CLEAR: mo_data->mr_s_settings->*,
           mf_sql_query.

*... 1) load query from database
    DATA(ls_query_data) = mo_query_f->get_query( mv_query_name ).

    mo_data->mr_s_query_info->* = CORRESPONDING #( ls_query_data ).
*... fill name of query on screen
    mo_data->mr_v_selmask_entity_text->* = mo_data->mr_s_query_info->description.
    mo_data->mr_v_selmask_entity_name->* = mv_query_name.


    IF ls_query_data-source IS INITIAL.
      DATA(lf_enable_selfield_control) = abap_true.
      load_standard_query( EXPORTING is_query_data = ls_query_data ).
    ELSE.
      mf_sql_query = abap_true.
      load_custom_query( EXPORTING is_query_data = ls_query_data ).
    ENDIF.

    zcl_dbbr_toolbar_util=>get_selscreen_table_tb( )->set_button_state( enabled = lf_enable_selfield_control fcode = zif_dbbr_c_selscreen_functions=>control_sel_fields ).

    mv_entity_id = mv_query_name.

    mo_data->mr_s_entity_info->* = VALUE #(
      entity_id   = mv_query_name
      entity_type = mv_entity_type
    ).

    update_entity_type_sh( ).

    finish_loading( ).

    rf_entity_loaded = abap_true.

  ENDMETHOD.


  METHOD maintain_jumps.
    DATA(lr_jumplist_table) = NEW zcl_dbbr_jumplist_table( ir_tabfield_list = NEW #( ) ).
    DATA(lr_jumplist_controller) = NEW zcl_dbbr_jumplist_controller(
      iv_query_id = mo_data->mr_s_query_info->query_id
      ir_table     = lr_jumplist_table
    ).

    lr_jumplist_controller->zif_uitb_screen_controller~call_screen( ).
  ENDMETHOD.


  METHOD set_custom_functions.
    mo_data->clear_custom_functions( ).

    mo_data->mr_s_top_custom_menu->text = 'query'(001).

*... fill custom functions
    mo_data->mr_s_entity_function1->text = 'Define Jumps'(002).
    mo_data->mr_s_entity_function2->text = 'Copy query'(003).
    mo_data->mr_s_entity_function3->text = 'Download query to PC'(004).
    mo_data->mr_s_entity_function4->text = 'Delete query'(005).
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = super->get_deactivated_functions( ).

    IF mv_query_name IS NOT INITIAL AND
       mo_data->mr_s_query_info->source IS NOT INITIAL.
*.... Deactivated functions for custom queries i.e. SQL queries
      result = VALUE #( BASE result
        ( zif_dbbr_c_selscreen_functions=>save_query )
*...... TODO: Activate count lines for custom queries
        ( zif_dbbr_c_selscreen_functions=>count_lines )
        ( zif_dbbr_c_selscreen_functions=>control_sel_fields )
        ( zif_dbbr_c_selscreen_functions=>control_output_fields )
        ( zif_dbbr_c_selscreen_functions=>control_sort_fields )
        ( zif_dbbr_c_selscreen_functions=>delete_all_or_tuple )
        ( zif_dbbr_c_selscreen_functions=>multi_or_selection )
        ( zif_dbbr_c_selscreen_functions=>define_joins )
        ( zif_dbbr_c_selscreen_functions=>edit_jump_fields )
        ( zif_dbbr_c_selscreen_functions=>select_additional_texts )
        ( zif_dbbr_c_selscreen_functions=>show_formula_manager )
      ).
    ENDIF.

    IF mo_data->mr_s_global_data->settings-advanced_mode = abap_false.
      result = VALUE #( BASE result
        ( zif_dbbr_c_selscreen_functions=>delete_query )
        ( zif_dbbr_c_selscreen_functions=>copy_query )
        ( zif_dbbr_c_selscreen_functions=>edit_jump_fields )
      ).
    ELSE.
*.... deactivate edit jump fields if user did not create query
      IF mo_data->mr_s_query_info->created_by = sy-uname AND
         mo_data->mr_s_global_data->settings-advanced_mode = abap_true.
        DELETE result WHERE table_line = zif_dbbr_c_selscreen_functions=>edit_jump_fields.
        DELETE result WHERE table_line = zif_dbbr_c_selscreen_functions=>delete_query.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_ui_function.
    CASE cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>copy_query.
        copy_query( ).

      WHEN zif_dbbr_c_selscreen_functions=>edit_jump_fields.
        maintain_jumps( ).

      WHEN zif_dbbr_c_selscreen_functions=>delete_query.
        delete_query( ).

      WHEN zif_dbbr_c_selscreen_functions=>export_query.
        download_query( ).

      WHEN zif_dbbr_c_selscreen_functions=>edit_sql_query.
        edit_sql_query( ).

      WHEN zif_dbbr_c_selscreen_functions=>create_sql_query.
        create_sql_query( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD fill_primary_entity.
*... set cursor to first line
    mo_data->mo_selection_table->zif_uitb_page_scroller~scroll_page_top( ).
    mo_data->mo_custom_f4_map->clear( ).

    check_edit_mode( ).

*... create selection fields for primary table
    IF is_primary_entity-type = zif_dbbr_c_entity_type=>cds_view.
      create_cds_fields( VALUE #(
            tabname          = is_primary_entity-tabname
            tabname_alias    = is_primary_entity-tabname_alias
            active_selection = abap_true
            is_primary       = abap_true
            selection_order  = is_primary_entity-selection_order )
      ).
    ELSE.
      create_table_fields( VALUE #(
          tabname          = is_primary_entity-tabname
          tabname_alias    = is_primary_entity-tabname_alias
          active_selection = abap_true
          is_primary       = abap_true
          selection_order  = is_primary_entity-selection_order )
      ).
    ENDIF.

    CLEAR mv_tab_size_text.
  ENDMETHOD.

  METHOD load_standard_query.
    ASSIGN mo_data->mr_s_join_def->* TO FIELD-SYMBOL(<ls_join_definition>).
    ASSIGN mo_data->mr_s_global_data->* TO FIELD-SYMBOL(<ls_global_data>).

*... get primary table from query
    <ls_global_data>-primary_table =
    <ls_join_definition>-primary_table = mo_data->mr_s_query_info->primary_table.

    DATA(ls_join_def) = is_query_data-join_def.
    DATA(lt_query_selfields) = is_query_data-fields.

    DATA(ls_primary_entity) = VALUE zdbbr_entity_info(
        tabname              = is_query_data-primary_table
        tabname_alias        = is_query_data-primary_table_alias
        type                 = is_query_data-entity_type
        selection_order      = VALUE #( is_query_data-tables[ tabname = is_query_data-primary_table ]-selection_order OPTIONAL )
        is_primary           = abap_true
    ).

    fill_primary_entity( ls_primary_entity ).

    IF ls_join_def IS NOT INITIAL.
      <ls_join_definition> = ls_join_def.
      update_join_definition(  ).
    ENDIF.

    DATA(lr_tabfield_list) = mo_data->mo_tabfield_list.

*.. update table info from stored query tables
    LOOP AT is_query_data-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      lr_tabfield_list->update_table_active_selection(
          iv_tabname_alias    = <ls_table>-tabname_alias
          if_active_selection = <ls_table>-active_selection
          iv_selection_order  = <ls_table>-selection_order
      ).
    ENDLOOP.

    lr_tabfield_list->clear_active_flag(
        if_clear_selection = abap_true
        if_clear_sort      = mo_data->mr_s_query_info->has_sort_fields
        if_clear_output    = mo_data->mr_s_query_info->has_output_fields
    ).

*... update selection fields from query
    lr_tabfield_list->initialize_iterator( ).
    WHILE lr_tabfield_list->has_more_lines( ).
      DATA(lr_current_field) = lr_tabfield_list->get_next_entry( ).

      ASSIGN lt_query_selfields[ tabname_alias = lr_current_field->tabname_alias
                                 fieldname     = lr_current_field->fieldname
                                 is_text_field = lr_current_field->is_text_field ] TO FIELD-SYMBOL(<ls_query_selfield>).
      IF sy-subrc = 0.
        lr_current_field->selection_active = <ls_query_selfield>-selection_active.
        lr_current_field->selection_order = <ls_query_selfield>-selection_order.
        lr_current_field->output_active = <ls_query_selfield>-output_active.
        lr_current_field->output_order = <ls_query_selfield>-output_order.
        lr_current_field->sort_active = <ls_query_selfield>-sort_active.
        lr_current_field->sort_order = <ls_query_selfield>-sort_order.
        lr_current_field->sort_direction = <ls_query_selfield>-sort_direction.
      ENDIF.
    ENDWHILE.

    IF mo_data->mr_s_query_info->formula IS NOT INITIAL.
*... validate formula
      TRY.
          DATA(lr_formula) = NEW zcl_dbbr_fe_validator(
              iv_formula   = mo_data->mr_s_query_info->formula
              ir_tabfields = lr_tabfield_list
          )->validate( ).

          zcl_dbbr_formula_helper=>update_tabflds_from_formula(
              ir_tabfields      = lr_tabfield_list
              ir_formula        = lr_formula
              it_form_selfields = lt_query_selfields
          ).

        CATCH zcx_dbbr_formula_exception.
          CLEAR mo_data->mr_s_query_info->formula.
          lr_tabfield_list->clear_calculation_flag( ).
          lr_tabfield_list->delete_formula_fields( ).
          " create the formula entity but with an invalid state
          lr_formula = NEW #( iv_formula  = mo_data->mr_s_query_info->formula
                              if_is_valid = abap_false ).
      ENDTRY.

    ENDIF.

    mo_data->set_formula( lr_formula ).

    fill_selection_mask( ).

*.. load the default variant if it was supplied
    IF is_query_data-has_filter_values = abap_true.
      DATA(lv_default_variant) = NEW zcl_dbbr_variant_factory( )->find_default_query_variant( is_query_data-query_id ).
      IF lv_default_variant IS NOT INITIAL.

        NEW zcl_dbbr_variant_loader(
            iv_variant_id        = lv_default_variant
            ir_t_multi_or        = mo_data->get_multi_or_all( )
            ir_t_selfields       = mo_data->mr_t_table_data
            ir_t_selfields_multi = mo_data->mr_t_selfields_multi
            ir_tabfields         = mo_data->mo_tabfield_list
            ir_s_global_data     = mo_data->mr_s_global_data
            ir_tabfields_grouped = mo_data->mo_tabfield_aggr_list
        )->load_variant( if_no_message = abap_true ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD load_custom_query.
    mt_parameters = is_query_data-parameters.

    fill_query_parameters( ).

    update_multi_selection_mask( ).
  ENDMETHOD.

  METHOD fill_query_parameters.
    DATA: ls_datel    TYPE dd04v,
          lv_rollname TYPE rollname.

    CHECK mt_parameters IS NOT INITIAL.

    LOOP AT mt_parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
      CLEAR: lv_rollname,
             ls_datel.

      DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
        tabname          = zif_dbbr_global_consts=>c_parameter_dummy_table
        fieldname        = <ls_param>-name
        fieldname_raw    = <ls_param>-name
        is_parameter     = abap_true
        is_range_param   = <ls_param>-is_range
        selection_active = abap_true
        default_option   = COND #( WHEN <ls_param>-is_range = abap_false THEN zif_dbbr_c_options=>equals )
        default_sign     = zif_dbbr_c_options=>includes
        default_low      = <ls_param>-default_value
      ).

*... get the data element
      IF <ls_param>-type IS NOT  INITIAL.
        DATA(lo_elem_type) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( <ls_param>-type ) ).
        DATA(ls_dfies) = lo_elem_type->get_ddic_field( ).
        ls_tabfield-rollname = ls_dfies-rollname.
        ls_tabfield-domname = ls_dfies-domname.
        ls_tabfield-inttype = ls_dfies-inttype.
        ls_tabfield-length = ls_dfies-leng.
        ls_tabfield-outputlen = ls_dfies-outputlen.
        ls_tabfield-convexit = ls_dfies-convexit.
      ELSE.
        ls_tabfield-decimals = <ls_param>-decimals.
        ls_tabfield-inttype = <ls_param>-inttype.
        ls_tabfield-length = <ls_param>-length.
      ENDIF.

      mo_data->mo_tabfield_list->add( REF #( ls_tabfield ) ).
    ENDLOOP.


*.. add cds view to list of tables
    DATA(ls_entity) = VALUE zdbbr_entity_info(
       active_selection     = abap_true
       tabname              = zif_dbbr_global_consts=>c_parameter_dummy_table
       tabname_alias        = zif_dbbr_global_consts=>c_parameter_dummy_table
       type                 = zif_dbbr_c_entity_type=>table
       description          = 'Parameters'
       no_output            = abap_true
    ).

    mo_data->mo_tabfield_list->add_table( ls_entity ).
  ENDMETHOD.

  METHOD finish_loading.
    super->finish_loading( ).

    DATA(lo_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_entity_tb( ).
    lo_toolbar->set_button_visible( fcode   = zif_dbbr_c_selscreen_functions=>edit_sql_query
                                    visible = mf_sql_query ).
  ENDMETHOD.


  METHOD edit_sql_query.
    DATA(lo_query_editor) = NEW zcl_dbbr_sql_query_editor( iv_query_name = mv_query_name ).
    lo_query_editor->show( ).

    DATA(lv_last_saved_query) = lo_query_editor->get_last_saved_query( ).
    CHECK lv_last_saved_query IS NOT INITIAL.

    RAISE EVENT request_new_entity
      EXPORTING
        ev_id            = lv_last_saved_query
        ev_type          = mv_entity_type
        ef_force_loading = abap_true.
  ENDMETHOD.


  METHOD create_sql_query.
    DATA(lo_query_editor) = NEW zcl_dbbr_sql_query_editor( ).
    lo_query_editor->show( ).

    DATA(lv_last_saved_query) = lo_query_editor->get_last_saved_query( ).
    CHECK lv_last_saved_query IS NOT INITIAL.

    RAISE EVENT request_new_entity
      EXPORTING
        ev_id            = lv_last_saved_query
        ev_type          = mv_entity_type
        ef_force_loading = abap_true.
  ENDMETHOD.

ENDCLASS.
