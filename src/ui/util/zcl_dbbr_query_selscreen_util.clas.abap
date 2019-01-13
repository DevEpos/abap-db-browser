class ZCL_DBBR_query_SELSCREEN_UTIL definition
  public
  inheriting from ZCL_DBBR_TABLE_SELSCREEN_UTIL
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IR_SELSCREEN_DATA type ref to ZCL_DBBR_SELSCREEN_DATA .

  methods CHECK_PRIMARY_ENTITY
    redefinition .
  methods CLEAR
    redefinition .
  methods GET_ENTITY_INFORMATION
    redefinition .
  methods GET_TITLE
    redefinition .
  methods LOAD_ENTITY
    redefinition .
  methods SET_CUSTOM_FUNCTIONS
    redefinition .
  methods ZIF_DBBR_SCREEN_UTIL~GET_DEACTIVATED_FUNCTIONS
    redefinition .
  methods ZIF_DBBR_SCREEN_UTIL~HANDLE_UI_FUNCTION
    redefinition .
protected section.
  PRIVATE SECTION.

    DATA mv_query_name TYPE zdbbr_query_name .
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory .

    METHODS delete_query .
    METHODS copy_query .
    METHODS maintain_jumps .
    METHODS download_query .
ENDCLASS.



CLASS ZCL_DBBR_QUERY_SELSCREEN_UTIL IMPLEMENTATION.


  METHOD check_primary_entity.
    rf_success = abap_true.
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
    mr_query_f = NEW zcl_dbbr_query_factory( ).
    mv_query_name = mr_data->mr_s_global_data->query_name.
    mr_data->mr_v_selmask_entity_type->* = 'Query Name'(006).

*.. fill custom menu with functions
    mr_custom_menu = NEW #( ).
    mr_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>edit_jump_fields
        text  = |{ 'Define Jumps'(002) }|
    ).
    mr_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>copy_query
        text  = |{ 'Copy query'(003) }|
    ).
    mr_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>export_query
        text  = |{ 'Download query to PC'(004) }|
    ).
    mr_custom_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>delete_query
        text  = |{ 'Delete query'(005) }|
    ).

    fill_toolbar( ).
  ENDMETHOD.


  METHOD copy_query.
    DATA(lr_query_copier_view) = NEW zcl_dbbr_copy_query_ctrl(
      is_query_info = CORRESPONDING #( DEEP mr_data->mr_s_query_info->* )
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
    mr_query_f->delete_query_by_id( iv_query_id = mr_data->mr_s_query_info->query_id ).
    MESSAGE s023(zdbbr_info) WITH mv_query_name.

*.. clear any remaining data in the selection screen for the old entity
    clear( ).

*.. show the current entry in the history
    IF NOT zcl_dbbr_selscreen_history=>navigate_to_current( ).
      CLEAR: mr_data->mr_s_entity_info->entity_id.
    ENDIF.

*.. Reload the favorite/history tree
    notify_of_deleted_entity(
        iv_entity_id   = lv_entity_id
        iv_entity_type = lv_entity_type
    ).
  ENDMETHOD.


  METHOD download_query.
    DATA(lr_query_exporter) = NEW zcl_dbbr_query_exporter(
      it_query_info = VALUE #( ( query_id   = mr_data->mr_s_query_info->query_id
                                  query_name = mv_query_name ) )
      iv_file_name   = |Query-{ mv_query_name }|
    ).

    lr_query_exporter->export_data( ).
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
    ASSIGN mr_data->mr_s_join_def->* TO FIELD-SYMBOL(<ls_join_definition>).
    ASSIGN mr_data->mr_s_global_data->* TO FIELD-SYMBOL(<ls_global_data>).

    CLEAR: mr_data->mr_s_settings->*.

*... 1) load query from database
    DATA(ls_query_data) = mr_query_f->get_query( mv_query_name ).

    mr_data->mr_s_query_info->* = CORRESPONDING #( ls_query_data ).

*... fill name of query on screen
    mr_data->mr_v_selmask_entity_text->* = mr_data->mr_s_query_info->description.
    mr_data->mr_v_selmask_entity_name->* = mv_query_name.

*... get primary table from query
    <ls_global_data>-primary_table =
    <ls_join_definition>-primary_table = mr_data->mr_s_query_info->primary_table.

    DATA(ls_join_def) = ls_query_data-join_def.
    DATA(lt_query_selfields) = ls_query_data-fields.

    fill_table( ).

    IF ls_join_def IS NOT INITIAL.
      <ls_join_definition> = ls_join_def.
      update_join_definition( ).
    ENDIF.

    DATA(lr_tabfield_list) = mr_data->mr_tabfield_list.

*.. update table info from stored query tables
    LOOP AT ls_query_data-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      lr_tabfield_list->update_table_active_selection(
          iv_tabname_alias    = <ls_table>-tabname
          if_active_selection = <ls_table>-active_selection
          iv_selection_order  = <ls_table>-selection_order
      ).
    ENDLOOP.

    lr_tabfield_list->clear_active_flag(
        if_clear_selection = abap_true
        if_clear_sort      = mr_data->mr_s_query_info->has_sort_fields
        if_clear_output    = mr_data->mr_s_query_info->has_output_fields
    ).

*... update selection fields from query
    lr_tabfield_list->initialize_iterator( ).
    WHILE lr_tabfield_list->has_more_lines( ).
      DATA(lr_current_field) = lr_tabfield_list->get_next_entry( ).

      ASSIGN lt_query_selfields[ tabname       = lr_current_field->tabname
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

    IF mr_data->mr_s_query_info->formula IS NOT INITIAL.
*... validate formula
      TRY.
          DATA(lr_formula) = NEW zcl_dbbr_fe_validator(
              iv_formula   = mr_data->mr_s_query_info->formula
              ir_tabfields = lr_tabfield_list
          )->validate( ).

          zcl_dbbr_formula_helper=>update_tabflds_from_formula(
              ir_tabfields      = lr_tabfield_list
              ir_formula        = lr_formula
              it_form_selfields = lt_query_selfields
          ).

        CATCH zcx_dbbr_formula_exception.
          CLEAR mr_data->mr_s_query_info->formula.
          lr_tabfield_list->clear_calculation_flag( ).
          lr_tabfield_list->delete_formula_fields( ).
          " create the formula entity but with an invalid state
          lr_formula = NEW #( iv_formula  = mr_data->mr_s_query_info->formula
                              if_is_valid = abap_false ).
      ENDTRY.

    ENDIF.

    mr_data->set_formula( lr_formula ).

    fill_selection_mask( ).

    update_alv_variant( ).

*.. load the default variant if it was supplied
    IF ls_query_data-has_filter_values = abap_true.
      DATA(lv_default_variant) = NEW zcl_dbbr_variant_factory( )->find_default_query_variant( ls_query_data-query_id ).
      IF lv_default_variant IS NOT INITIAL.

        NEW zcl_dbbr_variant_loader(
            iv_variant_id        = lv_default_variant
            ir_t_multi_or        = mr_data->get_multi_or_all( )
            ir_t_selfields       = mr_data->mr_t_table_data
            ir_t_selfields_multi = mr_data->mr_t_selfields_multi
            ir_tabfields         = mr_data->mr_tabfield_list
            ir_s_global_data     = mr_data->mr_s_global_data
            ir_tabfields_grouped = mr_data->mr_tabfield_aggr_list
        )->load_variant( if_no_message = abap_true ).
      ENDIF.
    ENDIF.

    mv_entity_id = mv_query_name.

    mr_data->mr_s_entity_info->* = VALUE #(
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
      iv_query_id = mr_data->mr_s_query_info->query_id
      ir_table     = lr_jumplist_table
    ).

    lr_jumplist_controller->zif_uitb_screen_controller~call_screen( ).
  ENDMETHOD.


  METHOD set_custom_functions.
    mr_data->clear_custom_functions( ).

    mr_data->mr_s_top_custom_menu->text = 'query'(001).

*... fill custom functions
    mr_data->mr_s_entity_function1->text = 'Define Jumps'(002).
    mr_data->mr_s_entity_function2->text = 'Copy query'(003).
    mr_data->mr_s_entity_function3->text = 'Download query to PC'(004).
    mr_data->mr_s_entity_function4->text = 'Delete query'(005).
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = super->get_deactivated_functions( ).

    IF mr_data->mr_s_global_data->settings-advanced_mode = abap_false.
      result = VALUE #( BASE result
        ( zif_dbbr_c_selscreen_functions=>delete_query )
        ( zif_dbbr_c_selscreen_functions=>copy_query )
        ( zif_dbbr_c_selscreen_functions=>edit_jump_fields )
      ).
    ELSE.
*.... deactivate edit jump fields if user did not create query
      IF mr_data->mr_s_query_info->created_by = sy-uname AND
         mr_data->mr_s_global_data->settings-advanced_mode = abap_true.
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

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
