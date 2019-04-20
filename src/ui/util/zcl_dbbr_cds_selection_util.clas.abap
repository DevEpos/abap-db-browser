"! <p class="shorttext synchronized" lang="en">Utitlity methods for CDS Views for Selection Controller</p>
CLASS zcl_dbbr_cds_selection_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selection_util
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS build_simple_alv_title
        REDEFINITION .
    METHODS execute_selection
        REDEFINITION .
    METHODS fill_header
        REDEFINITION .
    METHODS get_entity_name
        REDEFINITION .
    METHODS handle_alv_ctx_menu_request
        REDEFINITION .
    METHODS init
        REDEFINITION .
    METHODS refresh_selection
        REDEFINITION .
    METHODS zif_dbbr_screen_util~free_resources
        REDEFINITION .
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION .
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION .
    METHODS zif_dbbr_screen_util~handle_pbo
        REDEFINITION .
  PROTECTED SECTION.

    METHODS create_from_clause
        REDEFINITION .
    METHODS read_entity_infos
        REDEFINITION .
  PRIVATE SECTION.

    "! Name of CDS view
    DATA mv_cds_view TYPE zdbbr_cds_view_name .
    "! CDS View
    DATA mo_cds_view TYPE REF TO zcl_dbbr_cds_view .
    "! Utilities for screen
    DATA mr_assoc_selector TYPE REF TO zcl_dbbr_cds_sub_entity_sel .

    "! <p class="shorttext synchronized" lang="en">Change CDS parameter values</p>
    METHODS change_parameters .
    "! <p class="shorttext synchronized" lang="en">Choose Association for navigation</p>
    METHODS choose_association .
    "! <p class="shorttext synchronized" lang="en">Performs Association navigation</p>
    "! @parameter IS_ASSOC | <p class="shorttext synchronized" lang="en"></p>
    METHODS navigate
      IMPORTING
        !is_assoc TYPE zdbbr_cds_association .
    METHODS rebuild.
    "! <p class="shorttext synchronized" lang="en">Event handler for when association gets chosen</p>
    "! @parameter EV_CHOSEN_ENTITY_ID | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter EV_CHOSEN_ENTITY_TYPE | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ES_CHOSEN_ASSOCIATION | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_association_chosen
          FOR EVENT entity_chosen OF zcl_dbbr_cds_sub_entity_sel
      IMPORTING
          !ev_chosen_entity_id
          !ev_chosen_entity_type
          !es_chosen_association .
ENDCLASS.



CLASS zcl_dbbr_cds_selection_util IMPLEMENTATION.


  METHOD build_simple_alv_title.
    DATA: lv_title TYPE lvc_title.

    DATA(ls_header) = mo_cds_view->get_header( ).

    lv_title = |CDS View - { ls_header-entityname_raw } - { ls_header-description }|.

    " check max length of string
    IF strlen( lv_title ) >= 70.
      result = lv_title.
      result+67(3) = '...'.
    ELSE.
      result = lv_title.
    ENDIF.
  ENDMETHOD.


  METHOD change_parameters.
    DATA: lf_trigger_select TYPE abap_bool.

    DATA(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
        io_tabfields = mo_tabfields_all
        it_param_values = VALUE #(
          FOR param IN mt_param_values WHERE ( is_parameter = abap_true ) ( name = param-fieldname value = param-low )
        )
    ).
    lo_param_popup->show( ).
    DATA(lt_new_param_values) = lo_param_popup->get_param_values( ).

    IF lt_new_param_values IS NOT INITIAL.
      LOOP AT lt_new_param_values ASSIGNING FIELD-SYMBOL(<ls_new_param>).
        ASSIGN mt_param_values[ fieldname = <ls_new_param>-name is_parameter = abap_true ] TO FIELD-SYMBOL(<ls_old_param>).
        IF sy-subrc = 0 AND
           <ls_old_param>-low <> <ls_new_param>-value.
          lf_trigger_select = abap_true.
          <ls_old_param>-low = <ls_new_param>-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lf_trigger_select = abap_true.
      CLEAR mt_from.
      create_from_clause( ).
      mr_select_program->update_from( mt_from ).
      refresh_selection( ).
    ENDIF.
  ENDMETHOD.


  METHOD choose_association.
    DATA: lr_association_chooser TYPE REF TO zcl_dbbr_cds_sub_entity_sel.

    DATA(lf_show_docked) = xsdbool( ms_technical_info-assoc_sel_mode = zif_dbbr_c_assoc_select_mode=>docked ).

    IF lf_show_docked = abap_false.
      mt_selected_rows = mr_alv_grid->get_all_selected_rows( ).
*... check if at leas on row was selected
      IF mt_selected_rows IS INITIAL.
        MESSAGE i074(zdbbr_info) DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
    ELSE.
*... if docked mode and view is already visible dispose of it
      IF mr_assoc_selector IS BOUND.
        IF NOT mr_assoc_selector->is_visible( ).
          CLEAR mr_assoc_selector.
        ELSE.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

*... retrieve association
    lr_association_chooser = NEW #(
      ir_cds_view          = mo_cds_view
      if_only_associations = abap_true
      if_as_dock           = lf_show_docked
    ).

    IF lf_show_docked = abap_true.
      mr_assoc_selector = lr_association_chooser.
      SET HANDLER on_association_chosen FOR mr_assoc_selector.
    ENDIF.

    lr_association_chooser->show( ).

    IF lf_show_docked = abap_false.
      DATA(ls_assoc) = lr_association_chooser->get_chosen_association( ).
      navigate( is_assoc = ls_assoc ).
    ENDIF.
  ENDMETHOD.


  METHOD create_from_clause.
    DATA: lv_handled_params  TYPE i,
          lv_from_alias_text TYPE string,
          lv_entity          TYPE tabname.

    DATA(ls_header) = mo_cds_view->get_header( ).
    IF  ms_technical_info-use_ddl_view_for_select = abap_true AND
        ls_header-source_type <> zif_dbbr_c_cds_view_type=>table_function.
      lv_entity = mo_cds_view->get_header( )-ddlview.
      lv_from_alias_text = | AS { mo_cds_view->mv_view_name }|.
    ELSE.
      lv_entity = mv_cds_view.
    ENDIF.

    IF mo_cds_view->has_parameters( ).
*...from clause has to be single line for the parameters select to work
      DATA(lv_from_line) = |{ lv_entity }( |.

      DATA(lv_param_count) = lines( mo_cds_view->get_parameters( if_exclude_system_params = abap_true ) ).

*...fill parameter values
      LOOP AT mt_param_values ASSIGNING FIELD-SYMBOL(<ls_param>) WHERE is_parameter = abap_true.
        ADD 1 TO lv_handled_params.

        lv_from_line = |{ lv_from_line }{ <ls_param>-fieldname } = { cl_abap_dyn_prg=>quote( <ls_param>-low ) }|.

        IF lv_handled_params <> lv_param_count.
          lv_from_line = |{ lv_from_line }, |.
        ELSE.
          lv_from_line = |{ lv_from_line } ){ lv_from_alias_text }|.
        ENDIF.

        mt_from = VALUE #( BASE mt_from ( lv_from_line ) ).
        CLEAR: lv_from_line.
      ENDLOOP.

*... no parameter was added, so just add the name of the cds view
      IF sy-subrc <> 0.
        mt_from = VALUE #( ( lv_entity && lv_from_alias_text ) ).
      ENDIF.

    ELSE.
      mt_from = VALUE #( ( lv_entity && lv_from_alias_text ) ).
    ENDIF.
  ENDMETHOD.


  METHOD execute_selection.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    determine_group_by_state( ).
    determine_aggregation_state( ).

    IF mf_count_lines = abap_true.
      count_lines( ).
      RETURN.
    ENDIF.

    read_entity_infos( ).

    build_full_fieldnames( ).

    mo_tabfields->update_text_field_status( ).

    zcl_dbbr_addtext_helper=>prepare_text_fields(
      EXPORTING ir_fields    = mo_tabfields
      CHANGING  ct_add_texts = mt_add_texts
    ).

    create_where_clause( ).

    create_from_clause( ).

    create_group_by_clause( ).

    create_order_by_clause( ).

    create_field_catalog( ).

    create_select_clause( ).

    create_dynamic_table( ).

    CHECK select_data( ).

    " if no selection occurred, prevent screen visibility
    IF ms_control_info-number <= 0.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    set_miscinfo_for_selected_data( ).

    RAISE EVENT selection_finished
      EXPORTING
         ef_first_select = abap_true.
  ENDMETHOD.

  METHOD rebuild.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    init( ).

    CLEAR: mt_from,
           mt_where,
           mt_group_by,
           mt_fieldcat,
           mt_sort_alv,
           mt_selected_rows,
           mt_select,
           mt_dyntab_components,
           mt_current_live_filter.

    determine_group_by_state( ).
    determine_aggregation_state( ).

    read_entity_infos( ).

    build_full_fieldnames( ).

    mo_tabfields->update_text_field_status( ).

    zcl_dbbr_addtext_helper=>prepare_text_fields(
      EXPORTING ir_fields    = mo_tabfields
      CHANGING  ct_add_texts = mt_add_texts
    ).

    create_where_clause( ).

    create_from_clause( ).

    create_group_by_clause( ).

    create_order_by_clause( ).

    create_field_catalog( ).

    create_select_clause( ).

    create_dynamic_table( ).

    " if no selection occurred, prevent screen visibility
    IF ms_control_info-number <= 0.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    set_miscinfo_for_selected_data( ).

    RAISE EVENT selection_finished
      EXPORTING
         ef_first_select = abap_true.

  ENDMETHOD.


  METHOD fill_header.
    cr_dd_doc->add_table(
      EXPORTING no_of_columns = 3
                border        = '0'
                width         = '60%'
      IMPORTING table         = DATA(lo_table)
    ).

    lo_table->add_column( IMPORTING column = DATA(lo_col1) ).
    lo_table->add_column( IMPORTING column = DATA(lo_col2) ).
    lo_table->add_column( IMPORTING column = DATA(lo_col3) ).

*...add Table content
    lo_table->span_columns( col_start_span = lo_col1 no_of_cols = 2 ).
    lo_table->new_row( ).
    lo_col1->add_text(
        text          = 'CDS View'
        sap_emphasis  = cl_dd_document=>strong
    ).
    lo_table->new_row( ).

    lo_col1->add_text( text = |{ mo_cds_view->get_header( )-entityname_raw }| ).
    lo_col2->add_text( text = |{ mo_cds_view->get_header( )-description }| ).

*...create header lines for secondary tables
    IF mo_cds_view->has_associations( ).
      lo_table->new_row( ).
      lo_table->span_columns( col_start_span = lo_col1 no_of_cols = 2 ).
      lo_col1->add_text( text = 'Associations' sap_emphasis  = cl_dd_document=>strong ).
      lo_table->new_row( ).


*...print associations
      LOOP AT mo_cds_view->get_associations( ) ASSIGNING FIELD-SYMBOL(<ls_association>).
        lo_col1->add_text( text = |{ <ls_association>-raw_name }| ).
        lo_col2->add_text( text = |{ <ls_association>-ref_cds_view_raw }| ).
        lo_col3->add_text( text = |{ <ls_association>-ddtext }| ).

        lo_table->new_row( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_entity_name.
    result = mv_cds_view.
  ENDMETHOD.


  METHOD handle_alv_ctx_menu_request.
    DATA: lt_entries TYPE sctx_entrytab.

    IF if_selected_cols = abap_true AND
       if_selected_rows = abap_false.
      RETURN.
    ENDIF.

    IF mo_cds_view->has_associations( ).
      lt_entries = VALUE #(
        ( fcode = zif_dbbr_c_selection_functions=>navigate_association
          text  = 'Follow Association...'
          type  = sctx_c_type_function )
        ( type  = sctx_c_type_separator )
      ).
      INSERT LINES OF lt_entries INTO ct_menu_entries INDEX 1.
    ENDIF.
  ENDMETHOD.


  METHOD init.
    mv_cds_view = mv_entity_id.
    TRY.
        mo_cds_view = zcl_dbbr_cds_view_factory=>read_cds_view( mv_cds_view ).
      CATCH zcx_dbbr_data_read_error INTO DATA(lx_read_error).
        MESSAGE lx_read_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD navigate.
    CHECK is_assoc IS NOT INITIAL.

    DATA(lr_cds_navigator) = zcl_dbbr_navigator_creator=>create_cds_navigator(
        ir_t_data             = mr_t_data
        is_tech_info          = ms_technical_info
        it_source_index       = mt_selected_rows
        ir_source_tabfields   = mo_tabfields
        ir_source_cds_view    = mo_cds_view
        is_association        = is_assoc
        it_nav_breadcrumbs    = mt_nav_breadcrumbs
        iv_nav_count          = mv_navigation_count
    ).

    TRY.
        lr_cds_navigator->navigate( ).
      CATCH zcx_dbbr_association_nav_error INTO DATA(lx_assoc_nav).
        lx_assoc_nav->show_message(
            iv_message_type = 'I'
            iv_display_type = 'E'
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD on_association_chosen.
    CHECK es_chosen_association IS NOT INITIAL.

*... determine selected rows
    mt_selected_rows = mr_alv_grid->get_all_selected_rows( ).
    IF mt_selected_rows IS INITIAL.
      MESSAGE i074(zdbbr_info) DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    navigate( es_chosen_association ).
  ENDMETHOD.


  METHOD read_entity_infos.
  ENDMETHOD.


  METHOD refresh_selection.
*.. Check if CDS view was changed in the mean time
    " TOOD: reread complete cds view and rebuild tabfield list
*    if zcl_dbbr_cds_view_factory=>has_cds_view_changed( iv_cds_view_name     = mo_cds_view->mv_view_name
*                                                        iv_last_changed_date = mo_cds_view->get_header( )-chgdate
*                                                        iv_last_changed_time = mo_cds_view->get_header( )-chgtime ).
*      DATA(lf_cds_view_changed) = abap_true.
*      rebuild( ).
*    ENDIF.

    CHECK select_data( if_refresh_only = abap_true ).

    IF ms_control_info-number = 0.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    set_miscinfo_for_selected_data( ).

    RAISE EVENT selection_finished.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~free_resources.
    super->zif_dbbr_screen_util~free_resources( ).

    IF mr_assoc_selector IS BOUND.
      mr_assoc_selector->dispose( ).
      CLEAR mr_assoc_selector.
    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = super->get_deactivated_functions( ).

    IF mo_cds_view->has_parameters( if_exclude_system_params = abap_true ).
      DELETE result WHERE table_line = zif_dbbr_c_selection_functions=>change_cds_parameters.
    ENDIF.

    IF NOT mo_cds_view->has_associations( ).
      result = VALUE #( BASE result
        ( zif_dbbr_c_selection_functions=>navigate_association )
        ( zif_dbbr_c_selection_functions=>set_focus_to_assoc_list )
      ).
    ELSE.
      IF ms_technical_info-assoc_sel_mode <> zif_dbbr_c_assoc_select_mode=>docked.
        result = VALUE #( BASE result
          ( zif_dbbr_c_selection_functions=>set_focus_to_assoc_list )
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_pbo.
    IF if_first_call = abap_true.
      IF ms_technical_info-assoc_sel_mode = zif_dbbr_c_assoc_select_mode=>docked AND
         ms_technical_info-show_assoc_brws_at_start = abap_true AND
*... don't show association browser if no association exist
         mo_cds_view->has_associations( ).
        choose_association( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_ui_function.
    super->handle_ui_function( CHANGING cv_function = cv_function ).

    CASE cv_function.

      WHEN zif_dbbr_c_selection_functions=>navigate_association.
        choose_association( ).
        CLEAR cv_function.

      WHEN zif_dbbr_c_selection_functions=>set_focus_to_assoc_list.
        IF mr_assoc_selector IS BOUND AND mr_assoc_selector->is_visible( ).
          mr_assoc_selector->focus( ).
        ENDIF.

      WHEN zif_dbbr_c_selection_functions=>change_cds_parameters.
        change_parameters( ).

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
