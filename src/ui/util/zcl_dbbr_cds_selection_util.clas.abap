"! <p class="shorttext synchronized" lang="en">Utitlity methods for CDS Views for Selection Controller</p>
CLASS zcl_dbbr_cds_selection_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selection_util
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS build_simple_alv_title
        REDEFINITION .
    METHODS fill_header
        REDEFINITION .
    METHODS get_entity_name
        REDEFINITION .
    METHODS handle_alv_ctx_menu_request
        REDEFINITION .
    METHODS init
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
    METHODS ignore_empty_result
        REDEFINITION.
    METHODS before_selection
        REDEFINITION.
    METHODS after_selection
        REDEFINITION.
  PRIVATE SECTION.

    "! Name of CDS view
    DATA mv_cds_view TYPE zsat_cds_view_name .
    "! CDS View
    DATA mo_cds_view TYPE REF TO zcl_sat_cds_view .
    "! Utilities for screen
    DATA mr_assoc_selector TYPE REF TO zcl_dbbr_cds_sub_entity_sel .
    DATA mo_virtual_elem_handler TYPE REF TO zcl_dbbr_virtual_elem_handler.
    DATA mf_handle_virtual_elem TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Change CDS parameter values</p>
    METHODS change_parameters .
    "! <p class="shorttext synchronized" lang="en">Choose Association for navigation</p>
    METHODS choose_association .
    "! <p class="shorttext synchronized" lang="en">Performs Association navigation</p>
    "! @parameter IS_ASSOC | <p class="shorttext synchronized" lang="en"></p>
    METHODS navigate
      IMPORTING
        !is_assoc TYPE zsat_cds_association .
    "! <p class="shorttext synchronized" lang="en">Shows the Source Code of the CDS</p>
    METHODS show_cds_source.
    "! <p class="shorttext synchronized" lang="en">Get virtual element handler instance</p>
    "!
    "! @parameter ro_virtual_elem_handler | <p class="shorttext synchronized" lang="en">Instance of virtual element handler</p>
    METHODS get_virtual_elem_handler
      RETURNING
        VALUE(ro_virtual_elem_handler) TYPE REF TO zcl_dbbr_virtual_elem_handler.

    "! <p class="shorttext synchronized" lang="en">Mark fields that are needed for virtual element calculation</p>
    METHODS mark_virtual_elem_requested
      RAISING
        zcx_dbbr_application_exc.
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

  METHOD show_cds_source.
    TRY.
        DATA(lv_source) = zcl_sat_cds_view_factory=>read_ddls_source( mv_cds_view ).
        zcl_uitb_abap_code_viewer=>show_code(
            iv_title = |DDL Source { mo_cds_view->get_header( )-entityname_raw }|
            iv_code  = lv_source
            iv_theme = ms_technical_info-code_viewer_theme
        ).
      CATCH zcx_sat_application_exc INTO DATA(lx_app_error).
        lx_app_error->zif_sat_exception_message~print( ).
    ENDTRY.
  ENDMETHOD.

  METHOD change_parameters.
    DATA: lf_trigger_select TYPE abap_bool.

    DATA(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
        io_tabfields = mo_tabfields_all
        iv_cds_view_name = mv_cds_view
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
      mo_select_program->update_from( mt_from ).
      refresh_selection( ).
    ENDIF.
  ENDMETHOD.


  METHOD choose_association.
    DATA: lr_association_chooser TYPE REF TO zcl_dbbr_cds_sub_entity_sel.

    DATA(lf_show_docked) = xsdbool( ms_technical_info-assoc_sel_mode = zif_dbbr_c_assoc_select_mode=>docked ).

    IF lf_show_docked = abap_false.
      mt_selected_rows = mo_alv_grid->get_all_selected_rows( ).
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
    DATA: lv_from_alias_text TYPE string,
          lv_entity          TYPE string.

    DATA(ls_header) = mo_cds_view->get_header( ).
    IF mv_source_entity_id IS NOT INITIAL.
      lv_entity = mv_source_entity_id.
    ELSEIF ms_technical_info-use_ddl_view_for_select = abap_true AND
           ls_header-source_type <> zif_sat_c_cds_view_type=>table_function.
      lv_entity = mo_cds_view->get_header( )-ddlview.
      lv_from_alias_text = | AS { mo_cds_view->mv_view_name }|.
    ELSE.
      lv_entity = mv_cds_view.
    ENDIF.

*..if we have a source entity it means we also have to use it´s parameters
*... e.g when following associations
    IF mv_source_entity_id IS NOT INITIAL
        AND mv_source_params IS NOT INITIAL.
      DATA(lv_params) = mv_source_params.
    ELSEIF mo_cds_view->has_parameters( ).
      lv_params = zcl_dbbr_cds_param_util=>build_param_string(
        iv_param_indentation = strlen( lv_entity )
        it_param_values      = VALUE zif_sat_ty_global=>ty_t_cds_param_value(
          FOR ls_param_value IN mt_param_values
          WHERE ( is_parameter = abap_true )
          ( name  = ls_param_value-fieldname
            value = ls_param_value-low ) ) ).
    ENDIF.

    IF lv_params IS NOT INITIAL.
      SPLIT |{ lv_entity }{ lv_params }{ lv_from_alias_text }| AT cl_abap_char_utilities=>cr_lf INTO TABLE mt_from.
    ELSE.
      mt_from = VALUE #( ( lv_entity && lv_from_alias_text ) ).
    ENDIF.

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

    super->handle_alv_ctx_menu_request(
      EXPORTING if_selected_cols  = if_selected_cols
                if_selected_rows  = if_selected_rows
                if_selected_cells = if_selected_cells
                it_selected_cols  = it_selected_cols
                it_selected_cells = it_selected_cells
                it_fieldcat       = it_fieldcat
      CHANGING  ct_menu_entries   = ct_menu_entries ).

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


  METHOD ignore_empty_result.
    " If CDS is opened from ADT and has parameter the user should have the chance to
    " enter correct parameters if no data was found
    IF mr_s_global_data->called_from_adt = abap_true AND
        mo_cds_view->has_parameters( if_exclude_system_params = abap_true ).
      rf_ignore_empty = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD init.
    mv_cds_view = mv_entity_id.
    TRY.
        mo_cds_view = zcl_sat_cds_view_factory=>read_cds_view( mv_cds_view ).
      CATCH zcx_sat_data_read_error INTO DATA(lx_read_error).
        lx_read_error->zif_sat_exception_message~print( iv_msg_type = 'I' ).
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
        it_param_values       = VALUE #(
          FOR param IN mt_param_values WHERE ( is_parameter = abap_true ) ( name = param-fieldname value = param-low )
        )
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
    mt_selected_rows = mo_alv_grid->get_all_selected_rows( ).
    IF mt_selected_rows IS INITIAL.
      MESSAGE i074(zdbbr_info) DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    navigate( es_chosen_association ).
  ENDMETHOD.


  METHOD read_entity_infos.
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
    DELETE result WHERE table_line = zif_dbbr_c_selection_functions=>show_cds_source.

    IF mo_cds_view->has_parameters( if_exclude_system_params = abap_true )
      AND mv_source_entity_id IS INITIAL.
      DELETE result WHERE table_line = zif_dbbr_c_selection_functions=>change_cds_parameters.
    ENDIF.

    IF mo_cds_view->has_associations( ).
      IF ms_technical_info-assoc_sel_mode <> zif_dbbr_c_assoc_select_mode=>docked.
        result = VALUE #( BASE result
          ( zif_dbbr_c_selection_functions=>set_focus_to_assoc_list ) ).
      ENDIF.
    ELSE.
      result = VALUE #( BASE result
        ( zif_dbbr_c_selection_functions=>navigate_association )
        ( zif_dbbr_c_selection_functions=>set_focus_to_assoc_list ) ).
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
        CLEAR cv_function.

      WHEN zif_dbbr_c_selection_functions=>show_cds_source.
        show_cds_source( ).
        CLEAR cv_function.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD after_selection.

    IF mf_handle_virtual_elem = abap_true.
      TRY.
          get_virtual_elem_handler( )->calculate_elements(
            EXPORTING
              iv_entity_name = mo_cds_view->mv_view_name
            CHANGING
              ct_data = mr_t_data ).
        CATCH zcx_dbbr_application_exc INTO DATA(lo_exception).
          IF ms_technical_info-ignore_error_virt_elem_calc = abap_false.
            RAISE EXCEPTION lo_exception.
          ENDIF.
      ENDTRY.
    ENDIF.

    super->after_selection( ).

  ENDMETHOD.

  METHOD before_selection.

    IF ms_technical_info-calculate_virtual_element = abap_true
      AND mo_tabfields->has_virtual_element_fields(
            if_consider_output_only = ms_technical_info-use_reduced_memory ).
      mark_virtual_elem_requested( ).
    ENDIF.
    super->before_selection( ).

  ENDMETHOD.

  METHOD mark_virtual_elem_requested.

* when use_reduced_memory is activated consider only output fields
* else consider all fields as the user can include the fields
* through layout change at any time
    DATA(lt_fields) = mo_tabfields->get_fields_for_db_selection(
      if_consider_virtual_element = abap_true
      if_consider_output_only     = xsdbool( ms_technical_info-use_reduced_memory = abap_true ) ).

    mf_handle_virtual_elem = abap_true.

    IF ms_technical_info-use_reduced_memory = abap_true.
      DATA(lt_requested_elements) = get_virtual_elem_handler( )->determine_requested_elements(
          io_cds_view = mo_cds_view
          it_fields   = lt_fields ).

      LOOP AT lt_requested_elements ASSIGNING FIELD-SYMBOL(<lv_requested_element>).
        TRY.
            DATA(lr_s_field) = mo_tabfields->get_field_ref( iv_fieldname = CONV #( <lv_requested_element> ) ).
            lr_s_field->needed_for_virtual_elem = abap_true.

          CATCH cx_sy_itab_line_not_found.
            DATA(ls_new_field) = mo_tabfields_all->get_field( iv_fieldname = CONV #( <lv_requested_element> ) ) .
            ls_new_field-needed_for_virtual_elem = abap_true.
            mo_tabfields->append_tabfield_info( is_tabfield = ls_new_field ).
        ENDTRY.

      ENDLOOP.
    ENDIF.

    TRY.
        get_virtual_elem_handler( )->adjust_requested(
          iv_entity_name = mo_cds_view->mv_view_name
          it_fields      = lt_fields ).
      CATCH zcx_dbbr_application_exc INTO DATA(lo_exception).
        IF ms_technical_info-ignore_error_virt_elem_calc = abap_false.
          RAISE EXCEPTION lo_exception.
        ENDIF.
    ENDTRY.

  ENDMETHOD.

  METHOD get_virtual_elem_handler.
    IF mo_virtual_elem_handler IS INITIAL.
      mo_virtual_elem_handler = NEW #( ).
    ENDIF.
    ro_virtual_elem_handler = mo_virtual_elem_handler.
  ENDMETHOD.

ENDCLASS.
