CLASS zcl_dbbr_formula_editor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_view.
    CLASS-METHODS class_constructor.
    METHODS constructor
      IMPORTING
        ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list
        is_join_def      TYPE zdbbr_join_def
        iv_display_mode  TYPE zdbbr_display_mode
        iv_formula       TYPE string.
    METHODS has_formula_been_deleted
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS has_formula_been_activated
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_formula
      RETURNING
        VALUE(rr_formula) TYPE REF TO zcl_dbbr_formula.
  PROTECTED SECTION.

    METHODS save_formula.
    METHODS do_before_first_screen_call .
    METHODS on_before_output
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
    METHODS on_user_command
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          !ev_function_id
          !er_callback .
    METHODS on_exit
          FOR EVENT exit OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
  PRIVATE SECTION.
    CONSTANTS c_activate_function TYPE sy-ucomm VALUE zif_uitb_template_prog=>c_func_ctrl_f3.
    CONSTANTS c_check_function TYPE sy-ucomm VALUE zif_uitb_template_prog=>c_func_ctrl_f2.
    CONSTANTS c_view_edit TYPE sy-ucomm VALUE zif_uitb_template_prog=>c_func_ctrl_f1.
    CLASS-DATA sv_dummy_form_text TYPE string.

    DATA mr_template_program TYPE REF TO zif_uitb_template_prog.

    DATA mr_editor TYPE REF TO zcl_dbbr_fe_text_editor.
    DATA mf_formula_deleted TYPE abap_bool.
    DATA mr_template_tm TYPE REF TO zcl_dbbr_fe_dnd_tree_model.
    DATA mv_display_mode TYPE zdbbr_display_mode.
    DATA mr_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list.
    DATA mr_splitter_container TYPE REF TO cl_gui_splitter_container.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mv_formula TYPE string.
    DATA mv_current_formula TYPE string.
    DATA: mr_formula_input_container TYPE REF TO cl_gui_container,
          mf_formula_activated       TYPE abap_bool,
          mr_formula                 TYPE REF TO zcl_dbbr_formula.

    METHODS do_on_first_screen_call.
    METHODS create_formula_editor.

    METHODS validate_formula
      IMPORTING
        if_hide_log_on_success TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rf_valid)        TYPE abap_bool.
    METHODS create_splitter.
    METHODS create_dnd_tree_control.
    METHODS free_resources.
    METHODS show_subroutine_pool.
    METHODS show_ddic_value_help.
    METHODS delete_formula
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_field_coltexts.
    METHODS determine_calculation_fields
      RAISING
        zcx_dbbr_formula_exception.
    METHODS insert_icon.

ENDCLASS.



CLASS zcl_dbbr_formula_editor IMPLEMENTATION.


  METHOD class_constructor.
    sv_dummy_form_text = `* Define your Formula here` && cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.


  METHOD constructor.
    ms_join_def = is_join_def.
    mv_formula = COND #( WHEN iv_formula IS INITIAL THEN sv_dummy_form_text ELSE iv_formula ).
    mr_tabfield_list = ir_tabfield_list.
    mv_display_mode = iv_display_mode.

    " create template program
    mr_template_program = zcl_uitb_templt_prog_callback=>create_template_program( CONV #( TEXT-t01 ) ).

    " register event handlers
    SET HANDLER: on_before_output FOR mr_template_program,
                 on_user_command FOR mr_template_program,
                 on_exit FOR mr_template_program.
  ENDMETHOD.


  METHOD create_dnd_tree_control.
    " get right container of splitter control
    DATA(lr_right_container) = mr_splitter_container->get_container( row = 1 column = 2 ).

    mr_template_tm = NEW zcl_dbbr_fe_dnd_tree_model(
        ir_parent        = lr_right_container
        ir_tabfield_list = mr_tabfield_list
        it_join_tables   = ms_join_def-tables
    ).
    mr_template_tm->show( ).

  ENDMETHOD.


  METHOD create_formula_editor.
    CHECK mr_editor IS INITIAL.

    DATA(lr_container) = mr_splitter_container->get_container( row = 1 column = 1 ).

    mr_editor = NEW zcl_dbbr_fe_text_editor(
        ir_parent    = lr_container
        ir_tabfields = mr_tabfield_list
    ).

    mr_editor->set_text( iv_text = mv_formula ).
  ENDMETHOD.


  METHOD create_splitter.
    CHECK mr_splitter_container IS INITIAL.

    mr_splitter_container = NEW cl_gui_splitter_container(
        parent  = mr_template_program->get_container( )
        columns = 2
        rows    = 1
    ).

    mr_splitter_container->set_column_width( id = 1 width = 65 ).

    mr_formula_input_container = mr_splitter_container->get_container( row = 1 column = 1 ).

  ENDMETHOD.


  METHOD delete_formula.
    IF zcl_dbbr_appl_util=>popup_to_confirm(
         iv_title                 = 'Delete Formula'
         iv_query                 = 'Are you sure you want to delete the current Formula?'
         if_display_cancel_button = abap_false
         iv_icon_type             = 'ICON_INFORMATION' ) = '1'.
      CLEAR mr_formula.
      result =
      mf_formula_deleted = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD determine_calculation_fields.
    DATA: lr_field_ref TYPE REF TO zdbbr_tabfield_info_ui.

    CHECK mr_formula IS BOUND.

    " clear all previous calculation fields
    mr_tabfield_list->clear_calculation_flag( ).

    mr_formula->get_statements( IMPORTING et_statements = DATA(lt_stmnt) ).

    LOOP AT lt_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>) WHERE type <> 'U'.
      LOOP AT <ls_stmnt>-tokens ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE is_row_field = abap_true.
        " remove ROW- from string
        DATA(lv_row_field_raw) = substring_after( val = <ls_token>-str sub = '-' ).

        TRY.
            " check if there is another hyphen because than it is a join field
            IF matches( val = lv_row_field_raw regex = |\\w-.*| ).
              DATA(lv_alias) = substring_before( val = lv_row_field_raw sub = '-' ).
              DATA(lv_field) = substring_after( val = lv_row_field_raw sub = '-' ).

              lr_field_ref = mr_tabfield_list->get_field_ref_by_alv_name( |{ lv_alias }_{ lv_field }| ).
            ELSE.
              lr_field_ref = mr_tabfield_list->get_field_ref( iv_fieldname = CONV #( lv_row_field_raw ) ).
            ENDIF.

            " field was found -> add it to the list of needed calculation fields for the formula
            lr_field_ref->is_calculation_field = abap_true.
          CATCH cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
              EXPORTING
                textid = zcx_dbbr_formula_exception=>needed_calc_fld_not_in_list
                msgv1  = |{ lv_row_field_raw }|.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD do_before_first_screen_call.
    DATA: lf_test_mode TYPE abap_bool.

    DATA(lf_experimental_mode) = NEW zcl_dbbr_usersettings_factory( )->is_experimental_mode_active( ).

    mr_template_program->add_function(
        iv_function_id = c_activate_function
        iv_text        = 'Activate Formula'
        iv_icon        = icon_activate
    ).
    mr_template_program->add_function(
        iv_function_id = c_check_function
        iv_text        = 'Check Formula'
        iv_icon        = icon_check
    ).

    IF lf_test_mode = abap_true.
      mr_template_program->add_function(
          iv_function_id = zif_uitb_template_prog=>c_func_f7
          iv_text        = 'Subroutine'
          iv_icon        = icon_generate
          iv_quickinfo   = 'Show Subroutine'
      ).
    ENDIF.

    mr_template_program->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_shift_f4
        iv_text        = 'Find Data Element'
        iv_icon        = icon_search
        iv_quickinfo   = 'Find and Insert Data Element'
    ).

    mr_template_program->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_shift_f2
        iv_text        = 'Delete Formula'
        iv_icon        = icon_delete
        iv_quickinfo   = 'Delete Formula and Leave Editor'
    ).

    mr_template_program->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_ctrl_f5
        iv_text        = 'Insert Column Texts'
        iv_icon        = icon_create_text
        iv_icon_text   = 'Column Texts'
        iv_quickinfo   = 'Insert Columns Texts for Formula Field'
    ).

    mr_template_program->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_ctrl_f6
        iv_text        = 'Insert Icon'
    ).

  ENDMETHOD.


  METHOD do_on_first_screen_call.
    create_splitter( ).
    create_formula_editor( ).
    create_dnd_tree_control( ).
  ENDMETHOD.


  METHOD free_resources.
    FREE mr_splitter_container.
    FREE mr_editor.
  ENDMETHOD.


  METHOD get_formula.
    rr_formula = mr_formula.
  ENDMETHOD.


  METHOD has_formula_been_activated.
    result = mf_formula_activated.
  ENDMETHOD.


  METHOD has_formula_been_deleted.
    result = mf_formula_deleted.
  ENDMETHOD.


  METHOD insert_icon.
    DATA(lv_icon) = zcl_dbbr_icon_handler=>show_icon_value_help( ).

    IF lv_icon IS NOT INITIAL.
      mr_editor->set_selected_text( CONV #( lv_icon ) ).
    ENDIF.
  ENDMETHOD.


  METHOD on_before_output.
    IF er_callback->is_first_screen_call( ).
      do_on_first_screen_call( ).

      mr_editor->zif_uitb_gui_control~focus( ).
    ELSE.
      mv_current_formula = mr_editor->get_raw_text_as_string( ).
      IF mv_current_formula <> mv_formula.
        DATA(lv_edited_text) = | ({ TEXT-005 })|.
      ENDIF.
      er_callback->set_title( |DB Browser - Edit Formula{ lv_edited_text }| ).
    ENDIF.

  ENDMETHOD.


  METHOD on_exit.
    mv_current_formula = mr_editor->get_raw_text_as_string( ).

    IF mv_current_formula <> mv_formula.
      IF zcl_dbbr_appl_util=>popup_to_confirm(
          iv_title                 = 'Unsaved Changes'
          iv_query                 = 'There unsaved Changes. If you continue these Changes will be lost. Do you want to continue?'
          iv_icon_type             = 'ICON_INFORMATION' ) <> '1' .

        er_callback->cancel_exit( ).
      ENDIF.
    ENDIF.

    zcl_uitb_protocol=>get_instance( )->close_protocol( ).
  ENDMETHOD.


  METHOD on_user_command.
    CASE ev_function_id.

      WHEN c_activate_function.
        DATA(lf_valid) = validate_formula( if_hide_log_on_success = abap_true ).
        " if validation successful set current function
        " as mv_formula and return to DB Browser selection screen
        IF lf_valid = abap_true.
          mf_formula_activated = abap_true.
          MESSAGE |Formula has been activated| TYPE 'S'.

          mv_formula = mv_current_formula.
          er_callback->exit_screen( ).
        ENDIF.

      WHEN zif_uitb_template_prog=>c_save.
        save_formula( ).

      WHEN zif_uitb_template_prog=>c_func_shift_f2.
        IF delete_formula( ).
          er_callback->exit_screen( ).
        ENDIF.

      WHEN zif_uitb_template_prog=>c_func_shift_f4.
        show_ddic_value_help( ).

      WHEN c_view_edit.
        mr_editor->toggle_view_edit_mode( ).

      WHEN c_check_function.
        validate_formula( ).

      WHEN zif_uitb_template_prog=>c_func_ctrl_f5.
        set_field_coltexts( ).

      WHEN zif_uitb_template_prog=>c_func_ctrl_f6.
        insert_icon( ).

      WHEN zif_uitb_template_prog=>c_func_f7.
        show_subroutine_pool( ).
    ENDCASE.
  ENDMETHOD.


  METHOD save_formula.
    DATA(lv_formula) = mr_editor->get_raw_text_as_string( ).


    DATA(lv_formula_descr) = zcl_dbbr_appl_util=>popup_get_value(
      EXPORTING
        is_field = VALUE #( tabname = 'DD01T' fieldname = 'DDTEXT' field_obl = abap_true fieldtext = 'Formula Description'(007) )
        iv_title = |{ 'Enter Formula name'(008) }|
    ).

    IF lv_formula_descr IS INITIAL.
      MESSAGE |{ 'Formula has not been saved'(009) }| TYPE 'S'.
      RETURN.
    ENDIF.

    DATA(lr_formula_f) = NEW zcl_dbbr_formula_factory( ).

    lr_formula_f->save_formula_def(
        iv_formula_string = lv_formula
        iv_description    = CONV #( lv_formula_descr )
    ).

    MESSAGE |{ 'Formula has been saved with'(010) }{ lv_formula_descr }'| TYPE 'S'.

    mr_template_tm->refresh_saved_formulas( ).
  ENDMETHOD.


  METHOD set_field_coltexts.

    data(lt_fields) = VALUE zcl_dbbr_appl_util=>tt_input_val(
        ( tabname = 'DD03L' fieldname = 'FIELDNAME' field_obl = abap_true fieldtext = 'Formula Field Name'(011) )
        ( tabname = 'DD04T' fieldname = 'SCRTEXT_M' field_obl = abap_true fieldtext = 'Short Description'(012) )
        ( tabname = 'DD04T' fieldname = 'SCRTEXT_L' fieldtext = 'Long Text (Tooltip)'(013) )
    ).

    data(lf_cancelled) = zcl_dbbr_appl_util=>popup_get_values(
      EXPORTING
        iv_title     = |{ 'Define Column texts for formula Field'(014) }|
      CHANGING
        ct_fields    = lt_fields
    ).

    IF lf_cancelled = abap_false.
      DATA(lv_form_field) = lt_fields[ fieldname = 'FIELDNAME' ]-value.
      DATA(lv_shorttext) = lt_fields[ fieldname = 'SCRTEXT_M' ]-value.
      DATA(lv_longtext) = lt_fields[ fieldname = 'SCRTEXT_L' ]-value.
      DATA(lv_coltexts) = |$TEXT { lv_form_field } '{ lv_shorttext }'|.
      IF lv_longtext IS NOT INITIAL.
        lv_coltexts = |{ lv_coltexts } '{ lv_longtext }'|.
      ENDIF.
      lv_coltexts = lv_coltexts && '.'.
      mr_editor->set_selected_text( lv_coltexts ).

      mr_editor->zif_uitb_gui_control~focus( ).
    ENDIF.

  ENDMETHOD.


  METHOD show_ddic_value_help.
    DATA: lv_rollname TYPE se16n_value.

    " get value of selected text
    lv_rollname = mr_editor->get_selected_text( ).

    zcl_dbbr_f4_helper=>call_built_in_f4(
      EXPORTING iv_tablename = 'DD04L'
                iv_fieldname = 'ROLLNAME'
                iv_selvalue  = CONV #( lv_rollname )
      CHANGING  cv_value     = lv_rollname
    ).

    IF lv_rollname IS NOT INITIAL.
      mr_editor->set_selected_text( CONV #( lv_rollname ) ).
    ENDIF.

  ENDMETHOD.


  METHOD show_subroutine_pool.
    CHECK validate_formula( if_hide_log_on_success = abap_true ).

    DATA(lr_tabfield_list_copy) = mr_tabfield_list->copy( ).

    zcl_dbbr_formula_helper=>update_tabflds_from_formula(
        ir_tabfields                = mr_tabfield_list
        ir_formula                  = mr_formula
    ).

    lr_tabfield_list_copy->build_complete_fieldnames( ).

    " create component table for all output table fields
    DATA(lt_components) = zcl_dbbr_output_tab_builder=>create_dyn_comp_tab(
        ir_tabfields       = lr_tabfield_list_copy
        is_tech_info       = VALUE #( use_reduced_memory = abap_false )
    ).

    DATA(lr_builder) = zcl_dbbr_fe_form_builder=>get_builder_for_subroutine_gen(
       ir_formula        = mr_formula
       ir_tabfields      = lr_tabfield_list_copy
       it_tab_components = lt_components
    ).


    lr_builder->build_formula( IMPORTING et_lines = DATA(lt_lines) ).
    DATA(lv_program_string) = zcl_dbbr_appl_util=>convert_string_tab_to_string( lt_lines ).

    NEW zcl_uitb_popup_editor( iv_text = lv_program_string )->zif_uitb_view~show( ).
  ENDMETHOD.


  METHOD validate_formula.
    DATA(lr_protocol) = zcl_uitb_protocol=>get_instance( ).
    lr_protocol->close_protocol( ).
    lr_protocol->clear( ).

    mv_current_formula = mr_editor->get_raw_text_as_string( ).

    TRY.
        zcl_dbbr_screen_helper=>show_progress(
            iv_progress = 1
            iv_text     = |{ TEXT-006 }|
        ).

        DATA(lr_validator) = NEW zcl_dbbr_fe_validator( iv_formula    = mv_current_formula
                                                         ir_tabfields  = mr_tabfield_list ).
        mr_formula = lr_validator->validate( ).

        IF if_hide_log_on_success = abap_false.
          MESSAGE s046(zdbbr_info).
        ENDIF.

        " formula is ok
        rf_valid = abap_true.
      CATCH zcx_dbbr_formula_exception INTO DATA(lr_exception).
        IF lr_exception->invalid_row >= 0.
          mr_editor->select_row( lr_exception->invalid_row  ).
        ENDIF.
        CLEAR mr_formula.
        lr_exception->zif_dbbr_exception_message~get_message( ).
        lr_protocol->add_error_from_sy( ).
        lr_protocol->show_protocol( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_uitb_view~show.
    do_before_first_screen_call( ).

    mr_template_program->zif_uitb_view~show( ).
  ENDMETHOD.
ENDCLASS.
