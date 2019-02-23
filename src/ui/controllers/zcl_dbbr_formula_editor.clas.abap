CLASS zcl_dbbr_formula_editor DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_uitb_gui_screen
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.
    METHODS constructor
      IMPORTING
        io_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list
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
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.

    METHODS save_formula.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
    METHODS handle_exit_request
        REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        activate         TYPE ui_func VALUE 'ACTIVATE',
        focus_on_editor  TYPE ui_func VALUE 'FOCUS_EDITOR',
        check            TYPE ui_func VALUE 'CHECK',
        rollname_f4      TYPE ui_func VALUE 'ROLLNAME_F4',
        delete           TYPE ui_func VALUE 'DELETE',
        insert_col_texts TYPE ui_func VALUE 'INSERT_COL_TEXT',
        insert_icon      TYPE ui_func VALUE 'INSERT_ICON',
      END OF c_functions.
    CLASS-DATA sv_dummy_form_text TYPE string.

    DATA mr_template_program TYPE REF TO zif_uitb_template_prog.

    DATA mf_formula_deleted TYPE abap_bool.
    DATA mo_template_tm TYPE REF TO zcl_dbbr_fe_dnd_tree_model.
    DATA mv_display_mode TYPE zdbbr_display_mode.
    DATA mo_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mv_formula TYPE string.
    DATA mv_current_formula TYPE string.
    DATA mf_formula_activated TYPE abap_bool.
    DATA mo_formula TYPE REF TO zcl_dbbr_formula.
    DATA mo_splitter TYPE REF TO zcl_uitb_gui_splitter_cont.
    DATA mo_editor TYPE REF TO zcl_uitb_gui_code_editor.

    METHODS create_formula_editor.

    METHODS validate_formula
      IMPORTING
        if_hide_log_on_success TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rf_valid)        TYPE abap_bool.

    METHODS create_dnd_tree_control.

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
    super->constructor( |{ TEXT-t01 }| ).

    ms_join_def = is_join_def.
    mv_formula =
    mv_current_formula = iv_formula."COND #( WHEN iv_formula IS INITIAL THEN sv_dummy_form_text ELSE iv_formula ).
    mo_tabfield_list = io_tabfield_list.
    mv_display_mode = iv_display_mode.
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.

    CASE io_command->mv_function.

      WHEN c_functions-focus_on_editor.
        mo_editor->focus( ).

      WHEN c_functions-activate.
        DATA(lf_valid) = validate_formula( if_hide_log_on_success = abap_true ).
        " if validation successful set current function
        " as mv_formula and return to DB Browser selection screen
        IF lf_valid = abap_true.
          mf_formula_activated = abap_true.
          MESSAGE |{ 'Formula has been activated'(015) }| TYPE 'S'.

          mv_formula = mv_current_formula.
          leave_screen( ).
        ENDIF.

      WHEN zif_uitb_c_gui_screen=>c_functions-save.
        save_formula( ).

      WHEN c_functions-delete.
        IF delete_formula( ).
          leave_screen( ).
        ENDIF.

      WHEN c_functions-rollname_f4.
        show_ddic_value_help( ).

      WHEN c_functions-check.
        validate_formula( ).

      WHEN c_functions-insert_col_texts.
        set_field_coltexts( ).

      WHEN c_functions-insert_icon.
        insert_icon( ).

    ENDCASE.
  ENDMETHOD.

  METHOD create_content.

    create_control_toolbar(
      EXPORTING
        io_parent    = io_container
        it_button    = VALUE #(
        ( function = c_functions-check
          quickinfo   = |{ 'Check Formula'(016) }|
          icon        = icon_check )
        ( function = c_functions-activate
          quickinfo   = |{ 'Activate Formula'(017) }|
          icon        = icon_activate )
        ( function = c_functions-rollname_f4
          icon        = icon_search
          quickinfo   = |{ 'Find and Insert Data Element'(018) }| )
        ( function = c_functions-delete
          icon        = icon_delete
          quickinfo   = |{ 'Delete Formula and Leave Editor'(019) }| )
        ( function = c_functions-insert_col_texts
          text        = |{ 'Insert Column Texts'(020) }|
          icon        = icon_create_text
          quickinfo   = |{ 'Insert Col. Texts for Formula Field'(021) }| )
        ( function = c_functions-insert_icon
          text        = |{ 'Insert Icon'(022) }| )
        )
      IMPORTING
        eo_toolbar   = DATA(lo_toolbar)
        eo_client    = DATA(lo_container)
    ).

    mo_splitter = NEW zcl_uitb_gui_splitter_cont(
      iv_elements = 2
      iv_size     = '70:30'
      iv_mode     = zcl_uitb_gui_splitter_cont=>c_mode-cols
      io_parent   = lo_container
    ).
    create_formula_editor( ).
    create_dnd_tree_control( ).
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->map_fkey_functions( VALUE #(
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_f1
        mapped_function = c_functions-focus_on_editor
        text            = |{ 'Focus on Editor' }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_f2
        mapped_function = c_functions-check
        text            = |{ 'Check Formula'(016) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-ctrl_f3
        mapped_function = c_functions-activate
        text            = |{ 'Activate Formula'(017) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f4
        mapped_function = c_functions-rollname_f4
        text            = |{ 'Find and Insert Data Element'(018) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f2
        mapped_function = c_functions-delete
        text            = |{ 'Delete Formula and Leave Editor'(019) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f5
        mapped_function = c_functions-insert_col_texts
        text            = |{ 'Insert Col. Texts for Formula Field'(021) }| )
      ( fkey            = zif_uitb_c_gui_screen=>c_functions-shift_f7
        mapped_function = c_functions-insert_icon
        text            = |{ 'Insert Icon'(022) }| )
    ) ).


    IF NOT io_callback->is_first_screen_call( ).
      mv_current_formula = mo_editor->get_text( ).
      IF mv_current_formula <> mv_formula.
        DATA(lv_edited_text) = | ({ TEXT-005 })|.
      ENDIF.
      io_callback->set_title( |DB Browser - Edit Formula{ lv_edited_text }| ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_exit_request.
    mv_current_formula = mo_editor->get_text( ).

    IF mv_current_formula <> mv_formula.
      IF zcl_dbbr_appl_util=>popup_to_confirm(
          iv_title                 = 'Unsaved Changes'
          iv_query                 = 'There unsaved Changes. If you continue these Changes will be lost. Do you want to continue?'
          iv_icon_type             = 'ICON_INFORMATION' ) <> '1' .

        io_callback->cancel_exit( ).
      ENDIF.
    ENDIF.

    zcl_uitb_protocol=>get_instance( )->close_protocol( ).
  ENDMETHOD.


  METHOD create_dnd_tree_control.
    mo_template_tm = NEW zcl_dbbr_fe_dnd_tree_model(
        ir_parent        = mo_splitter->get_container( 2 )
        ir_tabfield_list = mo_tabfield_list
        it_join_tables   = ms_join_def-tables
    ).
    mo_template_tm->show( ).
  ENDMETHOD.


  METHOD create_formula_editor.
    CHECK mo_editor IS INITIAL.

    mo_editor = NEW zcl_uitb_gui_code_editor(
        io_parent = mo_splitter->get_container( 1 )
    ).

    mo_editor->set_text( iv_text = mv_formula ).
  ENDMETHOD.

  METHOD delete_formula.
    IF zcl_dbbr_appl_util=>popup_to_confirm(
         iv_title                 = 'Delete Formula'
         iv_query                 = 'Are you sure you want to delete the current Formula?'
         if_display_cancel_button = abap_false
         iv_icon_type             = 'ICON_INFORMATION' ) = '1'.
      CLEAR mo_formula.
      result =
      mf_formula_deleted = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD determine_calculation_fields.
    DATA: lr_field_ref TYPE REF TO zdbbr_tabfield_info_ui.

    CHECK mo_formula IS BOUND.

    " clear all previous calculation fields
    mo_tabfield_list->clear_calculation_flag( ).

    mo_formula->get_statements( IMPORTING et_statements = DATA(lt_stmnt) ).

    LOOP AT lt_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>) WHERE type <> 'U'.
      LOOP AT <ls_stmnt>-tokens ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE is_row_field = abap_true.
        " remove ROW- from string
        DATA(lv_row_field_raw) = substring_after( val = <ls_token>-str sub = '-' ).

        TRY.
            " check if there is another hyphen because than it is a join field
            IF matches( val = lv_row_field_raw regex = |\\w-.*| ).
              DATA(lv_alias) = substring_before( val = lv_row_field_raw sub = '-' ).
              DATA(lv_field) = substring_after( val = lv_row_field_raw sub = '-' ).

              lr_field_ref = mo_tabfield_list->get_field_ref_by_alv_name( |{ lv_alias }_{ lv_field }| ).
            ELSE.
              lr_field_ref = mo_tabfield_list->get_field_ref( iv_fieldname = CONV #( lv_row_field_raw ) ).
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


  METHOD get_formula.
    rr_formula = mo_formula.
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
      mo_editor->set_selected_text( |{ lv_icon }| ).
    ENDIF.
  ENDMETHOD.


  METHOD save_formula.
    DATA(lv_formula) = mo_editor->get_text( ).

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

    mo_template_tm->refresh_saved_formulas( ).
  ENDMETHOD.


  METHOD set_field_coltexts.

    DATA(lt_fields) = VALUE zcl_dbbr_appl_util=>tt_input_val(
        ( tabname = 'DD03L' fieldname = 'FIELDNAME' field_obl = abap_true fieldtext = 'Formula Field Name'(011) )
        ( tabname = 'DD04T' fieldname = 'SCRTEXT_M' field_obl = abap_true fieldtext = 'Short Description'(012) )
        ( tabname = 'DD04T' fieldname = 'SCRTEXT_L' fieldtext = 'Long Text (Tooltip)'(013) )
    ).

    DATA(lf_cancelled) = zcl_dbbr_appl_util=>popup_get_values(
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
      mo_editor->set_selected_text( lv_coltexts ).

      mo_editor->focus( ).
    ENDIF.

  ENDMETHOD.


  METHOD show_ddic_value_help.
    DATA: lv_rollname TYPE se16n_value.

    " get value of selected text
    lv_rollname = mo_editor->get_selected_text( ).

    zcl_dbbr_f4_helper=>call_built_in_f4(
      EXPORTING iv_tablename = 'DD04L'
                iv_fieldname = 'ROLLNAME'
                iv_selvalue  = CONV #( lv_rollname )
      CHANGING  cv_value     = lv_rollname
    ).

    IF lv_rollname IS NOT INITIAL.
      mo_editor->set_selected_text( |{ lv_rollname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD show_subroutine_pool.
    CHECK validate_formula( if_hide_log_on_success = abap_true ).

    DATA(lr_tabfield_list_copy) = mo_tabfield_list->copy( ).

    zcl_dbbr_formula_helper=>update_tabflds_from_formula(
        ir_tabfields                = mo_tabfield_list
        ir_formula                  = mo_formula
    ).

    lr_tabfield_list_copy->build_complete_fieldnames( ).

    " create component table for all output table fields
    DATA(lt_components) = zcl_dbbr_output_tab_builder=>create_dyn_comp_tab(
        ir_tabfields       = lr_tabfield_list_copy
        is_tech_info       = VALUE #( use_reduced_memory = abap_false )
    ).

    DATA(lr_builder) = zcl_dbbr_fe_form_builder=>get_builder_for_subroutine_gen(
       ir_formula        = mo_formula
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

    mv_current_formula = mo_editor->get_text( ).

    TRY.
        zcl_dbbr_screen_helper=>show_progress(
            iv_progress = 1
            iv_text     = |{ TEXT-006 }|
        ).

        DATA(lr_validator) = NEW zcl_dbbr_fe_validator( iv_formula    = mv_current_formula
                                                         ir_tabfields  = mo_tabfield_list ).
        mo_formula = lr_validator->validate( ).

        IF if_hide_log_on_success = abap_false.
          MESSAGE s046(zdbbr_info).
        ENDIF.

        " formula is ok
        rf_valid = abap_true.
      CATCH zcx_dbbr_formula_exception INTO DATA(lr_exception).
        IF lr_exception->invalid_row >= 0.
          mo_editor->select_row( lr_exception->invalid_row  ).
        ENDIF.
        CLEAR mo_formula.
        IF lr_exception->syntax_message IS NOT INITIAL.
          lr_protocol->add_error(
              iv_message     = lr_exception->syntax_message
              iv_line_number = CONV #( lr_exception->invalid_row )
          ).
        ELSE.
          lr_exception->zif_dbbr_exception_message~get_message( ).
          lr_protocol->add_error_from_sy( ).
        ENDIF.
        lr_protocol->show_protocol( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
