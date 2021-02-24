CLASS ZCL_DBBR_addtext_manager DEFINITION
  INHERITING FROM ZCL_UITB_FULLSCREEN_ALV_TABLE
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_table_list TYPE ZIF_SAT_TY_GLOBAL=>ty_t_selopt optional.
  PROTECTED SECTION.
    METHODS: select_data REDEFINITION,
      get_report_id REDEFINITION,
      get_status REDEFINITION,
      get_title REDEFINITION,
      get_selection_mode REDEFINITION,
      get_popup_dimensions REDEFINITION,
      adjust_columns REDEFINITION,
      on_user_command REDEFINITION,
      do_after_alv_creation redefinition.
    METHODS: get_table_reference REDEFINITION.
  PRIVATE SECTION.
    DATA: mt_addtext_data TYPE ZDBBR_addtext_ui_itab,
          mt_table_selopt TYPE ZIF_SAT_TY_GLOBAL=>ty_t_selopt,
          mt_tabinfo      TYPE ZDBBR_tabinfo_itab.

    METHODS add_manual_entry .
    METHODS edit_manual_entry .
    METHODS delete_manual_entry .
ENDCLASS.



CLASS ZCL_DBBR_ADDTEXT_MANAGER IMPLEMENTATION.


  METHOD add_manual_entry.
*&---------------------------------------------------------------------*
*& Description: Adds new manual textfield entry
*&---------------------------------------------------------------------*
    DATA(lf_data_saved) = abap_false.

    DATA(ls_addtext_entry) = VALUE zdbbr_addtext( ).

    DATA(lr_addtextfield_controller) = NEW zcl_dbbr_addtextfield_ctrl(
      is_addtext_data = ls_addtext_entry
      it_id_tables    = mt_tabinfo
      iv_mode         = zif_dbbr_c_global=>c_display_modes-create
    ).

    lr_addtextfield_controller->zif_uitb_screen_controller~call_screen( ).

    IF lr_addtextfield_controller->zif_uitb_screen_controller~was_not_cancelled( ).
      ls_addtext_entry = lr_addtextfield_controller->get_current_data( ).

      APPEND CORRESPONDING #( ls_addtext_entry ) TO mt_addtext_data ASSIGNING FIELD-SYMBOL(<ls_new_addtext>).
      <ls_new_addtext>-is_manual = abap_true.
      <ls_new_addtext>-selection_type = zif_dbbr_c_text_selection_type=>text_table.
    ENDIF.

    refresh( ).
  ENDMETHOD.


  METHOD adjust_columns.
    DATA: lr_column TYPE REF TO cl_salv_column_table.

    ir_columns->set_optimize( ).

    ir_columns->get_column( 'ADDTEXT_ID' )->set_technical( ).
    ir_columns->get_column( 'SELECTION_TYPE' )->set_technical( ).
    ir_columns->get_column( 'ID_FIELD_ALV_INT' )->set_technical( ).
    ir_columns->get_column( 'ID_FIELD2_ALV_INT' )->set_technical( ).
    ir_columns->get_column( 'CONDITION_FIELD_ALV_INT' )->set_technical( ).
    ir_columns->get_column( 'ID_FIELD_DOMNAME' )->set_technical( ).
    ir_columns->get_column( 'ID_FIELD_ROLLNAME' )->set_technical( ).
    ir_columns->get_column( 'TEXT_FIELD_ALV_INT' )->set_technical( ).
    ir_columns->get_column( 'CUSTOM_SH_REF_JOIN_ID' )->set_technical( ).

    ir_columns->get_column( 'IS_MANUAL' )->set_technical( ).

    lr_column ?= ir_columns->get_column( 'ID_TABLE' ).
    lr_column->set_key( ).

    lr_column ?= ir_columns->get_column( 'ID_FIELD' ).
    lr_column->set_key( ).

    lr_column ?= ir_columns->get_column( 'ID_FIELD2' ).
    lr_column->set_key( ).
    lr_column->set_short_text( space ).
    lr_column->set_long_text( space ).
    lr_column->set_medium_text( 'Field 2' ).

    lr_column ?= ir_columns->get_column( 'TEXT_TABLE' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( 'Text Table' ).

    lr_column ?= ir_columns->get_column( 'TEXT_FIELD' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( 'Text Field' ).

    lr_column ?= ir_columns->get_column( 'KEY_FIELD' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( 'Key Field' ).

    lr_column ?= ir_columns->get_column( 'KEY_FIELD2' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( 'Key Field 2' ).

    lr_column ?= ir_columns->get_column( 'LANGUAGE_FIELD' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( 'Language Field' ).

    lr_column ?= ir_columns->get_column( 'CONDITION_OP' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( 'Op.' ).
    lr_column->set_key( ).

    lr_column ?= ir_columns->get_column( 'CONDITION_FIELD' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( 'Field (Condition)' ).
    lr_column->set_key( ).

    lr_column ?= Ir_columns->get_column( 'CONDITION_VALUE' ).
    lr_column->set_key( ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mt_table_selopt = it_table_list.

    mt_tabinfo = VALUE ZDBBR_tabinfo_itab(
        FOR tab IN mt_table_selopt
        ( tabname    = tab-low
          description = zcl_sat_ddic_repo_access=>get_table_info( CONV #( tab-low ) )-ddtext )
    ).
  ENDMETHOD.


  METHOD delete_manual_entry.
*&---------------------------------------------------------------------*
*& Description: Deletes manual textfield entry
*&---------------------------------------------------------------------*
    DATA(lv_current_line) = get_selected_row( ).
    IF lv_current_line <= 0.
      RETURN.
    ENDIF.

    DATA(ls_current_line_value) = mt_addtext_data[ lv_current_line ].

    CHECK zcl_dbbr_appl_util=>popup_to_confirm(
        iv_title     = 'Delete'
        iv_query     = |Are you certain the Additional Text for Table { ls_current_line_value-id_table } and Field | &&
                       |{ ls_current_line_value-id_field } should be deleted?|
        iv_icon_type = 'ICON_WARNING'
    ) = '1'.

    " delete entry from database
    DATA(lr_addtext_f) = NEW zcl_dbbr_addtext_factory( ).
    lr_addtext_f->delete_add_text_by_id( VALUE #( addtext_id = ls_current_line_value-addtext_id ) ).

    DELETE mt_addtext_data INDEX lv_current_line.

    MESSAGE s054(zdbbr_info).

    " refresh alv output
    refresh( ).
  ENDMETHOD.


  METHOD do_after_alv_creation.
    DATA(lr_sorting) = mr_salv_table->get_sorts( ).

    lr_sorting->add_sort( columnname = 'ID_TABLE' ).
    lr_sorting->add_sort( columnname = 'ID_FIELD' ).
  ENDMETHOD.


  METHOD edit_manual_entry.
*&---------------------------------------------------------------------*
*& Description: Edits manual textfield entry
*&---------------------------------------------------------------------*
    " get line of with cursor
    DATA: ls_current_line_value TYPE zdbbr_addtext_ui.

    DATA(lv_current_line_index) = get_selected_row( ).
    IF lv_current_line_index <= 0.
      RETURN.
    ENDIF.

    DATA(lf_data_saved) = abap_false.

    DATA(ls_current_line_ui) = mt_addtext_data[ lv_current_line_index ].
    DATA(ls_current_line) = CORRESPONDING zdbbr_addtext( ls_current_line_ui ).

    DATA(lr_addtextfield_controller) = NEW zcl_dbbr_addtextfield_ctrl(
      is_addtext_data = ls_current_line
      it_id_tables    = mt_tabinfo
      iv_mode         = zif_dbbr_c_global=>c_display_modes-edit
    ).

    lr_addtextfield_controller->zif_uitb_screen_controller~call_screen( ).

    IF lr_addtextfield_controller->zif_uitb_screen_controller~was_not_cancelled( ).
      ls_current_line_Ui = CORRESPONDING #( lr_addtextfield_controller->get_current_data( ) ).

      MODIFY mt_addtext_data FROM ls_current_line_ui INDEX lv_current_line_index.

      refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_popup_dimensions.
    ev_start_column = 10.
    ev_end_column = 170.
    ev_start_line = 2.
    ev_end_line = 20.
  ENDMETHOD.


  METHOD get_report_id.
    rv_repid = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD get_selection_mode.
    rv_sel_mode = if_salv_c_selection_mode=>single.
  ENDMETHOD.


  METHOD get_status.
    rv_status = 'ADDTEXT_MANAGER'.
  ENDMETHOD.


  METHOD get_table_reference.
    rr_table_ref = REF #( mt_addtext_data ).
  ENDMETHOD.


  METHOD get_title.
    rv_title = 'DB Browser - Maintain Additional Fields'.
  ENDMETHOD.


  METHOD on_user_command.
    CASE e_salv_function.

      WHEN 'NEW'.
        add_manual_entry( ).

      WHEN 'EDIT'.
        edit_manual_entry( ).

      WHEN 'IMPORT'.
        NEW ZCL_DBBR_addtext_importer( )->import_data( ).

      WHEN 'EXPORT'.
        NEW ZCL_DBBR_addtext_exporter( )->export_data( ).

      WHEN 'SHOW'.

      WHEN 'DELETE'.
        delete_manual_entry( ).
    ENDCASE.
  ENDMETHOD.


  METHOD select_data.
    " select existing additional texts
    DATA(lr_addtext_f) = NEW ZCL_DBBR_addtext_factory( ).
    lr_addtext_f->find_add_texts_for_tablist(
      EXPORTING it_tabname_selopt = mt_table_selopt
      IMPORTING et_addtext        = DATA(lt_addtext_data)
    ).

    LOOP AT lt_addtext_data ASSIGNING FIELD-SYMBOL(<ls_addtext>).
      APPEND CORRESPONDING #( <ls_addtext> ) TO mt_addtext_data ASSIGNING FIELD-SYMBOL(<ls_addtext_db>).
      <ls_addtext_db>-is_manual = abap_true.
      <ls_addtext_db>-selection_type = zif_dbbr_c_text_selection_type=>text_table.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
