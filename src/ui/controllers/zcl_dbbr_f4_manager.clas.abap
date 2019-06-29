CLASS zcl_dbbr_f4_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_view.
    METHODS constructor.
  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_functions,
                 select_all      TYPE ui_func VALUE zif_uitb_template_prog=>c_func_f5,
                 deselect_all    TYPE ui_func VALUE zif_uitb_template_prog=>c_func_f6,
                 sort_ascending  TYPE ui_func VALUE zif_uitb_template_prog=>c_func_ctrl_f4,
                 sort_descending TYPE ui_func VALUE zif_uitb_template_prog=>c_func_ctrl_shift_f4,
                 define_built_in TYPE ui_func VALUE zif_uitb_template_prog=>c_func_ctrl_f5,
                 view_f4         TYPE ui_func VALUE zif_uitb_template_prog=>c_func_ctrl_f7,
                 change_f4       TYPE ui_func VALUE zif_uitb_template_prog=>c_func_ctrl_f6,
                 delete_f4       TYPE ui_func VALUE zif_uitb_template_prog=>c_func_shift_f2,
                 export_f4       TYPE ui_func VALUE zif_uitb_template_prog=>c_func_shift_f11,
                 import_f4       TYPE ui_func VALUE zif_uitb_template_prog=>c_func_shift_f12,
               END OF c_functions.
    DATA mr_template_prog TYPE REF TO zif_uitb_template_prog.
    DATA mr_f4_helps_alv TYPE REF TO cl_salv_table.
    DATA mt_f4_overview TYPE zdbbr_f4_overview_itab.
    DATA mt_edit_functions TYPE ui_functions.
    DATA mt_assignments TYPE zdbbr_f4_assignment_itab.
    DATA mt_deleted_f4 TYPE STANDARD TABLE OF zdbbr_f4_id.
    DATA mt_deleted_f4_assngmnt TYPE zdbbr_f4_assignment_itab.
    DATA mt_new_f4_assngmnt TYPE zdbbr_f4_assignment_itab.

    DATA mf_read_only TYPE abap_bool.
    DATA mv_title TYPE cua_tit_tx .

    DATA mv_selected_f4_id TYPE zdbbr_f4_id.
    DATA: mt_disabled_style TYPE lvc_t_styl.

    METHODS on_before_output
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          er_callback.
    METHODS on_user_command
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          er_callback
          ev_function_id.
    METHODS on_exit
          FOR EVENT exit OF zif_uitb_view_callback
      IMPORTING
          er_callback.


    METHODS do_on_first_screen_call.
    METHODS do_before_first_screen_call.
    METHODS get_f4_overviews.

    METHODS create_f4_alv
      RAISING
        cx_salv_msg
        cx_salv_not_found.

    METHODS toggle_mode.
    METHODS select_data
      RAISING
        zcx_uitb_alv_no_data.
    METHODS unlock.
    METHODS get_title
      RETURNING
        VALUE(r_result) TYPE cua_tit_tx.

    METHODS delete_value_helps.
    METHODS show_no_selection_message.
    METHODS show_f4_help.
    METHODS change_f4.
    METHODS manage_f4
      IMPORTING
        iv_display_mode TYPE zdbbr_display_mode.
    METHODS export_value_helps.
    METHODS import_value_helps.
    METHODS create_built_in_value_help.
ENDCLASS.



CLASS zcl_dbbr_f4_manager IMPLEMENTATION.


  METHOD change_f4.
    manage_f4( zif_dbbr_global_consts=>gc_display_modes-edit ).
  ENDMETHOD.


  METHOD constructor.
    mf_read_only = abap_true.

    " create the template program
    mr_template_prog = zcl_uitb_templt_prog_callback=>create_template_program( iv_title = get_title( ) ).

    " register events
    SET HANDLER:
      on_before_output FOR mr_template_prog,
      on_user_command FOR mr_template_prog,
      on_exit FOR mr_template_prog.

    mt_edit_functions = VALUE #(
      ( c_functions-define_built_in )
      ( c_functions-change_f4 )
      ( c_functions-delete_f4 )
      ( c_functions-import_f4 )
      ( zif_uitb_template_prog=>c_save )
    ).

    mt_disabled_style = VALUE #( ( style = zif_uitb_c_alv_cell_style=>disabled  ) ).

  ENDMETHOD.


  METHOD create_built_in_value_help.
    DATA(lo_built_in_f4_controller) = NEW zcl_dbbr_built_in_f4_sc(
      iv_display_mode  = zif_dbbr_global_consts=>gc_display_modes-create
    ).
    lo_built_in_f4_controller->zif_uitb_screen_controller~call_screen( ).
  ENDMETHOD.


  METHOD create_f4_alv.

    DATA lr_column TYPE REF TO cl_salv_column_table.

    cl_salv_table=>factory(
     EXPORTING
       r_container    = mr_template_prog->get_container( )
     IMPORTING
       r_salv_table   = mr_f4_helps_alv
     CHANGING
       t_table        = mt_f4_overview
    ).

    mr_f4_helps_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    DATA(lr_columns) = CAST cl_salv_columns_table( mr_f4_helps_alv->get_columns( ) ).
    lr_columns->set_optimize( ).

    lr_columns->set_column_position( columnname = 'DESCRIPTION' position = 1 ).
    lr_columns->set_column_position( columnname = 'SEARCH_TABLE' position = 2 ).
    lr_columns->set_column_position( columnname = 'SEARCH_FIELD' position = 3 ).

    lr_column ?= lr_columns->get_column( 'SEARCH_TABLE' ).

    lr_column ?= lr_columns->get_column( 'SEARCH_FIELD' ).

    lr_column ?= lr_columns->get_column( 'IS_BUILT_IN' ).
    lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

    lr_columns->get_column( 'TABNAME' )->set_technical( ).
    lr_columns->get_column( 'FIELDNAME' )->set_technical( ).
    lr_columns->get_column( 'F4_ID' )->set_technical( ).

    mr_f4_helps_alv->display( ).

  ENDMETHOD.


  METHOD delete_value_helps.
    " get selected line
    mr_f4_helps_alv->get_metadata( ).
    DATA(lt_selected_rows) = mr_f4_helps_alv->get_selections( )->get_selected_rows( ).
    IF lt_selected_rows IS INITIAL.
      show_no_selection_message( ).
      RETURN.
    ENDIF.

    DATA(lt_f4_id) = VALUE zdbbr_selopt_itab(
        FOR selected IN lt_selected_rows
        LET f4 = REF #( mt_f4_overview[ selected ] ) IN
        ( sign   = 'I'
          option = 'EQ'
          low    = f4->f4_id )
    ).

    DATA(lv_answer) = zcl_dbbr_appl_util=>popup_to_confirm(
        iv_title     = 'Delete Value Helps?'
        iv_query     = |Do you really want to delete { lines( lt_selected_rows ) } Value Help(s)?|
        iv_icon_type = 'ICON_QUESTION'
    ).

    IF lv_answer = '1'.
      zcl_dbbr_custom_f4_factory=>delete_multiple_by_id( lt_f4_id ).
      DELETE mt_f4_overview WHERE f4_id IN lt_f4_id.

      mr_f4_helps_alv->refresh( ).
      MESSAGE |{ lines( lt_f4_id ) } Value Helps were deleted| TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD do_before_first_screen_call.

    mr_template_prog->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_ctrl_f1
        iv_text        = 'Display/Edit'
        iv_icon        = icon_toggle_display_change
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-select_all
        iv_text        = 'Select All'
        iv_icon        = icon_select_all
    ).
    mr_template_prog->add_function(
        iv_function_id = c_functions-deselect_all
        iv_text        = 'Deselect All'
        iv_icon        = icon_deselect_all
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-sort_ascending
        iv_text        = 'Sort Up'
        iv_icon        = icon_sort_up
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-sort_descending
        iv_text        = 'Sort Down'
        iv_icon        = icon_sort_down
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-define_built_in
        iv_text        = 'Define existing Value Help'
        iv_quickinfo   = 'Define existing Value Help'
        iv_icon        = icon_create
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-view_f4
        iv_text        = 'Display Value Help'
        iv_icon        = icon_display
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-change_f4
        iv_text        = 'Edit Value Help'
        iv_icon        = icon_change
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-delete_f4
        iv_text        = 'Delete Value Help'
        iv_icon        = icon_delete
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-export_f4
        iv_text        = 'Export Value Helps'
        iv_icon        = icon_export
    ).

    mr_template_prog->add_function(
        iv_function_id = c_functions-import_f4
        iv_text        = 'Import Value Helps'
        iv_icon        = icon_import
    ).

  ENDMETHOD.


  METHOD do_on_first_screen_call.
    DATA: lr_column_salv TYPE REF TO cl_salv_column_table.

    IF mr_f4_helps_alv IS INITIAL.
      create_f4_alv( ).
    ENDIF.

  ENDMETHOD.


  METHOD export_value_helps.
    mr_f4_helps_alv->get_metadata( ).

    DATA(lt_selected_rows) = mr_f4_helps_alv->get_selections( )->get_selected_rows( ).
    IF lt_selected_rows IS INITIAL.
      show_no_selection_message( ).
      RETURN.
    ENDIF.

    DATA(lr_f4_exporter) = NEW zcl_dbbr_f4_exporter(
        VALUE #(
          FOR row IN lt_selected_rows
          LET <ls_f4_id> = mt_f4_overview[ row ] IN
          ( <ls_f4_id>-f4_id )
        )
    ).

    lr_f4_exporter->export_data( ).
  ENDMETHOD.


  METHOD get_f4_overviews.
    mt_f4_overview = zcl_dbbr_custom_f4_factory=>get_f4_overviews( ).
  ENDMETHOD.


  METHOD get_title.

    r_result = COND string( WHEN mf_read_only = abap_true THEN TEXT-001 ELSE TEXT-002 ).
  ENDMETHOD.


  METHOD import_value_helps.
    DATA(lr_f4_importer) = NEW zcl_dbbr_f4_importer( ).

    lr_f4_importer->import_data( ).

    mr_f4_helps_alv->refresh( ).
  ENDMETHOD.


  METHOD manage_f4.
    " get selected line
    mr_f4_helps_alv->get_metadata( ).
    DATA(lt_selected_rows) = mr_f4_helps_alv->get_selections( )->get_selected_rows( ).
    IF lt_selected_rows IS INITIAL OR lines( lt_selected_rows ) > 1.
      MESSAGE 'Select exactly 1 Value Help' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lr_f4) = REF #( mt_f4_overview[ lt_selected_rows[ 1 ] ] ).

    DATA(lr_f4_screen_controller) = NEW zcl_dbbr_custom_f4_sc(
        iv_f4_id        = lr_f4->f4_id
        iv_display_mode = iv_display_mode
    ).

    lr_f4_screen_controller->zif_uitb_screen_controller~call_screen( ).

  ENDMETHOD.


  METHOD on_before_output.
    IF er_callback->is_first_screen_call( ).
      do_on_first_screen_call( ).
    ELSE.
      er_callback->set_title( get_title( ) ).
    ENDIF.

    IF mf_read_only = abap_true.
      er_callback->deactivate_functions( mt_edit_functions ).
    ENDIF.
  ENDMETHOD.


  METHOD on_exit.
    " delete any existing locks on application
    unlock( ).
  ENDMETHOD.


  METHOD on_user_command.
    CASE ev_function_id.

      WHEN c_functions-select_all.
        mr_f4_helps_alv->set_function( cl_gui_alv_grid=>mc_fc_select_all ).

      WHEN c_functions-deselect_all.
        mr_f4_helps_alv->set_function( cl_gui_alv_grid=>mc_fc_deselect_all ).

      WHEN c_functions-sort_ascending.
        mr_f4_helps_alv->set_function( cl_gui_alv_grid=>mc_fc_sort_asc ).
        mr_f4_helps_alv->refresh( ).

      WHEN c_functions-sort_descending.
        mr_f4_helps_alv->set_function( cl_gui_alv_grid=>mc_fc_sort_dsc ).
        mr_f4_helps_alv->refresh( ).

      WHEN zif_uitb_template_prog=>c_func_ctrl_f1. " toggle change/display mode
        toggle_mode( ).

      WHEN c_functions-define_built_in.
        create_built_in_value_help( ).

      WHEN c_functions-view_f4.
        show_f4_help( ).

      WHEN c_functions-change_f4.
        change_f4( ).

      WHEN c_functions-delete_f4.
        delete_value_helps( ).

      WHEN c_functions-export_f4.
        export_value_helps( ).

      WHEN c_functions-import_f4.
        import_value_helps( ).
    ENDCASE.
  ENDMETHOD.


  METHOD select_data.
    get_f4_overviews( ).

    IF mt_f4_overview IS INITIAL.
*      RAISE EXCEPTION TYPE ZCX_DBBR_alv_no_data.
    ENDIF.

  ENDMETHOD.


  METHOD show_f4_help.
    manage_f4( zif_dbbr_global_consts=>gc_display_modes-view ).
  ENDMETHOD.


  METHOD show_no_selection_message.
    MESSAGE 'Select at least 1 Value Help' TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.


  METHOD toggle_mode.
    DATA: lf_editable       TYPE abap_bool,
          lv_title_suffix   TYPE string,
          lv_locked_by_user TYPE sy-uname.

    IF mf_read_only = abap_true.
      lf_editable = abap_true.
      lv_title_suffix = TEXT-m02.
    ELSE.
      lf_editable = abap_false.
      lv_title_suffix = TEXT-m01.
    ENDIF.

    IF lf_editable = abap_true.
      IF NOT zcl_dbbr_lock_util=>lock( EXPORTING iv_use_case  = zif_dbbr_c_use_case=>value_help_management
                                        IMPORTING ev_locked_by = lv_locked_by_user ).
        " locking failed
        MESSAGE |Locked by User { lv_locked_by_user }| TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      mf_read_only = abap_false.
    ELSE.
      unlock( ).
      mf_read_only = abap_true.
    ENDIF.

    select_data( ).

    mr_f4_helps_alv->refresh( ).
  ENDMETHOD.


  METHOD unlock.
    zcl_dbbr_lock_util=>unlock( zif_dbbr_c_use_case=>value_help_management ).
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    TRY.
        select_data( ).
      CATCH zcx_uitb_alv_no_data.
        MESSAGE 'There are no Value Helps' TYPE 'I'.
        RETURN.
    ENDTRY.

    do_before_first_screen_call( ).

    mr_template_prog->zif_uitb_view~show(
      iv_end_column = iv_end_column
      iv_start_column = iv_start_column
      iv_end_line = iv_end_line
      iv_start_line = iv_start_line
    ).
  ENDMETHOD.
ENDCLASS.
