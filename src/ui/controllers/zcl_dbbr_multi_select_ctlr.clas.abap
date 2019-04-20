CLASS zcl_dbbr_multi_select_ctlr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    METHODS constructor
      IMPORTING
        !ir_custom_f4_map   TYPE REF TO zcl_dbbr_custom_f4_map
        !ir_selection_table TYPE REF TO zcl_dbbr_multi_select_table .
    METHODS determine_line_count .
    METHODS call_f4_help
      IMPORTING
        !if_for_low TYPE boolean
        !if_multi   TYPE boolean OPTIONAL .
    METHODS should_data_be_transferred
      RETURNING
        VALUE(rf_transfer) TYPE boolean .
    METHODS transfer_data
      CHANGING
        !cs_selfield       TYPE zdbbr_selfield
        !ct_selfield_multi TYPE zdbbr_selfield_itab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    DATA mr_custom_f4_map TYPE REF TO zcl_dbbr_custom_f4_map .
    DATA mo_table TYPE REF TO zcl_dbbr_multi_select_table .
    DATA mf_transfer TYPE boolean .
    DATA mr_ui_multi_select_field TYPE REF TO zdbbr_selfield .
    DATA mr_ui_option_templ_button TYPE REF TO zdbbr_button .
    DATA mr_ui_multi_select_fields TYPE REF TO zdbbr_selfield_itab .
    DATA ms_option_template TYPE se16n_sel_option .

    METHODS call_selfield_f4_help_multi
      IMPORTING
        !if_low       TYPE boolean
        !iv_tablename TYPE tabname
        !iv_fieldname TYPE fieldname.
    METHODS take_entered_values .
    METHODS import_from_clipboard .
    METHODS call_selfield_f4_custom_multi
      IMPORTING
        !iv_current_line LIKE sy-tabix
        !if_low          TYPE boolean .
    METHODS choose_option_template .
ENDCLASS.



CLASS zcl_dbbr_multi_select_ctlr IMPLEMENTATION.


  METHOD call_f4_help.
*& Description: Calls value help for low/high values
*&---------------------------------------------------------------------*
    DATA(lv_current_line) = mo_table->get_current_line( ).
    TRY.
        DATA(ls_selfield) = mr_ui_multi_select_fields->*[ lv_current_line ].
        DATA(ls_selfield_save) = ls_selfield.

        IF if_multi = abap_true.
          call_selfield_f4_help_multi(
              if_low       = if_for_low
              iv_tablename = mr_ui_multi_select_field->tabname
              iv_fieldname = mr_ui_multi_select_field->fieldname
          ).

        ELSE.
          zcl_dbbr_f4_helper=>call_selfield_f4(
            EXPORTING
              if_low           = if_for_low
              iv_repid         = zif_dbbr_c_report_id=>main
              iv_selfield_name = 'GS_MULTI_SELECT'
              iv_current_line  = lv_current_line
              ir_custom_f4_map = mr_custom_f4_map
            CHANGING
              cs_selfield      = ls_selfield
          ).

          IF ls_selfield-low <> ls_selfield_save-low OR
             ls_selfield-high <> ls_selfield_save-high.
            mr_ui_multi_select_field->* = ls_selfield.
          ENDIF.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD call_selfield_f4_custom_multi.
*& Description: Calls custom multi searchhelp for field
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lv_selvalue> TYPE se16n_value.

    " 1) read f4 help definition
    mr_custom_f4_map->read_custom_f4_definition(
      EXPORTING
        iv_tablename = mr_ui_multi_select_field->tabname
        iv_fieldname = mr_ui_multi_select_field->fieldname
      IMPORTING
        et_custom_f4_definitions = DATA(lt_f4_definition) ).
    IF lt_f4_definition IS INITIAL.
      MESSAGE 'Value Help in ZDBBR_F4H is invalid' TYPE 'S'.
      RETURN.
    ENDIF.

    zcl_dbbr_custom_f4_helper=>call_custom_f4(
      EXPORTING
        if_multiple_select = abap_true
        it_f4_definition   = lt_f4_definition
        iv_current_line    = iv_current_line
        if_low             = if_low
      CHANGING
        cs_selfield        = mr_ui_multi_select_field->*
        ct_selfield        = mr_ui_multi_select_fields->*
    ).


  ENDMETHOD.


  METHOD call_selfield_f4_help_multi.
*& Description: Calls F4 multi for select field
*&---------------------------------------------------------------------*

    " get the current line
    DATA(lv_current_line) = mo_table->get_current_line( ).

    IF mr_ui_multi_select_field->has_cust_f4_help = abap_true.
      call_selfield_f4_custom_multi(
        iv_current_line = lv_current_line
        if_low          = if_low
      ).
    ELSE.
      zcl_dbbr_f4_helper=>call_built_in_f4_multi(
        EXPORTING
          if_low                  = if_low
          if_do_not_convert_alpha = abap_true
          iv_tablename            = iv_tablename
          iv_fieldname            = iv_fieldname
          iv_current_line         = lv_current_line
        CHANGING
          cs_selfield             = mr_ui_multi_select_field->*
          ct_selfield             = mr_ui_multi_select_fields->*
      ).
    ENDIF.

  ENDMETHOD.


  METHOD choose_option_template.
    DATA(ls_chosen_option) = zcl_dbbr_selscreen_util=>choose_sel_option(
      if_allow_null = xsdbool( mo_table->get_template_line( )-is_parameter = abap_false )
    ).

    IF ls_chosen_option IS INITIAL.
      RETURN.
    ENDIF.

    ms_option_template = ls_chosen_option.

    mo_table->update_option_template( is_option_template = ms_option_template ).
  ENDMETHOD.


  METHOD constructor.

    mr_custom_f4_map = ir_custom_f4_map.
    mo_table = ir_selection_table.
    mo_table->update_option_template( ms_option_template ).

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_ui_multi_select_field = CAST zdbbr_selfield( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_multi_select ) ).
    mr_ui_multi_select_fields = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_multi_select ) ).
    mr_ui_option_templ_button = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_bt_option_template ) ).

    DATA(lr_v_fieldname) = CAST fieldname( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_fieldname ) ).
    DATA(lr_v_field_descr) = CAST scrtext_m( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_field_descr ) ).

    DATA(ls_template) = mo_table->get_template_line( ).
    lr_v_fieldname->* = ls_template-fieldname.
    lr_v_field_descr->* = ls_template-scrtext_l.
  ENDMETHOD.


  METHOD determine_line_count.
*& Description: Determines the line count of the multi select tables
*&---------------------------------------------------------------------*
    mo_table->determine_line_count( ).

  ENDMETHOD.


  METHOD import_from_clipboard.
*& Description: Imports data from clipboard
*&---------------------------------------------------------------------*
    DATA: lt_clipboard_data TYPE filetable.

    cl_gui_frontend_services=>clipboard_import( IMPORTING data = lt_clipboard_data ).

    " remove empty lines from gt_multi_select
    DELETE mr_ui_multi_select_fields->* WHERE low IS INITIAL
                                          AND high IS INITIAL
                                          AND option IS INITIAL.

    mr_ui_multi_select_field->* = mo_table->get_template_line( ).

    " set sign/option from template
    mr_ui_multi_select_field->sign = ms_option_template-sign.
    mr_ui_multi_select_field->option = ms_option_template-option.

    LOOP AT lt_clipboard_data ASSIGNING FIELD-SYMBOL(<ls_clipboard_line>).
      CLEAR: mr_ui_multi_select_field->low, mr_ui_multi_select_field->high.

      mr_ui_multi_select_field->low(mr_ui_multi_select_field->outputlen) = <ls_clipboard_line>.

      IF mr_ui_multi_select_field->lowercase = abap_false.
        TRANSLATE mr_ui_multi_select_field->low TO UPPER CASE.
      ENDIF.

      " convert input
      zcl_dbbr_data_converter=>convert_selopt_to_int_format(
        EXPORTING iv_tabname   = mr_ui_multi_select_field->tabname
                  iv_fieldname = mr_ui_multi_select_field->fieldname
        CHANGING  cv_value1    = mr_ui_multi_select_field->low
      ).

      APPEND mr_ui_multi_select_field->* TO mr_ui_multi_select_fields->*.
    ENDLOOP.

  ENDMETHOD.


  METHOD should_data_be_transferred.

    rf_transfer = mf_transfer.

  ENDMETHOD.


  METHOD take_entered_values.
*& Description: Takes all lines there at on of the following fields
*& has to be filled: low, high, option
*&---------------------------------------------------------------------*
    DELETE mr_ui_multi_select_fields->* WHERE low IS INITIAL
                                          AND high IS INITIAL
                                          AND option IS INITIAL.

  ENDMETHOD.


  METHOD transfer_data.
    CHECK mf_transfer = abap_true.

    ASSIGN mr_ui_multi_select_fields->* TO FIELD-SYMBOL(<lt_multi>).

    SORT <lt_multi> BY sign option low high.
    DELETE ADJACENT DUPLICATES FROM <lt_multi> COMPARING sign option low high.

    " delete old values
    DELETE ct_selfield_multi WHERE fieldname = cs_selfield-fieldname AND
                                   tabname   = cs_selfield-tabname.
    DATA(lv_lines) = lines( <lt_multi> ).
    IF lv_lines > 1.
      cs_selfield-push = abap_true.
      DATA(ls_multi_line) = <lt_multi>[ 1 ].
      cs_selfield-low = ls_multi_line-low.
      cs_selfield-high = ls_multi_line-high.
      cs_selfield-option = ls_multi_line-option.
      cs_selfield-sign = ls_multi_line-sign.
      DELETE <lt_multi> INDEX 1.
      APPEND LINES OF <lt_multi> TO ct_selfield_multi.
    ELSEIF lv_lines = 1.
      ls_multi_line = <lt_multi>[ 1 ].
      CLEAR cs_selfield-push.
      cs_selfield-low = ls_multi_line-low.
      cs_selfield-high = ls_multi_line-high.
      cs_selfield-option = ls_multi_line-option.
      cs_selfield-sign = ls_multi_line-sign.

    ELSE.
      " user did delete all values!
      CLEAR: cs_selfield-push,
             cs_selfield-low,
             cs_selfield-high.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_multi_select_controller
            global_ref    = me )
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_multi_select_table
            global_ref    = mo_table )
        )
        iv_start_column = 10
        iv_start_line   = 2
        iv_end_column   = 80
        iv_end_line     = 17
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.
    zcl_uitb_screen_util=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_show_mult_select.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    DATA(lv_function_code) = cv_function_code.
    CLEAR cv_function_code.

    CASE lv_function_code.
      WHEN 'TAKE'.
        mf_transfer = abap_true.
        take_entered_values( ).
        zcl_uitb_screen_util=>leave_screen( ).
      WHEN '&F12'.
        zcl_uitb_screen_util=>leave_screen( ).
      WHEN 'APPEND'.
        mo_table->append_new_lines( ).
      WHEN 'OPTION'.
        mo_table->handle_option_selection( ).
      WHEN 'DELETE'.
        mo_table->delete_line( ).
      WHEN 'DELETE_ALL'.
        " delete all lines
        mo_table->delete_lines( ).
      WHEN 'MULTI_F4'.
        " call multi f4 selection
        DATA lv_field(60).
        GET CURSOR FIELD lv_field.
        IF lv_field = 'GS_MULTI_SELECT-LOW'.
          DATA(lf_low) = abap_true.
        ELSEIF lv_field = 'GS_MULTI_SELECT-HIGH'.
          lf_low = abap_false.
        ELSE.
          lf_low = abap_true.
        ENDIF.
        call_f4_help(
          if_for_low = lf_low
          if_multi   = abap_true
        ).
      WHEN 'OPTION_TEMPLATE'.
        choose_option_template( ).

      WHEN 'CLIPBOARD'.
        import_from_clipboard( ).
    ENDCASE.


  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    zif_uitb_screen_controller~set_status( ).

    LOOP AT SCREEN INTO DATA(ls_screen).
      IF ls_screen-name = 'OPTION_TEMPLATE'.
        IF NOT ms_option_template IS INITIAL.
          DATA(lv_option_icon_name) = zcl_dbbr_icon_handler=>get_sign_icon_name(
              iv_sign      = ms_option_template-sign
              iv_option    = ms_option_template-option
          ).
        ELSE.
          lv_option_icon_name = 'ICON_SELECTION'.
        ENDIF.

        zcl_dbbr_icon_handler=>create_icon(
          EXPORTING
            iv_icon_name = lv_option_icon_name
          IMPORTING
            ev_info_text = DATA(lv_icon_text)
            ev_push      = mr_ui_option_templ_button->*
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    SET PF-STATUS '0001' OF PROGRAM zif_dbbr_c_report_id=>main.
    SET TITLEBAR 'DATA_BROWSER_MULTI' OF PROGRAM zif_dbbr_c_report_id=>main.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
  ENDMETHOD.
ENDCLASS.
