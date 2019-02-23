CLASS zcl_dbbr_multi_select_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_base_select_tc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS change_attributes .
    METHODS get_current_line
      RETURNING
        VALUE(rv_index) LIKE sy-tabix .
    METHODS update_fields .
    METHODS determine_line_count .
    METHODS display_lines .
    METHODS delete_line .
    METHODS delete_lines .
    METHODS append_new_lines .
    METHODS handle_option_selection .
    METHODS constructor .
    METHODS init_table
      IMPORTING
        !is_template TYPE zdbbr_selfield
        !it_multi    TYPE zdbbr_selfield_itab OPTIONAL .
    METHODS set_template_line
      IMPORTING
        !is_selfield TYPE zdbbr_selfield .
    METHODS get_template_line
      RETURNING
        VALUE(rs_template) TYPE zdbbr_selfield .
    METHODS update_option_template
      IMPORTING
        !is_option_template TYPE se16n_sel_option .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_linecount LIKE sy-tabix .
    DATA ms_template TYPE zdbbr_selfield .
    DATA mr_custom_f4_map TYPE REF TO zcl_dbbr_custom_f4_map .
    DATA mr_selfield_lines TYPE REF TO zdbbr_selfield_itab.
    DATA mr_ui_multi_select_ctrl TYPE REF TO cxtab_control.
    DATA mr_ui_option_button TYPE REF TO zdbbr_button.
    DATA mr_ui_opt_template TYPE REF TO se16n_option.
    DATA mr_ui_push_button TYPE REF TO zdbbr_button.
    DATA mr_ui_linecount TYPE REF TO syst_tabix.
    DATA mr_ui_multi_select_lines TYPE REF TO syst_tabix.
    DATA mr_ui_sel_opt_init TYPE REF TO zdbbr_selopt_control_itab.
    DATA ms_option_template TYPE se16n_sel_option.

ENDCLASS.



CLASS zcl_dbbr_multi_select_table IMPLEMENTATION.


  METHOD append_new_lines.

*&---------------------------------------------------------------------*
*& Description: Append a number of new lines
*&---------------------------------------------------------------------*
    IF mr_selfield_lines->* IS NOT INITIAL.
      DATA(ls_new_line) = mr_selfield_lines->*[ 1 ].
    ELSE.
      ls_new_line = ms_template.
    ENDIF.
    CLEAR: ls_new_line-low,
           ls_new_line-high.

    ls_new_line-sign = ms_option_template-sign.
    ls_new_line-option = ms_option_template-option.

    DO 5 TIMES.
      APPEND ls_new_line TO mr_selfield_lines->*.
    ENDDO.

  ENDMETHOD.


  METHOD change_attributes.

*&---------------------------------------------------------------------*
*& Description: Change screen attributes
*&---------------------------------------------------------------------*
    " disable input if there aren't any lines at all
    IF mr_ui_multi_select_ctrl->current_line > mr_ui_linecount->*.
      LOOP AT SCREEN.
        screen-input = 0.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.

    IF mr_selfield_line->push = abap_true.
      DATA(lv_push_icon_name) = CONV iconname( 'ICON_DISPLAY_MORE' ).
    ELSE.
      lv_push_icon_name = 'ICON_ENTER_MORE'.
    ENDIF.

    zcl_dbbr_icon_handler=>create_icon(
      EXPORTING
        iv_icon_name = lv_push_icon_name
      IMPORTING
        ev_info_text = DATA(lv_icon_text)
        ev_push      = mr_ui_push_button->*
    ).

    IF NOT mr_selfield_line->option IS INITIAL.
      DATA(lv_option_icon_name) = zcl_dbbr_icon_handler=>get_sign_icon_name(
          iv_sign      = mr_selfield_line->sign
          iv_option    = mr_selfield_line->option
      ).
    ELSE.
      lv_option_icon_name = 'ICON_SELECTION'.
    ENDIF.

    zcl_dbbr_icon_handler=>create_icon(
      EXPORTING
        iv_icon_name = lv_option_icon_name
      IMPORTING
        ev_info_text = lv_icon_text
        ev_push      = mr_ui_option_button->*
    ).

    " depending on the select option, not all fields are inputable
    TRY .
        DATA(ls_sel_init) = mr_ui_sel_opt_init->*[ option = mr_selfield_line->option ].
        IF ls_sel_init-high <> abap_true.
          LOOP AT SCREEN.
            IF screen-name = 'GS_MULTI_SELECT-HIGH'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF ls_sel_init-low <> abap_true.
          LOOP AT SCREEN.
            IF screen-name = 'GS_MULTI_SELECT-LOW'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF mr_selfield_line->key = abap_true.
      LOOP AT SCREEN.
        IF screen-group2 = 'TXT'.
          screen-intensified = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.

    " set field to outputlength of current table column field
    LOOP AT SCREEN.
      IF screen-name = 'GS_MULTI_SELECT-LOW' OR
         screen-name = 'GS_MULTI_SELECT-HIGH'.
        screen-length = ms_template-outputlen.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.


  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_selfield_line = CAST zdbbr_selfield( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_multi_select ) ).
    mr_selfield_lines = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_multi_select ) ).
    mr_ui_multi_select_ctrl = CAST cxtab_control( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_multi_tc ) ).
    mr_ui_push_button = CAST zdbbr_button( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_push ) ).
    mr_ui_option_button = CAST zdbbr_button( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_option ) ).
    mr_ui_linecount = CAST syst_tabix( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_linecount ) ).
    mr_ui_multi_select_lines = CAST syst_tabix( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_multi_select_lines ) ).
    mr_ui_sel_opt_init = CAST zdbbr_selopt_control_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_sel_init ) ).
    mr_ui_opt_template = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_option_template ) ).
  ENDMETHOD.


  METHOD delete_line.

    DATA: lv_current_line LIKE sy-tabix.

    GET CURSOR LINE lv_current_line.
    lv_current_line = lv_current_line + mr_ui_multi_select_ctrl->current_line - 1.
    DELETE mr_selfield_lines->* INDEX lv_current_line.

  ENDMETHOD.


  METHOD delete_lines.

    CLEAR mr_selfield_lines->*.

  ENDMETHOD.


  METHOD determine_line_count.

    mr_ui_linecount->* = lines( mr_selfield_lines->* ).
    mr_ui_multi_select_ctrl->lines = mr_ui_linecount->*.

    mr_ui_multi_select_lines->* = 0.

    LOOP AT mr_selfield_lines->* ASSIGNING FIELD-SYMBOL(<ls_multi_select>)
       WHERE low IS NOT INITIAL
          OR high IS NOT INITIAL
          OR option IS NOT INITIAL.
      ADD 1 TO mr_ui_multi_select_lines->*.
    ENDLOOP.

  ENDMETHOD.


  METHOD display_lines.

    IF mr_selfield_line->is_parameter = abap_true.
      zcl_dbbr_data_converter=>convert_values_to_disp_format(
        EXPORTING
          iv_rollname = mr_selfield_line->rollname
          iv_type     = mr_selfield_line->inttype
          iv_length   = CONV #( mr_selfield_line->intlen )
          iv_decimals = CONV #( mr_selfield_line->decimals )
        CHANGING  cv_value1    = mr_selfield_line->low
                  cv_value2    = mr_selfield_line->high
      ).
    ELSE.
      zcl_dbbr_data_converter=>convert_selopt_to_disp_format(
        EXPORTING iv_tabname   = mr_selfield_line->tabname
                  iv_fieldname = mr_selfield_line->fieldname
        CHANGING  cv_value1    = mr_selfield_line->low
                  cv_value2    = mr_selfield_line->high
      ).
    ENDIF.

  ENDMETHOD.


  METHOD get_current_line.

    GET CURSOR LINE rv_index.
    rv_index = rv_index + mr_ui_multi_select_ctrl->top_line - 1.

  ENDMETHOD.


  METHOD get_template_line.
    rs_template = ms_template.
  ENDMETHOD.


  METHOD handle_option_selection.

*&---------------------------------------------------------------------*
*& Description: Shows f4 help for all possible options
*&---------------------------------------------------------------------*

    DATA(lv_current_line) = get_current_line( ).

    TRY .
        DATA(ls_current_line) = mr_selfield_lines->*[ lv_current_line ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    " get user selection
    DATA(ls_chosen_option) = zcl_dbbr_selscreen_util=>choose_sel_option(
      if_allow_null = xsdbool( ls_current_line-is_parameter = abap_false )
    ).
    IF ls_chosen_option IS INITIAL.
      RETURN.
    ENDIF.

    ls_current_line-sign   = ls_chosen_option-sign.
    ls_current_line-option = ls_chosen_option-option.
    " GT_sel_init contains info if low and/or high are allowed for
    " the selected option
    TRY .
        DATA(ls_sel_init) = mr_ui_sel_opt_init->*[ option = ls_chosen_option-option ].
        IF ls_sel_init-high <> abap_true.
          CLEAR ls_current_line-high.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    MODIFY mr_selfield_lines->* FROM ls_current_line INDEX lv_current_line.


  ENDMETHOD.


  METHOD init_table.
    CLEAR: mr_selfield_lines->*,
           mr_selfield_line->*.

    " initalize the table data
    ASSIGN mr_selfield_lines->* TO FIELD-SYMBOL(<lt_multi>).
    " set template
    set_template_line( is_template ).

    IF is_template-low <> space OR is_template-high <> space.
      <lt_multi> = VALUE #( BASE <lt_multi> ( is_template ) ).
    ENDIF.

    <lt_multi> = VALUE #(
      BASE <lt_multi>
      FOR multi IN it_multi
      WHERE ( tabname   = is_template-tabname AND
              fieldname = is_template-fieldname )
      ( multi )
    ).

    " set global variables for screen
    IF <lt_multi> IS INITIAL.
      append_new_lines( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_template_line.

    ms_template = is_selfield.

  ENDMETHOD.


  METHOD update_fields.
*& Description: Updates the internal table from the table control
*&---------------------------------------------------------------------*

    DATA(lf_no_uppercase_conversion) = is_uppercase_conv_disabled( ).

    DATA(lv_save_low) = mr_selfield_line->low.
    DATA(lv_save_high) = mr_selfield_line->high.

    precheck_selfield_value( EXPORTING is_selfield          = mr_selfield_line->*
                                       if_no_uppercase_conv = lf_no_uppercase_conversion
                             CHANGING  cv_value             = mr_selfield_line->low ).
    precheck_selfield_value( EXPORTING is_selfield          = mr_selfield_line->*
                                       if_no_uppercase_conv = lf_no_uppercase_conversion
                             CHANGING  cv_value             = mr_selfield_line->high ).

    TRY.
        conv_selfields_to_internal(
            if_no_uppercase_conversion = lf_no_uppercase_conversion ).

      CATCH zcx_dbbr_conversion_exc INTO DATA(lx_conv_error).
        lx_conv_error->zif_dbbr_exception_message~print( iv_msg_type = 'E' ).
        RETURN.
    ENDTRY.

    check_interval_validity(
      iv_save_low  = lv_save_low
      iv_save_high = lv_save_high ).

    fill_selopt_sign( ).

    MODIFY mr_selfield_lines->* FROM mr_selfield_line->* INDEX mr_ui_multi_select_ctrl->current_line.

  ENDMETHOD.


  METHOD update_option_template.
    ms_option_template = is_option_template.
  ENDMETHOD.

ENDCLASS.
