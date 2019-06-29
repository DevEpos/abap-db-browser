"! <p class="shorttext synchronized" lang="en">Table controller for multiple or input</p>
CLASS zcl_dbbr_multi_or_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_base_select_tc
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_table .
    INTERFACES zif_uitb_page_scroller .

    METHODS constructor .
    METHODS pbo .
    METHODS search
      IMPORTING
        !if_continue_previous_search TYPE boolean OPTIONAL
      RETURNING
        VALUE(rf_search_successful)  TYPE boolean .
    METHODS set_multi_or_all
      IMPORTING
        !ir_multi_or_all TYPE REF TO zdbbr_or_seltab_itab .
    METHODS set_multi_or_current_tuple
      IMPORTING
        !ir_multi_or_single TYPE REF TO zdbbr_or_seltab .
    METHODS set_multi_or_multi_select
      IMPORTING
        !ir_multi_or_multi TYPE REF TO zdbbr_selfield_itab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_fields,
        high_value TYPE string VALUE 'GS_MULTI_OR-HIGH' ##NO_TEXT,
        low_value  TYPE string VALUE 'GS_MULTI_OR-LOW' ##NO_TEXT,
        push       TYPE string VALUE 'PUSH' ##NO_TEXT,
        option     TYPE string VALUE 'OPTION' ##NO_TEXT,
      END OF c_fields .

    DATA mv_linecount TYPE sy-tabix .
    DATA mv_looplines TYPE sy-loopc .
    DATA mv_current_line TYPE sy-tabix .
    DATA mr_multi_or_all TYPE REF TO zdbbr_or_seltab_itab .
    DATA mr_multi_or_current TYPE REF TO zdbbr_or_seltab .
    DATA mr_multi_or_multi TYPE REF TO zdbbr_selfield_itab .
    DATA mv_search_value TYPE fieldname .
    DATA mr_ui_selfields TYPE REF TO zdbbr_selfield_itab .
    DATA mr_selfield_lines TYPE REF TO zdbbr_selfield_itab .
    DATA mr_ui_multi_or_tuple_no TYPE REF TO tswpos .
    DATA mr_ui_sel_option_init TYPE REF TO zdbbr_selopt_control_itab .
    DATA mr_ui_multi_or_ctrl TYPE REF TO cxtab_control .
    DATA mr_ui_push_button TYPE REF TO zdbbr_button .
    DATA mr_ui_option_button TYPE REF TO zdbbr_button .

    "! <p class="shorttext synchronized" lang="en">Delete input of current line</p>
    "!
    "! @parameter CS_SELFIELD | <p class="shorttext synchronized" lang="en"></p>
    METHODS delete_line
      CHANGING
        !cs_selfield TYPE zdbbr_selfield .
    METHODS query_for_search_criteria .
    "! <p class="shorttext synchronized" lang="en">Update the input status of the fields</p>
    "!
    METHODS update_field_input_state .
ENDCLASS.



CLASS zcl_dbbr_multi_or_table IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_selfield_line = CAST zdbbr_selfield( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_multi_or ) ).
    mr_ui_selfields = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields ) ).
    mr_selfield_lines = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_multi_or ) ).
    mr_ui_multi_or_tuple_no = CAST tswpos( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_or_tuple_number ) ).
    mr_ui_sel_option_init = CAST zdbbr_selopt_control_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_sel_init ) ).
    mr_ui_multi_or_ctrl = CAST cxtab_control( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_multi_or_tc ) ).
    mr_ui_push_button = CAST zdbbr_button( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_push ) ).
    mr_ui_option_button = CAST zdbbr_button( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_option ) ).

    mr_ui_multi_or_ctrl->top_line = 1.
  ENDMETHOD.


  METHOD delete_line.

    CLEAR: cs_selfield-low,
           cs_selfield-high,
           cs_selfield-sign,
           cs_selfield-option,
           cs_selfield-push.

    cs_selfield-sign = zif_dbbr_global_consts=>gc_options-i.

    " delete possible multi values
    DELETE mr_multi_or_multi->* WHERE tabname   = cs_selfield-tabname AND
                                      fieldname = cs_selfield-fieldname.

  ENDMETHOD.


  METHOD pbo.

    zif_uitb_table~determine_line_count( ).
    mv_looplines = sy-loopc.

    " handle output conversion
    zif_uitb_table~update_screen_attributes( ).

  ENDMETHOD.


  METHOD query_for_search_criteria.

    DATA: lt_fields     TYPE TABLE OF sval,
          lv_returncode TYPE char1.

    APPEND VALUE #(
        fieldname = 'FIELDNAME'
        tabname   = 'DD03L'
    ) TO lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = 'Search'
      IMPORTING
        returncode  = lv_returncode
      TABLES
        fields      = lt_fields
      EXCEPTIONS
        OTHERS      = 1.

    IF lv_returncode <> space OR sy-subrc <> 0.
      CLEAR mv_search_value.
      RETURN.
    ENDIF.

    " set the current fieldname
    mv_search_value = <ls_field>-value.


  ENDMETHOD.


  METHOD search.

    IF if_continue_previous_search = abap_true.
      IF mv_search_value IS INITIAL.
        RETURN.
      ENDIF.
      " get current line index
      zif_uitb_table~determine_current_line( ).
      DATA(lv_starting_index) = zif_uitb_table~get_current_line_index( ) + 1.
    ELSE.
      lv_starting_index = 1.
      query_for_search_criteria( ).
    ENDIF.

    " use table searcher for actual search
    DATA(lr_table_searcher) = NEW zcl_uitb_table_func_executor( ir_table = REF #( mr_selfield_lines->* )  ).
    DATA(lv_index) = lr_table_searcher->search(
      iv_search_value  = |{ mv_search_value }|
      it_search_fields = VALUE #( ( `FIELDNAME` ) ( `SCRTEXT_M` ) )
      iv_start_index   = lv_starting_index
    ).

    IF lv_index > 0.
      mr_ui_multi_or_ctrl->top_line = lv_index.
      rf_search_successful = abap_true.
    ELSE.
      MESSAGE mv_search_value && ` was not found` TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD set_multi_or_all.

    mr_multi_or_all = ir_multi_or_all.

  ENDMETHOD.


  METHOD set_multi_or_current_tuple.

    mr_multi_or_current = ir_multi_or_single.

  ENDMETHOD.


  METHOD set_multi_or_multi_select.

    mr_multi_or_multi = ir_multi_or_multi.

  ENDMETHOD.


  METHOD update_field_input_state.
*.. depending on the select option, not all fields are inputable
    DATA(ls_sel_init) = VALUE #( mr_ui_sel_option_init->*[ option = mr_selfield_line->option ] OPTIONAL ).
    IF ls_sel_init IS NOT INITIAL.
*.... Deactivate all field input
      IF ls_sel_init-no_input = abap_true.
        LOOP AT SCREEN.
          IF screen-name = c_fields-low_value OR
             screen-name = c_fields-high_value OR
             screen-name = c_fields-push.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        IF ls_sel_init-high <> abap_true.
          LOOP AT SCREEN.
            IF screen-name = c_fields-high_value.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF ls_sel_init-low <> abap_true.
          LOOP AT SCREEN.
            IF screen-name = c_fields-low_value.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF ls_sel_init-no_multi = abap_true.
          LOOP AT SCREEN.
            IF screen-name = c_fields-push.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_bottom.

    mr_ui_multi_or_ctrl->top_line = mv_linecount - mv_looplines + 1.
    IF mr_ui_multi_or_ctrl->top_line < 1.
      mr_ui_multi_or_ctrl->top_line = 1.
    ENDIF.

    " set cursor to last line
    DATA(lv_lastindex) = mv_looplines.
    IF lv_lastindex > lines( mr_selfield_lines->* ).
      lv_lastindex = lines( mr_selfield_lines->* ).
    ENDIF.

    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( lv_lastindex ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_down.

    mr_ui_multi_or_ctrl->top_line = mr_ui_multi_or_ctrl->top_line + mv_looplines.
    IF mr_ui_multi_or_ctrl->top_line >= mv_linecount.
      mr_ui_multi_or_ctrl->top_line = mv_linecount - mv_looplines + 1.
      IF mr_ui_multi_or_ctrl->top_line < 1.
        mr_ui_multi_or_ctrl->top_line = 1.
      ENDIF.
    ENDIF.

    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 1 ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_top.

    mr_ui_multi_or_ctrl->top_line = 1.
    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 1 ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_up.

    mr_ui_multi_or_ctrl->top_line = mr_ui_multi_or_ctrl->top_line - mv_looplines.
    IF mr_ui_multi_or_ctrl->top_line < 1.
      mr_ui_multi_or_ctrl->top_line = 1.
    ENDIF.

    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 1 ).

  ENDMETHOD.


  METHOD zif_uitb_table~add_line.


  ENDMETHOD.


  METHOD zif_uitb_table~delete_all.

    LOOP AT mr_selfield_lines->* ASSIGNING FIELD-SYMBOL(<ls_multi_or>).
      delete_line( CHANGING cs_selfield = <ls_multi_or> ).
    ENDLOOP.

    " delete multi select
    CLEAR mr_multi_or_multi->*.

  ENDMETHOD.


  METHOD zif_uitb_table~delete_current_line.

    zif_uitb_table~determine_current_line( ).
    CHECK mv_current_line > 0.

    " delete values of current line
    DATA(lr_multi_or) = REF #( mr_selfield_lines->*[ mv_current_line ] ).
    delete_line( CHANGING cs_selfield = lr_multi_or->* ).

    " delete possible multi selection values

  ENDMETHOD.


  METHOD zif_uitb_table~determine_current_line.

    GET CURSOR LINE mv_current_line.
    mv_current_line = mv_current_line + mr_ui_multi_or_ctrl->top_line - 1.

  ENDMETHOD.


  METHOD zif_uitb_table~determine_line_count.

    mr_ui_multi_or_ctrl->lines = lines( mr_selfield_lines->* ).
    mv_linecount = mr_ui_multi_or_ctrl->lines.

  ENDMETHOD.


  METHOD zif_uitb_table~get_current_line_index.

    rv_index = mv_current_line.

  ENDMETHOD.


  METHOD zif_uitb_table~get_current_line_value.

    zif_uitb_table~determine_current_line( ).
    IF mv_current_line <> 0.
      es_line = mr_selfield_lines->*[ mv_current_line ].
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_table~get_current_loop_line.
    rv_current_loop_line = mv_current_line - mr_ui_multi_or_ctrl->top_line + 1.
  ENDMETHOD.


  METHOD zif_uitb_table~update_fields.
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

    MODIFY mr_selfield_lines->* FROM mr_selfield_line->* INDEX mr_ui_multi_or_ctrl->current_line.

  ENDMETHOD.


  METHOD zif_uitb_table~update_screen_attributes.

*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/12
*&---------------------------------------------------------------------*
*& Description: Refreshes the screen attributes of the current table line
*&---------------------------------------------------------------------*
    DATA: ls_screen TYPE screen.

    " create icons according to current values for 'more' and 'option'
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
    " convert given values to output format
    zcl_dbbr_data_converter=>convert_selopt_to_disp_format(
      EXPORTING iv_tabname   = mr_selfield_line->tabname
                iv_fieldname = mr_selfield_line->fieldname
      CHANGING  cv_value1    = mr_selfield_line->low
                cv_value2    = mr_selfield_line->high
    ).

    " intensify key fields
    IF mr_selfield_line->key = abap_true.
      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-group1 = 'INP' OR
           ls_screen-group2 = 'TXT'.
          ls_screen-intensified = 1.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ENDIF.

    update_field_input_state( ).

    " disable input for table input
    IF mr_selfield_line->is_table_header = abap_true.
      LOOP AT SCREEN INTO ls_screen.
        IF ls_screen-group1 = 'INP' OR
           ls_screen-group3 = 'ICN' OR
           ls_screen-name   = 'OPTION'.
          ls_screen-active = 0.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.

        IF ls_screen-group2 = 'TXT'.
          ls_screen-intensified = 1.
          MODIFY SCREEN FROM ls_screen.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " set field to outputlength of current table column field
    LOOP AT SCREEN.
      IF screen-name = 'GS_MULTI_OR-LOW' OR
         screen-name = 'GS_MULTI_OR-HIGH'.
        screen-length = mr_selfield_line->outputlen.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
