CLASS zcl_dbbr_multi_or_controller DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    METHODS constructor
      IMPORTING
        !ir_multi_or_table     TYPE REF TO zcl_dbbr_multi_or_table
        !it_multi_selfield_all TYPE zdbbr_selfield_itab
        !it_multi_or_all       TYPE zdbbr_or_seltab_itab
        !ir_custom_f4_map      TYPE REF TO zcl_dbbr_custom_f4_map .
    METHODS call_selfield_value_help
      IMPORTING
        !if_for_low TYPE boolean OPTIONAL .
    METHODS get_multi_or_values
      RETURNING
        VALUE(rt_multi_or) TYPE zdbbr_or_seltab_itab .
    METHODS should_data_be_transferred
      RETURNING
        VALUE(rf_do_transfer) TYPE boolean .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    CONSTANTS:
      BEGIN OF mc_functions,
        search_for_field              TYPE sy-ucomm VALUE 'SEARCH',
        search_further                TYPE sy-ucomm VALUE 'SEARCHFROM',
        go_to_next_tuple              TYPE sy-ucomm VALUE 'NEXT',
        go_to_previous_tuple          TYPE sy-ucomm VALUE 'PREV',
        transfer_data                 TYPE sy-ucomm VALUE 'TAKE',
        delete_current_line           TYPE sy-ucomm VALUE 'DELETE',
        delete_all                    TYPE sy-ucomm VALUE 'DELETE_ALL',
        go_to_next_criteria           TYPE sy-ucomm VALUE 'CRITNEXT',
        go_to_previous_criteria       TYPE sy-ucomm VALUE 'CRITPREV',
        multi_selection               TYPE sy-ucomm VALUE 'MORE',
        select_option_choice          TYPE sy-ucomm VALUE 'OPTION',
        copy_selvalues_from_selscreen TYPE sy-ucomm VALUE 'COPYSELVAL',
      END OF mc_functions .
    DATA mr_multi_or_table TYPE REF TO zcl_dbbr_multi_or_table .
    DATA mt_multi_or_all TYPE zdbbr_or_seltab_itab .
    DATA mf_search_successful TYPE boolean .
    DATA mr_custom_f4_map TYPE REF TO zcl_dbbr_custom_f4_map .
    DATA mr_current_multi_or TYPE REF TO zdbbr_or_seltab .
    DATA mt_multi_selection TYPE zdbbr_selfield_itab .
    DATA mf_transfer_data TYPE boolean .
    DATA mf_first_call TYPE boolean .
    DATA mt_multi_selfield_all TYPE zdbbr_selfield_itab .
    DATA mr_ui_multi_or_field TYPE REF TO zdbbr_selfield .
    DATA mr_ui_multi_or_fields TYPE REF TO zdbbr_selfield_itab .
    DATA mr_ui_multi_or_tuple_no TYPE REF TO tswpos .
    DATA mr_ui_selfields TYPE REF TO zdbbr_selfield_itab .
    DATA mr_ui_sel_option_init TYPE REF TO zdbbr_selopt_control_itab .
    DATA mr_ui_multi_or_ctrl TYPE REF TO cxtab_control .

    METHODS copy_values_from_main_selscrn .
    METHODS get_tuple_data_for_tuple .
    METHODS delete_all_entries .
    METHODS delete_current_line .
    METHODS go_to_next_tuple .
    METHODS go_to_previous_tuple .
    METHODS go_to_next_criteria .
    METHODS go_to_previous_criteria .
    METHODS show_option_dialog .
    METHODS show_multi_selection_dialog .
    METHODS store_current_tuple_data .
ENDCLASS.



CLASS zcl_dbbr_multi_or_controller IMPLEMENTATION.


  METHOD call_selfield_value_help.
    DATA: ls_selfield TYPE zdbbr_selfield.

    mr_multi_or_table->zif_uitb_table~get_current_line_value( IMPORTING es_line = ls_selfield ).
    DATA(ls_selfield_save) = ls_selfield.

    zcl_dbbr_f4_helper=>call_selfield_f4(
      EXPORTING
        if_low           = if_for_low
        iv_repid         = zif_dbbr_c_report_id=>main
        iv_selfield_name = 'GS_MULTI_OR'
        iv_current_line  = mr_multi_or_table->zif_uitb_table~get_current_line_index( )
        ir_custom_f4_map = mr_custom_f4_map
      CHANGING
        cs_selfield      = ls_selfield
    ).

    IF ls_selfield-low <> ls_selfield_save-low OR
       ls_selfield-high <> ls_selfield_save-high.
      mr_ui_multi_or_field->* = ls_selfield.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mr_multi_or_table = ir_multi_or_table.
    mt_multi_or_all = it_multi_or_all.
    mr_custom_f4_map = ir_custom_f4_map.

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_ui_multi_or_field = CAST zdbbr_selfield( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_multi_or ) ).
    mr_ui_selfields = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields ) ).
    mr_ui_multi_or_fields = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_multi_or ) ).
    mr_ui_multi_or_tuple_no = CAST tswpos( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_v_or_tuple_number ) ).
    mr_ui_sel_option_init = CAST zdbbr_selopt_control_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_sel_init ) ).
    mr_ui_multi_or_ctrl = CAST cxtab_control( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_multi_or_tc ) ).

    " fill table from selection criteria table
    mr_ui_multi_or_fields->* = mr_ui_selfields->*.

    " delete parameters
    DELETE mr_ui_multi_or_fields->* WHERE tabname = zif_dbbr_global_consts=>c_parameter_dummy_table.

    " clear all values
    LOOP AT mr_ui_multi_or_fields->* ASSIGNING FIELD-SYMBOL(<ls_selfield_or>) WHERE is_table_header = abap_false.
      CLEAR: <ls_selfield_or>-low,
             <ls_selfield_or>-high,
             <ls_selfield_or>-push,
             <ls_selfield_or>-option,
             <ls_selfield_or>-sign.
    ENDLOOP.

    mr_ui_multi_or_tuple_no->* = 1.

    " is there already a tuple?
    get_tuple_data_for_tuple( ).

  ENDMETHOD.


  METHOD copy_values_from_main_selscrn.
*&---------------------------------------------------------------------*
*& Author: stockbal     Date: 2017/02/27
*&---------------------------------------------------------------------*
*& Description: Copies parameter values from main selection screen
*&---------------------------------------------------------------------*
    delete_all_entries( ).

    " transfer values from main selection screen table into current tuple
    LOOP AT mr_ui_multi_or_fields->* ASSIGNING FIELD-SYMBOL(<ls_multi_or_selfield>).
      ASSIGN mr_ui_selfields->*[ tabname   = <ls_multi_or_selfield>-tabname
                                 fieldname = <ls_multi_or_selfield>-fieldname ] TO FIELD-SYMBOL(<ls_selfield_line>).

      IF <ls_selfield_line>-low = space AND
         <ls_selfield_line>-high = space AND
         <ls_selfield_line>-option = space.
        CONTINUE.
      ENDIF.

      <ls_multi_or_selfield>-low = <ls_selfield_line>-low.
      <ls_multi_or_selfield>-high = <ls_selfield_line>-high.
      <ls_multi_or_selfield>-option = <ls_selfield_line>-option.
      <ls_multi_or_selfield>-sign = <ls_selfield_line>-sign.

      " fetch multi selection values
      LOOP AT mt_multi_selfield_all ASSIGNING FIELD-SYMBOL(<ls_selfield_multi>) WHERE tabname   = <ls_multi_or_selfield>-tabname AND
                                                                                      fieldname = <ls_multi_or_selfield>-fieldname.
        APPEND <ls_selfield_multi> TO mt_multi_selection.
      ENDLOOP.

      IF sy-subrc = 0.
        <ls_multi_or_selfield>-push = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD delete_all_entries.

    mr_multi_or_table->zif_uitb_table~delete_all( ).

  ENDMETHOD.


  METHOD delete_current_line.

    mr_multi_or_table->zif_uitb_table~determine_current_line( ).
    mr_multi_or_table->zif_uitb_table~delete_current_line( ).

  ENDMETHOD.


  METHOD get_multi_or_values.

    " 1) delete all tuples that have no data
    DELETE mt_multi_or_all WHERE values IS INITIAL.

    " 2) rename tuple numbers
    LOOP AT mt_multi_or_all ASSIGNING FIELD-SYMBOL(<ls_multi_or>).
      <ls_multi_or>-pos = sy-tabix.
    ENDLOOP.
    rt_multi_or = mt_multi_or_all.

  ENDMETHOD.


  METHOD get_tuple_data_for_tuple.

    " check if there is a tuple for the current tupel number
    mr_current_multi_or = REF #( mt_multi_or_all[ pos = mr_ui_multi_or_tuple_no->* ] DEFAULT NEW #( ) ).
    IF mr_current_multi_or->* IS INITIAL.
      APPEND VALUE #( pos = mr_ui_multi_or_tuple_no->* ) TO mt_multi_or_all ASSIGNING FIELD-SYMBOL(<ls_new_tuple>).
      mr_current_multi_or = REF #( <ls_new_tuple> ).
    ENDIF.

    " fill data for current tuple
    LOOP AT mr_current_multi_or->values ASSIGNING FIELD-SYMBOL(<ls_multi_or_tuple_line>).
      ASSIGN mr_ui_multi_or_fields->*[ tabname   = <ls_multi_or_tuple_line>-tabname
                                       fieldname = <ls_multi_or_tuple_line>-fieldname ] TO FIELD-SYMBOL(<ls_multi_or_line>).
      <ls_multi_or_line> = CORRESPONDING #( BASE ( <ls_multi_or_line> ) <ls_multi_or_tuple_line> ).

      IF <ls_multi_or_tuple_line>-multi_values IS NOT INITIAL.
        <ls_multi_or_line>-push = abap_true.
        " fill initial line from selfield line
        DATA(ls_selfield) = <ls_multi_or_line>.
        LOOP AT <ls_multi_or_tuple_line>-multi_values ASSIGNING FIELD-SYMBOL(<ls_multi_or_tuple_multi>).
          ls_selfield = CORRESPONDING #( BASE ( ls_selfield ) <ls_multi_or_tuple_multi> ).
          APPEND ls_selfield TO mt_multi_selection.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    " set current tuple of table
    mr_multi_or_table->set_multi_or_current_tuple( mr_current_multi_or ).
    mr_multi_or_table->set_multi_or_multi_select( REF #( mt_multi_selection ) ).

  ENDMETHOD.


  METHOD go_to_next_criteria.

    DATA: lv_found_index TYPE sy-tabix.

    mr_multi_or_table->zif_uitb_table~determine_current_line( ).
    DATA(lv_current_index) = mr_multi_or_table->zif_uitb_table~get_current_line_index( ) + 1.

    DATA(lr_cursor) = zcl_uitb_cursor=>get_cursor( if_reset = abap_false ).

    LOOP AT mr_ui_multi_or_fields->* ASSIGNING FIELD-SYMBOL(<ls_multi_or>) FROM lv_current_index
        WHERE low IS NOT INITIAL
           OR high IS NOT INITIAL
           OR option IS NOT INITIAL.
      mr_ui_multi_or_ctrl->top_line = sy-tabix.

      lr_cursor->set_field( 'GS_MULTI_OR-LOW' ).
      lr_cursor->set_line( 1 ).
      RETURN.
    ENDLOOP.


  ENDMETHOD.


  METHOD go_to_next_tuple.

    store_current_tuple_data( ).
    ADD 1 TO mr_ui_multi_or_tuple_no->*.

    get_tuple_data_for_tuple( ).

  ENDMETHOD.


  METHOD go_to_previous_criteria.

    mr_multi_or_table->zif_uitb_table~determine_current_line( ).

    DATA(lv_current_line) = mr_multi_or_table->zif_uitb_table~get_current_line_index( ) - 1.

    IF lv_current_line < 1.
      RETURN. " no processing possible
    ENDIF.

    WHILE lv_current_line > 0.
      DATA(lr_current_entry) = REF #( mr_ui_multi_or_fields->*[ lv_current_line ] ).
      IF lr_current_entry->low IS NOT INITIAL OR
         lr_current_entry->high IS NOT INITIAL OR
         lr_current_entry->option IS NOT INITIAL.
        mr_ui_multi_or_ctrl->top_line = lv_current_line.
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 1 ).
        RETURN.
      ENDIF.
      SUBTRACT 1 FROM lv_current_line.
    ENDWHILE.


  ENDMETHOD.


  METHOD go_to_previous_tuple.

    IF mr_ui_multi_or_tuple_no->* = 1. " no previous tuple exists
      MESSAGE i058(zdbbr_info).
      RETURN.
    ENDIF.

    store_current_tuple_data( ).

    SUBTRACT 1 FROM mr_ui_multi_or_tuple_no->*.

    get_tuple_data_for_tuple( ).

  ENDMETHOD.


  METHOD should_data_be_transferred.

    rf_do_transfer = mf_transfer_data.

  ENDMETHOD.


  METHOD show_multi_selection_dialog.

    mr_multi_or_table->zif_uitb_table~determine_current_line( ).

    zcl_dbbr_app_starter=>show_multi_select(
      EXPORTING
        iv_current_line   = mr_multi_or_table->zif_uitb_table~get_current_line_index( )
        ir_custom_f4_map  = mr_custom_f4_map
      CHANGING
        ct_selfield       = mr_ui_multi_or_fields->*
        ct_selfield_multi = mt_multi_selection
    ).

  ENDMETHOD.


  METHOD show_option_dialog.

    DATA: ls_current_line TYPE zdbbr_selfield.

    mr_multi_or_table->zif_uitb_table~get_current_line_value( IMPORTING es_line = ls_current_line ).

    IF ls_current_line IS INITIAL.
      RETURN.
    ENDIF.

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
        DATA(ls_sel_init) = mr_ui_sel_option_init->*[ option = ls_chosen_option-option ].
        IF ls_sel_init-high <> abap_true.
          CLEAR ls_current_line-high.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    MODIFY mr_ui_multi_or_fields->* FROM ls_current_line INDEX mr_multi_or_table->zif_uitb_table~get_current_line_index( ).

  ENDMETHOD.


  METHOD store_current_tuple_data.

    CLEAR mr_current_multi_or->values.

    LOOP AT mr_ui_multi_or_fields->* ASSIGNING FIELD-SYMBOL(<ls_multi_or>)
      WHERE low IS NOT INITIAL OR
            high IS NOT INITIAL OR
            option IS NOT INITIAL.

      APPEND VALUE #( tabname   = <ls_multi_or>-tabname
                      fieldname = <ls_multi_or>-fieldname
                      sign      = <ls_multi_or>-sign
                      option    = <ls_multi_or>-option
                      low       = <ls_multi_or>-low
                      high      = <ls_multi_or>-high ) TO mr_current_multi_or->values ASSIGNING FIELD-SYMBOL(<ls_multi_or_line>).

      """ handle special case for Date-type fields with only option filled
      IF <ls_multi_or>-datatype = 'DATS' AND
         <ls_multi_or>-option   <> space AND
         <ls_multi_or>-low      = space  AND
         <ls_multi_or>-high     = space.
        " fill date with initial date
        <ls_multi_or_line>-low = VALUE sy-datum( ).
      ENDIF.

      " get multi selection values
      <ls_multi_or_line>-multi_values = VALUE #(
        FOR multi_value IN mt_multi_selection
        WHERE ( tabname   = <ls_multi_or>-tabname AND
                fieldname = <ls_multi_or>-fieldname )
        ( CORRESPONDING #( multi_value ) )
      ).
    ENDLOOP.

    mr_multi_or_table->zif_uitb_table~delete_all( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.

    mf_first_call = abap_true.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_multi_or_controller
            global_ref    = me )
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_multi_or_table
            global_ref    = mr_multi_or_table )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.
    zcl_uitb_screen_util=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.

    CLEAR mr_ui_multi_or_fields->*.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_show_multi_or_select.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    DATA(lv_function_code) = cv_function_code.
    CLEAR cv_function_code.

    " get the current line
    mr_multi_or_table->zif_uitb_table~determine_current_line( ).
    " determine current cursor
    DATA(lr_cursor) = zcl_uitb_cursor=>get_cursor( ).

    CASE lv_function_code.
      WHEN zif_dbbr_global_consts=>gc_function_codes-cancel OR
           zif_dbbr_global_consts=>gc_function_codes-cancel_screen OR
           zif_dbbr_global_consts=>gc_function_codes-leave_screen OR
           zif_dbbr_global_consts=>gc_function_codes-quit_program.

        zcl_dbbr_screen_helper=>leave_screen( ).

      WHEN mc_functions-select_option_choice.
        show_option_dialog( ).
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_field( 'GS_MULTI_OR-LOW' ).
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( mr_multi_or_table->zif_uitb_table~get_current_loop_line( ) ).

      WHEN mc_functions-multi_selection.
        show_multi_selection_dialog( ).
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_field( 'GS_MULTI_OR-LOW' ).
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( mr_multi_or_table->zif_uitb_table~get_current_loop_line( ) ).

      WHEN mc_functions-transfer_data.
        mf_transfer_data = abap_true.
        store_current_tuple_data( ).
        zcl_dbbr_screen_helper=>leave_screen( ).

      WHEN mc_functions-delete_current_line.
        delete_current_line( ).

      WHEN mc_functions-delete_all.
        delete_all_entries( ).

      WHEN mc_functions-go_to_previous_tuple.
        go_to_previous_tuple( ).

      WHEN mc_functions-go_to_next_tuple.
        go_to_next_tuple( ).

      WHEN mc_functions-go_to_previous_criteria.
        go_to_previous_criteria( ).

      WHEN mc_functions-go_to_next_criteria.
        go_to_next_criteria( ).

      WHEN mc_functions-search_for_field.
        mf_search_successful = mr_multi_or_table->search( ).

      WHEN mc_functions-search_further.
        mf_search_successful = mr_multi_or_table->search( if_continue_previous_search = abap_true ).

      WHEN zif_dbbr_global_consts=>gc_function_codes-scroll_to_top.
        mr_multi_or_table->zif_uitb_page_scroller~scroll_page_top( ).

      WHEN zif_dbbr_global_consts=>gc_function_codes-scroll_page_up.
        mr_multi_or_table->zif_uitb_page_scroller~scroll_page_up( ).

      WHEN zif_dbbr_global_consts=>gc_function_codes-scroll_page_down.
        mr_multi_or_table->zif_uitb_page_scroller~scroll_page_down( ).

      WHEN zif_dbbr_global_consts=>gc_function_codes-scroll_to_bottom.
        mr_multi_or_table->zif_uitb_page_scroller~scroll_page_bottom( ).

      WHEN mc_functions-copy_selvalues_from_selscreen.
        copy_values_from_main_selscrn( ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    IF mf_first_call = abap_true.
      CLEAR mf_first_call.
    ENDIF.

    zif_uitb_screen_controller~set_status( ).

    IF mf_search_successful = abap_true.
      zcl_uitb_cursor=>set_cursor( iv_field = 'GS_MULTI_OR-LOW' iv_line = 1 ).
      CLEAR mf_search_successful.
      RETURN.
    ELSE.
      zcl_uitb_cursor=>refresh_cursor( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    DATA(lv_report_id) = get_report_id( ).

    SET PF-STATUS '0106' OF PROGRAM lv_report_id.
    SET TITLEBAR '106' OF PROGRAM lv_report_id.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
  ENDMETHOD.
ENDCLASS.
