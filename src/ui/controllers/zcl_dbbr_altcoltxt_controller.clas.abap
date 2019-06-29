CLASS ZCL_DBBR_altcoltxt_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_UITB_SCREEN_CONTROLLER.
    METHODS constructor
      IMPORTING
        ir_tabfield_list    TYPE REF TO ZCL_DBBR_tabfield_list
        ir_altcoltext_table TYPE REF TO ZCL_DBBR_altcoltext_table.
  PROTECTED SECTION.
private section.

  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

  constants:
    BEGIN OF mc_function_codes ,
                 save                  TYPE sy-ucomm VALUE 'SAVE' ##no_text,
                 delete_alternate_text TYPE sy-ucomm VALUE 'DEL_ALTTXT' ##no_text,
                 delete_all_alt_texts  TYPE sy-ucomm VALUE 'DEL_ALLTXT' ##no_text,
                 accept_changed_data   TYPE sy-ucomm VALUE 'ACCEPTDATA' ##no_text,
               END OF mc_function_codes .
  data MR_UI_ALTCOLTEXT_TAB type ref to ZDBBR_ALTCOLTEXT_DATA_ITAB .
  data MR_UI_ALTCOLTEXT_LINE type ref to ZDBBR_ALTCOLTEXT_DATA .
  data MR_UI_ALTCOTEXT_CONTROL type ref to CXTAB_CONTROL .
  data MR_UI_GLOBAL_DATA type ref to ZDBBR_GLOBAL_DATA .
  data MR_TABFIELD_LIST type ref to ZCL_DBBR_TABFIELD_LIST .
  data MR_TABLE type ref to ZCL_DBBR_ALTCOLTEXT_TABLE .
  data MF_TRANSFER_DATA type ABAP_BOOL .

  methods INIT_TABLIST .
  methods SAVE_ALTERNATIVE_TEXTS .
ENDCLASS.



CLASS ZCL_DBBR_ALTCOLTXT_CONTROLLER IMPLEMENTATION.


  METHOD constructor.
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_ui_altcoltext_line = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_s_altcoltext ) ).
    mr_ui_altcoltext_tab = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_t_altcoltext ) ).
    mr_ui_altcotext_control = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_altcoltext_tc ) ).
    mr_ui_global_data = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_s_data ) ).

    mr_tabfield_list = ir_tabfield_list.
    mr_table = ir_altcoltext_table.
  ENDMETHOD.


  METHOD init_tablist.
    mr_tabfield_list->initialize_iterator( ).

    WHILE mr_tabfield_list->has_more_lines( ).
      DATA(lr_current_entry) = mr_tabfield_list->get_next_entry( ).
      APPEND VALUE ZDBBR_altcoltext_data(
            tabname        = lr_current_entry->tabname
            fieldname      = lr_current_entry->fieldname
            language       = zcl_dbbr_system_helper=>get_system_language( )
            alt_short_text = lr_current_entry->alt_medium_text
            alt_long_text  = lr_current_entry->alt_long_text
            std_short_text = lr_current_entry->std_medium_text
            std_long_text  = lr_current_entry->std_long_text
      ) TO mr_ui_altcoltext_tab->*.
    ENDWHILE.
  ENDMETHOD.


  METHOD save_alternative_texts.
    DATA(lr_alttext_f) = NEW ZCL_DBBR_altcoltext_factory( ).
    LOOP AT mr_ui_altcoltext_tab->* ASSIGNING FIELD-SYMBOL(<ls_altcol_text>).
      lr_alttext_f->save_altcoltext( <ls_altcol_text> ).

      " update alternative text of tabfield list entry
      try.
          data(lr_field) = mr_tabfield_list->get_field_ref(
              iv_tabname_alias   = <ls_altcol_text>-tabname
              iv_fieldname = <ls_altcol_text>-fieldname
          ).
          lr_field->alt_long_text = <ls_altcol_text>-alt_long_text.
          lr_field->alt_medium_text = <ls_altcol_text>-alt_short_text.
        catch cx_sy_itab_line_not_found.
      endtry.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.

    init_tablist( ).

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_altcoltext_controller
            global_ref    = me )
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_altcoltext_table
            global_ref    = mr_table )
        )
    ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~cancel.
    CASE iv_function_code.
      WHEN ZIF_DBBR_global_consts=>gc_function_codes-cancel OR
           ZIF_DBBR_global_consts=>gc_function_codes-cancel_screen.
        ZCL_DBBR_screen_helper=>leave_screen( ).

      WHEN ZIF_DBBR_global_consts=>gc_function_codes-quit_program.
        ZCL_DBBR_screen_helper=>quit_program( ).

    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_maintain_alternative_cols.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
    DATA(lv_function) = cv_function_code.

    CLEAR cv_function_code.

    CASE lv_function.
      WHEN mc_function_codes-accept_changed_data.
        save_alternative_texts( ).
        mf_transfer_data = abap_true.
        zcl_dbbr_screen_helper=>leave_screen( ).

      WHEN mc_function_codes-delete_all_alt_texts.
        mr_table->zif_uitb_table~delete_all( ).

      WHEN mc_function_codes-delete_alternate_text.
        mr_table->zif_uitb_table~delete_current_line( ).

      WHEN zif_dbbr_global_consts=>gc_function_codes-leave_screen.
        zcl_dbbr_screen_helper=>leave_screen( ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~pbo.
    ZIF_UITB_SCREEN_CONTROLLER~set_status( ).

    mr_table->ZIF_UITB_TABLE~determine_line_count( ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~set_status.
    SET PF-STATUS '0108' OF PROGRAM zif_dbbr_c_report_id=>main.
    SET TITLEBAR 'PROGTITLE' OF PROGRAM zif_dbbr_c_report_id=>main with 'Alternative Column Texts'.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_transfer_data.
  ENDMETHOD.
ENDCLASS.
