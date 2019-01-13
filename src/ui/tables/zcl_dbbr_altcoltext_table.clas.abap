CLASS ZCL_DBBR_altcoltext_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_UITB_TABLE.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_ui_altcoltext_tab TYPE REF TO ZDBBR_altcoltext_data_itab.
    DATA mr_ui_altcoltext_line TYPE REF TO ZDBBR_altcoltext_data.
    DATA mr_ui_altcotext_control TYPE REF TO cxtab_control.
    DATA mr_ui_global_data TYPE REF TO ZDBBR_global_data.
    DATA mv_current_line TYPE sy-tabix.
    DATA mv_linecount TYPE sy-tabix.
    DATA mv_looplines TYPE sy-loopc.
ENDCLASS.



CLASS ZCL_DBBR_ALTCOLTEXT_TABLE IMPLEMENTATION.


  METHOD constructor.
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_ui_altcoltext_line = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_s_altcoltext ) ).
    mr_ui_altcoltext_tab = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_t_altcoltext ) ).
    mr_ui_altcotext_control = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_altcoltext_tc ) ).
    mr_ui_global_data = CAST #( lr_data_cache->get_data_ref( ZIF_DBBR_main_report_var_ids=>c_s_data ) ).
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~add_line ##needed.
    " not possible
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~delete_all.
    " delete all alternate short and long texts
    LOOP AT mr_ui_altcoltext_tab->* ASSIGNING FIELD-SYMBOL(<ls_altcoltext>).
      CLEAR: <ls_altcoltext>-alt_short_text,
             <ls_altcoltext>-alt_long_text.
    ENDLOOP.
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~delete_current_line.
    ZIF_UITB_TABLE~determine_current_line( ).
    CHECK mv_current_line > 0.

    CLEAR: mr_ui_altcoltext_tab->*[ mv_current_line ]-alt_short_text,
           mr_ui_altcoltext_tab->*[ mv_current_line ]-alt_long_text.
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~determine_current_line.
    mv_current_line = zcl_uitb_cursor=>get_cursor( )->get_line( ).
    mv_current_line = mv_current_line + mr_ui_altcotext_control->top_line - 1.
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~determine_line_count.
    mr_ui_altcotext_control->lines = lines( mr_ui_altcoltext_tab->* ).
    mv_linecount = mr_ui_altcotext_control->lines.
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~get_current_line_index.
    rv_index = mv_current_line.
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~update_fields.
    MODIFY mr_ui_altcoltext_tab->* FROM mr_ui_altcoltext_line->* INDEX mr_ui_altcotext_control->current_line.
  ENDMETHOD.


  METHOD ZIF_UITB_TABLE~update_screen_attributes.
    DATA: ls_screen TYPE screen.

    mv_looplines = sy-loopc.

    IF mr_ui_altcotext_control->current_line > lines( mr_ui_altcoltext_tab->* ).
      LOOP AT SCREEN INTO ls_screen.
        ls_screen-input = 0.
        MODIFY screen FROM ls_screen.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
