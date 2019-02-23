**********************************************************************
* PAI / PBO for selection screens
**********************************************************************
AT SELECTION-SCREEN.
  CASE sy-dynnr.

    WHEN zif_dbbr_screen_ids=>c_save_query.
      gr_save_query_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

    WHEN zif_dbbr_screen_ids=>c_maintain_join_table.
      gr_edit_join_table_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

    WHEN zif_dbbr_screen_ids=>c_maintain_join_cond.
      gr_edit_join_cond_view->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

    WHEN zif_dbbr_screen_ids=>c_table_variant OR
         zif_dbbr_screen_ids=>c_query_variant OR
         zif_dbbr_screen_ids=>c_cds_view_variant.
      gr_variant_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

    WHEN zif_dbbr_screen_ids=>c_save_sql_query.
      gr_save_sql_query_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

    WHEN 1300.
      gr_addtextfield_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

    WHEN 1400.
      gr_copy_query_controller->zif_uitb_screen_controller~handle_user_command( CHANGING cv_function_code = sscrfields-ucomm ).

  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  DATA(lv_function) = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.

  CASE sy-dynnr.

    WHEN zif_dbbr_screen_ids=>c_save_query.
      gr_save_query_controller->zif_uitb_screen_controller~cancel( lv_function ).

    WHEN zif_dbbr_screen_ids=>c_maintain_join_table.
      gr_edit_join_table_controller->zif_uitb_screen_controller~cancel( lv_function ).

    WHEN zif_dbbr_screen_ids=>c_maintain_join_cond.
      gr_edit_join_cond_view->zif_uitb_screen_controller~cancel( lv_function ).

    WHEN zif_dbbr_screen_ids=>c_table_variant OR
         zif_dbbr_screen_ids=>c_query_variant OR
         zif_dbbr_screen_ids=>c_cds_view_variant.
      gr_variant_controller->zif_uitb_screen_controller~cancel( lv_function ).

    WHEN zif_dbbr_screen_ids=>c_save_sql_query.
      gr_save_sql_query_controller->zif_uitb_screen_controller~cancel( lv_function ).

    WHEN 1300.
      gr_addtextfield_controller->zif_uitb_screen_controller~cancel( lv_function ).

    WHEN 1400.
      gr_copy_query_controller->zif_uitb_screen_controller~cancel( lv_function ).

  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.

    WHEN zif_dbbr_screen_ids=>c_save_query.
      gr_save_query_controller->zif_uitb_screen_controller~pbo( ).

    WHEN zif_dbbr_screen_ids=>c_maintain_join_table.
      gr_edit_join_table_controller->zif_uitb_screen_controller~pbo( ).

    WHEN zif_dbbr_screen_ids=>c_maintain_join_cond.
      gr_edit_join_cond_view->zif_uitb_screen_controller~pbo( ).

    WHEN zif_dbbr_screen_ids=>c_table_variant OR
         zif_dbbr_screen_ids=>c_query_variant OR
         zif_dbbr_screen_ids=>c_cds_view_variant.
      gr_variant_controller->zif_uitb_screen_controller~pbo( ).

    WHEN zif_dbbr_screen_ids=>c_save_sql_query.
      gr_save_sql_query_controller->zif_uitb_screen_controller~pbo( ).

    WHEN 1300.
      gr_addtextfield_controller->zif_uitb_screen_controller~pbo( ).

    WHEN 1400.
      gr_copy_query_controller->zif_uitb_screen_controller~pbo( ).

  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varnam.
  gr_variant_controller->call_variant_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_srcfld.
  gr_edit_join_cond_view->call_source_field_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trgfld.
  gr_edit_join_cond_view->call_target_field_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_val1.
  gr_edit_join_cond_view->call_value1_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_val2.
  gr_edit_join_cond_view->call_value2_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_idtab.
  gr_addtextfield_controller->call_id_table_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_idfld.
  gr_addtextfield_controller->call_id_field_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_idfld2.
  gr_addtextfield_controller->call_id_field2_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_condf.
  gr_addtextfield_controller->call_cond_field_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_txtfld.
  gr_addtextfield_controller->call_text_field_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lngfld.
  gr_addtextfield_controller->call_language_field_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_keyfld.
  gr_addtextfield_controller->call_keyfield_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_keyfd2.
  gr_addtextfield_controller->call_keyfield2_f4( ).

**********************************************************************
