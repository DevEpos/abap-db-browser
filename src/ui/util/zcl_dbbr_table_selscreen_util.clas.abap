CLASS zcl_dbbr_table_selscreen_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selscreen_util
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! CONSTRUCTOR
    METHODS constructor
      IMPORTING
        !ir_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data
        !iv_entity_type    TYPE zdbbr_entity_type DEFAULT zif_dbbr_c_entity_type=>table .

    METHODS check_edit_mode
        REDEFINITION .
    METHODS check_primary_entity
        REDEFINITION .

    METHODS get_entity_information
        REDEFINITION .
    METHODS get_title
        REDEFINITION .
    METHODS load_entity
        REDEFINITION .
    METHODS set_custom_functions
        REDEFINITION .
    METHODS update_description_texts
        REDEFINITION .

    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION .
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_tab_size_text TYPE string .
    DATA mf_is_view TYPE abap_bool.

    METHODS fill_selection_mask
        REDEFINITION .
    METHODS fill_primary_entity
        REDEFINITION .
  PRIVATE SECTION.



ENDCLASS.


CLASS zcl_dbbr_table_selscreen_util IMPLEMENTATION.

  METHOD check_edit_mode.
    CHECK mo_data->mr_s_global_data->edit = abap_true.

    delete_join_definition( ).
  ENDMETHOD.


  METHOD check_primary_entity.
    rf_success = abap_true.

    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( mo_data->mr_s_global_data->primary_table ).
    IF ls_table_info IS INITIAL.
      rf_success = abap_false.
      RETURN.
    ENDIF.

    mf_is_view = xsdbool( ls_table_info-tabclass = 'VIEW' ).

    mo_data->mr_s_global_data->client_dependent = ls_table_info-clidep.

    mo_data->mr_v_selmask_entity_text->* = ls_table_info-ddtext.

    " reset edit mode upon table change
    IF ls_table_info-mainflag = abap_true.
      " check if any kind of maintenance view exists
      SELECT COUNT( * ) FROM objs INTO @DATA(lv_maint_view_count)
         WHERE tabname    = @mo_data->mr_s_global_data->primary_table
           AND objecttype = 'V'.
      IF lv_maint_view_count > 0.
        MESSAGE s418(mo) WITH mo_data->mr_s_global_data->primary_table.
      ELSE.
        " check if edit flag should be set
        mo_data->mr_s_global_data->edit = mo_data->mr_s_global_data->maintain_entries.
      ENDIF.
    ELSE.
      mo_data->mr_s_global_data->edit = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
      ir_selscreen_data = ir_selscreen_data
      iv_entity_type    = iv_entity_type
    ).
    mo_data->mr_v_selmask_entity_type->* = 'Table'(001).

*.. Fill custom toolbar menu
    mo_custom_menu = NEW #( ).
    mo_custom_menu->add_function(
      fcode     = zif_dbbr_c_selscreen_functions=>navigate_to_table_def
      text      = |{ 'Navigate to Table Definition'(002) }|
    ).
    mo_custom_menu->add_function(
      fcode     = zif_dbbr_c_selscreen_functions=>cross_reference_table
      text      = |{ 'Where-Used-List for Table'(003) }|
    ).

    fill_toolbar( if_create_extended_search = abap_true ).
  ENDMETHOD.


  METHOD fill_selection_mask.
    IF mo_data->mr_s_join_def->tables IS NOT INITIAL.
      update_multi_selection_mask( ).
    ELSE.
      update_selection_mask( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_primary_entity.
*... set cursor to first line
    mo_data->mo_selection_table->zif_uitb_page_scroller~scroll_page_top( ).
    mo_data->mo_custom_f4_map->clear( ).

    check_edit_mode( ).

*... create selection fields for primary table
    create_table_fields( VALUE #(
        tabname          = is_primary_entity-tabname
        tabname_alias    = is_primary_entity-tabname_alias
        active_selection = abap_true
        is_primary       = abap_true
        selection_order  = is_primary_entity-selection_order )
    ).

    CLEAR mv_tab_size_text.
  ENDMETHOD.


  METHOD get_entity_information.
    super->get_entity_information(
      IMPORTING
        ev_description = ev_description
    ).
    ev_type = COND #( WHEN mf_is_view = abap_true THEN zif_dbbr_c_entity_type=>view ELSE zif_dbbr_c_entity_type=>table ).
    ev_entity =
    ev_entity_raw = mo_data->mr_s_global_data->primary_table.
  ENDMETHOD.


  METHOD get_title.
    IF has_content( ).
      result = |Table - { mo_data->mr_s_global_data->primary_table }{ mv_tab_size_text }|.
    ELSE.
      result = |Table Mode|.
    ENDIF.
  ENDMETHOD.


  METHOD load_entity.
    CHECK mo_data->mr_s_global_data->primary_table IS NOT INITIAL.

    zcl_dbbr_screen_helper=>show_progress( iv_text     = `Selection Mask for Table ` && mo_data->mr_s_global_data->primary_table && ` is loading...`
                                           iv_progress = 1 ).
    clear_edit_flags( ).
    mo_data->set_join_active( abap_false ).
    CLEAR: mo_data->mr_s_settings->*.

    CHECK check_primary_entity( ).

    fill_primary_entity( VALUE #( tabname = mo_data->mr_s_global_data->primary_table
                         alias   = mo_data->mr_s_global_data->primary_table ) ).

    mo_data->mr_v_selmask_entity_name->* = mo_data->mr_s_global_data->primary_table.

    " determine table size
    IF zcl_dbbr_usersettings_factory=>should_read_db_size( ).
      zcl_dbbr_screen_helper=>show_progress( iv_text     = `Determining Table size...`
                                             iv_progress = 70 ).
      mv_tab_size_text = | (rows { zcl_dbbr_selection_helper=>read_db_size( mo_data->mr_s_global_data->primary_table ) NUMBER = USER })|.
    ENDIF.

    update_selection_mask( ).

    mv_entity_id = mo_data->mr_s_global_data->primary_table.

    mo_data->mr_s_entity_info->* = VALUE #(
      entity_id   = mo_data->mr_s_global_data->primary_table
      entity_type = mv_entity_type
    ).

    update_entity_type_sh( ).

    finish_loading( ).

    rf_entity_loaded = abap_true.
  ENDMETHOD.


  METHOD set_custom_functions.
    mo_data->clear_custom_functions( ).

    mo_data->mr_s_top_custom_menu->text = 'Table'(001).

*... fill custom functions
    mo_data->mr_s_entity_function1->text = 'Navigate to Table Definition'(002).
    mo_data->mr_s_entity_function2->text = 'Where-Used-List for Table'(003).
  ENDMETHOD.


  METHOD update_description_texts.

*.. update table description
    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( mv_entity_id ).
    mo_data->mr_v_selmask_entity_text->* = ls_table_info-ddtext.

    DATA(lr_t_fields) = mo_data->mo_tabfield_list->get_fields_ref( ).

*.. update field descriptions
    LOOP AT lr_t_fields->* ASSIGNING FIELD-SYMBOL(<ls_field>)
      GROUP BY ( tabname = <ls_field>-tabname )
      ASSIGNING FIELD-SYMBOL(<ls_grouped_fields>).

      " get updated table field infos for new descriptions
      zcl_dbbr_dictionary_helper=>get_table_field_infos(
        EXPORTING iv_tablename    = <ls_grouped_fields>-tabname
        IMPORTING et_table_fields = DATA(lt_table_fields)
      ).

      LOOP AT GROUP <ls_grouped_fields> ASSIGNING FIELD-SYMBOL(<ls_group_entry>).
        DATA(lr_s_ddic_field) = REF #( lt_table_fields[ tabname   = <ls_group_entry>-tabname
                                                        fieldname = <ls_group_entry>-fieldname ] OPTIONAL ).
        IF lr_s_ddic_field IS NOT INITIAL.
          DATA(ls_alt_text) = mo_altcoltext_f->find_alternative_text(
            iv_tabname   = <ls_group_entry>-tabname
            iv_fieldname = <ls_group_entry>-fieldname ).

          <ls_group_entry>-std_short_text  = lr_s_ddic_field->scrtext_s.
          <ls_group_entry>-alt_medium_text = ls_alt_text-alt_short_text.
          <ls_group_entry>-std_medium_text = lr_s_ddic_field->scrtext_m.
          <ls_group_entry>-std_long_text   = lr_s_ddic_field->scrtext_l.
          <ls_group_entry>-alt_long_text   = ls_alt_text-alt_long_text.
          <ls_group_entry>-header_text     = lr_s_ddic_field->reptext.
          <ls_group_entry>-field_ddtext    = COND #( WHEN ls_alt_text-alt_long_text IS NOT INITIAL THEN ls_alt_text-alt_long_text ELSE lr_s_ddic_field->fieldtext ).

          " update selfield column description
          DATA(lr_s_selfield) = REF #( mo_data->mr_t_table_data->*[
              tabname         = <ls_group_entry>-tabname
              fieldname       = <ls_group_entry>-fieldname
              is_table_header = abap_false ] OPTIONAL ).

          IF lr_s_selfield IS NOT INITIAL.
            lr_s_selfield->texts = CORRESPONDING #( lr_s_ddic_field->* ).
            " try to find alternative col text

            IF ls_alt_text IS NOT INITIAL AND ls_alt_text-alt_long_text IS NOT INITIAL.
              lr_s_selfield->description = ls_alt_text-alt_long_text.
            ELSE.
              lr_s_selfield->description = COND #( WHEN lr_s_selfield->scrtext_l IS NOT INITIAL THEN
                                                      lr_s_selfield->scrtext_l
                                                   WHEN lr_s_selfield->scrtext_m IS NOT INITIAL THEN
                                                      lr_s_selfield->scrtext_m
                                                   WHEN lr_s_selfield->ddtext IS NOT INITIAL THEN
                                                      lr_s_selfield->ddtext
                                                   ELSE
                                                      <ls_group_entry>-fieldname_raw ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = super->get_deactivated_functions( ).
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_ui_function.
    CASE cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>navigate_to_table_def.
        zcl_dbbr_dictionary_helper=>navigate_to_table( mo_data->mr_s_global_data->primary_table ).
        CLEAR cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>cross_reference_table.
        zcl_dbbr_dictionary_helper=>cross_reference_table( mo_data->mr_s_global_data->primary_table ).
        CLEAR cv_function.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
