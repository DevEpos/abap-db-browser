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
    METHODS delete_join_definition
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
    METHODS update_join_definition
        REDEFINITION .
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION .
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_tab_size_text TYPE string .
    DATA mf_is_view TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Builds the alias to table map</p>
    METHODS build_table_to_alias_map .

    METHODS fill_selection_mask
        REDEFINITION .
    METHODS fill_table
        REDEFINITION .
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_dbbr_table_selscreen_util IMPLEMENTATION.


  METHOD build_table_to_alias_map.
    CLEAR: mr_data->mr_t_table_to_alias_map->*.

    DATA(lr_join_def) = mr_data->mr_s_join_def.
    ASSIGN mr_data->mr_s_join_def->* TO FIELD-SYMBOL(<ls_join_def>).

    " check if there is an active INNER JOIN
    IF lines( <ls_join_def>-tables ) > 0.
      mr_data->set_join_active( ).
      mr_data->mr_t_table_to_alias_map->* = zcl_dbbr_join_helper=>build_table_to_alias_map(
        ir_tabfields = mr_data->mr_tabfield_list
      ).
    ELSE.
      mr_data->set_join_active( abap_false ).
    ENDIF.
  ENDMETHOD.


  METHOD check_edit_mode.
    CHECK mr_data->mr_s_global_data->edit = abap_true.

    delete_join_definition( ).
  ENDMETHOD.


  METHOD check_primary_entity.
    rf_success = abap_true.

    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( mr_data->mr_s_global_data->primary_table ).
    IF ls_table_info IS INITIAL.
      rf_success = abap_false.
      RETURN.
    ENDIF.

    mf_is_view = xsdbool( ls_table_info-tabclass = 'VIEW' ).

    mr_data->mr_s_global_data->client_dependent = ls_table_info-clidep.

    mr_data->mr_v_selmask_entity_text->* = ls_table_info-ddtext.

    " reset edit mode upon table change
    IF ls_table_info-mainflag = abap_true.
      " check if any kind of maintenance view exists
      SELECT COUNT( * ) FROM objs INTO @DATA(lv_maint_view_count)
         WHERE tabname    = @mr_data->mr_s_global_data->primary_table
           AND objecttype = 'V'.
      IF lv_maint_view_count > 0.
        MESSAGE s418(mo) WITH mr_data->mr_s_global_data->primary_table.
      ELSE.
        " check if edit flag should be set
        mr_data->mr_s_global_data->edit = mr_data->mr_s_global_data->maintain_entries.
      ENDIF.
    ELSE.
      mr_data->mr_s_global_data->edit = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
      ir_selscreen_data = ir_selscreen_data
      iv_entity_type    = iv_entity_type
    ).
    mr_data->mr_v_selmask_entity_type->* = 'Table'(001).

*.. Fill custom toolbar menu
    mr_custom_menu = NEW #( ).
    mr_custom_menu->add_function(
      fcode     = zif_dbbr_c_selscreen_functions=>navigate_to_table_def
      text      = |{ 'Navigate to Table Definition'(002) }|
    ).
    mr_custom_menu->add_function(
      fcode     = zif_dbbr_c_selscreen_functions=>cross_reference_table
      text      = |{ 'Where-Used-List for Table'(003) }|
    ).

    fill_toolbar( if_create_extended_search = abap_true ).
  ENDMETHOD.


  METHOD delete_join_definition.
    CLEAR: mr_data->mr_s_join_def->tables.

    update_join_definition( ).
  ENDMETHOD.


  METHOD fill_selection_mask.
    IF mr_data->mr_s_join_def->tables IS NOT INITIAL.
      update_multi_selection_mask( ).
    ELSE.
      update_selection_mask( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_table.
*... set cursor to first line
    mr_data->mr_selection_table->zif_uitb_page_scroller~scroll_page_top( ).
    mr_data->mr_custom_f4_map->clear( ).

    check_edit_mode( ).

*... create selection fields for primary table
    create_table_fields(
        iv_tablename        = mr_data->mr_s_global_data->primary_table
        if_mark_for_output  = abap_true
        if_selection_active = abap_true
        if_is_primary       = abap_true
    ).

    CLEAR mv_tab_size_text.
  ENDMETHOD.


  METHOD get_entity_information.
    super->get_entity_information(
      IMPORTING
        ev_description = ev_description
    ).
    ev_type = cond #( when mf_is_view = abap_true then zif_dbbr_c_entity_type=>view else zif_dbbr_c_entity_type=>table ).
    ev_entity =
    ev_entity_raw = mr_data->mr_s_global_data->primary_table.
  ENDMETHOD.


  METHOD get_title.
    IF has_content( ).
      result = |Table - { mr_data->mr_s_global_data->primary_table }{ mv_tab_size_text }|.
    ELSE.
      result = |Table Mode|.
    ENDIF.
  ENDMETHOD.


  METHOD load_entity.
    CHECK mr_data->mr_s_global_data->primary_table IS NOT INITIAL.

    zcl_dbbr_screen_helper=>show_progress( iv_text     = `Selection Mask for Table ` && mr_data->mr_s_global_data->primary_table && ` is loading...`
                                           iv_progress = 1 ).
    clear_edit_flags( ).
    mr_data->set_join_active( abap_false ).
    CLEAR: mr_data->mr_s_settings->*.

    CHECK check_primary_entity( ).

    fill_table( ).

    mr_data->mr_v_selmask_entity_name->* = mr_data->mr_s_global_data->primary_table.

    " determine table size
    IF mr_usersettings_f->should_read_db_size( ).
      zcl_dbbr_screen_helper=>show_progress( iv_text     = `Determining Table size...`
                                             iv_progress = 70 ).
      mv_tab_size_text = | (rows { zcl_dbbr_selection_helper=>read_db_size( mr_data->mr_s_global_data->primary_table ) NUMBER = USER })|.
    ENDIF.

    update_alv_variant( ).

    update_selection_mask( ).

    mv_entity_id = mr_data->mr_s_global_data->primary_table.

    mr_data->mr_s_entity_info->* = VALUE #(
      entity_id   = mr_data->mr_s_global_data->primary_table
      entity_type = mv_entity_type
    ).

    update_entity_type_sh( ).

    finish_loading( ).

    rf_entity_loaded = abap_true.
  ENDMETHOD.


  METHOD set_custom_functions.
    mr_data->clear_custom_functions( ).

    mr_data->mr_s_top_custom_menu->text = 'Table'(001).

*... fill custom functions
    mr_data->mr_s_entity_function1->text = 'Navigate to Table Definition'(002).
    mr_data->mr_s_entity_function2->text = 'Where-Used-List for Table'(003).
  ENDMETHOD.


  METHOD update_description_texts.

*.. update table description
    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( mv_entity_id ).
    mr_data->mr_v_selmask_entity_text->* = ls_table_info-ddtext.

    DATA(lr_t_fields) = mr_data->mr_tabfield_list->get_fields_ref( ).

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
          DATA(ls_alt_text) = mr_altcoltext_f->find_alternative_text(
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
          DATA(lr_s_selfield) = REF #( mr_data->mr_t_table_data->*[
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


  METHOD update_join_definition.
    DATA(lt_join_tables_old) = mr_data->mr_s_old_join_def->tables.

    ASSIGN mr_data->mr_t_table_data->* TO FIELD-SYMBOL(<lt_selection_fields>).
    ASSIGN mr_data->mr_t_selfields_multi->* TO FIELD-SYMBOL(<lt_selection_fields_multi>).
    ASSIGN mr_data->mr_s_join_def->* TO FIELD-SYMBOL(<ls_join_definition>).
    ASSIGN mr_data->mr_s_global_data->* TO FIELD-SYMBOL(<ls_global_data>).


    mr_data->set_join_active( xsdbool( <ls_join_definition>-tables IS NOT INITIAL ) ).

    IF mr_data->is_join_active( ).
      CLEAR: mr_data->mr_s_global_data->edit.
    ENDIF.

    DATA(lt_old_tablist) = mr_data->mr_tabfield_list->get_table_list( ).
    SORT lt_old_tablist BY selection_order DESCENDING.
    DATA(lv_max_index) = VALUE #( lt_old_tablist[ 1 ]-selection_order OPTIONAL ).
    SORT lt_old_tablist BY selection_order ASCENDING.

    DATA(lt_old_tablist_selopt) = VALUE zdbbr_tabname_range_itab(
        FOR tab IN lt_old_tablist
        ( sign = 'I' option = 'EQ' low = tab-tabname ) ).

*.. add fields of join table to selfields output
    LOOP AT <ls_join_definition>-tables ASSIGNING FIELD-SYMBOL(<ls_join_table_info>).
      DATA(lf_is_new_table) = abap_false.

      ASSIGN lt_old_tablist[ tabname = <ls_join_table_info>-add_table ] TO FIELD-SYMBOL(<ls_existing_table>).
      IF sy-subrc <> 0.
        lf_is_new_table = abap_true.
        " check if fields for join table are already in selection fields table
        DATA(ls_new_table) = create_table_fields(
          iv_tablename         = <ls_join_table_info>-add_table
          if_conditional_table = <ls_join_table_info>-is_virtual
          if_selection_active  = abap_true
        ).

*...... add new join table to table list
        APPEND ls_new_table TO lt_old_tablist ASSIGNING <ls_existing_table>.
      ENDIF.

*.... check if join table is of type 'conditional join' to prevent selection
      IF <ls_join_table_info>-is_virtual = abap_true.
        <ls_existing_table>-no_selection_allowed = abap_true.
      ENDIF.

      IF <ls_existing_table>-selection_order IS INITIAL.
        ADD 1 TO lv_max_index.
        <ls_existing_table>-active_selection = abap_true.
        <ls_existing_table>-selection_order = lv_max_index.
      ENDIF.

*.... check if this table's join type changed to or from virtual join
      IF lf_is_new_table = abap_false.
        DATA(ls_old_join_table) = lt_join_tables_old[ add_table = <ls_existing_table>-tabname ].
        IF ls_old_join_table-is_virtual <> <ls_join_table_info>-is_virtual.

*........ update the is_post_join flag of all table fields for this table
          mr_data->mr_tabfield_list->update_virtual_join_for_table(
            iv_table_name  = <ls_existing_table>-tabname
            if_virtual_join = <ls_join_table_info>-is_virtual
          ).
        ENDIF.
      ENDIF.
    ENDLOOP.

*.. if no add tables are defined, or add tables changed since last update delete
*.. selection fields that are no longer valid
    DATA(lt_add_tables_selopt) = VALUE zdbbr_tabname_range_itab(
      ( sign   = zif_dbbr_global_consts=>gc_options-i
        option = zif_dbbr_global_consts=>gc_options-eq
        low    = <ls_global_data>-primary_table )
    ).
    lt_add_tables_selopt = VALUE #(
      BASE lt_add_tables_selopt
      FOR add_table IN <ls_join_definition>-tables
      ( sign   = zif_dbbr_global_consts=>gc_options-i
        option = zif_dbbr_global_consts=>gc_options-eq
        low    = add_table-add_table                    )
    ).

    DELETE <lt_selection_fields> WHERE tabname NOT IN lt_add_tables_selopt.
    DELETE <lt_selection_fields_multi> WHERE tabname NOT IN lt_add_tables_selopt.
    DELETE lt_old_tablist WHERE tabname NOT IN lt_add_tables_selopt.

*.. update tabfield list
    mr_data->mr_tabfield_list->delete_where_not_in_tablist( lt_add_tables_selopt ).

    mr_data->mr_tabfield_list->update_tables( ).

*.. are there any join tables?
    IF <ls_join_definition>-tables IS INITIAL.
      CLEAR mr_data->mr_t_table_to_alias_map->*.
      mr_data->mr_tabfield_list->clear_alias_names( ).
    ELSE.
      build_table_to_alias_map( ).
      mr_data->mr_tabfield_list->update_alias_names( )."mr_data->mr_t_table_to_alias_map->* ).
    ENDIF.

    IF mr_data->is_join_active( ).
      mr_data->mr_tabfield_list->sort_tables_by_active( ).
*.... manually update join table list of field list to prevent unwanted screen updates
      update_multi_selection_mask( ).
    ELSE.
      update_selection_mask( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = super->get_deactivated_functions( ).
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_ui_function.
    CASE cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>navigate_to_table_def.
        zcl_dbbr_dictionary_helper=>navigate_to_table( mr_data->mr_s_global_data->primary_table ).
        CLEAR cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>cross_reference_table.
        zcl_dbbr_dictionary_helper=>cross_reference_table( mr_data->mr_s_global_data->primary_table ).
        CLEAR cv_function.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
