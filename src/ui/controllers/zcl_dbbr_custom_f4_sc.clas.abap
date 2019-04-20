CLASS zcl_dbbr_custom_f4_sc DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_generic_f4_sc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_uitb_screen_controller~pbo REDEFINITION.
    METHODS zif_uitb_screen_controller~call_screen REDEFINITION.
    METHODS zif_uitb_screen_controller~free_screen_resources REDEFINITION.

    METHODS constructor
      IMPORTING
        iv_f4_id        TYPE zdbbr_f4_id OPTIONAL
        is_join_def     TYPE zdbbr_join_def OPTIONAL
        iv_display_mode TYPE zdbbr_display_mode DEFAULT zif_dbbr_global_consts=>gc_display_modes-create
        it_fieldcat     TYPE lvc_t_fcat OPTIONAL.
  PROTECTED SECTION.
    METHODS save_search_help REDEFINITION.
    METHODS should_save REDEFINITION.
    METHODS test_f4 REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS: c_search_fields_container   TYPE dynfnam VALUE 'SEARCHFIELDS_CONT',
               c_assigned_fields_container TYPE dynfnam VALUE 'ASSIGNED_CONT'.
    CONSTANTS: BEGIN OF c_field_names,
                 tabname           TYPE dynfnam VALUE 'TABNAME',
                 fieldname         TYPE dynfnam VALUE 'FIELDNAME',
                 scrtext_m         TYPE dynfnam VALUE 'SCRTEXT_M',
                 sortorder         TYPE dynfnam VALUE 'SORTORDER',
                 value             TYPE dynfnam VALUE 'VALUE',
                 allow_restriction TYPE dynfnam VALUE 'ALLOW_RESTRICTION',
                 sort_active       TYPE dynfnam VALUE 'SORT_ACTIVE',
                 is_text_field     TYPE dynfnam VALUE 'IS_TEXT_FIELD',
               END OF c_field_names.

    DATA mt_disabled_cols TYPE lvc_t_styl.


    DATA mr_ui_custom_search_help TYPE REF TO zdbbr_custom_searchhelp_ui.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mt_search_fields TYPE zdbbr_f4_search_itab.
    DATA mr_search_fields_alv TYPE REF TO zcl_uitb_alv.
    DATA mr_assigned_fields_alv TYPE REF TO zcl_uitb_alv.
    DATA mr_search_fields_cont TYPE REF TO cl_gui_custom_container.

    DATA mr_assigned_fields_cont TYPE REF TO cl_gui_custom_container.
    DATA mt_assignments TYPE zdbbr_f4_assignment_itab.
    DATA mt_deleted_f4 TYPE STANDARD TABLE OF zdbbr_f4_id.
    DATA mt_deleted_f4_assngmnt TYPE zdbbr_f4_assignment_itab.
    DATA mt_disabled_style TYPE lvc_t_styl.

    METHODS validate_custom_search_help.
    METHODS check_mandatory_fields.
    METHODS create_alv.
    METHODS create_f4_assgmnt_alv.
    METHODS fill_search_fields
      IMPORTING
        it_fieldcat TYPE lvc_t_fcat
        iv_f4_id    TYPE zdbbr_f4_id .
    METHODS get_current_search_fields
      RETURNING
        VALUE(result) TYPE zdbbr_f4_field_itab.
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          ev_column
          ev_row.
    METHODS on_data_changed
          FOR EVENT data_changed OF zcl_uitb_alv_events
      IMPORTING
          ef_onf4
          ef_onf4_after
          ef_onf4_before
          er_change_protocol
          ev_function.
    METHODS on_alv_assgnmt_f4
          FOR EVENT f4 OF zcl_uitb_alv_events
      IMPORTING
          ef_display
          er_event_data
          es_row_no
          et_bad_cells
          ev_fieldname
          ev_fieldvalue.
ENDCLASS.



CLASS zcl_dbbr_custom_f4_sc IMPLEMENTATION.


  METHOD check_mandatory_fields.

    DATA(lv_empty_mandatory_field) = COND string(
        WHEN mr_ui_custom_search_help->description IS INITIAL THEN 'DESCRIPTION'
    ).

    IF lv_empty_mandatory_field IS NOT INITIAL.
      DATA(lv_full_fieldname) = zif_dbbr_main_report_var_ids=>c_s_custom_search_help && '-' && lv_empty_mandatory_field.
      zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_field( CONV #( lv_full_fieldname ) ).
      zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 0 ) .
      zcl_uitb_cursor=>refresh_cursor( ).

      RAISE EXCEPTION TYPE zcx_dbbr_validation_exception
        EXPORTING
          textid = zcx_dbbr_validation_exception=>mandatory_fields_empty.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( iv_display_mode = iv_display_mode ).
    ms_join_def = is_join_def.
    mt_disabled_style = VALUE #( ( style = zif_uitb_c_alv_cell_style=>disabled  ) ).

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).
    mr_ui_custom_search_help = CAST zdbbr_custom_searchhelp_ui( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_custom_search_help ) ).

    " fill search fields from current select
    fill_search_fields(
       it_fieldcat = it_fieldcat
       iv_f4_id    = iv_f4_id
    ).

  ENDMETHOD.


  METHOD create_alv.
    CHECK mr_search_fields_alv IS INITIAL.

    mr_search_fields_cont = NEW #( container_name = c_search_fields_container ).
    mr_search_fields_alv = zcl_uitb_alv=>create_alv(
        ir_container = mr_search_fields_cont
        ir_data      = REF #( mt_search_fields )
        if_editable  = xsdbool( mv_display_mode <> zif_dbbr_global_consts=>gc_display_modes-view AND ms_f4_def-is_built_in = abap_false )
    ).

    DATA(lr_functions) = mr_search_fields_alv->get_functions( ).
    lr_functions->set_all( abap_false ).
    lr_functions->set_function( iv_name = zif_uitb_c_alv_functions=>local_delete_row ).
    lr_functions->set_function( iv_name = zif_uitb_c_alv_functions=>find ).
    lr_functions->set_function( iv_name = zif_uitb_c_alv_functions=>find_more ).
    mr_search_fields_alv->get_display_settings( )->set_row_insertions( abap_false ).

    DATA(lr_columns) = mr_search_fields_alv->get_columns( ).
    lr_columns->set_optimized( ).

    IF mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create.
      SET HANDLER:
        on_link_click FOR mr_search_fields_alv->get_events( ).
    ENDIF.

    DATA(lr_iterator) = lr_columns->zif_uitb_list~get_iterator( ).

    WHILE lr_iterator->has_next( ).
      DATA(lr_column) = CAST zcl_uitb_alv_column( lr_iterator->get_next( ) ).

      CASE lr_column->get_name( ).

        WHEN c_field_names-fieldname.
          IF mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create.
            lr_column->set_hotspot( ).
            lr_column->set_style( zif_uitb_c_alv_cell_style=>color_total ).
          ENDIF.

        WHEN c_field_names-sortorder.
          lr_column->set_technical( ).

        WHEN c_field_names-value.
          lr_column->set_technical( ).

        WHEN c_field_names-allow_restriction.
          lr_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
          lr_column->set_descriptions( iv_medium = 'Filterable' iv_long = 'Filterable' ).
          lr_column->set_editable( ).

        WHEN c_field_names-sort_active.
          lr_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
          lr_column->set_descriptions( iv_medium = 'Sorted' iv_long = 'Sorted' ).
          lr_column->set_editable( ).

        WHEN c_field_names-is_text_field.
          lr_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
          lr_column->set_descriptions( iv_medium = 'Text Field' iv_long = 'Text Field' ).
          lr_column->set_editable( ).
      ENDCASE.
    ENDWHILE.

    mr_search_fields_alv->display( ).
  ENDMETHOD.


  METHOD create_f4_assgmnt_alv.
    DATA: lr_column TYPE REF TO zcl_uitb_alv_column.

    CHECK mr_assigned_fields_cont IS INITIAL.

    mr_assigned_fields_cont = NEW #( container_name = c_assigned_fields_container ).

    mr_assigned_fields_alv = zcl_uitb_alv=>create_alv(
       ir_data      = REF #( mt_assignments )
       ir_container = mr_assigned_fields_cont
       if_editable  = xsdbool( mv_display_mode <> zif_dbbr_global_consts=>gc_display_modes-view )
    ).

    " configure columns for alv
    DATA(lr_columns) = mr_assigned_fields_alv->get_columns( ).

    lr_columns->set_style_column( 'CELL_STYLE' ).

    lr_columns->get_column( 'REF_F4_ID' )->set_technical( ).

    lr_column = lr_columns->get_column( 'ENTITY_ID' ).
    lr_column->set_editable( ).
    lr_column->set_custom_f4( ).
    lr_column->set_f4( ).

    lr_columns->get_column( 'PERSISTED' )->set_technical( ).

    lr_column = lr_columns->get_column( 'FIELDNAME' ).

    lr_column->set_editable( ).

    lr_column->set_f4( ).
    lr_column->set_custom_f4( ).

    mr_assigned_fields_alv->get_display_settings( )->set_title( 'Assigned Table Fields' ).

    " configure functions for alv
    DATA(lr_functions) = mr_assigned_fields_alv->get_functions( ).

    lr_functions->set_all( abap_false ).
    lr_functions->set_function( zif_uitb_c_alv_functions=>local_append_row ).
    lr_functions->set_function( zif_uitb_c_alv_functions=>local_insert_row ).
    lr_functions->set_function( zif_uitb_c_alv_functions=>local_delete_row ).

    SET HANDLER:
      on_data_changed FOR mr_assigned_fields_alv->get_events( ),
      on_alv_assgnmt_f4 FOR mr_assigned_fields_alv->get_events( ).

    mr_assigned_fields_alv->display( ).

  ENDMETHOD.


  METHOD fill_search_fields.
*&---------------------------------------------------------------------*
*& Description: Fills search fields from selection fields
*&---------------------------------------------------------------------*
    DATA: lt_search_fld_prio TYPE zdbbr_f4_search_itab.
    FIELD-SYMBOLS: <ls_field_line> TYPE any.

    CLEAR: mr_ui_custom_search_help->*.

    IF it_fieldcat IS NOT INITIAL.
      LOOP AT it_fieldcat ASSIGNING FIELD-SYMBOL(<ls_field>).
        mt_search_fields = VALUE #(
          BASE mt_search_fields
          ( tabname   = <ls_field>-ref_table
            fieldname = <ls_field>-ref_field
            scrtext_m = zcl_dbbr_dictionary_helper=>get_table_field_info(
               iv_tablename = <ls_field>-ref_table
               iv_fieldname = <ls_field>-ref_field )-fieldtext
            sortorder = sy-tabix )
        ).
      ENDLOOP.
    ELSEIF iv_f4_id IS NOT INITIAL.
      " read existing search help from database
      mr_custom_f4_factory->get_f4(
        EXPORTING iv_f4_id          = iv_f4_id
        IMPORTING es_f4_data        = ms_f4_def
                  et_f4_assignments = mt_assignments
      ).

      LOOP AT ms_f4_def-fields ASSIGNING FIELD-SYMBOL(<ls_f4_field>).
        mt_search_fields = VALUE #(
          BASE mt_search_fields
          ( tabname   = <ls_f4_field>-search_table
            fieldname = <ls_f4_field>-search_field
            scrtext_m = zcl_dbbr_dictionary_helper=>get_table_field_info(
               iv_tablename = <ls_f4_field>-search_table
               iv_fieldname = <ls_f4_field>-search_field )-fieldtext
            sortorder = sy-tabix
            allow_restriction = <ls_f4_field>-allow_restriction
            is_text_field     = <ls_f4_field>-is_text_field
            sort_active       = <ls_f4_field>-sort_active )
        ).
      ENDLOOP.

      LOOP AT mt_assignments ASSIGNING FIELD-SYMBOL(<ls_assignment>).
        <ls_assignment>-persisted = abap_true.
        <ls_assignment>-cell_style = mt_disabled_style.
      ENDLOOP.

      mr_ui_custom_search_help->description = ms_f4_def-description.
      mr_ui_custom_search_help->search_field = ms_f4_def-fields[ is_search_key = abap_true ]-search_field.
      mr_ui_custom_search_help->search_table = ms_f4_def-fields[ is_search_key = abap_true ]-search_table.
    ENDIF.

  ENDMETHOD.


  METHOD get_current_search_fields.
    result = VALUE #(
        BASE result
        FOR <ls_search_field> IN mt_search_fields
        INDEX INTO lv_tabix
        ( search_table      = <ls_search_field>-tabname
          search_field      = <ls_search_field>-fieldname
          counter           = lv_tabix
          is_search_key     = xsdbool( <ls_search_field>-fieldname = mr_ui_custom_search_help->search_field )
          allow_restriction = <ls_search_field>-allow_restriction
          sort_active       = <ls_search_field>-sort_active
          is_text_field     = <ls_search_field>-is_text_field )
    ).
  ENDMETHOD.


  METHOD on_alv_assgnmt_f4.
    TYPES: BEGIN OF lty_value_tab,
             fieldname TYPE fieldname,
             key       TYPE keyflag,
             fieldtext TYPE ddtext,
           END OF lty_value_tab.

    DATA: lt_values TYPE TABLE OF lty_value_tab.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    er_event_data->m_event_handled = abap_true.

    " get the current row
    DATA(lr_row) = REF #( mt_assignments[ es_row_no-row_id ] OPTIONAL ).

    IF lr_row IS NOT BOUND.
      RETURN.
    ENDIF.

    IF ev_fieldname = 'ENTITY_ID'.
*... Call other object name chooser to get the table name
      DATA(lo_object_chooser) = NEW zcl_dbbr_choose_object_ctrl(
        if_global_fav_mode = abap_true
      ).
      lo_object_chooser->zif_uitb_screen_controller~call_screen( ).
      IF lo_object_chooser->was_not_cancelled( ).
        lo_object_chooser->get_chosen_entry(
          IMPORTING ev_entry = DATA(lv_chosen_entity)
                    ev_type  = DATA(lv_chosen_entity_type)
        ).
        ASSIGN er_event_data->m_data->* TO <lt_data>.
        IF sy-subrc = 0.
          APPEND VALUE lvc_s_modi(
              row_id    = es_row_no-row_id
              fieldname = 'ENTITY_ID'
              value     = lv_chosen_entity
          ) TO <lt_data>.
        ENDIF.
      ENDIF.
    ELSE.
      zcl_dbbr_dictionary_helper=>get_table_field_infos(
        EXPORTING iv_tablename    = lr_row->entity_id
        IMPORTING et_table_fields = DATA(lt_table_fields)
      ).

      lt_values = VALUE #( FOR dfies IN lt_table_fields
                           WHERE ( fieldname <> '.NODE1' AND
                                   rollname  <> 'MANDT' )
                           ( fieldname = dfies-fieldname
                             key       = dfies-keyflag
                             fieldtext = dfies-fieldtext ) ).

      DATA(lv_chosen_field) = zcl_dbbr_f4_helper=>call_int_table_f4(
          it_table_search      = lt_values
          iv_f4_window_title   = 'Table Fields'
          iv_return_field_name = 'FIELDNAME'
      ).

      IF lv_chosen_field IS NOT INITIAL.
        ASSIGN er_event_data->m_data->* TO <lt_data>.
        IF sy-subrc = 0.
          APPEND VALUE lvc_s_modi(
              row_id    = es_row_no-row_id
              fieldname = 'FIELDNAME'
              value     = lv_chosen_field
          ) TO <lt_data>.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD on_data_changed.
    CHECK: ef_onf4 = abap_false,
           ef_onf4_after = abap_false,
           ef_onf4_before = abap_false.

    LOOP AT er_change_protocol->mt_deleted_rows ASSIGNING FIELD-SYMBOL(<ls_deleted>).
      DATA(lr_deleted_row) = REF #( mt_assignments[ <ls_deleted>-row_id ] OPTIONAL ).
      IF lr_deleted_row IS BOUND AND lr_deleted_row->persisted = abap_true.
        mt_deleted_f4_assngmnt = VALUE #( BASE mt_deleted_f4_assngmnt ( lr_deleted_row->* ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD on_link_click.
    DATA(lr_clicked_element) = REF #( mt_search_fields[ ev_row ] ).
    ASSIGN ('lr_clicked_element->fieldname') TO FIELD-SYMBOL(<lv_field>).

    mr_ui_custom_search_help->search_field = <lv_field>.
    mr_ui_custom_search_help->search_table = lr_clicked_element->tabname.
    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
  ENDMETHOD.


  METHOD save_search_help.
    validate_custom_search_help( ).

    IF NOT should_save( ).
      RETURN.
    ENDIF.

    IF mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create.
      " fill structures for search help
      ms_f4_def-created_by = sy-uname.
      ms_f4_def-description = mr_ui_custom_search_help->description.
      " save join definition
      ms_f4_def-join_def = ms_join_def.

      ms_f4_def-fields = get_current_search_fields( ).

    ELSE.
      " update the search fields
      LOOP AT mt_search_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
        ASSIGN ms_f4_def-fields[ search_field = <ls_field>-fieldname
                                 search_table = <ls_field>-tabname ] TO FIELD-SYMBOL(<ls_existing_search_field>).

        <ls_existing_search_field>-is_text_field = <ls_field>-is_text_field.
        <ls_existing_search_field>-sort_active = <ls_field>-sort_active.
        <ls_existing_search_field>-allow_restriction = <ls_field>-allow_restriction.
      ENDLOOP.
    ENDIF.

    DATA(lv_f4_id) = mr_custom_f4_factory->save_custom( is_f4_data = ms_f4_def ).

    " --- save the assignments
    mr_custom_f4_factory->update_f4_assignments(
        it_f4_assignments    = VALUE #( FOR <ls_new_assignment> IN mt_assignments
                                        WHERE ( ref_f4_id IS INITIAL )
                                        ( ref_f4_id = lv_f4_id
                                          entity_id = to_upper( <ls_new_assignment>-entity_id )
                                          fieldname = to_upper( <ls_new_assignment>-fieldname ) ) )
        it_f4_assgnmt_delete = mt_deleted_f4_assngmnt
    ).
  ENDMETHOD.


  METHOD should_save.
*& Description: Determination if the save action should be performed
*&---------------------------------------------------------------------*
    DATA: lv_answer         TYPE char1,
          lv_built_in_f4_id TYPE zdbbr_f4_id,
          lv_tablename      TYPE tabname,
          lv_fieldname      TYPE fieldname.

    mf_delete_existing = abap_false.
    rf_save = abap_true.

    CHECK: ms_f4_def-f4_id IS INITIAL,
           mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create.

    " --- check if there is already a search help with the given search field/search table
    IF mr_custom_f4_factory->exists_f4_for_search_field( iv_search_tab   = CONV #( mr_ui_custom_search_help->search_table )
                                                         iv_search_field = mr_ui_custom_search_help->search_field ).
      MESSAGE i046(zdbbr_exception) DISPLAY LIKE 'E'.
      CLEAR rf_save.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD test_f4.

    validate_custom_search_help( ).

    DATA(lt_search_fields) = get_current_search_fields( ).

    DATA(ls_selfield) = VALUE zdbbr_selfield(
        LET search_field = lt_search_fields[ is_search_key = abap_true ] IN
        tabname = search_field-search_table
        fieldname = search_field-search_field
    ).

    zcl_dbbr_custom_f4_helper=>call_custom_f4(
      EXPORTING
        it_f4_definition   = VALUE #(
          ( fields      = lt_search_fields
            join_def    = ms_join_def
            is_built_in = ms_f4_def-is_built_in )
        )
        if_low             = abap_true
      CHANGING
        cs_selfield        = ls_selfield
    ).
  ENDMETHOD.


  METHOD validate_custom_search_help.
    check_mandatory_fields( ).

    IF NOT ( mr_search_fields_alv->get_data_changes( )->check_changed( ) AND
             mr_assigned_fields_alv->get_data_changes( )->check_changed( ) ).
      zcx_dbbr_validation_exception=>raise_with_text( iv_text = 'Error in Search Fields / Field Assignments' ).
    ENDIF.

    IF mr_ui_custom_search_help->search_field IS INITIAL.
      MESSAGE e045(zdbbr_exception).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = zif_dbbr_screen_ids=>c_show_custom_f4_help    " Number of the following screen
        iv_report_id    = zif_dbbr_c_report_id=>main
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_f4_screen_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.
    IF mr_search_fields_cont IS BOUND.
      mr_search_fields_cont->free( ).
    ENDIF.

    FREE: mr_search_fields_alv,
          mr_search_fields_cont.

    IF mr_assigned_fields_cont IS BOUND.
      mr_assigned_fields_cont->free( ).
    ENDIF.

    FREE: mr_assigned_fields_alv,
          mr_assigned_fields_cont.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    DATA: ls_screen TYPE screen.

    super->zif_uitb_screen_controller~pbo( ).

    CASE mv_display_mode.
      WHEN zif_dbbr_global_consts=>gc_display_modes-edit OR
           zif_dbbr_global_consts=>gc_display_modes-view.

        LOOP AT SCREEN INTO ls_screen.
          IF ls_screen-name = |{ zif_dbbr_main_report_var_ids=>c_s_custom_search_help }-DESCRIPTION|.
            ls_screen-input = 0.
            MODIFY SCREEN FROM ls_screen.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    create_alv( ).
    create_f4_assgmnt_alv( ).
  ENDMETHOD.
ENDCLASS.
