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
        io_tabfields    TYPE REF TO zcl_dbbr_tabfield_list OPTIONAL
        it_fieldcat     TYPE lvc_t_fcat OPTIONAL.
  PROTECTED SECTION.
    METHODS save_search_help REDEFINITION.
    METHODS should_save REDEFINITION.
    METHODS test_f4 REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS: c_search_fields_container   TYPE dynfnam VALUE 'SEARCHFIELDS_CONT',
               c_assigned_fields_container TYPE dynfnam VALUE 'ASSIGNED_CONT',
               c_container_name            TYPE dynfnam VALUE 'CUST_CONT'.
    CONSTANTS: BEGIN OF c_field_names,
                 tabname           TYPE dynfnam VALUE 'TABNAME',
                 tabname_alias     TYPE dynfnam VALUE 'TABNAME_ALIAS',
                 fieldname         TYPE dynfnam VALUE 'FIELDNAME',
                 scrtext_m         TYPE dynfnam VALUE 'SCRTEXT_M',
                 sortorder         TYPE dynfnam VALUE 'SORTORDER',
                 value             TYPE dynfnam VALUE 'VALUE',
                 allow_restriction TYPE dynfnam VALUE 'ALLOW_RESTRICTION',
                 is_output_field   TYPE dynfnam VALUE 'IS_OUTPUT_FIELD',
                 sort_active       TYPE dynfnam VALUE 'SORT_ACTIVE',
                 is_text_field     TYPE dynfnam VALUE 'IS_TEXT_FIELD',
                 rollname          TYPE dynfnam VALUE 'ROLLNAME',
                 datatype          TYPE dynfnam VALUE 'DATATYPE',
                 inttype           TYPE dynfnam VALUE 'INTTYPE',
                 leng              TYPE dynfnam VALUE 'LENG',
               END OF c_field_names.

    TYPES:
      BEGIN OF ty_s_tabfield_f4,
        fieldname TYPE fieldname,
        is_key    TYPE keyflag,
        fieldtext TYPE ddtext,
      END OF ty_s_tabfield_f4.
    TYPES:
      BEGIN OF ty_s_cdsfield_f4,
        fieldname TYPE fieldname,
        is_key    TYPE keyflag,
        is_param  TYPE zdbbr_is_parameter,
        fieldtext TYPE ddtext,
      END OF ty_s_cdsfield_f4.
    TYPES: ty_t_cdsfield_f4 TYPE STANDARD TABLE OF ty_s_cdsfield_f4 WITH EMPTY KEY.
    DATA mr_ui_custom_search_help TYPE REF TO zdbbr_custom_searchhelp_ui.
    DATA ms_join_def TYPE zdbbr_join_def.
    DATA mt_search_fields TYPE zdbbr_f4_search_itab.
    DATA mo_search_fields_alv TYPE REF TO zcl_uitb_alv.
    DATA mo_assigned_fields_alv TYPE REF TO zcl_uitb_alv.
    DATA mo_alv_container TYPE REF TO cl_gui_container.

    DATA mt_assignments TYPE zdbbr_f4_assignment_itab.
    DATA mt_deleted_f4_assngmnt TYPE zdbbr_f4_assignment_itab.
    DATA mt_disabled_style TYPE lvc_t_styl.
    DATA: mo_splitter TYPE REF TO zcl_uitb_gui_splitter_cont.

    METHODS validate_custom_search_help.
    METHODS check_mandatory_fields.
    METHODS create_alv.
    METHODS create_f4_assgmnt_alv.
    METHODS fill_search_fields
      IMPORTING
        it_fieldcat  TYPE lvc_t_fcat
        io_tabfields TYPE REF TO zcl_dbbr_tabfield_list
        iv_f4_id     TYPE zdbbr_f4_id .
    METHODS get_current_search_fields
      RETURNING
        VALUE(result) TYPE zdbbr_f4_field_itab.
    "! <p class="shorttext synchronized" lang="en">Returns Fields/Parameters of CDS view</p>
    METHODS get_cds_view_fields
      IMPORTING
        iv_cds_view_name TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rt_fields) TYPE ty_t_cdsfield_f4.
    METHODS hide_tabname_alias_col
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          ev_column
          ev_row.
    METHODS on_search_field_alv_action
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          ev_function
          ev_tag.
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
    mt_disabled_style = VALUE #( ( fieldname = 'ENTITY_ID' style = zif_uitb_c_alv_cell_style=>disabled  )
                                 ( fieldname = 'FIELDNAME' style = zif_uitb_c_alv_cell_style=>disabled  ) ).

    DATA(lo_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).
    mr_ui_custom_search_help = CAST zdbbr_custom_searchhelp_ui( lo_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_custom_search_help ) ).

*.. fill search fields from current select
    fill_search_fields(
       it_fieldcat  = it_fieldcat
       io_tabfields = io_tabfields
       iv_f4_id     = iv_f4_id
    ).

  ENDMETHOD.


  METHOD create_alv.
    CHECK mo_search_fields_alv IS INITIAL.

    mo_search_fields_alv = zcl_uitb_alv=>create_alv(
        ir_container = mo_splitter->get_container( 1 )
        ir_data      = REF #( mt_search_fields )
        if_editable  = xsdbool( mv_display_mode <> zif_dbbr_global_consts=>gc_display_modes-view AND ms_f4_def-is_built_in = abap_false )
    ).

    DATA(lo_functions) = mo_search_fields_alv->get_functions( ).
    lo_functions->set_all( abap_false ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>local_delete_row ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>find ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>find_more ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>column_optimze ).

    IF mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-edit.
      lo_functions->add_function(
          iv_name    = 'ADDFIELD'
          iv_type    = zcl_uitb_alv_functions=>button
          iv_icon    = |{ icon_insert_row }|
          iv_tooltip = |{ 'Add fields from used tables'(001) }|
      ).
    ENDIF.

    DATA(lo_columns) = mo_search_fields_alv->get_columns( ).
    lo_columns->set_optimized( ).

    IF mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create.
      SET HANDLER:
        on_link_click FOR mo_search_fields_alv->get_events( ),
        on_search_field_alv_action FOR mo_search_fields_alv->get_events( ).
    ENDIF.

    DATA(lo_iterator) = lo_columns->zif_uitb_list~get_iterator( ).

    WHILE lo_iterator->has_next( ).
      DATA(lo_column) = CAST zcl_uitb_alv_column( lo_iterator->get_next( ) ).

      CASE lo_column->get_name( ).

        WHEN c_field_names-fieldname.
          IF mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create.
            lo_column->set_hotspot( ).
            lo_column->set_style( zif_uitb_c_alv_cell_style=>color_total ).
          ENDIF.

        WHEN c_field_names-sortorder.
          lo_column->set_technical( ).

        WHEN c_field_names-value.
          lo_column->set_technical( ).

        WHEN c_field_names-allow_restriction.
          lo_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
          lo_column->set_descriptions( iv_medium = 'Filterable' iv_long = 'Filterable' ).
          lo_column->set_editable( ).

        WHEN c_field_names-sort_active.
          lo_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
          lo_column->set_descriptions( iv_medium = 'Sorted' iv_long = 'Sorted' ).
          lo_column->set_editable( ).

        WHEN c_field_names-is_text_field.
          lo_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
          lo_column->set_descriptions( iv_medium = 'Text Field' iv_long = 'Text Field' ).
          lo_column->set_editable( ).

        WHEN c_field_names-is_output_field.
          lo_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
          lo_column->set_descriptions( iv_medium = 'Add. Output Field' iv_long = 'Additional Output Field' ).
          lo_column->set_editable( ).

        WHEN c_field_names-tabname_alias.

          IF hide_tabname_alias_col( ).
            lo_column->set_technical( ).
          ENDIF.

        WHEN c_field_names-rollname OR
             c_field_names-datatype OR
             c_field_names-inttype OR
             c_field_names-leng.
          lo_column->set_technical( ).

      ENDCASE.
    ENDWHILE.

    mo_search_fields_alv->display( ).
  ENDMETHOD.


  METHOD create_f4_assgmnt_alv.
    DATA: lo_col TYPE REF TO zcl_uitb_alv_column.

    mo_assigned_fields_alv = zcl_uitb_alv=>create_alv(
       ir_data      = REF #( mt_assignments )
       ir_container = mo_splitter->get_container( 2 )
       if_editable  = xsdbool( mv_display_mode <> zif_dbbr_global_consts=>gc_display_modes-view )
    ).


    " configure columns for alv
    DATA(lo_cols) = mo_assigned_fields_alv->get_columns( ).

    lo_cols->set_style_column( 'CELL_STYLE' ).

    lo_cols->get_column( 'REF_F4_ID' )->set_technical( ).

    lo_col = lo_cols->get_column( 'ENTITY_ID' ).
    lo_col->set_editable( ).
    lo_col->set_custom_f4( ).
    lo_col->set_f4( ).

    lo_cols->get_column( 'PERSISTED' )->set_technical( ).

    lo_col = lo_cols->get_column( 'PERFORM_ALPHA_CONVERSION').
    lo_col->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
    lo_col->set_descriptions(
        iv_medium  = 'Perf. Alpha Conv'
        iv_tooltip = 'Perform Alpha Conv. on Value transfer'
    ).
    lo_col->set_optimized( ).
    lo_col->set_editable( ).

    lo_col = lo_cols->get_column( 'FIELDNAME' ).

    lo_col->set_editable( ).

    lo_col->set_f4( ).
    lo_col->set_custom_f4( ).

    mo_assigned_fields_alv->get_display_settings( )->set_title( 'Assigned Table Fields' ).

    " configure functions for alv
    DATA(lo_functions) = mo_assigned_fields_alv->get_functions( ).

    lo_functions->set_all( abap_false ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>local_append_row ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>local_insert_row ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>local_delete_row ).

    SET HANDLER:
      on_data_changed FOR mo_assigned_fields_alv->get_events( ),
      on_alv_assgnmt_f4 FOR mo_assigned_fields_alv->get_events( ).

    lo_cols->set_optimized( ).
    mo_assigned_fields_alv->display( ).

  ENDMETHOD.


  METHOD fill_search_fields.
*& Description: Fills search fields from selection fields
*&---------------------------------------------------------------------*
    CLEAR: mr_ui_custom_search_help->*.

    IF it_fieldcat IS NOT INITIAL AND io_tabfields IS BOUND.
      LOOP AT it_fieldcat ASSIGNING FIELD-SYMBOL(<ls_field>).
*...... Retrieve field information from tabfield list
        TRY.
            DATA(lr_field) = io_tabfields->get_field_ref_by_alv_name( iv_alv_fieldname = <ls_field>-fieldname ).

            mt_search_fields = VALUE #(
              BASE mt_search_fields
              ( tabname       = lr_field->tabname
                tabname_alias = lr_field->tabname_alias
                fieldname     = <ls_field>-ref_field
                scrtext_m     = lr_field->field_ddtext
                sortorder     = sy-tabix
                datatype      = <ls_field>-datatype
                inttype       = <ls_field>-inttype
                rollname      = <ls_field>-rollname
                leng          = <ls_field>-intlen  )
            ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDLOOP.
    ELSEIF iv_f4_id IS NOT INITIAL.
*.... read existing search help from database
      zcl_dbbr_custom_f4_factory=>get_f4(
        EXPORTING iv_f4_id          = iv_f4_id
        IMPORTING es_f4_data        = ms_f4_def
                  et_f4_assignments = mt_assignments
      ).

      LOOP AT ms_f4_def-fields ASSIGNING FIELD-SYMBOL(<ls_f4_field>).
        mt_search_fields = VALUE #(
          BASE mt_search_fields
          ( tabname           = <ls_f4_field>-search_table
            tabname_alias     = <ls_f4_field>-search_table_alias
            fieldname         = <ls_f4_field>-search_field
            scrtext_m = zcl_dbbr_dictionary_helper=>get_table_field_info(
               iv_tablename = <ls_f4_field>-search_table
               iv_fieldname = <ls_f4_field>-search_field )-fieldtext
            sortorder = sy-tabix
            allow_restriction = <ls_f4_field>-allow_restriction
            is_text_field     = <ls_f4_field>-is_text_field
            sort_active       = <ls_f4_field>-sort_active
            is_output_field   = <ls_f4_field>-is_output_field
            datatype          = <ls_f4_field>-datatype
            inttype           = <ls_f4_field>-inttype
            rollname          = <ls_f4_field>-rollname
            leng              = <ls_f4_field>-leng )
        ).
      ENDLOOP.

      LOOP AT mt_assignments ASSIGNING FIELD-SYMBOL(<ls_assignment>).
        <ls_assignment>-persisted = abap_true.
        <ls_assignment>-cell_style = mt_disabled_style.
      ENDLOOP.

      mr_ui_custom_search_help->description = ms_f4_def-description.
      mr_ui_custom_search_help->apply_to_same_type = ms_f4_def-apply_to_same_type.
      mr_ui_custom_search_help->perform_alpha_conversion = ms_f4_def-perform_alpha_conversion.
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
          is_text_field     = <ls_search_field>-is_text_field
          is_output_field   = <ls_search_field>-is_output_field
          rollname          = <ls_search_field>-rollname
          datatype          = <ls_search_field>-datatype
          inttype           = <ls_search_field>-inttype
          leng              = <ls_search_field>-leng )
    ).
  ENDMETHOD.


  METHOD on_alv_assgnmt_f4.

    FIELD-SYMBOLS: <lt_data>   TYPE STANDARD TABLE,
                   <lt_values> TYPE ANY TABLE.

    er_event_data->m_event_handled = abap_true.

    " get the current row
    DATA(lo_row) = REF #( mt_assignments[ es_row_no-row_id ] OPTIONAL ).

    IF lo_row IS NOT BOUND.
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
      SELECT SINGLE *
        FROM zdbbr_i_databaseentitywotext
        WHERE entity = @lo_row->entity_id
      INTO @DATA(ls_entity).
      CHECK sy-subrc = 0.

      IF ls_entity-type = zif_dbbr_c_entity_type=>cds_view.
        DATA(lt_cds_fields) = get_cds_view_fields( ls_entity-entity ).
        ASSIGN lt_cds_fields TO <lt_values>.
      ELSE.
        zcl_dbbr_dictionary_helper=>get_table_field_infos(
          EXPORTING iv_tablename    = lo_row->entity_id
          IMPORTING et_table_fields = DATA(lt_table_fields)
        ).
        ASSIGN lt_table_fields TO <lt_values>.
      ENDIF.

      DATA(lv_chosen_field) = zcl_dbbr_f4_helper=>call_int_table_f4(
          it_table_search      = <lt_values>
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

  METHOD get_cds_view_fields.
    TRY.
        DATA(lo_cds_view) = zcl_dbbr_cds_view_factory=>read_cds_view( iv_cds_view_name ).
        LOOP AT lo_cds_view->get_parameters( ) ASSIGNING FIELD-SYMBOL(<ls_param>).
          rt_fields = VALUE #( BASE rt_fields
            ( fieldname = <ls_param>-parametername_raw
              is_param  = abap_true )
          ).
        ENDLOOP.

        rt_fields = VALUE #(
          BASE rt_fields
          ( LINES OF VALUE #(
              FOR field IN lo_cds_view->get_columns( )
              ( fieldname = field-fieldname
                fieldtext = COND #( WHEN field-fieldlabel IS NOT INITIAL THEN field-fieldlabel ELSE field-ddtext )
                is_key    = field-keyflag ) )
          )
        ).
      CATCH zcx_dbbr_data_read_error.
        "handle exception
    ENDTRY.
  ENDMETHOD.


  METHOD on_data_changed.
    CHECK: ef_onf4 = abap_false,
           ef_onf4_after = abap_false,
           ef_onf4_before = abap_false.

    LOOP AT er_change_protocol->mt_deleted_rows ASSIGNING FIELD-SYMBOL(<ls_deleted>).
      DATA(lo_deleted_row) = REF #( mt_assignments[ <ls_deleted>-row_id ] OPTIONAL ).
      IF lo_deleted_row IS BOUND AND lo_deleted_row->persisted = abap_true.
        mt_deleted_f4_assngmnt = VALUE #( BASE mt_deleted_f4_assngmnt ( lo_deleted_row->* ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_search_field_alv_action.

    CASE ev_function.
      WHEN 'ADDFIELD'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.


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
        <ls_existing_search_field>-is_output_field = <ls_field>-is_output_field.
        <ls_existing_search_field>-sort_active = <ls_field>-sort_active.
        <ls_existing_search_field>-allow_restriction = <ls_field>-allow_restriction.
      ENDLOOP.

    ENDIF.

    ms_f4_def-apply_to_same_type = mr_ui_custom_search_help->apply_to_same_type.
    ms_f4_def-perform_alpha_conversion = mr_ui_custom_search_help->perform_alpha_conversion.

    DATA(lv_f4_id) = zcl_dbbr_custom_f4_factory=>save_custom( is_f4_data = ms_f4_def ).

    " --- save the assignments
    zcl_dbbr_custom_f4_factory=>update_f4_assignments(
        it_f4_assignments    = VALUE #( FOR <ls_new_assignment> IN mt_assignments
                                        ( ref_f4_id                =  lv_f4_id
                                          entity_id                = to_upper( <ls_new_assignment>-entity_id )
                                          fieldname                = to_upper( <ls_new_assignment>-fieldname )
                                          perform_alpha_conversion = <ls_new_assignment>-perform_alpha_conversion ) )
        it_f4_assgnmt_delete = mt_deleted_f4_assngmnt
    ).

    mf_saved = abap_true.
  ENDMETHOD.


  METHOD should_save.
*& Description: Determination if the save action should be performed
*&---------------------------------------------------------------------*
    mf_delete_existing = abap_false.
    rf_save = abap_true.

    CHECK: ms_f4_def-f4_id IS INITIAL,
           mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create.

    " --- check if there is already a search help with the given search field/search table
    IF zcl_dbbr_custom_f4_factory=>exists_f4_for_search_field( iv_search_tab   = mr_ui_custom_search_help->search_table
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

    IF NOT ( mo_search_fields_alv->get_data_changes( )->check_changed( ) AND
             mo_assigned_fields_alv->get_data_changes( )->check_changed( ) ).
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
    IF mo_alv_container IS BOUND.
      mo_alv_container->free( ).
    ENDIF.

    FREE: mo_alv_container.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    DATA: ls_screen TYPE screen.

    super->zif_uitb_screen_controller~pbo( ).

    CASE mv_display_mode.
      WHEN zif_dbbr_global_consts=>gc_display_modes-view.

        LOOP AT SCREEN INTO ls_screen.
          IF ls_screen-group1 = 'INP'.
            ls_screen-input = 0.
            MODIFY SCREEN FROM ls_screen.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    CHECK mo_alv_container IS INITIAL.

    mo_alv_container = NEW cl_gui_custom_container( container_name = c_container_name ).
    mo_splitter = NEW zcl_uitb_gui_splitter_cont(
      iv_elements = 2
      iv_size     = COND #( WHEN mv_display_mode = zif_dbbr_global_consts=>gc_display_modes-create THEN '70:30' ELSE '50:50' )
      io_parent   = mo_alv_container
    ).
    create_alv( ).
    create_f4_assgmnt_alv( ).
  ENDMETHOD.

  METHOD hide_tabname_alias_col.
    DATA: lt_unique_tabname TYPE SORTED TABLE OF tabname WITH UNIQUE KEY table_line.

    result = abap_true.
    CHECK ms_join_def IS NOT INITIAL AND ms_join_def-tables IS NOT INITIAL.

    lt_unique_tabname = VALUE #( ( ms_join_def-primary_table ) ).

    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>).
      INSERT <ls_join_table>-add_table INTO TABLE lt_unique_tabname.
      IF sy-subrc <> 0.
        CLEAR result.
        RETURN.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

ENDCLASS.
