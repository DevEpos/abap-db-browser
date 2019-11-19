*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_usage_alv IMPLEMENTATION.

  METHOD constructor.
    mo_dependency_tree = io_dependency_tree.
    mt_usages = CORRESPONDING #( ZCL_SAT_CDS_DEP_ANALYZER=>get_used_entities(
                                  iv_cds_view_name = iv_cds_view_name
                                )-dependencies ).
*.. Enrich usages with icons
    LOOP AT mt_usages ASSIGNING FIELD-SYMBOL(<ls_usage>).
      <ls_usage>-type_icon = SWITCH #( <ls_usage>-object_type
         WHEN ZIF_SAT_C_ENTITY_TYPE=>table    THEN zif_dbbr_c_icon=>database_table
         WHEN ZIF_SAT_C_ENTITY_TYPE=>view     THEN zif_dbbr_c_icon=>database_view
         WHEN ZIF_SAT_C_ENTITY_TYPE=>cds_view THEN zif_dbbr_c_icon=>cds_view
      ).
    ENDLOOP.
    create_and_fill_usage_alv( io_parent ).
  ENDMETHOD.

  METHOD on_user_command.
    DATA: lo_command TYPE REF TO zif_uitb_gui_command.

    IF ev_function = c_functions-toggle_tree.
      lo_command = NEW zcl_uitb_gui_simple_command( iv_function = ev_function ).
    ELSE.
      DATA(lt_selections) = mo_alv->get_selections( )->get_selected_rows( ).
      DATA(lr_s_selected_entity) = REF #( mt_usages[ lt_selections[ 1 ] ] ).
      lo_command = NEW zcl_uitb_gui_simple_command(
        iv_function = ev_function
        ir_params   = NEW lty_s_command_info(
          entity_id   = lr_s_selected_entity->name
          entity_type = lr_s_selected_entity->object_type
          is_cds      = xsdbool( lr_s_selected_entity->object_type = ZIF_SAT_C_ENTITY_TYPE=>cds_view )
        )
      ).
    ENDIF.

    IF lo_command IS BOUND.
      mo_dependency_tree->execute_command( lo_command ).
    ENDIF.
  ENDMETHOD.

  METHOD on_double_click.
    DATA(lr_row) = REF #( mt_usages[ ev_row ] ).
    mo_dependency_tree->execute_command(
      NEW zcl_uitb_gui_simple_command(
        iv_function = c_functions-open_with_adt
        ir_params   = NEW lty_s_command_info(
          entity_id   = lr_row->name
          entity_type = lr_row->object_type
        )
      )
    ).
  ENDMETHOD.

  METHOD on_context_menu.

    er_menu->add_function(
       fcode = c_functions-open_with_adt
       text  = |{ 'Open with ADT'(042) }|
    ).

    er_menu->add_function(
        fcode = c_functions-open_in_new_window
        text  = |{ 'Open in new Window'(041) }|
    ).
    er_menu->add_separator( ).
    er_menu->add_function(
        fcode = c_functions-exec_with_dbbrs
        text  = |{ 'Show Content'(040) }|
    ).
    er_menu->add_function(
        fcode = c_functions-exec_with_dbbrs_new_window
        text  = |{ 'Show Content (New Task)'(039) }|
    ).
    DATA(lt_selected_rows) = mo_alv->get_selections( )->get_selected_rows( ).
    IF mt_usages[ lt_selected_rows[ 1 ] ]-object_type = ZIF_SAT_C_ENTITY_TYPE=>cds_view.
      er_menu->add_separator( ).
      er_menu->add_function(
          fcode = c_functions-show_ddl_source
          text  = |{ 'Show DDL Source Code'(043) }|
      ).
    ENDIF.
  ENDMETHOD.


  METHOD create_and_fill_usage_alv.

    mo_alv = zcl_uitb_alv=>create_alv(
       ir_data      = REF #( mt_usages )
       ir_container = io_container
    ).

    DATA(lo_cols) = mo_alv->get_columns( ).
    lo_cols->set_column_position( iv_columnname = 'TYPE_ICON' iv_position = 1 ).
    lo_cols->set_column_position( iv_columnname = 'RAW_NAME' iv_position = 2 ).
    lo_cols->set_column_position( iv_columnname = 'DESCRIPTION' iv_position = 3 ).
    lo_cols->set_column_position( iv_columnname = 'OCCURRENCE' iv_position = 4 ).
    lo_cols->set_column_position( iv_columnname = 'USED_ENTITIES_COUNT' iv_position = 5 ).
    lo_cols->set_column_position( iv_columnname = 'USED_JOIN_COUNT' iv_position = 6 ).
    lo_cols->set_column_position( iv_columnname = 'USED_UNION_COUNT' iv_position = 7 ).

    DATA(lo_col_iterator) = lo_cols->zif_uitb_list~get_iterator( ).
    WHILE lo_col_iterator->has_next( ).
      DATA(lo_col) = CAST zcl_uitb_alv_column( lo_col_iterator->get_next( ) ).

      CASE lo_col->get_name( ).

        WHEN 'TYPE_ICON'.
          lo_col->set_optimized( ).
          lo_col->set_icon( ).
          lo_col->set_descriptions( iv_long = 'Type' ).

        WHEN 'PACKAGE'.
          lo_col->set_descriptions( iv_long = 'Package' ).
          lo_col->set_optimized( ).

        WHEN 'RAW_NAME'.
          lo_col->set_optimized( ).
          lo_col->set_descriptions( iv_long = 'Data Source' ).

        WHEN 'DESCRIPTION'.
          lo_col->set_optimized( ).

        WHEN 'OCCURRENCE'.
          lo_col->set_descriptions( iv_long = 'Occurrence' iv_tooltip = 'Times the entity occurrs in the depend.' ).
          lo_col->set_optimized( ).

        WHEN 'USED_ENTITIES_COUNT'.
          lo_col->set_descriptions( iv_long = 'Used Tables/Views' iv_tooltip = 'Number of used Tables/Views' ).
          lo_col->set_optimized( ).

        WHEN 'USED_JOIN_COUNT'.
          lo_col->set_descriptions( iv_long = 'Used Joins' iv_tooltip = 'Number of Joins in Entity' ).
          lo_col->set_optimized( ).

        WHEN 'USED_UNION_COUNT'.
          lo_col->set_descriptions( iv_long = 'Used Unions' iv_tooltip = 'Number of Unions in Entity' ).
          lo_col->set_optimized( ).

        WHEN OTHERS.
          lo_col->set_technical( ).

      ENDCASE.
    ENDWHILE.

    lo_cols->set_optimized( ).

*.. Control toolbar
    DATA(lo_functions) = mo_alv->get_functions( ).
    lo_functions->set_all( abap_false ).
    lo_functions->set_default( abap_true ).
    lo_functions->set_function( iv_name = zif_uitb_c_alv_functions=>layout_change if_enable = abap_false ).

    lo_functions->add_function(
        iv_name    = c_functions-show_ddl_source
        iv_icon    = |{ icon_abap }|
        iv_text    = |{ TEXT-045 }|
        iv_tooltip = |{ TEXT-044 }|
    ).
    lo_functions->add_function( iv_type = zcl_uitb_alv_functions=>separator ).
    lo_functions->add_function(
        iv_name    = c_functions-exec_with_dbbrs
        iv_icon    = |{ icon_table_settings }|
        iv_text    = |{ TEXT-047 }|
        iv_tooltip = |{ TEXT-046 }|
    ).
    lo_functions->add_function(
        iv_name    = c_functions-exec_with_dbbrs_new_window
        iv_icon    = |{ icon_table_settings }|
        iv_text    = |{ TEXT-049 }|
        iv_tooltip = |{ TEXT-048 }|
    ).

    lo_functions->add_function(
        iv_name    = c_functions-toggle_tree
        iv_icon    = |{ icon_active_inactive }|
        iv_text    = |{ 'Show dependency tree' }|
    ).

*.. Register handlers
    DATA(lo_events) = mo_alv->get_events( ).
    SET HANDLER:
      on_user_command FOR lo_events,
      on_context_menu FOR lo_events,
      on_double_click FOR lo_events.

    mo_alv->display( ).

  ENDMETHOD.

  METHOD set_alv_function.
    CHECK mo_alv IS BOUND.

    mo_alv->set_function( iv_function ).
  ENDMETHOD.

  METHOD get_selected_entity.
    CHECK mo_alv IS BOUND.

    mo_alv->get_metadata( ).
    DATA(lt_sel_rows) = mo_alv->get_selections( )->get_selected_rows( ).
*    data(lt_sel_cells) = mo_alv->get_selections( )->get_selected_cells( ).
    CHECK lines( lt_sel_rows ) = 1.

    DATA(lr_row) = REF #( mt_usages[ lt_sel_rows[ 1 ] ] ).
    ev_entity_id = lr_row->name.
    ef_is_cds = xsdbool( lr_row->object_type = ZIF_SAT_C_ENTITY_TYPE=>cds_view ).
    ev_entity_type = lr_row->object_type.
  ENDMETHOD.

ENDCLASS.
