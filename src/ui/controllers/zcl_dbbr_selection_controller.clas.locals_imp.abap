CLASS lcl_choose_col_view IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
        iv_title         = 'Select Column to scroll to'
        iv_filter_prompt = 'Filter Column'
        if_use_alv_filter = abap_true
    ).

    mt_col = it_col.
  ENDMETHOD.

  METHOD get_chosen_column.
    CLEAR: mv_chosen_field.

    mt_col_filtered = mt_col.

    show(
        iv_top    = 2
        iv_left   = 20
        iv_width  = 90
        iv_height = 20
    ).

    CLEAR mo_alv.

    rv_col = mv_chosen_field.
  ENDMETHOD.

  METHOD create_content.
    super->create_content( io_container ).

    cl_gui_control=>set_focus( mo_filter_input ).
  ENDMETHOD.

  METHOD matches_filter.
    FIELD-SYMBOLS: <ls_field> TYPE lty_s_col_selection.

    ASSIGN is_data TO <ls_field>.

    DATA(lv_filter) = |*{ to_upper( iv_filter ) }*|.

    IF to_upper( <ls_field>-fieldname ) CP lv_filter OR
       to_upper( <ls_field>-tech_fieldname ) CP lv_filter OR
       to_upper( <ls_field>-description ) CP lv_filter.
      rf_matches = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_output_table.
    rr_table = REF #( mt_col_filtered ).
  ENDMETHOD.

  METHOD set_selected_element.
    mv_chosen_field = mt_col_filtered[ iv_row ]-tech_fieldname.
  ENDMETHOD.

  METHOD adjust_column.
    CASE io_column->get_name( ).

      WHEN 'TECH_FIELDNAME'.
        io_column->set_technical( ).

      WHEN 'DESCRIPTION'.
        io_column->set_optimized( ).

      WHEN 'FIELDNAME'.
        io_column->set_hotspot( ).
        io_column->set_optimized( ).

      WHEN 'IDX'.
        io_column->set_hotspot( ).
        io_column->set_output_length( 5 ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_detail_viewer IMPLEMENTATION.

  METHOD constructor.
    mo_util = io_util.
    ms_technical_info = is_technical_info.
    mo_tabfields_all = io_tabfields_all.
    mt_fieldcat = it_fieldcat.
  ENDMETHOD.

  METHOD show_details.
    DATA: ls_detail   TYPE ty_s_selfield.

    FIELD-SYMBOLS: <lv_fieldvalue>   TYPE any,
                   <lv_currency_ref> TYPE any.

    LOOP AT mt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WHERE fieldname <> 'LINE_INDEX'
                                                                AND fieldname <> 'HIDE_FLAG'
                                                                AND fieldname <> 'ZZ_EXTERNAL_DATA_ICON'.

      UNASSIGN: <lv_currency_ref>,
                <lv_currency_ref>.

      DATA(ls_dfies) = zcl_dbbr_dictionary_helper=>get_table_field_info( iv_tablename = <ls_fieldcat>-ref_table
                                                                          iv_fieldname = <ls_fieldcat>-ref_field ).
      IF <ls_fieldcat>-parameter2 = 'F'.
        IF ms_technical_info-tech_names = abap_true.
          ls_detail-fieldname = <ls_fieldcat>-tooltip.
        ELSE.
          ls_detail-fieldname = <ls_fieldcat>-coltext.
        ENDIF.
      ENDIF.

      TRY.
          DATA(lr_s_field) = mo_tabfields_all->get_field_ref_by_alv_name( <ls_fieldcat>-fieldname ).
          DATA(ls_fcat_for_detail) = VALUE lvc_s_fcat( ).

          mo_util->set_fieldcat_coltexts(
            EXPORTING ir_field    = lr_s_field
            CHANGING  cs_fieldcat = ls_fcat_for_detail
          ).
          IF ms_technical_info-tech_names = abap_true.
            ls_detail-fieldname = ls_fcat_for_detail-tooltip.
            ls_detail-tech_fieldname = ls_fcat_for_detail-scrtext_l.
          ELSE.
            ls_detail-fieldname = ls_fcat_for_detail-scrtext_l.
            ls_detail-tech_fieldname = ls_fcat_for_detail-tooltip.
          ENDIF.
          DATA(lf_char_like) = xsdbool( lr_s_field->inttype = cl_abap_typedescr=>typekind_char OR
                                        lr_s_field->inttype = cl_abap_typedescr=>typekind_string OR
                                        lr_s_field->inttype = cl_abap_typedescr=>typekind_num ).
        CATCH cx_sy_itab_line_not_found.
          ls_detail-tech_fieldname = <ls_fieldcat>-fieldname.
          IF ls_detail-fieldname IS INITIAL AND ls_dfies IS NOT INITIAL.
            ls_detail-fieldname = ls_dfies-fieldtext.
          ENDIF.
      ENDTRY.

      ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE is_line TO <lv_fieldvalue>.

      IF ls_dfies-datatype = 'CURR' AND
         ls_dfies-tabname = ls_dfies-reftable.
        " get name of reference field in case of active join
        IF mo_util->is_join_active( ).
          TRY .
              DATA(ls_reffield) = mt_fieldcat[ ref_table = ls_dfies-tabname ref_field = ls_dfies-reffield ].
              ASSIGN COMPONENT ls_reffield-fieldname OF STRUCTURE is_line TO <lv_currency_ref>.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ELSE.
          ASSIGN COMPONENT ls_dfies-reffield OF STRUCTURE is_line TO <lv_currency_ref>.
        ENDIF.

        IF <lv_currency_ref> IS ASSIGNED.
          WRITE <lv_fieldvalue> TO ls_detail-value CURRENCY <lv_currency_ref> LEFT-JUSTIFIED.
        ELSE.
          WRITE <lv_fieldvalue> TO ls_detail-value LEFT-JUSTIFIED.
        ENDIF.
      ELSE.
        DATA(lv_edit_mask) = COND #( WHEN <ls_fieldcat>-convexit IS NOT INITIAL AND
                                          <ls_fieldcat>-convexit <> 'NUM'            THEN '==' && <ls_fieldcat>-convexit ).

        IF lv_edit_mask IS NOT INITIAL.
          WRITE <lv_fieldvalue> TO ls_detail-value LEFT-JUSTIFIED USING EDIT MASK lv_edit_mask.
        ELSE.
          WRITE <lv_fieldvalue> TO ls_detail-value LEFT-JUSTIFIED.
        ENDIF.
      ENDIF.

      IF lf_char_like = abap_true.
        ls_detail-hidden = xsdbool( <lv_fieldvalue> IS INITIAL OR <lv_fieldvalue> CO '0.,' ).
      ELSE.
        ls_detail-hidden = xsdbool( <lv_fieldvalue> IS INITIAL ).
      ENDIF.

      " unconverted line
      ls_detail-value_unconverted = <lv_fieldvalue>.
      SHIFT ls_detail-value_unconverted LEFT DELETING LEADING space.

      APPEND ls_detail TO mt_selfield.
    ENDLOOP.

    mo_alv = zcl_uitb_alv=>create_alv(
        ir_data                 = REF #( mt_selfield )
        iv_display_type         = zif_uitb_c_alv_display_types=>modal_dialog
    ).
    mo_alv->get_display_settings( )->set_title( 'Detail View' ).
    DATA(lo_cols) = mo_alv->get_columns( ).
    DATA(lo_col_iterator) = lo_cols->zif_uitb_list~get_iterator( ).

    WHILE lo_col_iterator->has_next( ).
      DATA(lo_col) = CAST zcl_uitb_alv_column( lo_col_iterator->get_next( ) ).
      CASE lo_col->get_name( ).

        WHEN 'FIELDNAME'.
          lo_col->set_descriptions( iv_long = 'Field Name' iv_tooltip = 'Field Name' ).
          lo_col->set_optimized( ).
          lo_col->set_key( ).

        WHEN 'VALUE'.
          lo_col->set_output_length( 30 ).
          lo_col->set_optimized( ).

        WHEN 'VALUE_UNCONVERTED'.
          lo_col->set_descriptions( iv_long = 'Unconverted Value' iv_tooltip = 'Unconverted Value' ).
          lo_col->set_output_length( 30 ).
          lo_col->set_optimized( ).

        WHEN 'TECH_FIELDNAME'.
          lo_col->set_descriptions( iv_long = 'Technical Name' iv_tooltip = 'Technical Name' ).
          lo_col->set_optimized( ).

        WHEN 'HIDDEN'.
          lo_col->set_technical( ).

      ENDCASE.
    ENDWHILE.

    DATA(lo_functions) = mo_alv->get_functions( ).
    lo_functions->set_all( abap_false ).
    lo_functions->set_function( iv_name = zif_uitb_c_alv_functions=>sort_asc ).
    lo_functions->set_function( iv_name = zif_uitb_c_alv_functions=>sort_desc ).
    lo_functions->set_function( iv_name = zif_uitb_c_alv_functions=>column_optimze ).
    lo_functions->set_function( iv_name = zif_uitb_c_alv_functions=>find ).
    lo_functions->set_function( iv_name = zif_uitb_c_alv_functions=>find_more ).

    lo_functions->add_function(
        iv_name  = 'HIDE_EMPTY'
        iv_icon  = |{ icon_display }|
        iv_text  = 'Hide fields with no value'
    ).
    SET HANDLER:
       on_function FOR mo_alv->get_events( ).

    mo_alv->set_popup_dimensions(
        iv_top    = 2
        iv_left   = 10
        iv_right  = 140
        iv_bottom = 25
    ).
    mo_alv->get_selections( )->set_mode( zif_uitb_c_alv_selection=>cell ).
    lo_cols->set_optimized( ).
    mo_alv->display( ).
  ENDMETHOD.

  METHOD on_function.
    zcl_uitb_appl_util=>toggle( CHANGING value = mf_empty_hidden ).
    mo_alv->get_functions( )->set_function( iv_name = ev_function if_checked = mf_empty_hidden ).
    IF mf_empty_hidden = abap_true.
      mo_alv->get_filters( )->add_filter( iv_columnname = 'HIDDEN' iv_low = space iv_sign = 'I' iv_option = 'EQ' ).
    ELSE.
      mo_alv->get_filters( )->clear( ).
    ENDIF.
    mo_alv->refresh( ).
  ENDMETHOD.

ENDCLASS.
