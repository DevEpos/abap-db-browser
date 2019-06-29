*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_find_table_field_view IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
        iv_title         = 'Find Field'
        iv_filter_prompt = 'Filter Field'
        if_use_alv_filter = abap_true
    ).

    mf_hide_tabname_field = if_hide_tabname_field.
    mt_col = it_col.
  ENDMETHOD.

  METHOD get_chosen_field_index.
    CLEAR: mv_chosen_index.

    mt_col_filtered = mt_col.

    show(
        iv_top    = 2
        iv_left   = 20
        iv_width  = 90
        iv_height = 20
    ).

    CLEAR mo_alv.

    rv_col_index = mv_chosen_index.
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
    mv_chosen_index = mt_col_filtered[ iv_row ]-idx.
  ENDMETHOD.

  METHOD adjust_column.
    CASE io_column->get_name( ).

      WHEN 'TABNAME'.
        IF mf_hide_tabname_field = abap_true.
          io_column->set_technical( ).
        ELSE.
          io_column->set_optimized( ).
        ENDIF.

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
