CLASS lcl_choose_col_view DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_t_col TYPE REF TO data.
    METHODS get_chosen_column
      RETURNING
        VALUE(rv_col) TYPE fieldname.
  PRIVATE SECTION.
    DATA mr_alv TYPE REF TO zcl_uitb_alv.
    DATA mr_t_col TYPE REF TO data.
    DATA mr_dialog TYPE REF TO zif_uitb_template_prog.
    DATA mv_chosen_field TYPE fieldname.

    METHODS on_pai
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          er_callback
          ev_function_id.
    METHODS on_pbo
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          er_callback.
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          ev_column
          ev_row.
ENDCLASS.

CLASS lcl_choose_col_view IMPLEMENTATION.

  METHOD constructor.
    mr_dialog = zcl_uitb_templt_prog_callback=>create_template_program( iv_title = 'Choose Column' ).

    mr_dialog->add_function( iv_function_id = zif_uitb_template_prog=>c_func_find iv_icon = icon_search iv_text = 'Search' ).
    mr_t_col = ir_t_col.

    SET HANDLER: on_pai FOR mr_dialog,
                 on_pbo FOR mr_dialog.

    mr_dialog->show( iv_start_column = 50 iv_start_line = 10 iv_end_column = 110 iv_end_line = 30 ).
  ENDMETHOD.

  METHOD get_chosen_column.
    rv_col = mv_chosen_field.
  ENDMETHOD.

  METHOD on_link_click.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    DATA(lr_t_data) = mr_alv->get_data( ).
    ASSIGN lr_t_data->* TO <lt_data>.
    ASSIGN COMPONENT 'TECH_FIELDNAME' OF STRUCTURE <lt_data>[ ev_row ] TO FIELD-SYMBOL(<lv_chosen_column>).

    mv_chosen_field = <lv_chosen_column>.
    mr_dialog->leave_program( ).
  ENDMETHOD.

  METHOD on_pbo.
    er_callback->deactivate_function( zif_uitb_template_prog=>c_func_save ).

    IF er_callback->is_first_screen_call( ).
      mr_alv = zcl_uitb_alv=>create_alv(
          ir_data                 = mr_t_col
          ir_container            = mr_dialog->get_container( )
      ).

      SET HANDLER: on_link_click FOR mr_alv->get_events( ).

      DATA(lr_cols) = mr_alv->get_columns( ).
      lr_cols->set_single_click_sort( ).
      lr_cols->get_column( 'TECH_FIELDNAME' )->set_technical( ).
      lr_cols->get_column( 'FIELDNAME' )->set_hotspot( ).
      lr_cols->set_optimized( ).

      DATA(lr_func) = mr_alv->get_functions( ).
      lr_func->set_all( abap_false ).

      mr_alv->display( ).

      mr_alv->zif_uitb_gui_control~focus( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_pai.
    CASE ev_function_id.

      WHEN zif_uitb_template_prog=>c_func_find.
        mr_alv->set_function( zif_uitb_c_alv_functions=>find ).

      WHEN zif_uitb_template_prog=>c_func_find_more.
        mr_alv->set_function( zif_uitb_c_alv_functions=>find_more ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
