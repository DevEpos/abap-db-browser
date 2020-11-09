CLASS zcl_dbbr_distinct_val_chooser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view .

    METHODS constructor
      IMPORTING
        !ir_t_data           TYPE REF TO data
        !is_field_info       TYPE zdbbr_tabfield_info_ui
        !iv_fieldname        TYPE fieldname
        !if_via_db           TYPE abap_bool OPTIONAL
        !it_filtered_entries TYPE lvc_t_fidx OPTIONAL .
    METHODS get_chosen_value
      RETURNING
        VALUE(result) TYPE zsat_value .
    METHODS has_chosen_value
      RETURNING
        VALUE(result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_distinct,
        fieldvalue     TYPE zsat_value,
        fieldvalue_int TYPE zsat_value,
        count          TYPE sy-tabix,
      END OF ty_distinct .

    DATA mr_view TYPE REF TO zif_uitb_template_prog .
    DATA mr_data TYPE REF TO data .
    DATA mv_filter_fieldname TYPE fieldname .
    DATA:
      mt_distinct_values TYPE STANDARD TABLE OF ty_distinct WITH EMPTY KEY .
    DATA mv_chosen_value TYPE zsat_value .
    DATA mr_alv TYPE REF TO zcl_uitb_alv .
    DATA ms_field_info TYPE zdbbr_tabfield_info_ui .
    DATA mf_chosen_value TYPE abap_bool .
    DATA mf_via_db TYPE abap_bool .
    DATA mt_filtered_entries TYPE lvc_t_fidx .

    METHODS on_pai
        FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
        !er_callback
        !ev_function_id .
    METHODS on_pbo
        FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
        !er_callback .
    METHODS on_link_click
        FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
        !ev_column
        !ev_row .
    METHODS do_on_first_screen_call .
    METHODS group_by_field .
ENDCLASS.



CLASS zcl_dbbr_distinct_val_chooser IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <lt_data>      TYPE table,
                   <lt_data_copy> TYPE table.

    ASSIGN ir_t_data->* TO <lt_data>.
    CREATE DATA mr_data LIKE <lt_data>.

    DATA(lr_extended_list) = zcl_uitb_data_list=>create_for_table_ref(
        ir_t_data      = mr_data
    ).

    IF NOT lr_extended_list->has_component( 'LINE_INDEX' ).
      lr_extended_list->extend(
        it_comp_extend = VALUE #(
          ( component = 'LINE_INDEX' type = 'SYST-TABIX' )
        )
      ).

      mr_data = lr_extended_list->get_all( ).
    ENDIF.


    ASSIGN mr_data->* TO <lt_data_copy>.
    MOVE-CORRESPONDING <lt_data> TO <lt_data_copy>.

*.. Fill line index
    LOOP AT <lt_data_copy> ASSIGNING FIELD-SYMBOL(<ls_line>).
      ASSIGN COMPONENT 'LINE_INDEX' OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_line>).
      CHECK sy-subrc = 0.
      <lv_line> = sy-tabix.
    ENDLOOP.

    mf_via_db = if_via_db.
    mt_filtered_entries = it_filtered_entries.
    mr_view = zcl_uitb_templt_prog_callback=>create_template_program( iv_title = 'Choose Value' ).
    mv_filter_fieldname = iv_fieldname.
    ms_field_info = is_field_info.

*.. determine distinct values for given table
    group_by_field( ).

    SET HANDLER:
      on_pai FOR mr_view,
      on_pbo FOR mr_view.
  ENDMETHOD.


  METHOD do_on_first_screen_call.
    DATA: lr_col TYPE REF TO zcl_uitb_alv_column.

    mr_alv = zcl_uitb_alv=>create_alv(
       ir_data                 = REF #( mt_distinct_values )
       ir_container            = mr_view->get_container( )
       if_editable             = abap_false
    ).

    SET HANDLER: on_link_click FOR mr_alv->get_events( ).

    mr_alv->get_functions( )->set_all( abap_false ).

    DATA(lr_cols) = mr_alv->get_columns( ).
    lr_col = lr_cols->get_column( 'FIELDVALUE' ).
    lr_col->set_hotspot( ).
    lr_col->set_descriptions( iv_long = 'Distinct Value' ).

    lr_col = lr_cols->get_column( 'FIELDVALUE_INT' ).
    lr_col->set_technical( ).

    lr_col = lr_cols->get_column( 'COUNT' ).
    lr_col->set_descriptions( iv_long = 'Count' ).

    lr_cols->set_optimized( ).
    mr_alv->display( ).
  ENDMETHOD.


  METHOD get_chosen_value.
    result = mv_chosen_value.
  ENDMETHOD.


  METHOD group_by_field.
    DATA: lv_group_count TYPE i.

    FIELD-SYMBOLS: <lt_table>          TYPE STANDARD TABLE,
                   <lt_table_temp>     TYPE STANDARD TABLE,
                   <ls_distinct_value> TYPE ty_distinct.

    ASSIGN mr_data->* TO <lt_table>.

    SORT <lt_table> BY (mv_filter_fieldname).

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_row>).
*.... if index of row is no longer visible because of filtered entries it will be
*.... skipped during the grouping
      IF mt_filtered_entries IS NOT INITIAL.
        ASSIGN COMPONENT 'LINE_INDEX' OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_original_index>).
        IF sy-subrc = 0 AND line_exists( mt_filtered_entries[ table_line = <lv_original_index> ] ).
          CONTINUE.
        ENDIF.
      ENDIF.
      ASSIGN COMPONENT mv_filter_fieldname OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_field_value>).

      IF <ls_distinct_value> IS NOT ASSIGNED OR
         <ls_distinct_value>-fieldvalue <> <lv_field_value>.

        APPEND INITIAL LINE TO mt_distinct_values ASSIGNING <ls_distinct_value>.
        <ls_distinct_value>-count = 1.
        <ls_distinct_value>-fieldvalue = <lv_field_value>.
        <ls_distinct_value>-fieldvalue_int = <lv_field_value>.
*...... TODO: convert internal value to display value
      ELSE. " aggregate entry
        ADD 1 TO <ls_distinct_value>-count.
      ENDIF.

    ENDLOOP.

    SORT mt_distinct_values BY count DESCENDING.
  ENDMETHOD.


  METHOD has_chosen_value.
    result = mf_chosen_value.
  ENDMETHOD.


  METHOD on_link_click.
    mv_chosen_value = mt_distinct_values[ ev_row ]-fieldvalue.
    mf_chosen_value = abap_true.
    mr_view->leave_program( ).
  ENDMETHOD.


  METHOD on_pai.
    CASE ev_function_id.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD on_pbo.
    er_callback->deactivate_function( zif_uitb_template_prog=>c_save ).

    IF er_callback->is_first_screen_call( ).
      do_on_first_screen_call( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    mr_view->show(
        iv_start_line   = 10
        iv_start_column = 100
        iv_end_line     = 25
        iv_end_column   = 140
    ).
  ENDMETHOD.
ENDCLASS.
