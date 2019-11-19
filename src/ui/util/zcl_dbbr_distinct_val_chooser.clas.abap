class ZCL_DBBR_DISTINCT_VAL_CHOOSER definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_VIEW .

  methods CONSTRUCTOR
    importing
      !IR_T_DATA type ref to DATA
      !IS_FIELD_INFO type ZDBBR_TABFIELD_INFO_UI
      !IV_FIELDNAME type FIELDNAME
      !IF_VIA_DB type ABAP_BOOL optional
      !IT_FILTERED_ENTRIES type LVC_T_FIDX optional .
  methods GET_CHOSEN_VALUE
    returning
      value(RESULT) type ZSAT_VALUE .
  methods HAS_CHOSEN_VALUE
    returning
      value(RESULT) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_distinct,
        fieldvalue     TYPE ZSAT_VALUE,
        fieldvalue_int TYPE ZSAT_VALUE,
        count          TYPE sy-tabix,
      END OF ty_distinct .

  data MR_VIEW type ref to ZIF_UITB_TEMPLATE_PROG .
  data MR_DATA type ref to DATA .
  data MV_FILTER_FIELDNAME type FIELDNAME .
  data:
    mt_distinct_values TYPE STANDARD TABLE OF ty_distinct WITH EMPTY KEY .
  data MV_CHOSEN_VALUE type ZSAT_VALUE .
  data MR_ALV type ref to ZCL_UITB_ALV .
  data MS_FIELD_INFO type ZDBBR_TABFIELD_INFO_UI .
  data MF_CHOSEN_VALUE type ABAP_BOOL .
  data MF_VIA_DB type ABAP_BOOL .
  data MT_FILTERED_ENTRIES type LVC_T_FIDX .

  methods ON_PAI
    for event USER_COMMAND of ZIF_UITB_VIEW_CALLBACK
    importing
      !ER_CALLBACK
      !EV_FUNCTION_ID .
  methods ON_PBO
    for event BEFORE_OUTPUT of ZIF_UITB_VIEW_CALLBACK
    importing
      !ER_CALLBACK .
  methods ON_LINK_CLICK
    for event LINK_CLICK of ZCL_UITB_ALV_EVENTS
    importing
      !EV_COLUMN
      !EV_ROW .
  methods DO_ON_FIRST_SCREEN_CALL .
  methods GROUP_BY_FIELD .
ENDCLASS.



CLASS ZCL_DBBR_DISTINCT_VAL_CHOOSER IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <lt_data>      TYPE table,
                   <lt_data_copy> TYPE table.

    ASSIGN ir_t_data->* TO <lt_data>.
    CREATE DATA mr_data LIKE <lt_data>.

    DATA(lr_extended_list) = NEW zcl_uitb_data_list(
        ir_t_data      = mr_data

    ).
    IF NOT lr_extended_list->zif_uitb_data_ref_list~has_component( 'LINE_INDEX' ).
      lr_extended_list->extend_table(
        it_comp_extend = VALUE #(
          ( component = 'LINE_INDEX' type = 'SYST-TABIX' )
        )
      ).

      mr_data = lr_extended_list->zif_uitb_data_ref_list~get_all( ).
    ENDIF.


    ASSIGN mr_data->* TO <lt_data_copy>.
    MOVE-CORRESPONDING <lt_data> TO <lt_data_copy>.

*.. Fill line index
    LOOP AT <lt_data_copy> ASSIGNING FIELD-SYMBOL(<ls_line>).
      ASSIGN COMPONENT 'LINE_INDEX' of STRUCTURE <ls_line> to FIELD-SYMBOL(<lv_line>).
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
