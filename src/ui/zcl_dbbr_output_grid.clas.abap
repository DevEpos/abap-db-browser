class ZCL_DBBR_OUTPUT_GRID definition
  public
  inheriting from CL_GUI_ALV_GRID
  final
  create public .

public section.

  events SORTED_FIELDS_CHANGED .

  methods CONSTRUCTOR
    importing
      !IR_PARENT type ref to CL_GUI_CONTAINER .
  methods EXECUTE_USER_COMMAND
    importing
      !IV_FUNCTION_CODE type SY-UCOMM .
  methods GET_ALL_SELECTED_ROWS
    returning
      value(RESULT) type LVC_T_INDX .
  methods HAS_SELECTED_COLUMNS
    returning
      value(RF_COLUMNS_SELECTED) type BOOLEAN .
  methods HIDE_SELECTED_COLUMNS .
  methods OPTIMIZE_COLUMNS .
  methods SET_COLUMN_NAMES
    importing
      !IF_TECH_NAMES type BOOLEAN .

  methods DISPATCH
    redefinition .
  methods SET_SORT_CRITERIA
    redefinition .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_DBBR_OUTPUT_GRID IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    super->constructor(
        i_parent        = ir_parent
        i_lifetime      = cl_gui_control=>lifetime_dynpro
        i_fcat_complete = abap_true
    ).

  ENDMETHOD.


  METHOD DISPATCH.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/01
*&---------------------------------------------------------------------*
*& Description: Redefined because of additional event handling
*&---------------------------------------------------------------------*

    DATA: lv_action TYPE sy-ucomm.

    get_event_parameter( EXPORTING parameter_id = 0
                                   queue_only   = space
                         IMPORTING parameter    = lv_action ).

    super->dispatch(
      EXPORTING
        cargo             = cargo
        eventid           = eventid
        is_shellevent     = is_shellevent
        is_systemdispatch = is_systemdispatch
      EXCEPTIONS
        cntl_error        = 1
        OTHERS            = 2
    ).
    IF sy-subrc <> 0.
* Implement suitable error handling here
      RETURN.
    ENDIF.

    " handle specific function codes
    CASE lv_action.
      WHEN mc_mb_variant OR
           mc_fc_current_variant OR
           mc_fc_load_variant OR
           mc_fc_save_variant OR
           mc_fc_maintain_variant OR
           mc_fc_variant_admin OR
           mc_fc_sort OR
           mc_fc_sort_asc OR
           mc_fc_sort_dsc.
        IF eventid <> evt_toolbar_menubutton_click.
          RAISE EVENT sorted_fields_changed.

          " refresh the table
          refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true )
                                        i_soft_refresh = abap_true ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD EXECUTE_USER_COMMAND.
    fcode_bouncer( ).
  ENDMETHOD.


  METHOD get_all_selected_rows.
    get_selected_rows(
      IMPORTING
        et_index_rows = DATA(lt_rows)
    ).
    get_selected_cells(
      IMPORTING
        et_cell = DATA(lt_cells)
    ).

    result = VALUE #(
      FOR cell IN lt_cells
      ( cell-row_id-index  )
    ).

    result = VALUE #( BASE result
      FOR row IN lt_rows
      ( row-index )
    ).

    SORT result.
    DELETE ADJACENT DUPLICATES FROM result.
  ENDMETHOD.


  METHOD HAS_SELECTED_COLUMNS.
    get_selected_columns( IMPORTING et_index_columns = DATA(lt_col_index) ).

    rf_columns_selected = xsdbool( lt_col_index IS NOT INITIAL ).
  ENDMETHOD.


  METHOD HIDE_SELECTED_COLUMNS.

    get_scroll_info_via_id( IMPORTING es_row_info = DATA(ls_row_id)
                                      es_col_info = DATA(ls_col_id) ).

    get_selected_columns( IMPORTING et_index_columns = DATA(lt_index_columns) ).

    IF me->is_cache_valid( ) NE abap_true OR www_active EQ abap_true.
      cl_gui_cfw=>flush( ).
    ENDIF.

    get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    IF lt_index_columns IS NOT INITIAL.
      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        IF line_exists( lt_index_columns[ fieldname = <ls_fieldcat>-fieldname ] ).
          <ls_fieldcat>-no_out = 'X'.
        ENDIF.
      ENDLOOP.

      " update fieldcatalog
      set_frontend_fieldcatalog( lt_fieldcat ).

      set_scroll_info_via_id(
          is_row_info = ls_row_id
          is_col_info = ls_col_id
      ).
      refresh_table_display( ).
    ELSE.
      MESSAGE s005(0k).
    ENDIF.
  ENDMETHOD.


  METHOD OPTIMIZE_COLUMNS.
    optimize_all_cols(
*      EXPORTING
*        include_header = 1    " Spaltenüberschriften berücksichtigen (0=Nein, 1=Ja)
*      EXCEPTIONS
*        error          = 1
*        others         = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD SET_COLUMN_NAMES.
    DATA: lv_tooltip TYPE lvc_tip,
          lv_coltext TYPE lvc_txtcol.

    get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fcat) ).

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      DATA(ls_fcat) = <ls_fcat>.
      <ls_fcat>-tooltip = ls_fcat-coltext.
      <ls_fcat>-coltext = ls_fcat-tooltip.
    ENDLOOP.

    set_frontend_fieldcatalog( lt_fcat ).

    refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.


  METHOD SET_SORT_CRITERIA.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/01
*&---------------------------------------------------------------------*
    super->set_sort_criteria( EXPORTING it_sort = it_sort ).

    RAISE EVENT sorted_fields_changed.
  ENDMETHOD.
ENDCLASS.
