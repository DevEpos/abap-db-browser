"! <p class="shorttext synchronized" lang="en">Query history in SQL Query Editor</p>
CLASS zcl_dbbr_sqle_sb_history DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_view.
    INTERFACES zif_uitb_gui_control.
    METHODS constructor
      IMPORTING
        io_container TYPE REF TO cl_gui_container
        io_parent    TYPE REF TO zif_uitb_gui_composite_view.
    "! <p class="shorttext synchronized" lang="en">Refresh history entries</p>
    METHODS refresh_history.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_function,
        refresh    TYPE ui_func VALUE 'REFRESH',
        delete     TYPE ui_func VALUE 'DELETE',
        delete_all TYPE ui_func VALUE 'DELETE_ALL',
      END OF c_function.

    TYPES: BEGIN OF ty_s_history.
    TYPES icon TYPE c LENGTH 40.
    INCLUDE TYPE zdbbrsqlqh.
    TYPES simple_query TYPE zdbbr_sql_query_string.
    TYPES: END OF ty_s_history.

    DATA mo_alv TYPE REF TO zcl_uitb_alv.
    DATA mo_container TYPE REF TO cl_gui_container.
    DATA mo_parent TYPE REF TO zif_uitb_gui_composite_view.
    DATA mt_history TYPE TABLE OF ty_s_history.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    METHODS create_alv.
    METHODS delete_history_entries
      IMPORTING
        if_all TYPE abap_bool.

    METHODS on_double_click
        FOR EVENT double_click OF zcl_uitb_alv_events
      IMPORTING
        ev_column
        ev_row.
    METHODS on_function_click
        FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
        ev_function
        ev_tag.

ENDCLASS.


CLASS zcl_dbbr_sqle_sb_history IMPLEMENTATION.

  METHOD constructor.
    mo_container = io_container.
    mo_parent = io_parent.
    create_alv( ).
    refresh_history( ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    CHECK mo_alv IS BOUND.
    mo_alv->zif_uitb_gui_control~focus( ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    CHECK mo_alv IS BOUND.
    rf_has_focus = mo_alv->zif_uitb_gui_control~has_focus( ).
  ENDMETHOD.

  METHOD create_alv.

    mo_alv = zcl_uitb_alv=>create_alv(
     ir_data                 = REF #( mt_history )
     ir_container            = mo_container
    ).

    DATA(lo_functions) = mo_alv->get_functions( ).
    lo_functions->set_all( abap_false ).
    lo_functions->add_function(
        iv_name             = c_function-refresh
        iv_icon             = |{ icon_refresh }|
        iv_tooltip          = |{ 'Refresh History' }|
    ).
    lo_functions->add_function( iv_type = zcl_uitb_alv_functions=>separator ).
    lo_functions->add_function(
        iv_name             = c_function-delete
        iv_icon             = |{ icon_delete }|
        iv_tooltip          = |{ 'Delete selected entries' }|
    ).
    lo_functions->add_function(
        iv_name             = c_function-delete_all
        iv_icon             = |{ icon_delete }|
        iv_text             = |{ 'All' }|
        iv_tooltip          = |{ 'Delete all entries' }|
    ).

    SET HANDLER:
      on_double_click FOR mo_alv->get_events( ),
      on_function_click FOR mo_alv->get_events( ).

    DATA(lo_cols) = mo_alv->get_columns( ).
    lo_cols->set_single_click_sort( ).
    DATA(lo_col_iter) = lo_cols->zif_uitb_list~get_iterator( ).

    WHILE lo_col_iter->has_next( ).
      DATA(lo_col) = CAST zcl_uitb_alv_column( lo_col_iter->get_next( ) ).

      CASE lo_col->get_name( ).

        WHEN 'QUERY_HISTORY_ID' OR
             'QUERY_STRING' OR
             'CREATED_TIME' OR
             'CREATED_BY'.
          lo_col->set_technical( ).

        WHEN 'EXECUTION_TIME'.
          lo_col->set_output_length( 5 ).

        WHEN 'ICON'.
          lo_col->set_technical( ).
*          lo_col->set_icon( ).
*          lo_col->set_output_length( 3 ).

        WHEN 'SIMPLE_QUERY'.
          lo_col->set_output_length( 45 ).

        WHEN 'CREATED_DATE'.
          lo_col->set_output_length( 10 ).
      ENDCASE.
    ENDWHILE.

    lo_cols->set_column_position( iv_columnname = 'CREATED_DATE' iv_position = 1 ).
    lo_cols->set_column_position( iv_columnname = 'EXECUTION_TIME' iv_position = 2 ).
    lo_cols->set_column_position( iv_columnname = 'SIMPLE_QUERY' iv_position = 3 ).

    mo_alv->get_selections( )->set_mode( zif_uitb_c_alv_selection=>row_column ).

    mo_alv->display( ).
  ENDMETHOD.


  METHOD refresh_history.
    mt_history = CORRESPONDING #( zcl_dbbr_sql_query_factory=>get_history_entries( ) ).

    LOOP AT mt_history ASSIGNING FIELD-SYMBOL(<ls_history>).
      <ls_history>-icon = |{ icon_query }|.
      <ls_history>-simple_query = <ls_history>-query_string.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN <ls_history>-simple_query WITH space.
      <ls_history>-simple_query = replace( val = <ls_history>-simple_query regex = '(\s)(\s*)' with = '$1' occ = 0 ).
      DATA(lv_length) = strlen( <ls_history>-simple_query ).
      IF lv_length > 60.
        lv_length = 60.
        <ls_history>-simple_query = <ls_history>-simple_query(60) && '...'.
      ENDIF.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN <ls_history>-query_string WITH cl_abap_char_utilities=>newline.
    ENDLOOP.

    mo_alv->refresh( ).
  ENDMETHOD.

  METHOD on_double_click.
    ASSIGN mt_history[ ev_row ] TO FIELD-SYMBOL(<ls_history>).
    CHECK sy-subrc = 0.

    mo_parent->execute_command(
        NEW zcl_uitb_gui_simple_command(
          iv_function = zcl_uitb_gui_code_editor=>c_command_ids-replace_content
          ir_params   = NEW string( <ls_history>-query_string )
        )
    ).
  ENDMETHOD.

  METHOD on_function_click.
    CASE ev_function.

      WHEN c_function-delete.
        delete_history_entries( if_all = abap_false ).

      WHEN c_function-delete_all.
        delete_history_entries( if_all = abap_true ).

      WHEN c_function-refresh.
        refresh_history( ).

    ENDCASE.
  ENDMETHOD.


  METHOD delete_history_entries.
    DATA: lt_history_query_range TYPE zcl_dbbr_sql_query_factory=>ty_t_history_id_range,
          lt_node_keys           TYPE treemnotab.

    IF if_all = abap_true.
      IF zcl_dbbr_appl_util=>popup_to_confirm(
           iv_title                 = 'Delete history?'
           iv_query                 = |Are you sure you want to clear the history?|
           iv_icon_type             = 'ICON_QUESTION' ) = '1'.

        CLEAR mt_history.
        zcl_dbbr_sql_query_factory=>delete_history_entries( ).
        mo_alv->refresh( ).
      ENDIF.
    ELSE.
      DATA(lt_rows) = mo_alv->get_selections( )->get_selected_rows( ).
      IF lt_rows IS INITIAL.
        MESSAGE |Select at least 1 history entry| TYPE 'S'.
        RETURN.
      ENDIF.

      LOOP AT lt_rows INTO DATA(lv_row).
        ASSIGN mt_history[ lv_row ] TO FIELD-SYMBOL(<ls_history>).
        lt_history_query_range = VALUE #( BASE lt_history_query_range ( sign = 'I' option = 'EQ' low = <ls_history>-query_history_id ) ).
      ENDLOOP.

      zcl_dbbr_sql_query_factory=>delete_history_entries( lt_history_query_range ).
      DELETE mt_history WHERE query_history_id IN lt_history_query_range.
      mo_alv->refresh( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
