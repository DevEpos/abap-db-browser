"! <p class="shorttext synchronized" lang="en">Functional Sidebar for SQL Editor</p>
CLASS zcl_dbbr_sqle_sidebar DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_view.
    INTERFACES zif_uitb_gui_composite_view.
    INTERFACES zif_uitb_gui_control .
    INTERFACES zif_uitb_content_searcher .

    CONSTANTS:
      BEGIN OF c_views,
        history             TYPE ui_func VALUE 'HISTORY' ##NO_TEXT,
        data_source_browser TYPE ui_func VALUE 'ENTITIES' ##NO_TEXT,
      END OF c_views.

    "! <p class="shorttext synchronized" lang="en">Create new instance of alv field control</p>
    "!
    METHODS constructor
      IMPORTING
        io_parent_container TYPE REF TO cl_gui_container
        io_parent_view      TYPE REF TO zif_uitb_gui_composite_view.

    "! <p class="shorttext synchronized" lang="en">Shows the view with the given id</p>
    METHODS show_view
      IMPORTING
        iv_view_id TYPE ui_func.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_view_info,
        function  TYPE ui_func,
        icon      TYPE iconname,
        butn_type TYPE tb_btype,
        text      TYPE c LENGTH 40,
        is_active TYPE abap_bool,
        control   TYPE REF TO zif_uitb_gui_control,
      END OF ty_s_view_info .
    DATA mt_views TYPE TABLE OF ty_s_view_info.
    DATA mo_parent TYPE REF TO zif_uitb_gui_composite_view.
    DATA mo_container TYPE REF TO cl_gui_container.
    DATA mo_switch TYPE REF TO zcl_uitb_gui_switch_container.
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar.

    "! <p class="shorttext synchronized" lang="en">Initializes the layout of the browser</p>
    METHODS init_layout.

    METHODS toggle_control
      IMPORTING
        iv_view TYPE ui_func.

    METHODS get_active_control
      RETURNING
        VALUE(ro_control) TYPE REF TO zif_uitb_gui_control.
    METHODS get_active_content_searcher
      RETURNING
        VALUE(ro_control) TYPE REF TO zif_uitb_content_searcher.
    "! <p class="shorttext synchronized" lang="en">Handler for toolbar button click event</p>
    METHODS on_toolbar_button
      FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        fcode.
ENDCLASS.



CLASS zcl_dbbr_sqle_sidebar IMPLEMENTATION.

  METHOD constructor.
    mo_container = io_parent_container.
    mo_parent = io_parent_view.
    mt_views = VALUE #(
      ( function  = c_views-history
        butn_type = cntb_btype_check
        icon      = icon_history
        text      = |{ 'History' }| )
      ( function  = c_views-data_source_browser
        butn_type = cntb_btype_check
        icon      = icon_list
        text      = |{ 'Data Source Browser' }| )
    ).
    init_layout( ).
  ENDMETHOD.

  METHOD show_view.
    toggle_control( iv_view_id ).
  ENDMETHOD.

  METHOD zif_uitb_gui_composite_view~set_child_visibility.
    RETURN.
  ENDMETHOD.

  METHOD zif_uitb_gui_composite_view~execute_command.
    FIELD-SYMBOLS: <ls_view> TYPE zcl_dbbr_sqle_sidebar=>ty_s_view_info.

    CASE io_command->mv_function.

      WHEN zif_dbbr_c_sql_query_editor=>fc_refresh_history.
        ASSIGN mt_views[ function = c_views-history ] TO <ls_view>.
        IF sy-subrc = 0 AND <ls_view>-control IS BOUND.
          CAST zcl_dbbr_sqle_sb_history( <ls_view>-control )->refresh_history( ).
        ENDIF.

      WHEN zif_dbbr_c_sql_query_editor=>fc_load_entity.
        ASSIGN mt_views[ function = c_views-data_source_browser ] TO <ls_view>.
        IF sy-subrc = 0 AND <ls_view>-control IS BOUND.
          ASSIGN io_command->mr_params->* TO FIELD-SYMBOL(<lt_db_entity_range>).
          CAST zcl_dbbr_sqle_sb_entity_tree( <ls_view>-control )->load_entities( <lt_db_entity_range> ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_uitb_content_searcher~search.
    DATA(lo_control) = get_active_content_searcher( ).
    CHECK lo_control IS BOUND.
    lo_control->search( ).
  ENDMETHOD.

  METHOD zif_uitb_content_searcher~search_next.
    DATA(lo_control) = get_active_content_searcher( ).
    CHECK lo_control IS BOUND.
    lo_control->search_next( ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    DATA(lo_control) = get_active_control( ).
    CHECK lo_control IS BOUND.
    lo_control->focus( ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    DATA(lo_control) = get_active_control( ).
    rf_has_focus = lo_control->has_focus( ).
  ENDMETHOD.

  METHOD init_layout.
    zcl_uitb_gui_helper=>create_control_toolbar(
      EXPORTING io_parent       = mo_container
                iv_toolbar_size = cl_gui_cfw=>compute_metric_from_dynp(
                  metric = cl_gui_control=>metric_pixel
                  x_or_y = 'Y'
                  in = 2 ) + 10 "10px margin
                iv_mode         = cl_gui_toolbar=>m_mode_vertical
                it_button       = CORRESPONDING #( mt_views )
      IMPORTING eo_toolbar      = mo_toolbar
                eo_client       = DATA(lo_client) ).
    SET HANDLER:
      on_toolbar_button FOR mo_toolbar.

    mo_switch = NEW #( io_parent = lo_client ).
    toggle_control( iv_view = c_views-data_source_browser ).
  ENDMETHOD.

  METHOD on_toolbar_button.
    toggle_control( fcode ).
  ENDMETHOD.

  METHOD toggle_control.

    FIELD-SYMBOLS: <ls_active_view> TYPE ty_s_view_info.

    LOOP AT mt_views ASSIGNING FIELD-SYMBOL(<ls_view>).
      IF <ls_view>-function = iv_view.
        <ls_view>-is_active = abap_true.
        ASSIGN <ls_view> TO <ls_active_view>.
      ELSE.
        CLEAR <ls_view>-is_active.
      ENDIF.

      mo_toolbar->set_button_state( fcode = <ls_view>-function checked = <ls_view>-is_active ).
      mo_switch->set_child_visible( iv_id = |{ <ls_view>-function }| if_visible = <ls_view>-is_active ).
    ENDLOOP.

    IF <ls_active_view>-control IS INITIAL AND NOT mo_switch->has_child( |{ iv_view }| ).
      DATA(lo_container) = mo_switch->add_child( iv_id = |{ iv_view }| ).
    ENDIF.

    CASE iv_view.

      WHEN c_views-history.
        IF <ls_active_view>-control IS INITIAL.
          <ls_active_view>-control = NEW zcl_dbbr_sqle_sb_history(
            io_container = lo_container
            io_parent    = mo_parent
          ).
        ENDIF.

        <ls_active_view>-control->focus( ).

      WHEN c_views-data_source_browser.
        IF <ls_active_view>-control IS INITIAL.
          <ls_active_view>-control = NEW zcl_dbbr_sqle_sb_entity_tree(
            io_container = lo_container
            io_parent    = mo_parent
          ).
        ENDIF.

        <ls_active_view>-control->focus( ).

    ENDCASE.
  ENDMETHOD.

  METHOD get_active_control.
    ASSIGN mt_views[ is_active = abap_true ] TO FIELD-SYMBOL(<ls_active_view>).
    CHECK sy-subrc = 0.

    ro_control = <ls_active_view>-control.
  ENDMETHOD.

  METHOD get_active_content_searcher.
    DATA(lo_active_control) = get_active_control( ).
    CHECK lo_active_control IS BOUND.

    TRY.
        ro_control = CAST #( lo_active_control ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
