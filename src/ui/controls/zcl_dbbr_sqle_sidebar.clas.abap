"! <p class="shorttext synchronized" lang="en">Functional Sidebar for SQL Editor</p>
CLASS zcl_dbbr_sqle_sidebar DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_view.
    INTERFACES zif_uitb_gui_composite_view.

    "! <p class="shorttext synchronized" lang="en">Create new instance of alv field control</p>
    "!
    METHODS constructor
      IMPORTING
        io_parent_container TYPE REF TO cl_gui_container
        io_parent_view      TYPE REF TO zif_uitb_gui_composite_view.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        history             TYPE ui_func VALUE 'HISTORY' ##NO_TEXT,
        data_source_browser TYPE ui_func VALUE 'ENTITIES' ##NO_TEXT,
      END OF c_functions.
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
      ( function  = c_functions-history
        butn_type = cntb_btype_check
        icon      = icon_history
        text      = |{ 'History' }| )
      ( function  = c_functions-data_source_browser
        butn_type = cntb_btype_check
        icon      = icon_list
        text      = |{ 'Data Source Browser' }| )
    ).
    init_layout( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_composite_view~set_child_visibility.
    RETURN.
  ENDMETHOD.

  METHOD zif_uitb_gui_composite_view~execute_command.
    FIELD-SYMBOLS: <ls_view> TYPE zcl_dbbr_sqle_sidebar=>ty_s_view_info.

    CASE io_command->mv_function.

      WHEN zif_dbbr_c_sql_query_editor=>fc_refresh_history.
        ASSIGN mt_views[ function = c_functions-history ] TO <ls_view>.
        IF sy-subrc = 0 AND <ls_view>-control IS BOUND.
          CAST zcl_dbbr_sqle_sb_history( <ls_view>-control )->refresh_history( ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD init_layout.
    zcl_uitb_gui_helper=>create_control_toolbar(
      EXPORTING io_parent       = mo_container
                iv_toolbar_size = 60
                iv_mode         = cl_gui_toolbar=>m_mode_vertical
                it_button       = CORRESPONDING #( mt_views )
      IMPORTING eo_toolbar      = mo_toolbar
                eo_client       = DATA(lo_client)
    ).
    SET HANDLER:
      on_toolbar_button FOR mo_toolbar.

    mo_switch = NEW #(
      io_parent = lo_client
    ).
    toggle_control( iv_view = c_functions-data_source_browser ).
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

      WHEN c_functions-history.
        IF <ls_active_view>-control IS INITIAL.
          <ls_active_view>-control = NEW zcl_dbbr_sqle_sb_history(
            io_container = lo_container
            io_parent    = mo_parent
          ).
        ENDIF.

        <ls_active_view>-control->focus( ).

      WHEN c_functions-data_source_browser.
        IF <ls_active_view>-control IS INITIAL.
          <ls_active_view>-control = NEW zcl_dbbr_sqle_sb_entity_tree(
            io_container = lo_container
            io_parent    = mo_parent
          ).
        ENDIF.

        <ls_active_view>-control->focus( ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
