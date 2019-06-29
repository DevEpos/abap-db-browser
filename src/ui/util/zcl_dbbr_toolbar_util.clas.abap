"! <p class="shorttext synchronized" lang="en">Util for Toolbars in DB Browser</p>
CLASS zcl_dbbr_toolbar_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Toolbar button was pressed</p>
    CLASS-EVENTS toolbar_button_pressed
      EXPORTING
        VALUE(ev_function) TYPE ui_func .

    "! <p class="shorttext synchronized" lang="en">Free selection screen entity toolbar</p>
    CLASS-METHODS free_selscreen_entity_tb .
    "! <p class="shorttext synchronized" lang="en">Free selection screen table toolbar</p>
    CLASS-METHODS free_selscreen_table_tb .
    "! <p class="shorttext synchronized" lang="en">Free main selection screen toolbar</p>
    CLASS-METHODS free_main_selscreen_tb.
    "! <p class="shorttext synchronized" lang="en">Get Selection Screen entity toolbar</p>
    CLASS-METHODS get_selscreen_entity_tb
      RETURNING
        VALUE(rr_toolbar) TYPE REF TO cl_gui_toolbar .
    "! <p class="shorttext synchronized" lang="en">Get selection Screen Table Toolbar</p>
    CLASS-METHODS get_selscreen_table_tb
      RETURNING
        VALUE(rr_toolbar) TYPE REF TO cl_gui_toolbar .
    "! <p class="shorttext synchronized" lang="en">Get main toolbar of selection screen</p>
    CLASS-METHODS get_selscreen_main_tb
      RETURNING
        VALUE(ro_toolbar) TYPE REF TO cl_gui_toolbar.
    CLASS-METHODS update_main_tb_functions.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_selscreen_entity_toolbar_c TYPE REF TO cl_gui_container .
    CLASS-DATA go_selscreen_entity_toolbar TYPE REF TO cl_gui_toolbar .
    CLASS-DATA go_selscreen_table_toolbar_c TYPE REF TO cl_gui_container .
    CLASS-DATA go_selscreen_table_toolbar TYPE REF TO cl_gui_toolbar .
    CLASS-DATA go_selcreen_main_toolbar_c TYPE REF TO cl_gui_container.
    CLASS-DATA go_selcreen_main_toolbar TYPE REF TO cl_gui_toolbar.

    "! <p class="shorttext synchronized" lang="en">Handler for TB function set</p>
    CLASS-METHODS on_tb_set_function_handler
          FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
          !fcode .
ENDCLASS.



CLASS zcl_dbbr_toolbar_util IMPLEMENTATION.


  METHOD free_selscreen_entity_tb.
    CHECK go_selscreen_entity_toolbar_c IS BOUND.
    go_selscreen_entity_toolbar_c->free( ).

    CLEAR: go_selscreen_entity_toolbar_c,
           go_selscreen_entity_toolbar.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD free_selscreen_table_tb.
    CHECK go_selscreen_table_toolbar_c IS BOUND.
    go_selscreen_table_toolbar_c->free( ).

    CLEAR: go_selscreen_table_toolbar_c,
           go_selscreen_table_toolbar.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD free_main_selscreen_tb.
    CHECK go_selcreen_main_toolbar_c IS BOUND.
    go_selcreen_main_toolbar_c->free( ).

    CLEAR: go_selcreen_main_toolbar_c,
           go_selcreen_main_toolbar.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD get_selscreen_entity_tb.
    DATA: lt_events TYPE cntl_simple_events.

    IF go_selscreen_entity_toolbar_c IS INITIAL.
      go_selscreen_entity_toolbar_c = NEW cl_gui_custom_container(  container_name = 'CUSTOM_TOOLBAR' ).
      go_selscreen_entity_toolbar = NEW #(  parent = go_selscreen_entity_toolbar_c ).

      lt_events = VALUE #(
        ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
        ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  appl_event = abap_true )
      ).
      go_selscreen_entity_toolbar->set_registered_events( lt_events ).

      SET HANDLER:
        on_tb_set_function_handler FOR go_selscreen_entity_toolbar.
    ENDIF.

    rr_toolbar = go_selscreen_entity_toolbar.
  ENDMETHOD.


  METHOD get_selscreen_main_tb.
    DATA: lt_events TYPE cntl_simple_events.

    IF go_selcreen_main_toolbar_c IS INITIAL.
      go_selcreen_main_toolbar_c = NEW cl_gui_docking_container(
        side                    = cl_gui_docking_container=>dock_at_top
        extension               = 15
        lifetime                = cl_gui_control=>lifetime_dynpro
      ).
*      go_selcreen_main_toolbar_c = NEW cl_gui_custom_container(  container_name = 'SELSCREEN_TOOLBAR' ).
      go_selcreen_main_toolbar = NEW #(  parent = go_selcreen_main_toolbar_c ).

      lt_events = VALUE #(
        ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
        ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  appl_event = abap_true )
      ).
      go_selcreen_main_toolbar->set_registered_events( lt_events ).

      SET HANDLER:
        on_tb_set_function_handler FOR go_selcreen_main_toolbar.
    ENDIF.

    ro_toolbar = go_selcreen_main_toolbar.
  ENDMETHOD.

  METHOD update_main_tb_functions.

  ENDMETHOD.

  METHOD get_selscreen_table_tb.
    DATA: lt_events TYPE cntl_simple_events.

    IF go_selscreen_table_toolbar_c IS INITIAL.
      go_selscreen_table_toolbar_c = NEW cl_gui_custom_container(  container_name = 'TABLE_TOOLBAR' ).
      go_selscreen_table_toolbar = NEW #(  parent = go_selscreen_table_toolbar_c ).

      lt_events = VALUE #(
        ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
        ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  appl_event = abap_true )
      ).
      go_selscreen_table_toolbar->set_registered_events( lt_events ).

      SET HANDLER:
        on_tb_set_function_handler FOR go_selscreen_table_toolbar.
    ENDIF.

    rr_toolbar = go_selscreen_table_toolbar.
  ENDMETHOD.


  METHOD on_tb_set_function_handler.
    RAISE EVENT toolbar_button_pressed
      EXPORTING
        ev_function = fcode.
  ENDMETHOD.

ENDCLASS.
