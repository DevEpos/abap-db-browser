class ZCL_DBBR_TOOLBAR_UTIL definition
  public
  final
  create public .

public section.

  class-events TOOLBAR_BUTTON_PRESSED
    exporting
      value(EV_FUNCTION) type UI_FUNC .

  class-methods FREE_SELSCREEN_ENTITY_TB .
  class-methods FREE_SELSCREEN_TABLE_TB .
  class-methods GET_SELSCREEN_ENTITY_TB
    returning
      value(RR_TOOLBAR) type ref to CL_GUI_TOOLBAR .
  class-methods GET_SELSCREEN_TABLE_TB
    returning
      value(RR_TOOLBAR) type ref to CL_GUI_TOOLBAR .
  PROTECTED SECTION.
private section.

  class-data SR_SELSCREEN_ENTITY_TOOLBAR_C type ref to CL_GUI_CONTAINER .
  class-data SR_SELSCREEN_ENTITY_TOOLBAR type ref to CL_GUI_TOOLBAR .
  class-data SR_SELSCREEN_TABLE_TOOLBAR_C type ref to CL_GUI_CONTAINER .
  class-data SR_SELSCREEN_TABLE_TOOLBAR type ref to CL_GUI_TOOLBAR .

  class-methods ON_TB_SET_FUNCTION_HANDLER
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
ENDCLASS.



CLASS ZCL_DBBR_TOOLBAR_UTIL IMPLEMENTATION.


  METHOD free_selscreen_entity_tb.
    CHECK sr_selscreen_entity_toolbar_c IS BOUND.
    sr_selscreen_entity_toolbar_c->free( ).

    CLEAR: sr_selscreen_entity_toolbar_c,
           sr_selscreen_entity_toolbar.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD free_selscreen_table_tb.
    CHECK sr_selscreen_table_toolbar_c IS BOUND.
    sr_selscreen_table_toolbar_c->free( ).

    CLEAR: sr_selscreen_table_toolbar_c,
           sr_selscreen_table_toolbar.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD get_selscreen_entity_tb.
    DATA: lt_events TYPE cntl_simple_events.

    IF sr_selscreen_entity_toolbar_c IS INITIAL.
      sr_selscreen_entity_toolbar_c = NEW cl_gui_custom_container(  container_name = 'CUSTOM_TOOLBAR' ).
      sr_selscreen_entity_toolbar = NEW #(  parent = sr_selscreen_entity_toolbar_c ).

      lt_events = VALUE #(
        ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
        ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  appl_event = abap_true )
      ).
      sr_selscreen_entity_toolbar->set_registered_events( lt_events ).

      SET HANDLER:
        on_tb_set_function_handler FOR sr_selscreen_entity_toolbar.
    ENDIF.

    rr_toolbar = sr_selscreen_entity_toolbar.
  ENDMETHOD.


  METHOD get_selscreen_table_tb.
    DATA: lt_events TYPE cntl_simple_events.

    IF sr_selscreen_table_toolbar_c IS INITIAL.
      sr_selscreen_table_toolbar_c = NEW cl_gui_custom_container(  container_name = 'TABLE_TOOLBAR' ).
      sr_selscreen_table_toolbar = NEW #(  parent = sr_selscreen_table_toolbar_c ).

      lt_events = VALUE #(
        ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
        ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  appl_event = abap_true )
      ).
      sr_selscreen_table_toolbar->set_registered_events( lt_events ).

      SET HANDLER:
        on_tb_set_function_handler FOR sr_selscreen_table_toolbar.
    ENDIF.

    rr_toolbar = sr_selscreen_table_toolbar.
  ENDMETHOD.


  METHOD on_tb_set_function_handler.
*    zcl_uitb_screen_util=>set_function_code( fcode ).
    RAISE EVENT toolbar_button_pressed
      EXPORTING
        ev_function = fcode.
  ENDMETHOD.
ENDCLASS.
