CLASS ZCL_DBBR_screen_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS leave_screen .
    CLASS-METHODS quit_program.
    CLASS-METHODS disable_program_quit.
    CLASS-METHODS enable_program_quit.
    CLASS-METHODS show_progress
      IMPORTING
        iv_progress TYPE i DEFAULT 0
        !iv_text    TYPE string .
    CLASS-METHODS set_parameter_value
      IMPORTING
        !iv_parameter_id    TYPE any
        !iv_parameter_value TYPE any .
    CLASS-METHODS get_paramater_from_memory
      IMPORTING
        iv_parameter_id  TYPE memoryid
        iv_default_value TYPE any OPTIONAL
      EXPORTING
        ev_param_value   TYPE any.
    CLASS-METHODS set_selscreen_status
      IMPORTING
        iv_status              TYPE syst_pfkey
        iv_repid               TYPE repid OPTIONAL
        it_excluding_functions TYPE ui_functions OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: sf_program_quit_disabled.
ENDCLASS.



CLASS ZCL_DBBR_SCREEN_HELPER IMPLEMENTATION.


  METHOD disable_program_quit.
    sf_program_quit_disabled = abap_true.
  ENDMETHOD.


  METHOD enable_program_quit.
    CLEAR sf_program_quit_disabled.
  ENDMETHOD.


  METHOD get_paramater_from_memory.
    GET PARAMETER ID iv_parameter_id FIELD ev_param_value.
    IF sy-subrc <> 0 AND iv_default_value IS SUPPLIED.
      ev_param_value = iv_default_value.
    ENDIF.
  ENDMETHOD.


  METHOD leave_screen.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDMETHOD.


  METHOD quit_program.
    IF sf_program_quit_disabled = abap_false.
      LEAVE PROGRAM.
    ELSE.
      leave_screen( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_parameter_value.
    CALL FUNCTION 'SMAN_SET_USER_PARAMETER'
      EXPORTING
        parameter_id    = CONV memoryid( iv_parameter_id )
        parameter_value = CONV xuvalue( iv_parameter_value )
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.


  METHOD set_selscreen_status.
    DATA: lt_exclude TYPE ui_functions.

    lt_exclude = it_excluding_functions.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = iv_status
        p_program = iv_repid
      TABLES
        p_exclude = lt_exclude.
  ENDMETHOD.


  METHOD show_progress.
*&---------------------------------------------------------------------*
*& Description: Displays text in Status bar of current workbench
*&---------------------------------------------------------------------*
    DATA(lv_progress) = COND #( WHEN iv_progress <= 0 THEN 0
                                WHEN iv_progress > 100 THEN 100
                                ELSE iv_progress ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_progress
        text       = iv_text.
  ENDMETHOD.
ENDCLASS.
