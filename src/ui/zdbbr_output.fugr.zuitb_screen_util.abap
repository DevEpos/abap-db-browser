*&---------------------------------------------------------------------*
*&  Include           ZDBBR_SCREEN_UTIL
*&---------------------------------------------------------------------*
CLASS cl_screen_util DEFINITION.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Calls the given Screen </p>
    "!
    CLASS-METHODS call_screen
      IMPORTING
        iv_screen_id        TYPE dynnr
        if_selection_screen TYPE abap_bool OPTIONAL
        iv_start_line       TYPE i OPTIONAL
        iv_start_column     TYPE i OPTIONAL
        iv_end_line         TYPE i OPTIONAL
        iv_end_column       TYPE i OPTIONAL.
ENDCLASS.

CLASS cl_screen_util IMPLEMENTATION.
  METHOD call_screen.

    IF iv_start_column IS NOT INITIAL AND iv_start_line IS NOT INITIAL.

      IF iv_end_line IS NOT INITIAL AND iv_end_column IS NOT INITIAL.
        IF if_selection_screen = abap_true.
          CALL SELECTION-SCREEN iv_screen_id STARTING AT iv_start_column iv_start_line ENDING AT iv_end_column iv_end_line.
        ELSE.
          CALL SCREEN iv_screen_id STARTING AT iv_start_column iv_start_line ENDING AT iv_end_column iv_end_line.
        ENDIF.
      ELSE.
        IF if_selection_screen = abap_true.
          CALL SELECTION-SCREEN iv_screen_id STARTING AT iv_start_column iv_start_line.
        ELSE.
          CALL SCREEN iv_screen_id STARTING AT iv_start_column iv_start_line.
        ENDIF.
      ENDIF.
    ELSE.
      IF if_selection_screen = abap_true.
        CALL SELECTION-SCREEN iv_screen_id.
      ELSE.
        CALL SCREEN iv_screen_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
