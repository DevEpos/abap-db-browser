CLASS ZCL_DBBR_lock_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS lock
      IMPORTING
        iv_use_case       TYPE ZDBBR_use_case
      EXPORTING
        ev_locked_by      TYPE sy-uname
      RETURNING
        VALUE(rf_success) TYPE abap_bool.
    CLASS-METHODS unlock
      IMPORTING
        iv_use_case TYPE ZDBBR_use_case.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_lock_util IMPLEMENTATION.

  METHOD lock.
    CLEAR: rf_success,
           ev_locked_by.

    CALL FUNCTION 'ENQUEUE_EZDBBRLOCKBASIC'
      EXPORTING
        mode_ZDBBR_lockbasic = 'E'
        use_case              = iv_use_case
        _scope                = '3'
      EXCEPTIONS
        foreign_lock          = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      ev_locked_by = sy-msgv1.
    ELSE.
      rf_success = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD unlock.
    CALL FUNCTION 'DEQUEUE_EZDBBRLOCKBASIC'
      EXPORTING
        mode_ZDBBR_lockbasic = 'E'
        use_case              = iv_use_case.
  ENDMETHOD.

ENDCLASS.
