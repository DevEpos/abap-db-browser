"! <p class="shorttext synchronized">Helper for data selection (SQL)</p>
CLASS zcl_dbbr_selection_helper DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Read DB table size</p>
    CLASS-METHODS read_db_size
      IMPORTING
        iv_db_tab     TYPE tabname
      RETURNING
        VALUE(result) TYPE sy-tabix.

    "! <p class="shorttext synchronized">Creates additional where conditions for ignore case</p>
    CLASS-METHODS create_ignore_case_cond
      IMPORTING
        is_selfield TYPE zdbbr_selfield
      CHANGING
        ct_selfield TYPE zdbbr_selfield_itab.
ENDCLASS.


CLASS zcl_dbbr_selection_helper IMPLEMENTATION.
  METHOD create_ignore_case_cond.
  ENDMETHOD.

  METHOD read_db_size.
    SELECT COUNT( * ) FROM (iv_db_tab) INTO @result.
  ENDMETHOD.
ENDCLASS.
