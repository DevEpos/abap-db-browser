"! <p class="shorttext synchronized" lang="en">Holds version information</p>
CLASS zcl_dbbr_version DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Displays the current version of DB Browser</p>
    CLASS-METHODS show_version.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gv_version TYPE string VALUE '1.0.0'.
ENDCLASS.



CLASS zcl_dbbr_version IMPLEMENTATION.
  METHOD show_version.
    MESSAGE |DB Browser Version: { gv_version }| TYPE 'I'.
  ENDMETHOD.

ENDCLASS.
