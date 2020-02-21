"! <p class="shorttext synchronized" lang="en">Entity Browser for SQL Editor</p>
CLASS zcl_dbbr_sqle_entity_browser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_view.

    "! <p class="shorttext synchronized" lang="en">Create new instance of alv field control</p>
    "!
    METHODS constructor
      IMPORTING
        io_parent_container TYPE REF TO cl_gui_container
        io_parent_view      TYPE REF TO zif_uitb_gui_composite_view.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_parent TYPE REF TO zif_uitb_gui_composite_view.
    DATA mo_container TYPE REF TO cl_gui_container.
ENDCLASS.



CLASS zcl_dbbr_sqle_entity_browser IMPLEMENTATION.

  METHOD constructor.
    mo_container = io_parent_container.
    mo_parent = io_parent_view.
  ENDMETHOD.

ENDCLASS.
