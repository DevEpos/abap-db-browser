"! <p class="shorttext synchronized" lang="en">Generic ALV control for Field Control</p>
CLASS zcl_dbbr_fc_generic_alv DEFINITION
  PUBLIC
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
    DATA mo_alv TYPE REF TO zcl_uitb_alv.
    DATA mo_parent_container TYPE REF TO cl_gui_container.
    DATA mo_parent_view TYPE REF TO zif_uitb_gui_composite_view.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fc_generic_alv IMPLEMENTATION.
  METHOD constructor.
    mo_parent_container = io_parent_container.
    mo_parent_view = io_parent_view.
  ENDMETHOD.

ENDCLASS.
