"! <p class="shorttext synchronized" lang="en">Tree for managing the fields for output/selection/sorting</p>
CLASS zcl_dbbr_field_control_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_view.

    "! <p class="shorttext synchronized" lang="en">Create new instance of control tree</p>
    "!
    METHODS constructor
      IMPORTING
        io_parent_container TYPE REF TO cl_gui_container
        io_parent_view      TYPE REF TO zif_uitb_gui_composite_view.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_parent_container TYPE REF TO cl_gui_container.
    DATA mo_parent_view TYPE REF TO zif_uitb_gui_composite_view.
ENDCLASS.



CLASS zcl_dbbr_field_control_tree IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

ENDCLASS.
