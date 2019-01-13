*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_grid_grabber DEFINITION
INHERITING FROM cl_salv_model_list.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_alv_model TYPE REF TO cl_salv_model.
    METHODS get_grid
      RETURNING
        VALUE(rr_grid) TYPE REF TO cl_gui_alv_grid.
  PRIVATE SECTION.
    DATA mr_salv_model TYPE REF TO cl_salv_model.
    data mr_grid type ref to cl_gui_alv_grid.
ENDCLASS.
