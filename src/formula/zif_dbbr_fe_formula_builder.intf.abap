INTERFACE zif_dbbr_fe_formula_builder
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en">Builds lines of code for formula</p>
  "!
  METHODS build_formula
    EXPORTING
      et_lines         TYPE string_table
      ev_starting_line TYPE i.
ENDINTERFACE.
