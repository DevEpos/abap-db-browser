"! <p class="shorttext synchronized" lang="en">DB Browser exit for calculated fields</p>
INTERFACE zif_dbbr_exit_calc_field
  PUBLIC.

  INTERFACES zif_dbbr_exit.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Adds calculated fields</p>
    add_fields
      IMPORTING
        io_field_adder TYPE REF TO zif_dbbr_calc_field_adder,
    "! <p class="shorttext synchronized" lang="en">Fill calculated fields</p>
    calulate
      CHANGING
        it_original_data   TYPE STANDARD TABLE
        ct_calculated_data TYPE STANDARD TABLE.
ENDINTERFACE.
