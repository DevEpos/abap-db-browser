CLASS zcl_dbbr_fe_form_builder DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_builder_for_check
      IMPORTING
        io_formula        TYPE REF TO zcl_dbbr_formula
        io_tabfields      TYPE REF TO zcl_dbbr_tabfield_list
      RETURNING
        VALUE(rr_builder) TYPE REF TO zif_dbbr_fe_formula_builder.

    CLASS-METHODS get_builder_for_subroutine_gen
      IMPORTING
        ir_formula        TYPE REF TO zcl_dbbr_formula
        ir_tabfields      TYPE REF TO zcl_dbbr_tabfield_list
        it_tab_components TYPE zdbbr_abap_comp_type_itab
      RETURNING
        VALUE(rr_builder) TYPE REF TO zif_dbbr_fe_formula_builder.

ENDCLASS.


CLASS zcl_dbbr_fe_form_builder IMPLEMENTATION.
  METHOD get_builder_for_check.
    rr_builder = NEW zcl_dbbr_fe_bldr_for_checks( ir_formula   = io_formula
                                                  ir_tabfields = io_tabfields ).
  ENDMETHOD.

  METHOD get_builder_for_subroutine_gen.
    rr_builder = NEW zcl_dbbr_fe_bldr_for_subroutn( ir_formula    = ir_formula
                                                    ir_tabfields  = ir_tabfields
                                                    it_comp_types = it_tab_components ).
  ENDMETHOD.
ENDCLASS.
