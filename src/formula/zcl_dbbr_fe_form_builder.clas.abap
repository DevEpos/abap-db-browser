CLASS ZCL_DBBR_fe_form_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_builder_for_check
      IMPORTING
        io_formula        TYPE REF TO ZCL_DBBR_formula
        io_tabfields      type ref to ZCL_DBBR_tabfield_list
      RETURNING
        VALUE(rr_builder) TYPE REF TO ZIF_DBBR_fe_formula_builder.
    CLASS-METHODS get_builder_for_subroutine_gen
      IMPORTING
        ir_formula        TYPE REF TO ZCL_DBBR_formula
        ir_tabfields type ref to ZCL_DBBR_tabfield_list
        it_tab_components type ZDBBR_abap_comp_type_itab
      RETURNING
        VALUE(rr_builder) TYPE REF TO ZIF_DBBR_fe_formula_builder.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS ZCL_DBBR_fe_form_builder IMPLEMENTATION.

  METHOD get_builder_for_check.
    rr_builder = new ZCL_DBBR_fe_bldr_for_checks(
        ir_formula   = io_formula
        ir_tabfields = io_tabfields
    ).
  ENDMETHOD.

  METHOD get_builder_for_subroutine_gen.
    rr_builder = new ZCL_DBBR_fe_bldr_for_subroutn(
        ir_formula    = ir_formula
        ir_tabfields  = ir_tabfields
        it_comp_types = it_tab_components
    ).
  ENDMETHOD.

ENDCLASS.
