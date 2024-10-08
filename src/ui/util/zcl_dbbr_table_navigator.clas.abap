CLASS zcl_dbbr_table_navigator DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_dbbr_navigator_creator.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_table_navigator.

    METHODS constructor
      IMPORTING
        ir_t_data       TYPE data
        iv_source_table TYPE tabname
        is_association  TYPE zsat_cds_association.

  PROTECTED SECTION.
    DATA mr_t_data TYPE REF TO data.
    DATA ms_association TYPE zsat_cds_association.
    DATA mv_source_table TYPE tabname.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_dbbr_table_navigator IMPLEMENTATION.
  METHOD constructor.
    mr_t_data = ir_t_data.
    mv_source_table = iv_source_table.
    ms_association = is_association.
  ENDMETHOD.

  METHOD zif_dbbr_table_navigator~navigate.
  ENDMETHOD.
ENDCLASS.
