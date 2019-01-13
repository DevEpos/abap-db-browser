CLASS zcl_dbbr_navigator_creator DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create_cds_navigator
      IMPORTING
        !ir_t_data           TYPE REF TO data
        is_tech_info         TYPE zdbbr_tech_info
        !ir_source_cds_view  TYPE REF TO zcl_dbbr_cds_view
        !it_source_index     TYPE lvc_t_indx
        !ir_source_tabfields TYPE REF TO zcl_dbbr_tabfield_list
        !is_association      TYPE zdbbr_cds_association
        !it_nav_breadcrumbs  TYPE zdbbr_string_t
        !iv_nav_count        TYPE i
      RETURNING
        VALUE(rr_navigator)  TYPE REF TO zif_dbbr_table_navigator .
    CLASS-METHODS create_table_navigator
      IMPORTING
        !ir_t_data          TYPE REF TO data
        !iv_source_table    TYPE tabname
        !is_association     TYPE zdbbr_cds_association
      RETURNING
        VALUE(rr_navigator) TYPE REF TO zif_dbbr_table_navigator .
  PROTECTED SECTION.

    DATA mr_t_data TYPE REF TO data .
    DATA mv_source_entity_id TYPE zdbbr_entity_id .
    DATA ms_association TYPE zdbbr_cds_association.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_NAVIGATOR_CREATOR IMPLEMENTATION.


  METHOD create_cds_navigator.
    rr_navigator = NEW zcl_dbbr_cds_navigator(
        ir_t_data             = ir_t_data
        is_tech_info          = is_tech_info
        ir_source_cds_view    = ir_source_cds_view
        it_source_index       = it_source_index
        ir_source_tabfields   = ir_source_tabfields
        is_association        = is_association
        it_nav_breadcrumbs    = it_nav_breadcrumbs
        iv_nav_count          = iv_nav_count
    ).
  ENDMETHOD.


  METHOD create_table_navigator.
    rr_navigator = NEW zcl_dbbr_table_navigator(
        ir_t_data             = ir_t_data
        iv_source_table       = iv_source_table
        is_association        = is_association
    ).
  ENDMETHOD.
ENDCLASS.
