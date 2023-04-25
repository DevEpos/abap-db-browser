CLASS zcl_dbbr_query_selection_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_join_selection_util
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS build_simple_alv_title
        REDEFINITION .
    METHODS init
        REDEFINITION .
    METHODS get_entity_name
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_query_name TYPE zsat_query_name .
    DATA mv_query_descr TYPE ddtext .
ENDCLASS.



CLASS zcl_dbbr_query_selection_util IMPLEMENTATION.

  METHOD build_simple_alv_title.
    result = |Query - { mv_query_name } - { mv_query_descr }|.
  ENDMETHOD.


  METHOD get_entity_name.
    result = mv_query_name.
  ENDMETHOD.


  METHOD init.
*... read the query description
    DATA(lr_query_f) = NEW zcl_dbbr_query_factory( ).
    DATA(ls_query) = lr_query_f->get_query(
      iv_query_name      = mv_entity_id
      if_load_completely = abap_false ).
    mv_query_name = ls_query-query_name.
    mv_query_descr = ls_query-description.

    ms_control_info-primary_table = ls_query-primary_table.

*... fill jump destination fields
    mt_jumpdest = NEW zcl_dbbr_jump_destination_f( )->get_jump_destinations( iv_query_id = ls_query-query_id ).
    DELETE mt_jumpdest WHERE is_active = abap_false.

    super->init( ).
  ENDMETHOD.

ENDCLASS.
