class ZCL_DBBR_query_SELECTION_UTIL definition
  public
  inheriting from ZCL_DBBR_TABLE_SELECTION_UTIL
  final
  create public .

public section.

  methods BUILD_SIMPLE_ALV_TITLE
    redefinition .
  methods INIT
    redefinition .
  methods GET_ENTITY_NAME
    redefinition .
protected section.
private section.

  data MV_query_NAME type ZDBBR_query_NAME .
  data MV_query_DESCR type DDTEXT .
ENDCLASS.



CLASS ZCL_DBBR_QUERY_SELECTION_UTIL IMPLEMENTATION.


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
        iv_query_name     = mv_entity_id
        if_load_completely = abap_false
    ).
    mv_query_name = ls_query-query_name.
    mv_query_descr = ls_query-description.

    ms_control_info-primary_table = ls_query-primary_table.

*... fill jump destination fields
    mt_jumpdest = NEW zcl_dbbr_jump_destination_f( )->get_jump_destinations( iv_query_id = ls_query-query_id ).
    DELETE mt_jumpdest WHERE is_active = abap_false.
  ENDMETHOD.
ENDCLASS.
