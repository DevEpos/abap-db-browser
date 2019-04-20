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
    METHODS read_entity_infos
        REDEFINITION.
  PRIVATE SECTION.

    DATA mv_query_name TYPE zdbbr_query_name .
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
        iv_query_name     = mv_entity_id
        if_load_completely = abap_false
    ).
    mv_query_name = ls_query-query_name.
    mv_query_descr = ls_query-description.

    ms_control_info-primary_table = ls_query-primary_table.

*... fill jump destination fields
    mt_jumpdest = NEW zcl_dbbr_jump_destination_f( )->get_jump_destinations( iv_query_id = ls_query-query_id ).
    DELETE mt_jumpdest WHERE is_active = abap_false.

    super->init( ).
  ENDMETHOD.

  METHOD read_entity_infos.
***    DATA(lv_special_group_count) = 1.
***
***    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( ms_control_info-primary_table ).
***
***    ms_control_info-primary_table_name = ls_table_info-ddtext.
***    ms_control_info-client_dependent = ls_table_info-clidep.
***    ms_control_info-primary_table_tabclass = ls_table_info-tabclass.
***
***    DATA(lv_sp_group) = CONV lvc_spgrp( c_col_group_prefix && lv_special_group_count ).
***    mt_column_groups = VALUE #(
***      ( sp_group = lv_sp_group
***        text     = `Table - ` && ms_control_info-primary_table )
***    ).
***
***    mt_group_tab_map = VALUE #( ( sp_group = lv_sp_group tabname = ms_control_info-primary_table ) ).
***
***    ADD 1 TO lv_special_group_count.
***
***    " update join table names
***    IF ms_join_def-tables IS NOT INITIAL.
***      LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE table_name IS INITIAL.
***        <ls_join_table>-table_name = zcl_dbbr_dictionary_helper=>get_table_info( <ls_join_table>-add_table )-ddtext.
***        lv_sp_group = c_col_group_prefix && lv_special_group_count.
***        mt_column_groups = VALUE #(
***          BASE mt_column_groups
***          ( sp_group = lv_sp_group
***            text     = `Table - ` && <ls_join_table>-add_table )
***        ).
***        mt_group_tab_map = VALUE #(
***          BASE mt_group_tab_map
***          ( sp_group = lv_sp_group
***            tabname  = <ls_join_table>-add_table )
***        ).
***        ADD 1 TO lv_special_group_count.
***      ENDLOOP.
***    ELSE.
***      CLEAR mt_column_groups.
***    ENDIF.
  ENDMETHOD.
ENDCLASS.
