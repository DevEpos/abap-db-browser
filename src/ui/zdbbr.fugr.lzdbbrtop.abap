FUNCTION-POOL zdbbr.             "MESSAGE-ID ..

" global variables
TABLES: varid, varit, dd02l, sscrfields.

TYPE-POOLS: slis.

**********************************************************************
" DEFINITION OF DATA
**********************************************************************
DATA: gs_data TYPE zdbbr_global_data. " global data for ZDBBR
DATA gv_value TYPE zdbbr_value.
DATA: gv_selfield_count TYPE char26.
DATA: gs_user_settings TYPE zdbbr_usrsettng.
DATA: gv_entity_name         TYPE tabname,
      gv_entity_type         TYPE char20,
      gv_entity_type_sh      TYPE zdbbr_entity_type,
      gv_selmask_entity_text TYPE ddtext.

DATA: push TYPE zdbbr_button.
DATA: expand TYPE zdbbr_button.
DATA: option TYPE zdbbr_button.
DATA: option_template TYPE zdbbr_button.
DATA: subquery TYPE zdbbr_button.
*data: calc_field type ZDBBR_button.

DATA: gt_selection_fields       TYPE zdbbr_selfield_itab,
      gt_multi_or               TYPE zdbbr_selfield_itab,
      gs_multi_or               TYPE zdbbr_selfield,
      gv_or_tuple_number        TYPE tswpos,
      gs_selfields              TYPE zdbbr_selfield,
      gv_selfields_title        TYPE char128,
      gt_selection_fields_multi TYPE zdbbr_selfield_itab,
      gv_linecount              LIKE sy-tabix,
      gt_multi_select           TYPE zdbbr_selfield_itab,
      gs_multi_select           TYPE zdbbr_selfield,
      gv_fieldname              TYPE fieldname,
      gv_field_descr            TYPE scrtext_m,
      gv_multi_select_lines     LIKE sy-tabix,
      gv_option_template        TYPE se16n_option.

DATA: gt_sel_init TYPE zdbbr_selopt_control_itab.

FIELD-SYMBOLS: <gt_table> TYPE table.
DATA: gr_dynamic_table TYPE REF TO data.

DATA:
  BEGIN OF gs_object_search,
    search_type  TYPE zdbbr_obj_browser_mode,
    search_input TYPE string,
  END OF gs_object_search.

DATA: gr_alv_grid         TYPE REF TO cl_gui_alv_grid,
      gr_custom_container TYPE REF TO cl_gui_custom_container.

DATA: gs_layout    TYPE lvc_s_layo.
DATA: gs_variant TYPE disvariant.
DATA: gs_print         TYPE slis_print_alv.

" data for saving custom search help
DATA: gs_custom_search_help TYPE zdbbr_custom_searchhelp_ui.

" data for saving built in f4 help
DATA: gs_built_in_f4 TYPE zdbbr_build_in_f4_ui_data.

" OK codes
DATA: ok_code         LIKE sy-ucomm,
      save_ok_code    LIKE sy-ucomm,
      ok_code_multi   LIKE sy-ucomm,
      save_code_multi LIKE sy-ucomm.

" dynamic texts for gui status
DATA: gs_sel_text_gui_text          TYPE smp_dyntxt,
      multi_or_icon                 TYPE smp_dyntxt,
      gs_sel_text_gui_queryeditshow TYPE smp_dyntxt,
      entity_top_menu               TYPE smp_dyntxt,
      save_dyn                      TYPE smp_dyntxt,
      savenew_dyn                   TYPE smp_dyntxt,
      entityfc01                    TYPE smp_dyntxt,
      entityfc02                    TYPE smp_dyntxt,
      entityfc03                    TYPE smp_dyntxt,
      entityfc04                    TYPE smp_dyntxt,
      entityfc05                    TYPE smp_dyntxt,
      entityfc11                    TYPE smp_dyntxt,
      entityfc12                    TYPE smp_dyntxt.

""" data for query manager
**********************************************************************
DATA: gv_tree_screen TYPE sy-dynnr,
      gv_active_tab  TYPE sy-ucomm.

""" data for first screen input (query / table mode)
**********************************************************************
DATA: gs_browser_mode TYPE zdbbr_browser_mode_data,
      gs_entity_info  TYPE zdbbr_entity_info_simple,
      gs_entity       TYPE zdbbr_browser_mode_data.

""" data for alternative columns texts
DATA: gt_altcoltext TYPE zdbbr_altcoltext_data_itab,
      gs_altcoltext TYPE zdbbr_altcoltext_data.
**********************************************************************
" CONTROLS FOR DYNPROS
**********************************************************************
" Table controls
CONTROLS:
  selfields_tc  TYPE TABLEVIEW USING SCREEN 0100,
  multi_tc      TYPE TABLEVIEW USING SCREEN 0001,
  multi_or_tc   TYPE TABLEVIEW USING SCREEN 0106,
  altcoltext_tc TYPE TABLEVIEW USING SCREEN 0108,
  tree_tab      TYPE TABSTRIP.

DATA: filter_fields TYPE zdbbr_button.
" master include for local classes

INCLUDE lzdbbrd01.

**********************************************************************
" GLOBAL CLASS INSTANCES FOR SCREEN HANDLING
**********************************************************************
DATA: gr_tabfield_manager           TYPE REF TO zcl_dbbr_tabfield_manager,
      gr_edit_join_table_controller TYPE REF TO zcl_dbbr_edit_join_table_ctrl,
      gr_edit_join_cond_view        TYPE REF TO zcl_dbbr_edit_join_cond_view,
      gr_choose_entity_controller   TYPE REF TO zcl_dbbr_choose_object_ctrl,
      gr_f4_screen_controller       TYPE REF TO zcl_dbbr_generic_f4_sc,
      gr_multi_select_controller    TYPE REF TO zcl_dbbr_multi_select_ctlr,
      gr_multi_select_table         TYPE REF TO zcl_dbbr_multi_select_table,
      gr_selscreen_controller       TYPE REF TO zcl_dbbr_selscreen_controller,
      gr_selscreen_table            TYPE REF TO zcl_dbbr_selscreen_table,
      gr_sort_controller            TYPE REF TO zcl_dbbr_field_sorter_ctrl,
      gr_save_query_controller      TYPE REF TO zcl_dbbr_save_query_ctrl,
      gr_save_sql_query_controller  TYPE REF TO zcl_dbbr_save_sql_query_ctrl,
      gr_variant_controller         TYPE REF TO zcl_dbbr_variant_controller,
      gr_multi_or_table             TYPE REF TO zcl_dbbr_multi_or_table,
      gr_multi_or_controller        TYPE REF TO zcl_dbbr_multi_or_controller,
      gr_addtextfield_controller    TYPE REF TO zcl_dbbr_addtextfield_ctrl,
      gr_copy_query_controller      TYPE REF TO zcl_dbbr_copy_query_ctrl,
      gr_altcoltext_controller      TYPE REF TO zcl_dbbr_altcoltxt_controller,
      gr_obj_brws_search_enter_ctrl TYPE REF TO zcl_dbbr_obj_brws_search_sc,
      gr_altcoltext_table           TYPE REF TO zcl_dbbr_altcoltext_table.

**********************************************************************
* SELECTION SCREENS
**********************************************************************
" selection screen for saving a query
SELECTION-SCREEN BEGIN OF SCREEN 1100 TITLE TEXT-t01 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK query.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) TEXT-t07 FOR FIELD p_scrnam.
SELECTION-SCREEN POSITION 25.
PARAMETERS: p_scrnam TYPE zdbbr_query_name OBLIGATORY.
SELECTION-SCREEN POSITION 60.
PARAMETERS: p_xglob TYPE zdbbr_is_global_query AS CHECKBOX MODIF ID glo.
SELECTION-SCREEN COMMENT 65(10) TEXT-t08 FOR FIELD p_xglob MODIF ID glo.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK query.
SELECTION-SCREEN BEGIN OF BLOCK query_descr.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) TEXT-t12 FOR FIELD p_scrdec.
SELECTION-SCREEN POSITION 25.
PARAMETERS: p_scrdec TYPE ddtext OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK query_descr.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK user_param WITH FRAME TITLE TEXT-t02 NO INTERVALS.
PARAMETERS: p_xsort  TYPE zdbbr_defined_sortfields AS CHECKBOX MODIF ID par,
            p_xfield TYPE zdbbr_defined_outputfields AS CHECKBOX MODIF ID par,
            p_xfiltv TYPE zdbbr_has_filter_values AS CHECKBOX MODIF ID par.
SELECTION-SCREEN END OF BLOCK user_param.

SELECTION-SCREEN END OF SCREEN 1100.

" selection screen for defining / reading / deleting variant for table
SELECTION-SCREEN BEGIN OF SCREEN 1200 TITLE tvar_t.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-t14 FOR FIELD p_tab.
SELECTION-SCREEN POSITION 25.
PARAMETERS: p_tab TYPE tabname16.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK variant.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-t13 FOR FIELD p_varnam.
SELECTION-SCREEN POSITION 25.
PARAMETERS p_varnam TYPE zdbbr_variant_name OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-t12 FOR FIELD p_vartxt MODIF ID txt.
SELECTION-SCREEN POSITION 25.
PARAMETERS p_vartxt TYPE ddtext MODIF ID txt.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK variant.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN INCLUDE BLOCKS user_param.
SELECTION-SCREEN END OF SCREEN 1200.

" selection screen for defining / reading / deleting variant for query
SELECTION-SCREEN BEGIN OF SCREEN 1201 TITLE svar_t.
SELECTION-SCREEN INCLUDE BLOCKS query.
SELECTION-SCREEN INCLUDE BLOCKS variant.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN INCLUDE BLOCKS user_param.
SELECTION-SCREEN END OF SCREEN 1201.

" Selection screen for defining / reading / deleting variant for cds view
SELECTION-SCREEN BEGIN OF SCREEN 1202 TITLE cvar_t.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-t23 FOR FIELD p_cdsnam.
SELECTION-SCREEN POSITION 25.
PARAMETERS: p_cdsnam TYPE zdbbr_cds_view_name OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN INCLUDE BLOCKS variant.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN INCLUDE BLOCKS user_param.
SELECTION-SCREEN END OF SCREEN 1202.

" selection screen for creating a manual text field for an id field or data element
SELECTION-SCREEN BEGIN OF SCREEN 1300 TITLE TEXT-t04 AS WINDOW.

SELECTION-SCREEN BEGIN OF BLOCK tab_source WITH FRAME TITLE TEXT-t05 NO INTERVALS.

*SELECTION-SCREEN BEGIN OF BLOCK tt_table WITH FRAME TITLE text-t18 NO INTERVALS.
PARAMETERS: p_idtab  TYPE tabname MODIF ID con OBLIGATORY,
            p_idfld  TYPE fieldname VISIBLE LENGTH 20 MODIF ID con OBLIGATORY,
            p_idfld2 TYPE fieldname VISIBLE LENGTH 20 MODIF ID con.
*SELECTION-SCREEN END OF BLOCK tt_table.

SELECTION-SCREEN END OF BLOCK tab_source.

SELECTION-SCREEN BEGIN OF BLOCK cond_tab WITH FRAME TITLE TEXT-t15 NO INTERVALS.
PARAMETERS: p_condf TYPE fieldname MODIF ID con,
            p_condo TYPE zdbbr_addtext-condition_op MODIF ID con,
            p_condv TYPE zdbbr_addtext_cond_val MODIF ID con.
SELECTION-SCREEN END OF BLOCK cond_tab.
SELECTION-SCREEN BEGIN OF BLOCK text_tab WITH FRAME TITLE TEXT-t06 NO INTERVALS.
PARAMETERS: p_txttab TYPE tabname OBLIGATORY,
            p_keyfld TYPE fieldname VISIBLE LENGTH 20 OBLIGATORY,
            p_keyfd2 TYPE fieldname VISIBLE LENGTH 20,
            p_txtfld TYPE fieldname VISIBLE LENGTH 20 OBLIGATORY,
            p_lngfld TYPE fieldname VISIBLE LENGTH 20.
SELECTION-SCREEN END OF BLOCK text_tab.
SELECTION-SCREEN END OF SCREEN 1300.

" selection screen for copying a query
SELECTION-SCREEN BEGIN OF SCREEN 1400 TITLE TEXT-t09 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK source_query WITH FRAME TITLE TEXT-t10.
SELECTION-SCREEN INCLUDE BLOCKS query.
SELECTION-SCREEN INCLUDE BLOCKS query_descr.
SELECTION-SCREEN END OF BLOCK source_query.
SELECTION-SCREEN BEGIN OF BLOCK target_query WITH FRAME TITLE TEXT-t11.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) TEXT-t07 FOR FIELD p_scrnt.
SELECTION-SCREEN POSITION 25.
PARAMETERS: p_scrnt TYPE zdbbr_query_name OBLIGATORY.
SELECTION-SCREEN POSITION 60.
PARAMETERS: p_xglobt TYPE zdbbr_is_global_query AS CHECKBOX.
SELECTION-SCREEN COMMENT 65(10) TEXT-t08 FOR FIELD p_xglobt.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) TEXT-t12 FOR FIELD p_scrdtg.
SELECTION-SCREEN POSITION 25.
PARAMETERS: p_scrdtg TYPE ddtext OBLIGATORY.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_copyv TYPE boolean AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK target_query.
SELECTION-SCREEN END OF SCREEN 1400.

" assign searchhelp to
SELECTION-SCREEN BEGIN OF SCREEN 1600 TITLE TEXT-t17.
SELECTION-SCREEN BEGIN OF BLOCK assign WITH FRAME TITLE TEXT-010.
PARAMETERS: p_f4ktab TYPE tabname.
PARAMETERS: p_f4kfld TYPE fieldname.
PARAMETERS: p_f4rlln TYPE rollname.
PARAMETERS: p_f4tab TYPE tabname.
PARAMETERS: p_f4fld TYPE fieldname.
SELECTION-SCREEN END OF BLOCK assign.
SELECTION-SCREEN END OF SCREEN 1600.

*... Choose/Edit Table for Join definition
SELECTION-SCREEN BEGIN OF SCREEN 1700 TITLE TEXT-t24 AS WINDOW.

SELECTION-SCREEN BEGIN OF BLOCK jointab WITH FRAME TITLE TEXT-b01. " NO INTERVALS.
PARAMETERS: p_jointb TYPE tabname MATCHCODE OBJECT zdbbr_dbentity_sh OBLIGATORY.
***PARAMETERS: p_jointb TYPE tabname MATCHCODE OBJECT zdbbr_dbtab_sh OBLIGATORY.
PARAMETERS: p_jointa TYPE zdbbr_entity_alias MODIF ID exp.
SELECTION-SCREEN END OF BLOCK jointab.

SELECTION-SCREEN BEGIN OF BLOCK jointabdef WITH FRAME TITLE TEXT-b02. " NO INTERVALS.
PARAMETERS: p_jointy TYPE zdbbr_jointype AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY DEFAULT zif_dbbr_c_join_types=>inner_join,
            p_xjvirt TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK jointabdef.

SELECTION-SCREEN END OF SCREEN 1700.

*... Create/Edit Join Condition (Field or Value condition)
SELECTION-SCREEN BEGIN OF SCREEN 1701 TITLE joincond AS WINDOW.

SELECTION-SCREEN BEGIN OF BLOCK sourcefield WITH FRAME TITLE TEXT-b03. " NO INTERVALS.
PARAMETERS: p_srcfld TYPE fieldname.
PARAMETERS: p_srctab TYPE zdbbr_entity_alias.
PARAMETERS: p_srcdtp TYPE datatype_d MODIF ID off.
PARAMETERS: p_srclng TYPE ddleng MODIF ID off.
SELECTION-SCREEN END OF BLOCK sourcefield.

SELECTION-SCREEN BEGIN OF BLOCK compare1 WITH FRAME TITLE TEXT-b04. " NO INTERVALS.
PARAMETERS: p_comp1 TYPE voperator AS LISTBOX VISIBLE LENGTH 25 USER-COMMAND cmp1chng OBLIGATORY.
SELECTION-SCREEN END OF BLOCK compare1.

SELECTION-SCREEN BEGIN OF BLOCK values WITH FRAME TITLE TEXT-b05. " NO INTERVALS.
PARAMETERS: p_valty TYPE zdbbr_join_cond_value_type AS LISTBOX VISIBLE LENGTH 20 MODIF ID val USER-COMMAND valtypchanged.
PARAMETERS: p_val1 TYPE zdbbr_value MODIF ID val.
SELECTION-SCREEN COMMENT /5(20) TEXT-t25 MODIF ID vl2.
PARAMETERS: p_val2 TYPE zdbbr_value MODIF ID vl2.
SELECTION-SCREEN END OF BLOCK values.

SELECTION-SCREEN BEGIN OF BLOCK targetfield WITH FRAME TITLE TEXT-b06. " NO INTERVALS.
PARAMETERS: p_trgfld TYPE fieldname MODIF ID trg.
PARAMETERS: p_trgtab TYPE zdbbr_entity_alias MODIF ID trg.
PARAMETERS: p_trgdtp TYPE datatype_d MODIF ID off.
PARAMETERS: p_trglng TYPE ddleng MODIF ID off.
SELECTION-SCREEN END OF BLOCK targetfield.

SELECTION-SCREEN BEGIN OF BLOCK offset_to_source WITH FRAME TITLE TEXT-b07. " NO INTERVALS.
PARAMETERS: p_tfldof TYPE doffset MODIF ID off.
PARAMETERS: p_tfldol TYPE ddleng MODIF ID off.
SELECTION-SCREEN END OF BLOCK offset_to_source.

SELECTION-SCREEN END OF SCREEN 1701.

*... Create SQL Query
" selection screen for saving a query
SELECTION-SCREEN BEGIN OF SCREEN 1800 TITLE TEXT-t26 AS WINDOW.
SELECTION-SCREEN INCLUDE BLOCKS query.
SELECTION-SCREEN INCLUDE BLOCKS query_descr.
SELECTION-SCREEN END OF SCREEN 1800.
