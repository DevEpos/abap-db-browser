FUNCTION ZDBBR_GENERIC_SH_EXIT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PARAMETER_OPTIMIZE) TYPE  C OPTIONAL
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR_T
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------
  IF callcontrol-step = 'SELONE' OR
     callcontrol-step = 'PRESEL1'.
    zcl_dbbr_entity_sh_helper=>control_list_select_params(
      EXPORTING iv_entity_type = gv_entity_type
      CHANGING  cs_search_help = shlp
    ).
  ELSEIF callcontrol-step = 'SELECT'.
    NEW zcl_dbbr_entity_sh_helper( iv_entity_type = gv_entity_type )->select_and_fill_result(
      CHANGING
        cs_shlp        = shlp
        cs_callcontrol = callcontrol
        ct_shlp_tab    = shlp_tab[]
        ct_records     = record_tab[]
    ).
  ENDIF.

ENDFUNCTION.
