FUNCTION ZDBBR_CDS_VIEW_SH_EXIT.
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
  DATA: lt_cds_view           TYPE STANDARD TABLE OF zdbbr_cds_view,
        lt_entity_selopt      TYPE RANGE OF tabname,
        lt_description_selopt TYPE RANGE OF ddtext.

  IF callcontrol-step = 'SELECT'.


    LOOP AT shlp-selopt ASSIGNING FIELD-SYMBOL(<ls_selopt>).

      IF <ls_selopt>-shlpfield = 'ENTITY_NAME'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_entity_selopt.
      ELSEIF <ls_selopt>-shlpfield = 'DDTEXT'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_description_selopt.
      ENDIF.

    ENDLOOP.

    " The result is not sorted, because sorting is performed by the UI
    data(lt_cds_search_result) = ZCL_SAT_CDS_VIEW_FACTORY=>find_cds_views(
       iv_cds_view_name = VALUE #( lt_entity_selopt[ 1 ]-low OPTIONAL )
       iv_description   = VALUE #( lt_description_selopt[ 1 ]-low OPTIONAL )
       iv_max_rows      = callcontrol-maxrecords
    ).

    lt_cds_view = CORRESPONDING #( lt_cds_search_result mapping ddtext      = description
                                                                entity_name = entity_id_raw ).
    IF sy-subrc = 0.
      CALL FUNCTION 'F4UT_RESULTS_MAP'
        EXPORTING
          source_structure = 'ZDBBR_CDS_VIEW'
        TABLES
          shlp_tab         = shlp_tab
          record_tab       = record_tab
          source_tab       = lt_cds_view
        CHANGING
          shlp             = shlp
          callcontrol      = callcontrol.
    ELSE.
      CLEAR: record_tab[].
    ENDIF.
    callcontrol-step = 'DISP'.

  ENDIF.

ENDFUNCTION.
