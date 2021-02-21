FUNCTION ZDBBR_DBENTITY_SH_EXIT.
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
  IF callcontrol-step = 'SELECT'.
    DATA: lt_entity_range       TYPE RANGE OF zsat_entity_id,
          lt_package_range      TYPE RANGE OF devclass,
          lt_description_selopt TYPE RANGE OF ddtext.

    LOOP AT shlp-selopt ASSIGNING FIELD-SYMBOL(<ls_selopt>).

      IF <ls_selopt>-shlpfield = 'ENTITY_ID'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_entity_range.
      ELSEIF <ls_selopt>-shlpfield = 'DDTEXT'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_description_selopt.
      ELSEIF <ls_selopt>-shlpfield = 'DEVCLASS'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_package_range.
      ENDIF.

    ENDLOOP.

    DATA: lt_result TYPE TABLE OF zdbbr_entity_sh_result.

    SELECT entityraw AS entity_id, description AS ddtext, developmentpackage AS devclass
      FROM zsat_i_databaseentity
      WHERE entity IN @lt_entity_range
        AND developmentpackage IN @lt_package_range
        AND description IN @lt_description_selopt
      ORDER BY entity ASCENDING
    INTO CORRESPONDING FIELDS OF TABLE @lt_result
      UP TO @callcontrol-maxrecords ROWS.


    IF sy-subrc = 0.

      CALL FUNCTION 'F4UT_RESULTS_MAP'
        EXPORTING
          source_structure = 'ZDBBR_ENTITY_SH_RESULT'
        TABLES
          shlp_tab         = shlp_tab
          record_tab       = record_tab
          source_tab       = lt_result
        CHANGING
          shlp             = shlp
          callcontrol      = callcontrol.
    ELSE.
      CLEAR record_tab[].
    ENDIF.
    callcontrol-step = 'DISP'.
  ENDIF.


ENDFUNCTION.
