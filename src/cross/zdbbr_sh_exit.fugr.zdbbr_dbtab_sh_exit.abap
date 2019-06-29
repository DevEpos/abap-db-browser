FUNCTION ZDBBR_DBTAB_SH_EXIT.
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
    DATA: lt_tabname_selopt     TYPE RANGE OF tabname,
          lt_language_selopt    TYPE RANGE OF ddlanguage,
          lt_package_selopt     TYPE RANGE OF devclass,
          lt_description_selopt TYPE RANGE OF ddtext.

    LOOP AT shlp-selopt ASSIGNING FIELD-SYMBOL(<ls_selopt>).

      IF <ls_selopt>-shlpfield = 'TABNAME'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_tabname_selopt.
      ELSEIF <ls_selopt>-shlpfield = 'DDTEXT'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_description_selopt.
      ELSEIF <ls_selopt>-shlpfield = 'DEVCLASS'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_package_selopt.
      ENDIF.

    ENDLOOP.

    DATA: lt_result TYPE STANDARD TABLE OF zdbbr_db_tab_sh_result.
    DATA(lv_language) = zcl_dbbr_system_helper=>get_system_language( ).

    SELECT table~tabname, devclass, ddlanguage, ddtext
      FROM zdbbrdbtab_v AS table
        LEFT OUTER JOIN dd02t AS text
          ON table~tabname = text~tabname
         AND text~ddlanguage = @lv_language
      WHERE table~tabname IN @lt_tabname_selopt
        AND text~ddtext IN @lt_description_selopt
        AND devclass IN @lt_package_selopt
      ORDER BY table~tabname
    into corresponding fields of table @lt_result
      UP TO 500 ROWS.


    IF sy-subrc = 0.

      CALL FUNCTION 'F4UT_RESULTS_MAP'
        EXPORTING
          source_structure = 'ZDBBR_DB_TAB_SH_RESULT'
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
