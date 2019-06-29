CLASS zcl_dbbr_entity_sh_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_v_entity_type TYPE string VALUE 'GV_ENTITY_TYPE' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !iv_entity_type TYPE zdbbr_entity_type .
    METHODS select_and_fill_result
      CHANGING
        !cs_shlp        TYPE shlp_descr
        !cs_callcontrol TYPE ddshf4ctrl
        !ct_shlp_tab    TYPE shlp_desct
        !ct_records     TYPE ddshreslts .
    CLASS-METHODS control_list_select_params
      IMPORTING
        !iv_entity_type TYPE zdbbr_entity_type
      CHANGING
        !cs_search_help TYPE shlp_descr .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_entity_type TYPE zdbbr_entity_type .
ENDCLASS.



CLASS zcl_dbbr_entity_sh_helper IMPLEMENTATION.


  METHOD constructor.
    mv_entity_type = iv_entity_type.
  ENDMETHOD.


  METHOD control_list_select_params.
    FIELD-SYMBOLS: <ls_entity_id_field> TYPE dfies.

    IF iv_entity_type = zif_dbbr_c_entity_type=>query.

      ASSIGN cs_search_help-fieldprop[ fieldname = 'DEVCLASS' ] TO FIELD-SYMBOL(<ls_package_field>).
      CLEAR: <ls_package_field>-shlplispos,
             <ls_package_field>-shlpselpos.
    ENDIF.

    ASSIGN cs_search_help-fieldprop[ fieldname = 'DDLANGUAGE' ] TO FIELD-SYMBOL(<ls_language>).
    CLEAR: <ls_language>-shlplispos.

    ASSIGN cs_search_help-fielddescr[ fieldname = 'ENTITY_ID' ] TO <ls_entity_id_field>.

    CASE iv_entity_type.
      WHEN zif_dbbr_c_entity_type=>table.
        cs_search_help-intdescr-title = 'Search Help for DB Table/View'(001).
        <ls_entity_id_field>-scrtext_s =
        <ls_entity_id_field>-scrtext_m =
        <ls_entity_id_field>-scrtext_l =
        <ls_entity_id_field>-fieldtext =
        <ls_entity_id_field>-reptext   = 'Tabname'(006).

      WHEN zif_dbbr_c_entity_type=>query.
        cs_search_help-intdescr-title = 'Search Help for query'(002).
        <ls_entity_id_field>-scrtext_s =
        <ls_entity_id_field>-scrtext_m =
        <ls_entity_id_field>-scrtext_l =
        <ls_entity_id_field>-fieldtext =
        <ls_entity_id_field>-reptext   = 'Query Name'(003).

      WHEN zif_dbbr_c_entity_type=>cds_view.
        cs_search_help-intdescr-title = 'Search Help for CDS Views'(004).
        <ls_entity_id_field>-scrtext_s =
        <ls_entity_id_field>-scrtext_m =
        <ls_entity_id_field>-scrtext_l =
        <ls_entity_id_field>-fieldtext =
        <ls_entity_id_field>-reptext   = 'CDS View'(005).

    ENDCASE.
  ENDMETHOD.


  METHOD select_and_fill_result.
*........ Ranges for Table selection
    DATA: lt_entity_id_selopt   TYPE RANGE OF zdbbr_entity_id,
          lt_result             TYPE STANDARD TABLE OF zdbbr_entity_sh_result,
          lt_package_selopt     TYPE RANGE OF devclass,
          lt_description_selopt TYPE RANGE OF ddtext.


*.. 1) fill range tables
    LOOP AT cs_shlp-selopt ASSIGNING FIELD-SYMBOL(<ls_selopt>).

      IF <ls_selopt>-shlpfield = 'ENTITY_ID'.
        APPEND VALUE #( sign   = <ls_selopt>-sign
                        option = <ls_selopt>-option
                        low    = <ls_selopt>-low
                        high   = <ls_selopt>-high ) TO lt_entity_id_selopt.
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

*.. 2) Retrieve the current language key for which the texts should be selected
    DATA(lv_language) = zcl_dbbr_system_helper=>get_system_language( ).

*.. 3) determine for which type the search help should be called
    CASE mv_entity_type.

      WHEN zif_dbbr_c_entity_type=>table.
        SELECT entity AS entity_id, developmentpackage AS devclass, language AS ddlanguage, description AS ddtext
          FROM zdbbr_i_databaseentity( p_language = @lv_language )
          WHERE entity IN @lt_entity_id_selopt
            AND (    type = @zif_dbbr_c_entity_type=>table
                  OR type = @zif_dbbr_c_entity_type=>view )
            AND developmentpackage IN @lt_package_selopt
          ORDER BY entity
        INTO CORRESPONDING FIELDS OF TABLE @lt_result
          UP TO @cs_callcontrol-maxrecords ROWS.

      WHEN zif_dbbr_c_entity_type=>query.
        SELECT query_name AS entity_id, description AS ddtext
          FROM zdbbr_queryh
          WHERE query_name IN @lt_entity_id_selopt
           AND  description IN @lt_description_selopt
          ORDER BY query_name
        INTO CORRESPONDING FIELDS OF TABLE @lt_result
          UP TO @cs_callcontrol-maxrecords ROWS.

      WHEN zif_dbbr_c_entity_type=>cds_view.

        DATA(lt_cds_search_result) = zcl_dbbr_cds_view_factory=>find_cds_views(
           iv_cds_view_name = VALUE #( lt_entity_id_selopt[ 1 ]-low OPTIONAL )
           iv_description   = VALUE #( lt_description_selopt[ 1 ]-low OPTIONAL )
           iv_max_rows      = cs_callcontrol-maxrecords
         ).
        lt_result = CORRESPONDING #(
          lt_cds_search_result MAPPING ddtext    = description
                                       entity_id = entity_id_raw

        ).

    ENDCASE.

    IF lt_result IS NOT INITIAL.
*.... transfer result to result table
      CALL FUNCTION 'F4UT_RESULTS_MAP'
        EXPORTING
          source_structure = 'ZDBBR_ENTITY_SH_RESULT'
        TABLES
          shlp_tab         = ct_shlp_tab
          record_tab       = ct_records
          source_tab       = lt_result
        CHANGING
          shlp             = cs_shlp
          callcontrol      = cs_callcontrol.
    ELSE.
      CLEAR ct_records.
    ENDIF.

    cs_callcontrol-step = 'DISP'.

  ENDMETHOD.
ENDCLASS.
