*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS cl_query_option_validator IMPLEMENTATION.

  METHOD create_validator.
    CASE iv_type.

      WHEN zif_dbbr_c_object_browser_mode=>cds_view.
        rr_validator = NEW cl_qov_cds( ).

      WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
        rr_validator = NEW cl_qov_database_tab_view( ).

      WHEN zif_dbbr_c_object_browser_mode=>query.
        rr_validator = NEW cl_qov_query( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS cl_qov_cds IMPLEMENTATION.

  METHOD validate.
    DATA: lf_invalid TYPE abap_bool.

    IF iv_option = zif_dbbr_c_object_browser=>c_search_option-by_type.
      CASE iv_value.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-function OR
             zif_dbbr_c_object_browser=>c_type_option_value-hierarchy OR
             zif_dbbr_c_object_browser=>c_type_option_value-view.

        WHEN OTHERS.
          lf_invalid = abap_true.
      ENDCASE.
    ELSEIF iv_option = zif_dbbr_c_object_browser=>c_search_option-by_api.
      CASE iv_value.

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-released.

        WHEN OTHERS.
          lf_invalid = abap_true.
      ENDCASE.

    ELSEIF iv_option = zif_dbbr_c_object_browser=>c_search_option-by_params.
      IF iv_value <> 'TRUE'.
        lf_invalid = abap_true.
      ENDIF.
    ENDIF.

    IF lf_invalid = abap_true.
      RAISE EXCEPTION TYPE zcx_dbbr_object_search
        EXPORTING
          textid = zcx_dbbr_object_search=>invalid_option_value
          msgv1  = |{ iv_option }|
          msgv2  = |{ iv_value }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_qov_database_tab_view IMPLEMENTATION.

  METHOD validate.
    DATA: lf_invalid TYPE abap_bool.

    IF iv_option = zif_dbbr_c_object_browser=>c_search_option-by_type.
      CASE iv_value.

        WHEN zif_dbbr_c_object_browser=>c_type_option_value-table OR
             zif_dbbr_c_object_browser=>c_type_option_value-view.

        WHEN OTHERS.
          lf_invalid = abap_true.
      ENDCASE.
    ENDIF.

    IF lf_invalid = abap_true.
      RAISE EXCEPTION TYPE zcx_dbbr_object_search
        EXPORTING
          textid = zcx_dbbr_object_search=>invalid_option_value
          msgv1  = |{ iv_option }|
          msgv2  = |{ iv_value }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_qov_query IMPLEMENTATION.

  METHOD validate.

  ENDMETHOD.

ENDCLASS.
