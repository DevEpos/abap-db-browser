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

      WHEN zif_dbbr_c_object_browser_mode=>package.
        rr_validator = NEW cl_qov_package( ).
    ENDCASE.

  ENDMETHOD.

  METHOD validate.
    IF iv_value IS INITIAL.
      RAISE EXCEPTION TYPE zcx_dbbr_object_search
        EXPORTING
          textid = zcx_dbbr_object_search=>option_incomplete
          msgv1  = |{ iv_option }|.
    ENDIF.

    CASE iv_option.

      WHEN zif_dbbr_c_object_browser=>c_search_option-max_rows.
        IF iv_value CN '0123456789'.
          RAISE EXCEPTION TYPE zcx_dbbr_object_search
            EXPORTING
              textid = zcx_dbbr_object_search=>option_val_not_numeric
              msgv1  = |{ iv_option }|.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS cl_qov_cds IMPLEMENTATION.

  METHOD validate.
    DATA: lf_invalid TYPE abap_bool.

    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).

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

        WHEN zif_dbbr_c_object_browser=>c_api_option_value-released OR
             zif_dbbr_c_object_browser=>c_api_option_value-key_user OR
             zif_dbbr_c_object_browser=>c_api_option_value-key_user_long OR
             zif_dbbr_c_object_browser=>c_api_option_value-cloud_user OR
             zif_dbbr_c_object_browser=>c_api_option_value-cloud_user_long OR
             zif_dbbr_c_object_browser=>c_api_option_value-remote_api OR
             zif_dbbr_c_object_browser=>c_api_option_value-remote_api_long OR
             zif_dbbr_c_object_browser=>c_api_option_value-custom_fields.

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

    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).

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
    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_qov_package IMPLEMENTATION.

  METHOD validate.
    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).
  ENDMETHOD.

ENDCLASS.
