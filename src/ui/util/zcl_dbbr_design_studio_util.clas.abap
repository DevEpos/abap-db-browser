"! <p class="shorttext synchronized" lang="en">Utility for Design Studio jump</p>
CLASS zcl_dbbr_design_studio_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    "! @parameter io_cds_view | <p class="shorttext synchronized" lang="en">Reference to CDS View</p>
    METHODS constructor
      IMPORTING
        io_cds_view       TYPE REF TO zcl_sat_cds_view
        io_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data
      RAISING
        zcx_dbbr_application_exc.

    "! <p class="shorttext synchronized" lang="en">Opens the CDS in the Design Studio</p>
    METHODS open_in_design_studio.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_http               TYPE i VALUE 1,
               c_https              TYPE i VALUE 2,
               c_flp_url_part       TYPE string VALUE '/sap/bc/ui2/flp',
               c_design_studio      TYPE string VALUE '#AnalyticQuery-analyze',
               c_query_param        TYPE string VALUE 'XQUERY',
               c_sap_language_param TYPE string VALUE 'sap-language',
               c_sap_client_param   TYPE string VALUE 'sap-client',
               c_system_param       TYPE string VALUE 'XSYSTEM=LOCAL'.
    DATA mo_cds_view TYPE REF TO zcl_sat_cds_view.
    DATA mv_url_params TYPE string.
    DATA mo_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data.
    CLASS-DATA gv_design_studio_base_url TYPE string.

    "! <p class="shorttext synchronized" lang="en"></p>
    METHODS determine_param_values.
    METHODS build_base_url
      RAISING
        zcx_dbbr_application_exc.
ENDCLASS.



CLASS zcl_dbbr_design_studio_util IMPLEMENTATION.

  METHOD constructor.
    mo_cds_view = io_cds_view.
    mo_selscreen_data = io_selscreen_data.
    build_base_url( ).
    determine_param_values( ).
  ENDMETHOD.

  METHOD open_in_design_studio.

    DATA(lv_url) = |{ gv_design_studio_base_url }?{ c_query_param }=2C{ mo_cds_view->get_header( )-ddlview }| &&
                   mv_url_params.

    cl_gui_frontend_services=>execute(
     EXPORTING
       document = lv_url
     EXCEPTIONS
       OTHERS   = 1
   ).
  ENDMETHOD.

  METHOD determine_param_values.
    TYPES: BEGIN OF lty_s_field,
             fieldname TYPE fieldname,
           END OF lty_s_field.
    DATA lt_anno_params TYPE TABLE OF lty_s_field.
    DATA lt_param_values TYPE zif_sat_ty_global=>ty_t_cds_param_value.
    DATA(lt_cds_params) = mo_cds_view->get_parameters( if_exclude_system_params = abap_true ).

    DATA(lt_anno) = mo_cds_view->get_annotations( VALUE #( ( sign = 'I' option = 'CP' low = 'CONSUMPTION.*FILTER.*' ) ) ).

*.. Fill the values from the CDS parameters. Those are mandatory in any case
    LOOP AT lt_cds_params ASSIGNING FIELD-SYMBOL(<ls_param>).
      ASSIGN mo_selscreen_data->mr_t_table_data->*[ tabname_alias = zif_dbbr_c_global=>c_parameter_dummy_table
                                                    fieldname     = <ls_param>-parametername ] TO FIELD-SYMBOL(<ls_param_value>).
      CHECK <ls_param_value>-low IS NOT INITIAL.
      mv_url_params = |{ mv_url_params }&{ <ls_param>-parametername_raw }={ <ls_param_value>-low }|.
    ENDLOOP.

*.. Fill the values from the parameters which were declared via annotation
    lt_anno_params = CORRESPONDING #( lt_anno ).
    DELETE ADJACENT DUPLICATES FROM lt_anno_params.

    LOOP AT lt_anno_params INTO DATA(ls_anno_param).
      ASSIGN mo_selscreen_data->mr_t_table_data->*[ tabname_alias = mo_cds_view->get_header( )-entityname
                                                    fieldname     = ls_anno_param ] TO <ls_param_value>.
      CHECK <ls_param_value>-low IS NOT INITIAL.
      mv_url_params = |{ mv_url_params }&{ <ls_param_value>-fieldname_raw }={ <ls_param_value>-low }|.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_base_url.
    DATA: lt_server_list TYPE TABLE OF icm_sinfo2.

    CHECK gv_design_studio_base_url IS INITIAL.

    CALL FUNCTION 'ICM_GET_INFO2'
      TABLES
        servlist           = lt_server_list
      EXCEPTIONS
        icm_error          = 1
        icm_timeout        = 2
        icm_not_authorized = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg).
      RAISE EXCEPTION TYPE zcx_dbbr_application_exc.
    ENDIF.

    ASSIGN lt_server_list[ protocol = c_https ] TO FIELD-SYMBOL(<ls_server>).
    IF sy-subrc <> 0.
      ASSIGN lt_server_list[ protocol = c_http ] TO <ls_server>.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc
          EXPORTING
            text = 'Error during protocol determination'.
      ENDIF.
    ENDIF.

    IF <ls_server>-protocol = c_http.
      gv_design_studio_base_url = 'http'.
    ELSE.
      gv_design_studio_base_url = 'https'.
    ENDIF.

    gv_design_studio_base_url = |{ gv_design_studio_base_url }://{ <ls_server>-hostname }:{ <ls_server>-service }| &&
                                |{ c_flp_url_part }?{ c_sap_language_param }={ sy-langu }| &&
                                |&{ c_sap_client_param }={ sy-mandt }{ c_design_studio }|.
  ENDMETHOD.

ENDCLASS.
