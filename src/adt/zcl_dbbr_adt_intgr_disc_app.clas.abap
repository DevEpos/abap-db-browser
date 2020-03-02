"! <p class="shorttext synchronized" lang="en">Router ABAP DB Browser</p>
CLASS zcl_dbbr_adt_intgr_disc_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_root_scheme TYPE string VALUE 'http://www.devepos.com/adt/dbbrowser/integration'.
    CONSTANTS c_static_uri TYPE string VALUE '/devepos/adt/dbbr/integration'.

    METHODS if_adt_rest_rfc_application~get_static_uri_path
        REDEFINITION.
  PROTECTED SECTION.
    METHODS: get_application_title REDEFINITION,
      register_resources REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS c_app_title TYPE string VALUE 'Discovery for ADT Integration of DB Browser'.
ENDCLASS.



CLASS zcl_dbbr_adt_intgr_disc_app IMPLEMENTATION.

  METHOD get_application_title.
    result = c_app_title.
  ENDMETHOD.

  METHOD register_resources ##needed.
*.. No rescources are currently needed. The specific discovery URI for the DB Browser integration
*... is enough to recognize the availability of the DB Browser transaction i.e. ZDBBR
  ENDMETHOD.

  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = c_static_uri.
  ENDMETHOD.

ENDCLASS.
