CLASS zcl_dbbr_application_component DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mv_app_component_text TYPE udtext READ-ONLY.
    DATA mv_app_component TYPE ufps_posid READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_app_component      TYPE ufps_posid
        iv_app_component_text TYPE udtext.
    METHODS add_application_component
      IMPORTING
        iv_app_component      TYPE ufps_posid
        iv_app_component_text TYPE udtext
      RETURNING
        VALUE(result)         TYPE REF TO zcl_dbbr_application_component.
    METHODS add_package
      IMPORTING
        iv_package_name TYPE devclass
        iv_package_text TYPE as4text.
    METHODS get_package_list
      RETURNING
        VALUE(result) TYPE REF TO zif_uitb_list.
    METHODS get_app_comp_list
      RETURNING
        VALUE(result) TYPE REF TO zif_uitb_list.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_package_list TYPE REF TO zif_uitb_list.
    DATA mr_app_comp_list TYPE REF TO zif_uitb_list.
ENDCLASS.



CLASS zcl_dbbr_application_component IMPLEMENTATION.
  METHOD constructor.
    mv_app_component = iv_app_component.
    mv_app_component_text = iv_app_component_text.
    mr_package_list = NEW zcl_uitb_object_list( ).
    mr_app_comp_list = NEW zcl_uitb_object_list( ).
  ENDMETHOD.

  METHOD add_package.
    mr_package_list->add( NEW zcl_dbbr_package( iv_package_name = iv_package_name iv_package_text = iv_package_text ) ).
  ENDMETHOD.

  METHOD get_package_list.
    result = mr_package_list.
  ENDMETHOD.

  METHOD add_application_component.
    result = NEW zcl_dbbr_application_component( iv_app_component = iv_app_component iv_app_component_text = iv_app_component_text ).
    mr_app_comp_list->add( result ).
  ENDMETHOD.

  METHOD get_app_comp_list.
    result = mr_app_comp_list.
  ENDMETHOD.

ENDCLASS.
