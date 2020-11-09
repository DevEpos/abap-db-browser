"! <p class="shorttext synchronized" lang="en">Configuration for Package Query</p>
CLASS zcl_dbbr_package_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: zif_sat_object_search_config~get_type REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_dbbr_package_query_config IMPLEMENTATION.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_dbbr_c_object_browser=>c_search_type-package.
  ENDMETHOD.

ENDCLASS.
