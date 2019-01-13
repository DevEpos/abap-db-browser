"! <p class="shorttext synchronized" lang="en">Package searcher for Object browser</p>
CLASS zcl_dbbr_ob_package_searcher DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_ob_generic_searcher
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_object_searcher.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_ob_package_searcher IMPLEMENTATION.

  METHOD zif_dbbr_object_searcher~search.

  ENDMETHOD.

ENDCLASS.
