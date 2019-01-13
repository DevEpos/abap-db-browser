"! <p class="shorttext synchronized" lang="en">Query searcher for Object Browser</p>
CLASS zcl_dbbr_ob_query_searcher DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_dbbr_ob_generic_searcher.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_object_searcher.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_dbbr_ob_query_searcher IMPLEMENTATION.

  METHOD zif_dbbr_object_searcher~search.

  ENDMETHOD.

ENDCLASS.
