"! <p class="shorttext synchronized" lang="en">Configuration for Db Browser Query</p>
CLASS zcl_dbbr_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: zif_sat_object_search_config~get_type REDEFINITION.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_query_config IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mt_options = VALUE #(
      ( option = c_search_option-by_owner allowed_length = 12 )
      ( option = c_search_option-by_select_from allowed_length = 30 )
      ( option = c_search_option-by_description allowed_length = 40 )
      ( option = c_search_option-max_rows single = abap_true no_negation = abap_true )
    ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_dbbr_c_object_browser=>c_search_type-query.
  ENDMETHOD.

ENDCLASS.
