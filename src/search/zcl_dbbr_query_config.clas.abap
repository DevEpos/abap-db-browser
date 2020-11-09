"! <p class="shorttext synchronized" lang="en">Configuration for Db Browser Query</p>
CLASS zcl_dbbr_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_sat_object_search_config~get_type
        REDEFINITION.
    METHODS zif_sat_object_search_config~map_option
        REDEFINITION.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_query_config IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mt_options = VALUE #(
      ( option = c_general_options-user allowed_length = 12 )
      ( option = zif_dbbr_c_object_browser=>c_query_options-from allowed_length = 30 )
      ( option = c_general_options-description allowed_length = 40 )
      ( option = c_general_options-max_rows single = abap_true no_negation = abap_true )
    ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_dbbr_c_object_browser=>c_search_type-query.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~map_option.
    cv_option = SWITCH #( cv_option
      WHEN c_search_option-by_owner       THEN c_general_options-user
      WHEN c_search_option-by_select_from THEN zif_dbbr_c_object_browser=>c_query_options-from
      WHEN c_search_option-by_description THEN c_general_options-description
      WHEN c_search_option-max_rows       THEN c_general_options-max_rows
      ELSE cv_option
    ).
  ENDMETHOD.

ENDCLASS.
