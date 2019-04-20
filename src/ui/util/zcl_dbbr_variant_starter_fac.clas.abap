CLASS zcl_dbbr_variant_starter_fac DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create_variant_starter
      IMPORTING
        !iv_variant_id        TYPE zdbbr_variant_id
        !iv_entity_type       TYPE zdbbr_entity_type
        !iv_variant_entity_id TYPE string
      RETURNING
        VALUE(result)         TYPE REF TO zif_dbbr_variant_starter .
    CLASS-METHODS create_default_variant_starter
      IMPORTING
        !iv_variant_entity_id TYPE string
        !iv_entity_type       TYPE zdbbr_entity_type
      RETURNING
        VALUE(result)         TYPE REF TO zif_dbbr_variant_starter .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_variant_starter_fac IMPLEMENTATION.


  METHOD create_default_variant_starter.
    CASE iv_entity_type.

      WHEN zif_dbbr_c_entity_type=>cds_view.
        result = NEW zcl_dbbr_cds_variant_starter(
            iv_cds_view         = CONV #( iv_variant_entity_id )
            iv_variant_id       = zif_dbbr_global_consts=>c_dummy_variant
        ).
      WHEN zif_dbbr_c_entity_type=>query.
        result = NEW zcl_dbbr_query_var_starter(
            iv_query_id  = iv_variant_entity_id
            iv_variant_id = zif_dbbr_global_consts=>c_dummy_variant
        ).

      WHEN zif_dbbr_c_entity_type=>table.
        result = NEW zcl_dbbr_table_var_starter(
            iv_tabname    = CONV #( iv_variant_entity_id )
            iv_variant_id = zif_dbbr_global_consts=>c_dummy_variant
        ).
    ENDCASE.
  ENDMETHOD.


  METHOD create_variant_starter.
    CASE iv_entity_type.

      WHEN zif_dbbr_c_entity_type=>cds_view.
        result = NEW zcl_dbbr_cds_variant_starter(
            iv_cds_view         = CONV #( iv_variant_entity_id )
            iv_variant_id       = iv_variant_id
        ).
      WHEN zif_dbbr_c_entity_type=>query.
        result = NEW zcl_dbbr_query_var_starter(
            iv_query_id  = iv_variant_entity_id
            iv_variant_id = iv_variant_id
        ).

      WHEN zif_dbbr_c_entity_type=>table or
           zif_dbbr_c_entity_type=>view.
        result = NEW zcl_dbbr_table_var_starter(
            iv_tabname    = CONV #( iv_variant_entity_id )
            iv_variant_id = iv_variant_id
        ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
