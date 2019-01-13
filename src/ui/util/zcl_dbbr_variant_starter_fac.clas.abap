class ZCL_DBBR_VARIANT_STARTER_FAC definition
  public
  final
  create private .

public section.

  class-methods CREATE_VARIANT_STARTER
    importing
      !IV_VARIANT_ID type ZDBBR_VARIANT_ID
      !IV_ENTITY_TYPE type ZDBBR_ENTITY_TYPE
      !IV_VARIANT_ENTITY_ID type STRING
    returning
      value(RESULT) type ref to ZIF_DBBR_VARIANT_STARTER .
  class-methods CREATE_DEFAULT_VARIANT_STARTER
    importing
      !IV_VARIANT_ENTITY_ID type STRING
      !IV_ENTITY_TYPE type ZDBBR_ENTITY_TYPE
    returning
      value(RESULT) type ref to ZIF_DBBR_VARIANT_STARTER .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_VARIANT_STARTER_FAC IMPLEMENTATION.


  METHOD create_default_variant_starter.
    CASE iv_entity_type.

      WHEN zif_dbbr_c_entity_type=>cds_view.
        result = NEW zcl_dbbr_cds_variant_starter(
            iv_cds_view   = CONV #( iv_variant_entity_id )
            iv_variant_id = zif_dbbr_global_consts=>c_dummy_variant
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
            iv_cds_view   = conv #( iv_variant_entity_id )
            iv_variant_id = iv_variant_id
        ).
      WHEN zif_dbbr_c_entity_type=>query.
        result = NEW zcl_dbbr_query_var_starter(
            iv_query_id  = iv_variant_entity_id
            iv_variant_id = iv_variant_id
        ).

      WHEN zif_dbbr_c_entity_type=>table.
        result = NEW zcl_dbbr_table_var_starter(
            iv_tabname    = CONV #( iv_variant_entity_id )
            iv_variant_id = iv_variant_id
        ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
