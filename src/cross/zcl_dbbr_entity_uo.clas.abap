class ZCL_DBBR_ENTITY_UO definition
  public
  final
  create public .

public section.

  data MV_ENTITY_ID type ZSAT_ENTITY_ID read-only .
  data MV_ENTITY_TYPE type ZSAT_ENTITY_TYPE read-only .
  data MV_SOURCE_TYPE type DDTARGETKIND read-only .

  methods CONSTRUCTOR
    importing
      !IV_ENTITY_ID type ZSAT_ENTITY_ID
      !IV_ENTITY_TYPE type ZSAT_ENTITY_TYPE
      !IV_SOURCE_TYPE type DDTARGETKIND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_ENTITY_UO IMPLEMENTATION.


  METHOD constructor.
    mv_entity_id = iv_entity_id.
    mv_entity_type = iv_entity_type.
    mv_source_type = iv_source_type.
  ENDMETHOD.
ENDCLASS.
