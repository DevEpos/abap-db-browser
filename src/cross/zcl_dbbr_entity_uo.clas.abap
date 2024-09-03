CLASS zcl_dbbr_entity_uo DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mv_entity_id TYPE zsat_entity_id READ-ONLY.
    DATA mv_entity_type TYPE zsat_entity_type READ-ONLY.
    DATA mv_source_type TYPE ddtargetkind READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_entity_id   TYPE zsat_entity_id
        iv_entity_type TYPE zsat_entity_type
        iv_source_type TYPE ddtargetkind.
ENDCLASS.


CLASS zcl_dbbr_entity_uo IMPLEMENTATION.
  METHOD constructor.
    mv_entity_id = iv_entity_id.
    mv_entity_type = iv_entity_type.
    mv_source_type = iv_source_type.
  ENDMETHOD.
ENDCLASS.
