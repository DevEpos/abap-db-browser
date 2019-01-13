INTERFACE zif_dbbr_global_types
  PUBLIC .

  TYPES: ty_tabname16_range TYPE RANGE OF tabname16.
  TYPES: ty_tabname_range TYPE RANGE OF tabname.
  TYPES: ty_created_by_range TYPE RANGE OF zdbbr_created_by.
  TYPES: ty_query_name_range TYPE RANGE OF zdbbr_query_name.
  TYPES: ty_ddtext_range TYPE RANGE OF ddtext.
  TYPES: ty_entity_id_range TYPE RANGE OF zdbbr_entity_id.
  TYPES: ty_cds_entity_range TYPE RANGE OF ddstrucobjname.
  TYPES: tt_cds_api_state TYPE STANDARD TABLE OF zdbbr_cds_api_state WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_cds_param_value,
      name  TYPE ddparname,
      value TYPE zdbbr_value,
    END OF ty_cds_param_value.

  TYPES: tt_cds_param_value TYPE STANDARD TABLE OF ty_cds_param_value WITH KEY name.
  TYPES: tt_mostused_k TYPE STANDARD TABLE OF zdbbr_mostused_k WITH EMPTY KEY.
  TYPES: tt_domain2convext TYPE STANDARD TABLE OF zdbbr_conve2dom WITH KEY convexit domname.
ENDINTERFACE.
