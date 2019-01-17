INTERFACE zif_dbbr_global_types
  PUBLIC .

  TYPES: ty_tabname16_range TYPE RANGE OF tabname16.

  "! <p class="shorttext synchronized" lang="en">Range for table name</p>
  TYPES: ty_tabname_range TYPE RANGE OF tabname.
  "! Range for created by
  TYPES: ty_created_by_range TYPE RANGE OF zdbbr_created_by.
  "! Range for query name
  TYPES: ty_query_name_range TYPE RANGE OF zdbbr_query_name.
  "! Range for description
  TYPES: ty_ddtext_range TYPE RANGE OF ddtext.
  "! Range for Entity id
  TYPES: ty_entity_id_range TYPE RANGE OF zdbbr_entity_id.
  "! Range for annotation name
  TYPES: ty_cds_entity_range TYPE RANGE OF ddstrucobjname.
  "! <p class="shorttext synchronized" lang="en">Range for Annotation name</p>
  TYPES: tt_cds_anno_name_range TYPE RANGE OF ddannotation_key.
  TYPES: tt_cds_api_state TYPE STANDARD TABLE OF zdbbr_cds_api_state WITH EMPTY KEY.
  TYPES: BEGIN OF ty_cds_tadir,
           created_by   TYPE as4user,
           created_date TYPE as4date,
         END OF ty_cds_tadir.

  TYPES:
    BEGIN OF ty_cds_param_value,
      name  TYPE ddparname,
      value TYPE zdbbr_value,
    END OF ty_cds_param_value.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">CDS View Annoation data</p>
    BEGIN OF ty_cds_annotation,
      fieldname TYPE fieldname,
      name      TYPE string,
      value     TYPE string,
    END OF ty_cds_annotation.
  "! <p class="shorttext synchronized" lang="en">List of annotations</p>
  TYPES: tt_cds_annotation TYPE STANDARD TABLE OF ty_cds_annotation WITH KEY name.
  TYPES: tt_cds_param_value TYPE STANDARD TABLE OF ty_cds_param_value WITH KEY name.
  TYPES: tt_mostused_k TYPE STANDARD TABLE OF zdbbr_mostused_k WITH EMPTY KEY.
  TYPES: tt_domain2convext TYPE STANDARD TABLE OF zdbbr_conve2dom WITH KEY convexit domname.
ENDINTERFACE.
