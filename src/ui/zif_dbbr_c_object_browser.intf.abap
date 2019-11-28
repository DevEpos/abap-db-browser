"! <p class="shorttext synchronized" lang="en">Constants for Object Browser</p>
INTERFACE zif_dbbr_c_object_browser
  PUBLIC .

  CONSTANTS:
    BEGIN OF c_search_type,
      query   TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'QUERY',
      package TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'PACKAGE',
    END OF c_search_type.

  CONSTANTS:
    BEGIN OF c_query_options,
      from type string value 'selectFrom',
    END OF c_query_options.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Type for tree node of object browser</p>
    BEGIN OF c_tree_node_type,
      package               TYPE i VALUE 10,
      pak_ddl_folder        TYPE i VALUE 11,
      pak_dtab_folder       TYPE i VALUE 12,
      pak_view_folder       TYPE i VALUE 13,
      cds_view              TYPE i VALUE 20,
      cds_properties        TYPE i VALUE 21,
      cds_api_states        TYPE i VALUE 22,
      cds_assocs            TYPE i VALUE 23,
      cds_tables            TYPE i VALUE 24,
      cds_variants          TYPE i VALUE 25,
      cds_package_prop      TYPE i VALUE 26,
      cds_owner_prop        TYPE i VALUE 27,
      query                 TYPE i VALUE 30,
      query_properties      TYPE i VALUE 33,
      query_tables          TYPE i VALUE 31,
      query_variants        TYPE i VALUE 32,
      query_owner_prop      TYPE i VALUE 33,
      dbtable               TYPE i VALUE 40,
      dbtable_variants      TYPE i VALUE 41,
      dbtable_foreign_key   TYPE i VALUE 42,
      dbtable_properties    TYPE i VALUE 43,
      dbtable_tech_settings TYPE i VALUE 44,
      dbtable_package_prop  TYPE i VALUE 45,
      dbtable_owner_prop    TYPE i VALUE 46,
      view                  TYPE i VALUE 50,
      view_variants         TYPE i VALUE 51,
      view_tables           TYPE i VALUE 52,
      view_properties       TYPE i VALUE 53,
      view_package_prop     TYPE i VALUE 54,
      view_owner_prop       TYPE i VALUE 55,
    END OF c_tree_node_type.
ENDINTERFACE.
