"! <p class="shorttext synchronized" lang="en">Constants for Object Browser</p>
INTERFACE zif_dbbr_c_object_browser
  PUBLIC .
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search option for object browser</p>
    BEGIN OF c_search_option,
      by_owner       TYPE string VALUE 'OWNER',
      by_select_from TYPE string VALUE 'FROM',
      by_association TYPE string VALUE 'ASSOC',
      by_api         TYPE string VALUE 'API',
      by_description TYPE string VALUE 'DESC',
      by_type        TYPE string VALUE 'TYPE',
      by_package     TYPE string VALUE 'PACKAGE',
      by_anno        TYPE string VALUE 'ANNO',
      by_field       TYPE string VALUE 'FIELD',
      by_params      TYPE string VALUE 'PARAMS',
      max_rows       TYPE string VALUE 'MAXROWS',
      by_extensions   TYPE string VALUE 'EXTBY',
    END OF c_search_option.

  CONSTANTS:
    "! Values for API option
    BEGIN OF c_api_option_value,
      released        TYPE string VALUE 'RELEASED',
      custom_fields   TYPE string VALUE 'ADD_CUSTOM_FIELDS',
      key_user        TYPE string VALUE 'KEY_USER',
      key_user_long   TYPE string VALUE 'USE_IN_KEY_USER_APPS',
      remote_api      TYPE string VALUE 'REMOTE',
      remote_api_long TYPE string VALUE 'USE_AS_REMOTE_API',
      cloud_user      TYPE string VALUE 'CLOUD',
      cloud_user_long TYPE string VALUE 'USE_IN_SAP_CLOUD_PLATFORM',
    END OF c_api_option_value.
  CONSTANTS:
    "! Values for 'TYPE' option
    BEGIN OF c_type_option_value,
      function  TYPE string VALUE 'FUNCTION',
      hierarchy TYPE string VALUE 'HIERARCHY',
      view      TYPE string VALUE 'VIEW',
      table     TYPE string VALUE 'TABLE',
    END OF c_type_option_value.

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
