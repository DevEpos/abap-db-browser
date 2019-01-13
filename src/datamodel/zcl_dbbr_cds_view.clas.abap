"! <p class="shorttext synchronized" lang="en">CDS View</p>
CLASS zcl_dbbr_cds_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_dbbr_cds_view_factory .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Name of CDS view</p>
    DATA mv_view_name TYPE zdbbr_cds_view_name READ-ONLY .
    DATA mv_association_count TYPE i READ-ONLY .
    DATA mv_select_table_count TYPE i READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Requests API States</p>
    EVENTS request_api_states .
    "! <p class="shorttext synchronized" lang="en">Requests author of CDS View</p>
    EVENTS request_author.
    "! <p class="shorttext synchronized" lang="en">Requests Base Tables</p>
    EVENTS request_base_tables .
    "! <p class="shorttext synchronized" lang="en">Requests Description</p>
    EVENTS request_description.

    METHODS constructor
      IMPORTING
        !is_header      TYPE zdbbr_cds_view_header
        !it_columns     TYPE dd03ndvtab
        !it_parameters  TYPE zdbbr_cds_parameter_t
        !it_association TYPE zdbbr_cds_association_t OPTIONAL
        !it_api_states  TYPE zif_dbbr_global_types=>tt_cds_api_state OPTIONAL
        !it_base_tables TYPE zdbbr_cds_view_base_table_t OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Retrieve API States of CDS View</p>
    "!
    "! @parameter rt_api_states | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_api_states
      RETURNING
        VALUE(rt_api_states) TYPE zif_dbbr_global_types=>tt_cds_api_state .
    "! <p class="shorttext synchronized" lang="en">Returns associations</p>
    METHODS get_associations
      RETURNING
        VALUE(result) TYPE zdbbr_cds_association_t .
    "! <p class="shorttext synchronized" lang="en">Get author of CDS view</p>
    "!
    METHODS get_author
      RETURNING
        VALUE(rv_author) TYPE responsibl.
    "! <p class="shorttext synchronized" lang="en">Returns base tables for CDS View</p>
    METHODS get_base_tables
      RETURNING
        VALUE(result) TYPE zdbbr_cds_view_base_table_t .
    "! <p class="shorttext synchronized" lang="en">Returns columns information</p>
    METHODS get_columns
      RETURNING
        VALUE(result) TYPE dd03ndvtab .
    "! <p class="shorttext synchronized" lang="en">Retrieve description of cds view</p>
    "!
    "! @parameter rv_description | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_description
      RETURNING
        VALUE(rv_description) TYPE ddtext.
    "! <p class="shorttext synchronized" lang="en">Returns header information</p>
    METHODS get_header
      RETURNING
        VALUE(result) TYPE zdbbr_cds_view_header .
    "! <p class="shorttext synchronized" lang="en">Returns parameter information</p>
    METHODS get_parameters
      IMPORTING
        !if_exclude_system_params TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)             TYPE zdbbr_cds_parameter_t .
    "! <p class="shorttext synchronized" lang="en">Returns 'X' if cds view has associations</p>
    METHODS has_associations
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Checks if this cds view has parameters</p>
    METHODS has_parameters
      RETURNING
        VALUE(result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Header for CDS View</p>
    DATA ms_header TYPE zdbbr_cds_view_header .
    "! <p class="shorttext synchronized" lang="en">Parameters for cds view</p>
    DATA mt_parameters TYPE zdbbr_cds_parameter_t .
    "! <p class="shorttext synchronized" lang="en">DD: Table for Node Attributes</p>
    DATA mt_columns TYPE dd03ndvtab .
    DATA mt_api_states TYPE zif_dbbr_global_types=>tt_cds_api_state.
    DATA mt_association_header TYPE dd08bvtab .
    DATA mt_association_field TYPE dd05bvtab .
    DATA mf_has_associations TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Associations for CDS View</p>
    DATA mt_association TYPE zdbbr_cds_association_t .
    "! <p class="shorttext synchronized" lang="en">List Base tables of CDS View</p>
    DATA mt_base_tables TYPE zdbbr_cds_view_base_table_t .
    DATA mf_base_tables_loaded TYPE abap_bool .
    DATA mf_api_states_loaded TYPE abap_bool.
    DATA mf_author_loaded TYPE abap_bool.
    DATA mv_author TYPE responsibl.
    DATA mv_description TYPE ddtext.
    DATA mf_description_loaded TYPE abap_bool.
ENDCLASS.



CLASS zcl_dbbr_cds_view IMPLEMENTATION.


  METHOD constructor.
    ms_header = is_header.
    mv_view_name = is_header-entityname.
    mt_parameters         = it_parameters.
    mt_columns            = it_columns.
    mt_association        = it_association.
    IF it_base_tables IS SUPPLIED.
      mf_base_tables_loaded = abap_true.
      mt_base_tables        = it_base_tables.
    ENDIF.
    IF it_api_states IS SUPPLIED.
      mf_api_states_loaded = abap_true.
      mt_api_states = it_api_states.
    ENDIF.
    mf_has_associations   = xsdbool( mt_association IS NOT INITIAL ).

    mv_association_count = lines( it_association ).
    mv_select_table_count = lines( it_base_tables ).
  ENDMETHOD.


  METHOD get_api_states.
    IF mf_api_states_loaded = abap_false.
      RAISE EVENT request_api_states.
      mf_api_states_loaded = abap_true.
    ENDIF.

    rt_api_states = mt_api_states.
  ENDMETHOD.


  METHOD get_associations.
    result = mt_association.
  ENDMETHOD.

  METHOD get_author.
    IF mf_author_loaded = abap_false.
      RAISE EVENT request_author.
      mf_author_loaded = abap_true.
    ENDIF.

    rv_author = mv_author.
  ENDMETHOD.

  METHOD get_base_tables.
*... were the base tables already loaded?
    IF mf_base_tables_loaded = abap_false.
      RAISE EVENT request_base_tables.
      mf_base_tables_loaded = abap_true.
    ENDIF.
    result = mt_base_tables.
  ENDMETHOD.


  METHOD get_columns.
    result = mt_columns.
  ENDMETHOD.

  METHOD get_description.
    IF mf_description_loaded = abap_false.
      RAISE EVENT request_description.
      mf_description_loaded = abap_true.
    ENDIF.

    rv_description = mv_description.
  ENDMETHOD.


  METHOD get_header.
    result = ms_header.
  ENDMETHOD.


  METHOD get_parameters.
    result = mt_parameters.

    IF if_exclude_system_params = abap_true.
      DELETE result WHERE has_system_anno = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD has_associations.
    result = mf_has_associations.
  ENDMETHOD.


  METHOD has_parameters.
    result = xsdbool( mt_parameters IS NOT INITIAL ).
  ENDMETHOD.
ENDCLASS.
