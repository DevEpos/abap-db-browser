*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CONSTANTS:
  BEGIN OF c_cls,
    BEGIN OF sadl_mpd_exposure,
      name TYPE string VALUE 'CL_SADL_MDP_EXPOSURE',
      BEGIN OF meth,
        BEGIN OF get_exposure,
          name TYPE string VALUE 'GET_EXPOSURE_LOAD_ID',
          BEGIN OF param,
            rv_id        TYPE string VALUE 'RV_ID',
            iv_entity_id TYPE string VALUE 'IV_ENTITY_ID',
          END OF param,
        END OF get_exposure,
      END OF meth,
    END OF sadl_mpd_exposure,

    BEGIN OF sadl_mdp_factory,
      name TYPE string VALUE 'CL_SADL_MDP_FACTORY',
      BEGIN OF meth,
        BEGIN OF get_mdp_for_id,
          name TYPE string VALUE 'GET_MDP_FOR_ID',
        END OF get_mdp_for_id,
        BEGIN OF get_entity_load_by_id,
          name TYPE string VALUE 'GET_ENTITY_LOAD_BY_ID',
        END OF get_entity_load_by_id,
      END OF meth,
    END OF sadl_mdp_factory,

    BEGIN OF sadl_exit_handler,
      name TYPE string VALUE 'CL_SADL_EXIT_HANDLER',
      BEGIN OF meth,
        BEGIN OF execute_calculation,
          name TYPE string VALUE 'EXECUTE_CALCULATION',
        END OF execute_calculation,
        BEGIN OF adjust_requested,
          name TYPE string VALUE 'ADJUST_REQUESTED',
        END OF adjust_requested,
      END OF meth,
    END OF sadl_exit_handler,
  END OF c_cls.

CLASS lcl_sadl_mdp_exposure DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_exposure_load_id
        IMPORTING
          iv_entity_name    TYPE zsat_cds_view_name
        RETURNING
          VALUE(rr_load_id) TYPE REF TO data
        RAISING
          cx_sy_dyn_call_error,
      create_method_param_data
        IMPORTING
          io_class_descr       TYPE REF TO cl_abap_classdescr
          iv_method_name       TYPE string
          iv_param_name        TYPE string
        RETURNING
          VALUE(rr_param_data) TYPE REF TO data
        RAISING
          cx_sy_dyn_call_excp_not_found.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_sadl_mdp_factory DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_entity_name TYPE zsat_cds_view_name
        RAISING
          cx_sy_dyn_call_error,
      get_mdp_for_id
        RETURNING
          VALUE(rr_mdp) TYPE REF TO data,
      get_entity_load_by_id
        RETURNING
          VALUE(rr_entity_load) TYPE REF TO data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mr_load_id TYPE REF TO data.
    METHODS:
      create_metadata_provider_ref
        RETURNING
          VALUE(rr_mdp) TYPE REF TO data.
ENDCLASS.

CLASS lcl_sadl_exit_handler DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_entity_name TYPE zsat_cds_view_name,
      adjust_requested
        CHANGING
          ct_requested_element TYPE stringtab
        RAISING
          zcx_dbbr_application_exc,
      calculate_elements
        CHANGING
          ct_data TYPE REF TO data
        RAISING
          zcx_dbbr_application_exc.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_sadl_exit_handler TYPE REF TO object.
    DATA mv_entity_name TYPE zsat_cds_view_name.
ENDCLASS.
