*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CONSTANTS:
  BEGIN OF c_cls,
    BEGIN OF sadl_mpd_exposure,
      name TYPE string VALUE '',
      BEGIN OF meth,
        BEGIN OF get_exposure,
          name TYPE string VALUE '',
          BEGIN OF param,
            rv_id TYPE string VALUE '',
          END OF param,
        END OF get_exposure,
      END OF meth,
    END OF sadl_mpd_exposure,
  END OF c_cls.

CLASS lcl_sadl_mdp_exposure DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_exposure_load_id
        IMPORTING
          iv_entity_name    TYPE zsat_cds_view_name
        RETURNING
          VALUE(rr_load_id) TYPE REF TO data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_class TYPE string VALUE 'CL_SADL_MDP_EXPOSURE',
      BEGIN OF c_methods,
        get_exposure_load_id TYPE string VALUE 'GET_EXPOSURE_LOAD_ID',
      END OF c_methods,
      BEGIN OF c_params,
        rv_id        TYPE string VALUE 'RV_ID',
        iv_entity_id TYPE string VALUE 'IV_ENTITY_ID',
      END OF c_params.
ENDCLASS.

CLASS lcl_sadl_mdp_factory DEFINITION.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      create_metadata_provider_ref,
      get_mdp_for_id,
      get_entity_load_by_id.
ENDCLASS.

CLASS lcl_sadl_exit_handler DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      calculate_elements.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.
