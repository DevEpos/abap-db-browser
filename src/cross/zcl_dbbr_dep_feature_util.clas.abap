"! <p class="shorttext synchronized" lang="en">Util for dependent features</p>
CLASS zcl_dbbr_dep_feature_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      c_cds_annotation_definition TYPE string VALUE 'DDLA_RT_HEADER',
      c_if_sadl_exit_calc_element TYPE string VALUE 'IF_SADL_EXIT_CALC_ELEMENT_READ'.

    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if SE16N exists in the system</p>
    CLASS-METHODS is_se16n_available
      RETURNING
        VALUE(rf_available) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Returns 'X' CDS virtual elements are supported</p>
    CLASS-METHODS is_cds_virtelem_supported
      RETURNING
        VALUE(rf_available) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Check if CDS virtual elements are supported</p>
    CLASS-METHODS check_cds_virtelem_supported.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA sf_se16n_available TYPE abap_bool.
    CLASS-DATA sf_cds_virtelem_supported TYPE abap_bool VALUE abap_undefined.
ENDCLASS.



CLASS zcl_dbbr_dep_feature_util IMPLEMENTATION.
  METHOD is_se16n_available.
    rf_available = sf_se16n_available.
  ENDMETHOD.

  METHOD class_constructor.
    SELECT SINGLE @abap_true FROM tfdir WHERE funcname = 'SE16N_INTERFACE' INTO @sf_se16n_available.
  ENDMETHOD.

  METHOD is_cds_virtelem_supported.

    check_cds_virtelem_supported( ).

    rf_available = sf_cds_virtelem_supported.
  ENDMETHOD.

  METHOD check_cds_virtelem_supported.

    CHECK sf_cds_virtelem_supported = abap_undefined.

    TRY.
        cl_abap_typedescr=>describe_by_name(
          EXPORTING
            p_name         = c_if_sadl_exit_calc_element
          RECEIVING
            p_descr_ref    = DATA(lo_inf)
          EXCEPTIONS
            type_not_found = 1 ).

        IF sy-subrc = 0 AND lo_inf->kind = cl_abap_typedescr=>kind_intf .
          SELECT SINGLE @abap_true FROM (c_cds_annotation_definition)
            WHERE key_upper = @zif_dbbr_c_annotations=>objectmodel-virtual_elem_calc_by
              INTO @sf_cds_virtelem_supported.
        ELSE.
          sf_cds_virtelem_supported = abap_false.
        ENDIF.
      CATCH cx_sy_dyn_call_error. "virtual element is not supported
        sf_cds_virtelem_supported = abap_false.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
