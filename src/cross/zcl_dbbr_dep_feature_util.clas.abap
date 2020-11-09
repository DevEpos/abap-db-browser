"! <p class="shorttext synchronized" lang="en">Util for dependent features</p>
CLASS zcl_dbbr_dep_feature_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if SE16N exists in the system</p>
    CLASS-METHODS is_se16n_available
      RETURNING
        VALUE(rf_available) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA sf_se16n_available TYPE abap_bool.
ENDCLASS.



CLASS zcl_dbbr_dep_feature_util IMPLEMENTATION.
  METHOD is_se16n_available.
    rf_available = sf_se16n_available.
  ENDMETHOD.

  METHOD class_constructor.
    SELECT SINGLE @abap_true FROM tfdir WHERE funcname = 'SE16N_INTERFACE' INTO @sf_se16n_available.
  ENDMETHOD.

ENDCLASS.
