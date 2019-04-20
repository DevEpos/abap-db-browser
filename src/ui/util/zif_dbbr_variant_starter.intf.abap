"! <p class="shorttext synchronized" lang="en">Starter of variant</p>
INTERFACE zif_dbbr_variant_starter
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Execute Variant</p>
  "!
  "! @parameter rf_no_data | <p class="shorttext synchronized" lang="en">'X' if no data was found during selection</p>
  "! @raising ZCX_DBBR_VARIANT_ERROR | Error during execution
  METHODS execute_variant
    RETURNING
      VALUE(rf_no_data) TYPE abap_bool
    RAISING
      zcx_dbbr_variant_error.
  "! <p class="shorttext synchronized" lang="en">Initialize Variant</p>
  METHODS initialize .
ENDINTERFACE.
