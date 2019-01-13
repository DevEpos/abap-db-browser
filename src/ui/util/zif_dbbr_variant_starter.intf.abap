"! <p class="shorttext synchronized" lang="en">Starter of variant</p>
INTERFACE zif_dbbr_variant_starter
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Execute Variant</p>
  "! @raising ZCX_DBBR_VARIANT_ERROR | Error during execution
  METHODS execute_variant
    RAISING
      zcx_dbbr_variant_error .
  "! <p class="shorttext synchronized" lang="en">Initialize Variant</p>
  METHODS initialize .
ENDINTERFACE.
