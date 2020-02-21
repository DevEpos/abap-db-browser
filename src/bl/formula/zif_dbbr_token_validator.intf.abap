"! <p class="shorttext synchronized" lang="en">Token Validator</p>
INTERFACE zif_dbbr_token_validator
  PUBLIC .
  METHODS validate
    CHANGING
      cs_token TYPE zif_dbbr_fe_types=>ty_token
    RAISING
      zcx_dbbr_fe_stmnt_valid_exc.
ENDINTERFACE.
