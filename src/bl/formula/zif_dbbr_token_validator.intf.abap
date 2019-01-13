INTERFACE ZIF_DBBR_token_validator
  PUBLIC .
  METHODS validate
    CHANGING
      cs_token TYPE ZIF_DBBR_fe_types=>ty_token
    RAISING
      ZCX_DBBR_fe_stmnt_valid_exc.
ENDINTERFACE.
