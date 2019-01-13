class ZCL_DBBR_FE_COLOR_DEF_TV definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_TOKEN_VALIDATOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_COLOR_DEF_TV IMPLEMENTATION.


  method ZIF_DBBR_TOKEN_VALIDATOR~VALIDATE.


    CASE cs_token-id.
      WHEN 1.
      WHEN OTHERS.
    ENDCASE.

  endmethod.
ENDCLASS.
