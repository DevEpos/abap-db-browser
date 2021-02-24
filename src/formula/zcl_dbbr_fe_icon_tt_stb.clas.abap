class ZCL_DBBR_FE_ICON_TT_STB definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_STMNT_STRING_BUILDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_ICON_TT_STB IMPLEMENTATION.


  method ZIF_DBBR_STMNT_STRING_BUILDER~BUILD_STRING.

    cs_statement-stringform =
       |DATA { cs_statement-tokens[ 2 ]-str } TYPE { zif_dbbr_c_fe_global=>c_icon_tt_type }.|.

  endmethod.
ENDCLASS.
