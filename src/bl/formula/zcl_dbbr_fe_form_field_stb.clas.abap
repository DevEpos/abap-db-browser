class ZCL_DBBR_FE_FORM_FIELD_STB definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_STMNT_STRING_BUILDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_FORM_FIELD_STB IMPLEMENTATION.


  method ZIF_DBBR_STMNT_STRING_BUILDER~BUILD_STRING.

    " $DEF <formula field> TYPE <type name>.

    cs_statement-stringform =
       |DATA { cs_statement-tokens[ 2 ]-str } TYPE { cs_statement-tokens[ 4 ]-str }.|
    .

  endmethod.
ENDCLASS.
