class ZCL_DBBR_FE_SET_ICON_STB definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_STMNT_STRING_BUILDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_SET_ICON_STB IMPLEMENTATION.


  method ZIF_DBBR_STMNT_STRING_BUILDER~BUILD_STRING.

    DATA: lv_stringform            TYPE string,
          lv_stringform_subroutine TYPE string.

    DATA(lv_ffname) = cs_statement-tokens[ 2 ]-str.
    DATA(lv_icon) = CONV ZDBBR_icon_lx( cs_statement-tokens[ 3 ]-str ).
    DATA(lv_tooltip) = cs_statement-tokens[ 4 ]-str.

    IF lv_tooltip = 'SPACE' OR lv_tooltip = ''.
      lv_stringform = |{ lv_ffname } = { lv_icon }.|.
      lv_stringform_subroutine = |<{ ZIF_DBBR_global_consts=>c_formula_alias }_{ lv_ffname }> = { lv_icon }.|.
    ELSE.
      lv_tooltip = replace( val = lv_tooltip regex = |'(.+)'| with = '$1' ).
      " |@{ icon_delete+1(2) }\\QLöschmodus ist aktiv@|

      ZCL_DBBR_icon_handler=>create_icon(
        EXPORTING
          iv_icon_name = CONV #( lv_icon )
          iv_info      = lv_tooltip
        IMPORTING
          ev_push      = lv_icon
      ).

      lv_stringform = |{ lv_ffname } = '{ lv_icon }'.|.
      lv_stringform_subroutine = |<{ ZIF_DBBR_global_consts=>c_formula_alias }_{ lv_ffname }> = '{ lv_icon }'.|.
    ENDIF.

    cs_statement-stringform = lv_stringform.
    cs_statement-stringform_subroutine = lv_stringform_subroutine.


  endmethod.
ENDCLASS.
