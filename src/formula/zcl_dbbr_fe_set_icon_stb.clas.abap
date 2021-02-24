CLASS zcl_dbbr_fe_set_icon_stb DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_stmnt_string_builder .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_set_icon_stb IMPLEMENTATION.


  METHOD zif_dbbr_stmnt_string_builder~build_string.

    DATA: lv_stringform            TYPE string,
          lv_stringform_subroutine TYPE string.

    DATA(lv_ffname) = cs_statement-tokens[ 2 ]-str.
    DATA(lv_icon) = CONV zdbbr_icon_lx( cs_statement-tokens[ 3 ]-str ).
    DATA(lv_tooltip) = cs_statement-tokens[ 4 ]-str.

    IF lv_tooltip = 'SPACE' OR lv_tooltip = ''.
      lv_stringform = |{ lv_ffname } = { lv_icon }.|.
      lv_stringform_subroutine = |<{ zif_dbbr_c_global=>c_formula_alias }_{ lv_ffname }> = { lv_icon }.|.
    ELSE.
      lv_tooltip = replace( val = lv_tooltip regex = |'(.+)'| with = '$1' ).
      " |@{ icon_delete+1(2) }\\QLöschmodus ist aktiv@|

      zcl_dbbr_icon_handler=>create_icon(
        EXPORTING
          iv_icon_name = CONV #( lv_icon )
          iv_info      = lv_tooltip
        IMPORTING
          ev_push      = lv_icon
      ).

      lv_stringform = |{ lv_ffname } = '{ lv_icon }'.|.
      lv_stringform_subroutine = |<{ zif_dbbr_c_global=>c_formula_alias }_{ lv_ffname }> = '{ lv_icon }'.|.
    ENDIF.

    cs_statement-stringform = lv_stringform.
    cs_statement-stringform_subroutine = lv_stringform_subroutine.


  ENDMETHOD.
ENDCLASS.
