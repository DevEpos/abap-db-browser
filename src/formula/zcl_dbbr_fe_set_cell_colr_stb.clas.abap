class ZCL_DBBR_FE_SET_CELL_COLR_STB definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_STMNT_STRING_BUILDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_SET_CELL_COLR_STB IMPLEMENTATION.


  method ZIF_DBBR_STMNT_STRING_BUILDER~BUILD_STRING.

    DATA(lv_row_field) = ZCL_DBBR_formula_helper=>get_raw_row_field( cs_statement-tokens[ 2 ]-str ).
    DATA(lv_color_code) = replace( val = cs_statement-tokens[ 3 ]-str regex = |'(.+)'| with = '$1' ).
    DATA(lv_cell_color) =
      |&1 = VALUE { zif_dbbr_c_special_out_columns=>alv_col_color_type }( | &&
      |  BASE &1 | &&
      |  ( fname = '{ lv_row_field }' | &&
      |    color = VALUE #( COL = { lv_color_code+1(1) } INT = { lv_color_code+2(1) } INV = { lv_color_code+3(1) } ) | &&
      |  ) ).|.

    DATA(lv_color_col) = zif_dbbr_c_special_out_columns=>cell_col_row_color.
    DATA(lv_subroutine_color_col) = |<{ zif_dbbr_c_special_out_columns=>cell_col_row_color }>|.

    cs_statement-stringform = replace( val = lv_cell_color sub = '&1' with = lv_color_col occ = 0 ).
    cs_statement-stringform_subroutine = replace( val = lv_cell_color sub = '&1' with = lv_subroutine_color_col occ = 0 ).

  endmethod.
ENDCLASS.
