*&---------------------------------------------------------------------*
*& Report zdbbr_test_cds_field_vh
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdbbr_test_cds_field_vh.

new zcl_dbbr_cds_field_value_help(
  iv_entity = 'I_PRODUCT'
  iv_field  = 'PRODUCTTYPE'
)->show(
    iv_top    = 5
    iv_left   = 15
    iv_width  = 120
    iv_height = 25
).
