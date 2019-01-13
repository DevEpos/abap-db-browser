*&---------------------------------------------------------------------*
*& Report  ZDBBR_TEST_ALV_EDIT_POPUP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdbbr_test_alv_edit_popup.

DATA(lr_settings_view) = NEW zcl_dbbr_settings_view(
    iv_title    = 'Parameter Settings'
    it_settings = VALUE #(
      ( setting_id   = 'S1'
        setting_name = 'Description'
        setting_value = 'asödflask'
        input_type   = zif_dbbr_c_input_type=>text_input
        lowercase    = abap_true )
      ( setting_id   = 'S2'
        setting_name = 'Is Mandatory'
        setting_value = abap_true
        input_type   = zif_dbbr_c_input_type=>checkbox )
    )
).

lr_settings_view->show( ).
