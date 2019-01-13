INTERFACE zif_dbbr_c_cds_api_state
  PUBLIC .
  CONSTANTS use_in_sap_cloud_platform TYPE zdbbr_cds_api_state VALUE '2' ##no_text.
  CONSTANTS use_in_key_user_apps TYPE zdbbr_cds_api_state VALUE '1' ##no_text.
  CONSTANTS use_as_remote_api TYPE zdbbr_cds_api_state VALUE '3' ##no_text.
  CONSTANTS add_custom_fields TYPE zdbbr_cds_api_state VALUE '4' ##no_text.
  CONSTANTS not_released TYPE zdbbr_cds_api_state VALUE '5' ##no_text.
ENDINTERFACE.
