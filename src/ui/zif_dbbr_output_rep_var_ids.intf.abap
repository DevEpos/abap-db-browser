INTERFACE zif_dbbr_output_rep_var_ids
  PUBLIC .


  "! Type - ZSAT_QUERY_NAME
  CONSTANTS c_p_queryn TYPE string VALUE 'P_QUERYN' ##NO_TEXT.
  "! Type - string
  CONSTANTS c_p_retfld TYPE string VALUE 'P_RETFLD' ##NO_TEXT.
  "! Type - tabname16
  CONSTANTS c_p_rettab TYPE string VALUE 'P_RETTAB' ##NO_TEXT.
  "! Type - boolean
  CONSTANTS c_p_distct TYPE string VALUE 'P_DISTCT' ##NO_TEXT.
  "! Type - ddtext
  CONSTANTS c_p_descr TYPE string VALUE 'P_DESCR' ##NO_TEXT.
  "! Type - etobj_name
  CONSTANTS c_p_eccon TYPE string VALUE 'P_ECCON' ##NO_TEXT.
  "! Type - etvar_id
  CONSTANTS c_p_ecvar TYPE string VALUE 'P_ECVAR' ##NO_TEXT.
  "! Type - etp_name
  CONSTANTS c_p_ecpar TYPE string VALUE 'P_ECPAR' ##NO_TEXT.
  "! Type - tabname16
  CONSTANTS c_p_ecdbn TYPE string VALUE 'P_ECDBN' ##NO_TEXT.
  CONSTANTS c_r_controller TYPE string VALUE 'GR_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_query_save_controller TYPE string VALUE 'GR_QUERY_SAVE_CONTROLLER' ##NO_TEXT.
  CONSTANTS c_r_read_ecatt_controller TYPE string VALUE 'GR_READ_ECATT_CONTROLLER' ##NO_TEXT.
ENDINTERFACE.
