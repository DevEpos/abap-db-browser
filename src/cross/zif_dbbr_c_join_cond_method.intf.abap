INTERFACE zif_dbbr_c_join_cond_method
  PUBLIC.


  CONSTANTS constant TYPE zdbbr_join_method VALUE 'CONSTANT' ##NO_TEXT.
  CONSTANTS reference_field TYPE zdbbr_join_method VALUE 'REF_FIELD' ##NO_TEXT.
  CONSTANTS constant_reference_field TYPE zdbbr_join_method VALUE 'CONST_REF' ##NO_TEXT.
  CONSTANTS system_constant TYPE zdbbr_join_method VALUE 'SYST_CONST' ##NO_TEXT.
ENDINTERFACE.
