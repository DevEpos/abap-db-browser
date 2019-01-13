INTERFACE zif_dbbr_c_entity_type
  PUBLIC .


  CONSTANTS table TYPE zdbbr_entity_type VALUE 'T' ##NO_TEXT.
  CONSTANTS view TYPE zdbbr_entity_type VALUE 'V' ##no_text.
  CONSTANTS cds_view TYPE zdbbr_entity_type VALUE 'C' ##NO_TEXT.
  CONSTANTS query TYPE zdbbr_entity_type VALUE 'Q' ##NO_TEXT.
ENDINTERFACE.
