INTERFACE zif_dbbr_c_cds_view_type
  PUBLIC .


  CONSTANTS view TYPE zdbbr_cds_source_type VALUE 'V' ##NO_TEXT.
  CONSTANTS extend TYPE zdbbr_cds_source_type VALUE 'E' ##NO_TEXT.
  CONSTANTS table_function TYPE zdbbr_cds_source_type VALUE 'F' ##NO_TEXT.
  CONSTANTS table_entity TYPE zdbbr_cds_source_type VALUE 'T' ##NO_TEXT.
  CONSTANTS abstract_entity TYPE zdbbr_cds_source_type VALUE 'A' ##NO_TEXT.
  CONSTANTS custom_entity TYPE zdbbr_cds_source_type VALUE 'Q' ##NO_TEXT.
  CONSTANTS hierarchy TYPE zdbbr_cds_source_type VALUE 'H' ##NO_TEXT.
ENDINTERFACE.
