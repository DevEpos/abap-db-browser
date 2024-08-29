INTERFACE zif_dbbr_c_fe_global
  PUBLIC.

  "! Type for Icon Formula Fields
  CONSTANTS c_icon_type TYPE rollname VALUE 'ICON_D' ##NO_TEXT.
  "! Type for Tooltip Icon Formula Fields
  CONSTANTS c_icon_tt_type TYPE rollname VALUE 'ZDBBR_ICON_LX' ##NO_TEXT.
  "! Name of the Form in the formula subroutine which will
  "! be called for every row in the output table
  CONSTANTS c_formula_subroutine_form TYPE progname VALUE 'ZZ_FORM' ##NO_TEXT.
ENDINTERFACE.
