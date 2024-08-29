INTERFACE zif_dbbr_c_sh_selmethod_type
  PUBLIC.
  CONSTANTS table_selection TYPE selmtype VALUE 'T' ##NO_TEXT.
  CONSTANTS view_selection TYPE selmtype VALUE 'V' ##NO_TEXT.
  CONSTANTS with_text_table_selection TYPE selmtype VALUE 'X' ##NO_TEXT.
  CONSTANTS function_exit TYPE selmtype VALUE 'F' ##NO_TEXT.
  CONSTANTS help_view TYPE selmtype VALUE 'H' ##NO_TEXT.
ENDINTERFACE.
