INTERFACE zif_dbbr_c_sh_selmethod_type
  PUBLIC .
  CONSTANTS table_selection TYPE selmtype VALUE 'T' ##no_text.
  CONSTANTS view_selection TYPE selmtype VALUE 'V' ##no_text.
  CONSTANTS with_text_table_selection TYPE selmtype VALUE 'X' ##no_text.
  CONSTANTS function_exit TYPE selmtype VALUE 'F' ##no_text.
  CONSTANTS help_view TYPE selmtype VALUE 'H' ##no_text.
ENDINTERFACE.
