PROCESS BEFORE OUTPUT.
  MODULE status_0101.

PROCESS AFTER INPUT.

  MODULE back_0101 AT EXIT-COMMAND.

  FIELD gs_built_in_f4-search_table MODULE check_search_table.
  FIELD gs_built_in_f4-search_field MODULE check_search_field.

  MODULE user_command_0101.

PROCESS ON VALUE-REQUEST.

  FIELD gs_built_in_f4-search_table MODULE search_table_f4.
  FIELD gs_built_in_f4-search_field MODULE search_field_f4.
