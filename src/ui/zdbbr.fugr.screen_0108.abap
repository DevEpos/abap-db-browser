PROCESS BEFORE OUTPUT.
  MODULE pbo_0108.

  LOOP AT gt_altcoltext INTO gs_altcoltext
        WITH CONTROL altcoltext_tc
        CURSOR altcoltext_tc-current_line.
    MODULE table_line_pbo_0108.
  ENDLOOP.

PROCESS AFTER INPUT.
  MODULE back_0108 AT EXIT-COMMAND.

  LOOP AT gt_altcoltext.
    CHAIN.
      FIELD gs_altcoltext-alt_short_text.
      FIELD gs_altcoltext-alt_long_text.
      MODULE table_line_pai_0108.
    ENDCHAIN.
  ENDLOOP.

  MODULE pai_0108.
