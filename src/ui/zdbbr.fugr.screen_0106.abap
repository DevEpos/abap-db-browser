PROCESS BEFORE OUTPUT.
  MODULE get_linecount_0106.

  LOOP AT gt_multi_or INTO gs_multi_or
       WITH CONTROL multi_or_tc
       CURSOR multi_or_tc-current_line.
    MODULE table_pbo_0106.
  ENDLOOP.

  MODULE pbo_0106.

PROCESS AFTER INPUT.
  MODULE cancel_0106 AT EXIT-COMMAND.

  LOOP AT gt_multi_or.
    CHAIN.
      FIELD gs_multi_or-low.
      FIELD gs_multi_or-high.
      MODULE update_selfield_0106 ON CHAIN-REQUEST.
    ENDCHAIN.
  ENDLOOP.

  MODULE pai_0106.

PROCESS ON VALUE-REQUEST.

  FIELD gs_multi_or-low  MODULE field_f4_multi_or_low.
  FIELD gs_multi_or-high MODULE field_f4_multi_or_high.
