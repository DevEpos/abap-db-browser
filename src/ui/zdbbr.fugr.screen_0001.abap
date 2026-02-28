PROCESS BEFORE OUTPUT.

  MODULE pbo_0001.
  MODULE get_linecount_0001.

  LOOP AT gt_multi_select INTO gs_multi_select
       WITH CONTROL multi_tc
       CURSOR multi_tc-current_line.

    MODULE display_lines.
    MODULE prepare_fields.
  ENDLOOP.

PROCESS AFTER INPUT.

  MODULE cancel_0001 AT EXIT-COMMAND.

  LOOP AT gt_multi_select.
    CHAIN.
      FIELD gs_multi_select-low.
      FIELD gs_multi_select-high.
      MODULE update_selection_field_multi on CHAIN-REQUEST.
    ENDCHAIN.
  ENDLOOP.

  MODULE pai_0001.

PROCESS ON VALUE-REQUEST.

  FIELD gs_multi_select-low  MODULE field_f4_multi_low.
  FIELD gs_multi_select-high MODULE field_f4_multi_high.
