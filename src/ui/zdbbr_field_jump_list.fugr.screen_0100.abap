  PROCESS BEFORE OUTPUT.
    MODULE pbo_0100.

    LOOP AT gt_jumpfields INTO gs_jumpfield
          WITH CONTROL jump_fields_tc
          CURSOR jump_fields_tc-current_line.
      MODULE table_line_pbo_0100.
    ENDLOOP.

  PROCESS AFTER INPUT.
    MODULE determine_cursor_0100.
    MODULE back_0100 AT EXIT-COMMAND.

    LOOP AT gt_jumpfields.
      CHAIN.
        FIELD gs_jumpfield-marked.
        FIELD gs_jumpfield-jump_field.
        FIELD gs_jumpfield-criterion.
        FIELD gs_jumpfield-crit_operation.
        FIELD gs_jumpfield-crit_value.
        FIELD gs_jumpfield-jump_target.
        FIELD gs_jumpfield-skip_1st_screen.
        FIELD gs_jumpfield-is_active.
        field gs_jumpfield-jump_target_type.
        FIELD gs_jumpfield-is_hotspot.
        MODULE table_line_pai_0100.
      ENDCHAIN.
    ENDLOOP.

    MODULE table_line_validate_0100.

    MODULE pai_0100.

  PROCESS ON VALUE-REQUEST.
    FIELD gs_jumpfield-jump_field MODULE jump_field_0100_f4.
    FIELD gs_jumpfield-criterion MODULE jump_crit_0100_f4.
    FIELD gs_jumpfield-jump_target MODULE jump_target_0100_f4.
