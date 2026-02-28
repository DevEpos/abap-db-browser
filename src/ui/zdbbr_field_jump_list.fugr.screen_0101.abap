  PROCESS BEFORE OUTPUT.
    MODULE pbo_0101.

    LOOP AT gt_params INTO gs_param
          WITH CONTROL params_tc
          CURSOR params_tc-current_line.
      MODULE table_line_pbo_0101.
    ENDLOOP.

  PROCESS AFTER INPUT.
    MODULE determine_cursor_0101.
    MODULE back_0101 AT EXIT-COMMAND.

    LOOP AT gt_params.
      CHAIN.
        FIELD gs_param-parameter_id.
        FIELD gs_param-value.
        FIELD gs_param-active.
        FIELD gs_param-obligatory.
        MODULE table_line_pai_0101.
      ENDCHAIN.
    ENDLOOP.

    MODULE table_line_validate_0101.

    MODULE pai_0101.

  PROCESS ON VALUE-REQUEST.
    FIELD gs_param-value MODULE param_value_f4_0101.
    FIELD gs_param-parameter_id MODULE param_id_f4_0101.
