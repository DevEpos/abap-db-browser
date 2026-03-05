PROCESS BEFORE OUTPUT.

  MODULE table_pbo_0100.
  MODULE get_linecount_0100.

  LOOP AT gt_selection_fields INTO gs_selfields
     WITH CONTROL selfields_tc
     CURSOR selfields_tc-current_line.
    MODULE display_data.
    MODULE table_line_pbo_0100.
    MODULE determine_looplines_0100.
  ENDLOOP.

  MODULE modify_screen.

PROCESS AFTER INPUT.

  MODULE cancel AT EXIT-COMMAND.

  CHAIN.
    FIELD gs_entity_info-entity_id.
    MODULE check_entity ON CHAIN-REQUEST.
  ENDCHAIN.

  FIELD gs_data-edit MODULE check_edit_mode.

  MODULE reset_flags.

  LOOP AT gt_selection_fields.
    CHAIN.
      FIELD gs_selfields-low.
      FIELD gs_selfields-high.
      FIELD gs_selfields-group_by.
      FIELD gs_selfields-totals.
      FIELD gs_selfields-aggregation.
      FIELD gs_selfields-system_value_type.
      MODULE update_selfields ON CHAIN-REQUEST.
    ENDCHAIN.
  ENDLOOP.

  MODULE user_command_0100.

PROCESS ON VALUE-REQUEST.
  FIELD gs_selfields-low MODULE field_f4_low.
  FIELD gs_selfields-high MODULE field_f4_high.
