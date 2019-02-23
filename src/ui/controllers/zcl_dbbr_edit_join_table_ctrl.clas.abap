"! <p class="shorttext synchronized" lang="en">Controlling for editing a single join table</p>
CLASS zcl_dbbr_edit_join_table_ctrl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    "! <p class="shorttext synchronized" lang="en">Created new join table</p>
    EVENTS created
      EXPORTING
        VALUE(es_join_table) TYPE zdbbr_join_table_ui .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !is_join_table TYPE zdbbr_joint OPTIONAL
        !if_is_new     TYPE abap_bool DEFAULT abap_true .
    "! <p class="shorttext synchronized" lang="en">Retrieves the updated join table</p>
    METHODS get_updated_join_table
      RETURNING
        VALUE(result) TYPE zdbbr_join_table_ui .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    "! Type - tabname
    CONSTANTS c_p_join_table TYPE dynfnam VALUE 'P_JOINTB' ##NO_TEXT.
    "! Type - zdbbr_jointype
    CONSTANTS c_p_join_type TYPE dynfnam VALUE 'P_JOINTY' ##NO_TEXT.
    "! Type - abap_bool
    CONSTANTS c_f_is_virtual_join TYPE dynfnam VALUE 'P_XJVIRT' ##NO_TEXT.
    CONSTANTS c_r_edit_join_table_controller TYPE dynfnam VALUE 'GR_EDIT_JOIN_TABLE_CONTROLLER' ##NO_TEXT.
    "! Type - zdbbr_entity_alias
    CONSTANTS c_p_alias TYPE dynfnam VALUE 'P_JOINTA' ##no_text.
    "! Type - zdbbr_entity_alias_alv
    CONSTANTS c_p_alias_alv TYPE dynfnam VALUE 'P_JONTIA' ##no_text.

    DATA mr_join_table TYPE REF TO tabname .
    DATA mr_is_virtual_join TYPE REF TO abap_bool .
    DATA mr_alias TYPE REF TO zdbbr_entity_alias.
    DATA mr_join_type TYPE REF TO zdbbr_jointype .
    DATA mf_saved TYPE abap_bool .
    data ms_entity_temp type zdbbr_entity.
    "! <p class="shorttext synchronized" lang="en">Join table</p>
    DATA ms_join_table TYPE zdbbr_join_table_ui .
    DATA mv_old_alias TYPE zdbbr_entity_alias.
    DATA mf_is_new TYPE abap_bool .
    DATA mo_cursor TYPE REF TO zcl_uitb_cursor .
    DATA mr_save_func TYPE REF TO smp_dyntxt .
    DATA mr_save_new_func TYPE REF TO smp_dyntxt .

    "! <p class="shorttext synchronized" lang="en">Sends update value via event and clears screen values</p>
    "!
    METHODS send_updated_value .
    "! <p class="shorttext synchronized" lang="en">Validate screen parameters</p>
    "!
    METHODS validate .
    "! <p class="shorttext synchronized" lang="en">Set function icons/texts</p>
    "!
    METHODS set_functions .
    "! <p class="shorttext synchronized" lang="en">Saves the validated Join Entity</p>
    "!
    METHODS save_entity
      IMPORTING
        if_leave_screen TYPE abap_bool.
ENDCLASS.



CLASS zcl_dbbr_edit_join_table_ctrl IMPLEMENTATION.


  METHOD constructor.
    DEFINE read_cached_field.
      &1 = CAST #( lr_data_cache->get_data_ref( |{ &2 }| ) ).
    END-OF-DEFINITION.

    " init global data references from cache
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    read_cached_field:
       mr_join_table         c_p_join_table,
       mr_is_virtual_join    c_f_is_virtual_join,
       mr_alias              c_p_alias,
       mr_join_type          c_p_join_type,
       mr_save_func          zif_dbbr_main_report_var_ids=>c_s_save_function,
       mr_save_new_func      zif_dbbr_main_report_var_ids=>c_s_save_and_stay_function.

*... update current screen fields
    mf_is_new = if_is_new.
    IF mf_is_new = abap_true.
      CLEAR: mr_join_table->*,
             mr_alias->*,
             mr_is_virtual_join->*.
      mr_join_type->* = zif_dbbr_c_join_types=>inner_join.
    ELSE.
      mr_join_table->* = is_join_table-add_table.
      mr_join_type->* = is_join_table-join_type.
      mr_is_virtual_join->* = is_join_table-is_virtual.
      mr_alias->* = mv_old_alias
                  = is_join_table-add_table_alias.
      ms_entity_temp = value #(
        entity_type = is_join_table-entity_type
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_updated_join_table.
    result = ms_join_table.
  ENDMETHOD.


  METHOD send_updated_value.
    RAISE EVENT created
      EXPORTING
        es_join_table = ms_join_table.

    CLEAR: ms_join_table,
           ms_entity_temp,
           mr_is_virtual_join->*,
           mr_join_table->*,
           mr_alias->*,
*......... clear save flag for next saving
           mf_saved.

    mr_join_type->* = zif_dbbr_c_join_types=>inner_join.
    mf_is_new = abap_true.
    mo_cursor->set_field( c_p_join_table ).
    mo_cursor->request_update( ).
  ENDMETHOD.


  METHOD set_functions.
    IF mf_is_new = abap_true.
      mr_save_func->icon_id = icon_create.
      mr_save_func->icon_text = 'Create'.
      mr_save_func->text = 'Create and close'.

      mr_save_new_func->icon_id = icon_create.
      mr_save_new_func->icon_text = 'Create +'.
      mr_save_new_func->text = 'Create and continue with another entry'.
    ELSE.
      mr_save_func->icon_id = icon_system_save.
      mr_save_func->icon_text = 'Save'.
    ENDIF.
  ENDMETHOD.


  METHOD validate.
    TRANSLATE mr_join_table->* TO UPPER CASE.

    TRY.
        ms_entity_temp = zcl_dbbr_dictionary_helper=>get_entity( mr_join_table->* ).
      CATCH  zcx_dbbr_data_read_error INTO DATA(lx_read_error).
        RAISE EXCEPTION TYPE zcx_dbbr_validation_exception
          EXPORTING
            previous       = lx_read_error
            parameter_name = c_p_join_table.
    ENDTRY.

*.. Define Entity alias if the field is still empty and check if it is unique
    IF mr_alias->* IS INITIAL.
      mr_alias->* = zcl_dbbr_entity_alias_util=>create_entity_alias(
          iv_entity = to_upper( mr_join_table->* )
      ).
    ENDIF.

    IF mf_is_new = abap_false.
      IF mv_old_alias IS NOT INITIAL AND mv_old_alias <> mr_alias->*.
        zcl_dbbr_entity_alias_util=>unregister_alias( mv_old_alias ).
        CLEAR: mv_old_alias.
      ELSE.
        DATA(lf_do_not_check_alias) = abap_true.
      ENDIF.
    ENDIF.

*.. Check the alias for uniqueness
    TRY.
        IF lf_do_not_check_alias = abap_false.
          zcl_dbbr_entity_alias_util=>check_entity_alias( mr_alias->* ).
        ENDIF.
      CATCH zcx_dbbr_validation_exception INTO DATA(lx_valid_error).
*...... Re raise exception with screen parameter connection
        zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = lx_valid_error->get_text( )
            iv_parameter = c_p_alias
        ).
    ENDTRY.

    IF mr_join_type->* = zif_dbbr_c_join_types=>right_outer_join AND
       mr_is_virtual_join->* = abap_true.
      zcx_dbbr_validation_exception=>raise_with_text(
          iv_text      = |Right Outer Join is not possible with Virtual Join|
          iv_parameter = c_p_join_type
      ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = c_r_edit_join_table_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.
    CLEAR mf_saved.

*.. free already registered Entity Alias
    IF mr_alias->* IS NOT INITIAL.
      zcl_dbbr_entity_alias_util=>unregister_alias( mr_alias->* ).
    ENDIF.

    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_maintain_join_table.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    DATA(lv_function) = cv_function_code.
    CLEAR cv_function_code.
    mo_cursor = zcl_uitb_cursor=>get_cursor( ).

    TRY.
        CASE lv_function.

          WHEN 'SAVE' OR 'SAVENEW'.
            validate( ).

            save_entity( if_leave_screen = xsdbool( lv_function <> 'SAVENEW' ) ).

        ENDCASE.

      CATCH zcx_dbbr_validation_exception INTO DATA(lx_valid).
        IF lx_valid->parameter_name IS NOT INITIAL.
          mo_cursor->set_field( lx_valid->parameter_name ).
          mo_cursor->refresh( ).
        ENDIF.
        lx_valid->print_message( iv_msg_type = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).

    IF mo_cursor IS BOUND AND mo_cursor->is_update_requested( ).
      mo_cursor->refresh( ).
    ENDIF.

    LOOP AT SCREEN.
      IF screen-name = c_p_join_table.
        IF mf_is_new = abap_false.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ELSEIF screen-name = c_p_alias_alv.
        screen-input = 0.
        MODIFY SCREEN.
      ELSEIF screen-name = c_p_alias.
        screen-required = '2'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    set_functions( ).
    zcl_dbbr_screen_helper=>set_selscreen_status(
        iv_status              = 'EDIT_DIALOG_STATUS'
        iv_repid               = zif_dbbr_c_report_id=>main
        it_excluding_functions = COND #(
          WHEN mf_is_new = abap_false THEN
             VALUE #( ( 'SAVENEW' ) )
        )
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_saved.
  ENDMETHOD.

  METHOD save_entity.
    ms_join_table = VALUE #(
      add_table           = mr_join_table->*
      add_table_alias     = mr_alias->*
      add_table_raw       = ms_entity_temp-entity_id_raw
      join_type           = mr_join_type->*
      is_virtual          = mr_is_virtual_join->*
      entity_type         = ms_entity_temp-entity_type
      table_name          = ms_entity_temp-description
    ).

    zcl_dbbr_entity_alias_util=>add_entity_alias( mr_alias->* ).
    mf_saved = abap_true.

    IF if_leave_screen = abap_false.
      send_updated_value( ).
    ELSE.
      zcl_dbbr_screen_helper=>leave_screen( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
