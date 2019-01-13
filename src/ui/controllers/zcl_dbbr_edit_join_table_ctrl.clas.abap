CLASS zcl_dbbr_edit_join_table_ctrl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    EVENTS created
      EXPORTING
        VALUE(es_join_table) TYPE zdbbr_joint .

    METHODS constructor
      IMPORTING
        !is_join_table TYPE zdbbr_joint OPTIONAL
        !if_is_new     TYPE abap_bool DEFAULT abap_true .
    METHODS get_updated_join_table
      RETURNING
        VALUE(result) TYPE zdbbr_joint .
  PROTECTED SECTION.
private section.

  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

    "! Type - tabname16
  constants C_P_JOIN_TABLE type DYNFNAM value 'P_JOINTB' ##NO_TEXT.
    "! Type - zdbbr_jointype
  constants C_P_JOIN_TYPE type DYNFNAM value 'P_JOINTY' ##NO_TEXT.
    "! Type - abap_bool
  constants C_F_IS_VIRTUAL_JOIN type DYNFNAM value 'P_XJVIRT' ##NO_TEXT.
  constants C_R_EDIT_JOIN_TABLE_CONTROLLER type DYNFNAM value 'GR_EDIT_JOIN_TABLE_CONTROLLER' ##NO_TEXT.
  data MR_V_JOIN_TABLE type ref to TABNAME .
  data MR_F_IS_VIRTUAL_JOIN type ref to ABAP_BOOL .
  data MR_V_JOIN_TYPE type ref to ZDBBR_JOINTYPE .
  data MF_SAVED type ABAP_BOOL .
  data MS_JOIN_TABLE type ZDBBR_JOINT .
  data MF_IS_NEW type ABAP_BOOL .
  data MR_CURSOR type ref to ZCL_UITB_CURSOR .
  data MR_S_SAVE_FUNC type ref to SMP_DYNTXT .
  data MR_S_SAVE_NEW_FUNC type ref to SMP_DYNTXT .

  methods SEND_UPDATED_VALUE .
  methods VALIDATE .
  methods SET_FUNCTIONS .
ENDCLASS.



CLASS ZCL_DBBR_EDIT_JOIN_TABLE_CTRL IMPLEMENTATION.


  METHOD constructor.
    DEFINE read_cached_field.
      &1 = cast #( lr_data_cache->get_data_ref( |{ &2 }| ) ).
    END-OF-DEFINITION.

    " init global data references from cache
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    read_cached_field:
       mr_v_join_table         c_p_join_table,
       mr_f_is_virtual_join    c_f_is_virtual_join,
       mr_v_join_type          c_p_join_type,
       mr_s_save_func          zif_dbbr_main_report_var_ids=>c_s_save_function,
       mr_s_save_new_func      zif_dbbr_main_report_var_ids=>c_s_save_and_stay_function.

*... update current screen fields
    mf_is_new = if_is_new.
    IF mf_is_new = abap_true.
      CLEAR: mr_v_join_table->*,
             mr_f_is_virtual_join->*.
      mr_v_join_type->* = zif_dbbr_c_join_types=>inner_join.
    ELSE.
      mr_v_join_table->* = is_join_table-add_table.
      mr_v_join_type->* = is_join_table-join_type.
      mr_f_is_virtual_join->* = is_join_table-is_virtual.
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
           mr_f_is_virtual_join->*,
           mr_v_join_table->*,
*......... clear save flag for next saving
           mf_saved.

    mr_v_join_type->* = zif_dbbr_c_join_types=>inner_join.
    mf_is_new = abap_true.
  ENDMETHOD.


  METHOD set_functions.
    IF mf_is_new = abap_true.
      mr_s_save_func->icon_id = icon_create.
      mr_s_save_func->icon_text = 'Create'.
      mr_s_save_func->text = 'Create and close'.

      mr_s_save_new_func->icon_id = icon_create.
      mr_s_save_new_func->icon_text = 'Create +'.
      mr_s_save_new_func->text = 'Create and continue with another entry'.
    ELSE.
      mr_s_save_func->icon_id = icon_system_save.
      mr_s_save_func->icon_text = 'Save'.
    ENDIF.
  ENDMETHOD.


  METHOD validate.
    TRANSLATE mr_v_join_table->* TO UPPER CASE.

    zcl_dbbr_dictionary_helper=>validate_table_name(
        iv_table_name       = CONV #( mr_v_join_table->* )
        iv_dynpro_fieldname = c_p_join_table
    ).

    IF mr_v_join_type->* = zif_dbbr_c_join_types=>right_outer_join and
       mr_f_is_virtual_join->* = abap_true.
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
    mr_cursor = zcl_uitb_cursor=>get_cursor( ).

    TRY.
        CASE lv_function.

          WHEN 'SAVE' OR 'SAVENEW'.
            validate( ).

            ms_join_table = VALUE #(
              add_table  = mr_v_join_table->*
              join_type  = mr_v_join_type->*
              is_virtual = mr_f_is_virtual_join->*
            ).
            mf_saved = abap_true.

            IF lv_function = 'SAVENEW'.
              send_updated_value( ).
            ELSE.
              zcl_dbbr_screen_helper=>leave_screen( ).
            ENDIF.

        ENDCASE.

      CATCH zcx_dbbr_validation_exception INTO DATA(lx_valid).
        IF lx_valid->parameter_name IS NOT INITIAL.
          mr_cursor->set_field( lx_valid->parameter_name ).
          mr_cursor->refresh( ).
        ENDIF.
        lx_valid->print_message( iv_msg_type = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    zif_uitb_screen_controller~set_status( ).

    IF mr_cursor is bound and mr_cursor->is_update_requested( ).
      mr_cursor->refresh( ).
    ENDIF.

    LOOP AT SCREEN.
      IF screen-name = c_p_join_table.
        IF mf_is_new = abap_false.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    set_functions( ).
    zcl_dbbr_screen_helper=>set_selscreen_status(
        iv_status              = 'EDIT_DIALOG_STATUS'
        iv_repid               = zif_dbbr_c_report_id=>main
        it_excluding_functions = cond #(
          when mf_is_new = abap_false then
             value #( ( 'SAVENEW' ) )
        )
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_saved.
  ENDMETHOD.
ENDCLASS.
