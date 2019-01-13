class ZCL_DBBR_EDIT_JOIN_COND_VIEW definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_SCREEN_CONTROLLER .

  aliases SHOW
    for ZIF_UITB_SCREEN_CONTROLLER~CALL_SCREEN .
  aliases WAS_NOT_CANCELLED
    for ZIF_UITB_SCREEN_CONTROLLER~WAS_NOT_CANCELLED .

  constants C_FIELD_MODE type I value 1 ##NO_TEXT.
  constants C_VALUE_MODE type I value 2 ##NO_TEXT.

  events CREATED_FILTER_CONDITION
    exporting
      value(ES_FILTER) type ZDBBR_JOINFIL .
  events CREATED_FIELD_CONDITION
    exporting
      value(ES_FIELD) type ZDBBR_JOINFLD .

  class-methods CLASS_CONSTRUCTOR .
  methods CALL_SOURCE_FIELD_F4 .
  methods CALL_TARGET_FIELD_F4 .
  methods CALL_VALUE1_F4 .
  methods CALL_VALUE2_F4 .
  methods CONSTRUCTOR
    importing
      !IF_IS_NEW type ABAP_BOOL default ABAP_TRUE
      !IS_FILTER_CONDITION type ZDBBR_JOINFIL optional
      !IS_FIELD_CONDITION type ZDBBR_JOINFLD optional
      !IV_MODE type I default C_FIELD_MODE
      !IF_ALLOW_OFFSET type ABAP_BOOL optional
      !IV_SOURCE_TABLE type TABNAME optional
      !IT_TARGET_TABLE_LIST type ZDBBR_TABNAME_RANGE_ITAB optional .
  methods GET_UPDATED_CONDITION
    exporting
      !ES_FIELD_CONDITION type ZDBBR_JOINFLD
      !ES_FILTER_CONDITION type ZDBBR_JOINFIL .
  PROTECTED SECTION.
private section.

  aliases MF_FIRST_CALL
    for ZIF_UITB_SCREEN_CONTROLLER~MF_FIRST_CALL .
  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

    "! type - syst_title
  constants C_V_JOIN_CONDITION_TITLE type DYNFNAM value 'JOINCOND' ##NO_TEXT.
    "! Type - fieldname
  constants C_P_JOIN_SRC_FIELD type DYNFNAM value 'P_SRCFLD' ##NO_TEXT.
    "! Type - tabname16
  constants C_P_JOIN_SRC_TAB type DYNFNAM value 'P_SRCTAB' ##NO_TEXT.
    "! Type - voperator
  constants C_P_JOIN_COND_COMPARATOR1 type DYNFNAM value 'P_COMP1' ##NO_TEXT.
    "! Type - char length 60
  constants C_V_TYPE_FOR_JOIN_COND_VAL type DYNFNAM value 'GV_TYPE_OF_VALUE' ##NO_TEXT.
    "! Type - dynamic from gv_type_of_value
  constants C_P_JOIN_COND_VALUE1 type DYNFNAM value 'P_VAL1' ##NO_TEXT.
    "! Type - dynamic from gv_type_of_value
  constants C_P_JOIN_COND_VALUE2 type DYNFNAM value 'P_VAL2' ##NO_TEXT.
    "! Type - fieldname
  constants C_P_JOIN_TRGT_FIELD type DYNFNAM value 'P_TRGFLD' ##NO_TEXT.
    "! Type - doffset
  constants C_P_JOIN_TRGT_OFFSET type DYNFNAM value 'P_TFLDOF' ##NO_TEXT.
    "! Type - ddleng
  constants C_P_JOIN_TRGT_OFFSET_LENGTH type DYNFNAM value 'P_TFLDOL' ##NO_TEXT.
    "! Type datatype_d
  constants C_P_JOIN_TRGT_FLD_DATATYPE type DYNFNAM value 'P_TRGDTP' ##NO_TEXT.
    "! Type datatype_d
  constants C_P_JOIN_SOURCE_FLD_DATATYPE type DYNFNAM value 'P_SRCDTP' ##NO_TEXT.
    "! Type ddleng
  constants C_P_JOIN_TRGT_FLD_LENGTH type DYNFNAM value 'P_TRGLNG' ##NO_TEXT.
    "! Type ddleng
  constants C_P_JOIN_SOURCE_FLD_LENGTH type DYNFNAM value 'P_SRCLNG' ##NO_TEXT.
    "! Type - tabname16
  constants C_P_JOIN_TRGT_TAB type DYNFNAM value 'P_TRGTAB' ##NO_TEXT.
  constants C_P_JOIN_COND_VALUE_TYPE type DYNFNAM value 'P_VALTY' ##NO_TEXT.
  constants C_R_EDIT_JOIN_COND_VIEW type DYNFNAM value 'GR_EDIT_JOIN_COND_VIEW' ##NO_TEXT.
  data MF_SAVED type ABAP_BOOL .
  data MF_IS_NEW type ABAP_BOOL .
  data MR_V_JOIN_SOURCE_FIELD type ref to FIELDNAME .
  data MR_V_JOIN_SOURCE_TAB type ref to TABNAME16 .
  data MR_V_JOIN_COND_COMP1 type ref to VOPERATOR .
  data MR_V_JOIN_COND_VAL_TYPE type ref to ZDBBR_JOIN_COND_VALUE_TYPE .
  data MR_V_JOIN_COND_VALUE1 type ref to ZDBBR_VALUE .
  data MR_V_JOIN_COND_VALUE2 type ref to ZDBBR_VALUE .
  data MR_V_JOIN_TARGET_FIELD type ref to FIELDNAME .
  data MR_V_JOIN_TARGET_TAB type ref to TABNAME16 .
  data MR_V_JOIN_TARGET_OFFSET type ref to DOFFSET .
  data MR_V_JOIN_TARGET_OFFSET_LENGTH type ref to DDLENG .
  data MV_MODE type I .
  class-data ST_COMPARATOR_FIX_VALS type VRM_VALUES .
  data MF_VALUE2_VISIBLE type ABAP_BOOL .
  data MR_V_TITLE type ref to SYST_TITLE .
  data MV_VALUE_FIELD_LENGTH type I .
  data MV_VALUE_FIELD_ROLLNAME type ROLLNAME .
  data MT_TARGET_TABLE_LIST type ZDBBR_TABNAME_RANGE_ITAB .
  data MF_ALLOW_OFFSET type ABAP_BOOL .
  data MR_V_JOIN_TRGT_FIELD_DATATYPE type ref to DATATYPE_D .
  data MR_V_JOIN_SOURCE_FLD_DATATYPE type ref to DATATYPE_D .
  data MR_V_JOIN_TRGT_FLD_LENGTH type ref to DDLENG .
  data MR_V_JOIN_SOURCE_FLD_LENGTH type ref to DDLENG .
  data MS_FIELD_CONDITION type ZDBBR_JOINFLD .
  data MS_FILTER_CONDITION type ZDBBR_JOINFIL .
  data MR_CURSOR type ref to ZCL_UITB_CURSOR .
  data MR_S_SAVE_FUNC type ref to SMP_DYNTXT .
  data MR_S_SAVE_NEW_FUNC type ref to SMP_DYNTXT .
  data MF_ERROR type ABAP_BOOL .

  methods VALIDATE_PARAMETER .
  methods CALL_BUILT_IN_F4_FOR_VALUE
    importing
      !IV_DYNP_FIELDNAME type DYNFNAM
    changing
      !CV_VALUE type ZDBBR_VALUE .
  methods CLEAR_FIELD_ATTRIBUTES .
  methods CONVERT_VALUES_TO_INTERNAL .
  methods CONVERT_VALUES_TO_DISPLAY .
  methods FILL_COMPARATOR_LIST .
  methods FILL_FIELD_ATTRIBUTE_FIELDS
    importing
      !IF_SOURCE type ABAP_BOOL optional
      !IR_SCREEN_FIELD_MANAGER type ref to ZCL_UITB_SCREEN_FIELD_MANAGER optional
      !IV_FIELDNAME type FIELDNAME optional
      !IV_TABNAME type TABNAME16 optional .
  methods SEND_NEW_CONDITION_VIA_EVENT .
  methods SET_FUNCTIONS .
  methods TRANSFER_VALUES .
  methods TRANSFER_VALUES_TO_SCREEN .
  methods VALIDATE .
  methods VALIDATE_SYSTEM_FIELD .
ENDCLASS.



CLASS ZCL_DBBR_EDIT_JOIN_COND_VIEW IMPLEMENTATION.


  METHOD call_built_in_f4_for_value.
    DATA: lv_tabname   TYPE tabname,
          lv_value     TYPE zdbbr_value,
          lv_fieldname TYPE fieldname.

    DATA(lr_screen_field_manager) = NEW zcl_uitb_screen_field_manager(
        iv_repid = zif_dbbr_c_report_id=>main
    ).

    lr_screen_field_manager->read_values(
      IMPORTING et_field_values     = DATA(lt_field_values)
    ).

    lv_fieldname = to_upper( lt_field_values[ fieldname = c_p_join_src_field ]-fieldvalue ).
    lv_tabname = to_upper( lt_field_values[ fieldname = c_p_join_src_tab ]-fieldvalue ).

    CHECK: lv_fieldname IS NOT INITIAL,
           lv_tabname IS NOT INITIAL.

    zcl_dbbr_f4_helper=>call_built_in_f4(
      EXPORTING
        iv_tablename            = lv_tabname
        iv_fieldname            = lv_fieldname
        iv_selfield_name        = iv_dynp_fieldname
        iv_repid                = zif_dbbr_c_report_id=>main
      CHANGING
        cv_value                = cv_value
    ).

  ENDMETHOD.


  METHOD call_source_field_f4.
    DATA: lv_fieldname TYPE fieldname,
          lv_value     TYPE zdbbr_value.

*... Table is always filled during field mode as the source field always
*... comes from the join table
    IF mv_mode = c_field_mode.
      zcl_dbbr_f4_helper=>call_table_field_f4(
        EXPORTING
          iv_repid              = zif_dbbr_c_report_id=>main
          iv_dynpname_tablename = |{ c_p_join_src_tab }|
          iv_dynpname_fieldname = |{ c_p_join_src_field }|
        CHANGING
          cv_fieldname          = mr_v_join_source_field->*
      ).
      IF mf_allow_offset = abap_true.
        fill_field_attribute_fields(
          if_source    = abap_true
          iv_tabname   = mr_v_join_source_tab->*
          iv_fieldname = mr_v_join_source_field->*
        ).
      ENDIF.
    ELSE.
*.... call join f4 field value help like in old join definition screen
      DATA(lr_join_field_f4) = NEW zcl_dbbr_tabfield_tree_f4(
        iv_screen_title     = 'Field F4 for Source'
        ir_tree_node_filler = NEW zcl_dbbr_table_treeno_fill(
           it_join_tables     = CORRESPONDING #( mt_target_table_list )
        )
      ).

      lr_join_field_f4->display_value_help(
        IMPORTING ev_chosen_field            = DATA(lv_field)
                  ev_chosen_table            = DATA(lv_table) ).

      IF lv_field IS NOT INITIAL AND
         lv_table IS NOT INITIAL.
        mr_v_join_source_field->* = lv_field.
*...... update target field via screen field manager
        DATA(lr_screen_field_manager) = NEW zcl_uitb_screen_field_manager( get_report_id( ) ).
        lr_screen_field_manager->read_values( ).
        lr_screen_field_manager->set_field_value(
            iv_name             = |{ c_p_join_src_tab }|
            iv_value            = lv_table
            if_immediate_update = abap_true
        ).
        mr_v_join_source_tab->* = lv_table.

        IF mf_allow_offset = abap_true.
          fill_field_attribute_fields(
            if_source    = abap_true
            iv_tabname   = |{ lv_table }|
            iv_fieldname = lv_field
          ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD call_target_field_f4.
    DATA: lv_source_tab   TYPE tabname,
          lv_source_field TYPE fieldname.

    DATA(lr_screen_field_manager) = NEW zcl_uitb_screen_field_manager( get_report_id( ) ).
    lr_screen_field_manager->read_values( ).
    lr_screen_field_manager->get_value( EXPORTING iv_fieldname = c_p_join_src_field
                                        IMPORTING ev_value     = lv_source_field    ).
    lr_screen_field_manager->get_value( EXPORTING iv_fieldname = c_p_join_src_tab
                                        IMPORTING ev_value     = lv_source_tab    ).

    IF lv_source_field IS NOT INITIAL AND
       lv_source_tab   IS NOT INITIAL.
      DATA(ls_source_field_dfies) = zcl_dbbr_dictionary_helper=>get_table_field_info(
          iv_tablename = lv_source_tab
          iv_fieldname = to_upper( lv_source_field )
      ).
    ENDIF.

    data(lr_join_field_f4) = new zcl_dbbr_tabfield_tree_f4(
        iv_screen_title     = 'Field F4 for Target'
        ir_tree_node_filler = NEW zcl_dbbr_table_treeno_fill(
          it_join_tables     = CORRESPONDING #( mt_target_table_list )
          is_join_field_info = ls_source_field_dfies
        )
    ).
*... Call the Value help for the Target field
    lr_join_field_f4->display_value_help(
      IMPORTING
        ev_chosen_field            = DATA(lv_field)
        ev_chosen_table            = DATA(lv_table)
        ev_chosen_field_with_alias = DATA(lv_field_with_alias)
    ).
    IF lv_field IS NOT INITIAL AND
       lv_table IS NOT INITIAL.
      mr_v_join_target_field->* = lv_field.
*.... update target field via screen field manager
      lr_screen_field_manager->set_field_value(
          iv_name             = |{ c_p_join_trgt_tab }|
          iv_value            = lv_table
      ).
      mr_v_join_target_tab->* = lv_table.
      IF mf_allow_offset = abap_true.
        fill_field_attribute_fields(
          iv_tabname              = |{ lv_table }|
          ir_screen_field_manager = lr_screen_field_manager
          iv_fieldname            = lv_field
        ).
      ELSE.
        lr_screen_field_manager->update_fields( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD call_value1_f4.
    IF mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
      call_built_in_f4_for_value(
        EXPORTING iv_dynp_fieldname = |{ c_p_join_cond_value1 }|
        CHANGING  cv_value          = mr_v_join_cond_value1->*
      ).
    ENDIF.
  ENDMETHOD.


  METHOD call_value2_f4.
    IF mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
      call_built_in_f4_for_value(
        EXPORTING iv_dynp_fieldname = |{ c_p_join_cond_value2 }|
        CHANGING  cv_value          = mr_v_join_cond_value2->*
      ).
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.

    st_comparator_fix_vals = VALUE #(
      ( key = zif_dbbr_c_operator=>equals              text = 'Equals'(005) )
      ( key = zif_dbbr_c_operator=>greater_than        text = 'Greater than'(006) )
      ( key = zif_dbbr_c_operator=>greater_or_equal_to text = 'Greater or equal to'(007) )
      ( key = zif_dbbr_c_operator=>lesser_than         text = 'Lesser than'(008) )
      ( key = zif_dbbr_c_operator=>lesser_or_equal_to  text = 'Lesser or equal to'(009) )
      ( key = zif_dbbr_c_operator=>not_equals          text = 'Not equal to'(010) )
      ( key = zif_dbbr_c_operator=>like                text = 'Like'(011) )
      ( key = zif_dbbr_c_operator=>not_like            text = 'Not Like'(012) )
      ( key = zif_dbbr_c_operator=>between             text = 'Between'(013) )
    ).
  ENDMETHOD.


  METHOD clear_field_attributes.
  ENDMETHOD.


  METHOD constructor.
    DEFINE read_cached_field.
      &1 = cast #( lr_data_cache->get_data_ref( |{ &2 }| ) ).
    END-OF-DEFINITION.

    mv_mode = iv_mode.
    mf_is_new = if_is_new.
    mt_target_table_list = it_target_table_list.
    mf_allow_offset = if_allow_offset.
    mr_cursor = zcl_uitb_cursor=>get_cursor( ).

    " init global data references from cache
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    read_cached_field:
      mr_v_join_source_field          c_p_join_src_field,
      mr_v_join_source_tab            c_p_join_src_tab,
      mr_v_join_cond_comp1            c_p_join_cond_comparator1,
      mr_v_join_cond_value1           c_p_join_cond_value1,
      mr_v_join_cond_value2           c_p_join_cond_value2,
      mr_v_join_target_field          c_p_join_trgt_field,
      mr_v_join_target_tab            c_p_join_trgt_tab,
      mr_v_title                      c_v_join_condition_title,
      mr_v_join_cond_val_type         c_p_join_cond_value_type,
      mr_v_join_target_offset         c_p_join_trgt_offset,
      mr_v_join_target_offset_length  c_p_join_trgt_offset_length,
      mr_v_join_trgt_field_datatype   c_p_join_trgt_fld_datatype,
      mr_v_join_source_fld_datatype   c_p_join_source_fld_datatype,
      mr_v_join_trgt_fld_length       c_p_join_trgt_fld_length,
      mr_v_join_source_fld_length     c_p_join_source_fld_length,
      mr_s_save_func                  zif_dbbr_main_report_var_ids=>c_s_save_function,
      mr_s_save_new_func              zif_dbbr_main_report_var_ids=>c_s_save_and_stay_function.

*.. clear all screen values first
    CLEAR: mr_v_join_source_field->*,
           mr_v_join_source_tab->*,
           mr_v_join_cond_comp1->*,
           mr_v_join_cond_value1->*,
           mr_v_join_cond_value2->*,
           mr_v_join_target_field->*,
           mr_v_join_target_tab->*,
           mr_v_title->*,
           mr_v_join_cond_val_type->*,
           mr_v_join_target_offset->*,
           mr_v_join_target_offset_length->*.

    mr_cursor->set_field( c_p_join_src_field ).
    mr_cursor->request_update( ).

    IF mv_mode = c_field_mode.
      IF if_is_new = abap_true.
        mr_v_title->* = 'Create new Field Condition'(001).
      ELSE.
        mr_v_title->* = 'Change Field Condition'(002).
      ENDIF.
    ELSE.
      IF if_is_new = abap_true.
        mr_v_title->* = 'Create new Filter Condition'(003).
      ELSE.
        mr_v_title->* = 'Change Filter Condition'(004).
      ENDIF.
    ENDIF.

*... update current screen fields
    IF mf_is_new = abap_true.
      IF mv_mode = c_value_mode.
        mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
      ELSE.
        mr_v_join_source_tab->* = iv_source_table.
      ENDIF.
      mr_v_join_cond_comp1->* = '='.
    ELSE.
      ms_field_condition = is_field_condition.
      ms_filter_condition = is_filter_condition.
      IF mv_mode = c_field_mode.
        mr_v_join_source_tab->* = iv_source_table.
      ENDIF.
      transfer_values_to_screen( ).
*.... Call validate to fill additional values
      validate( ).
    ENDIF.
  ENDMETHOD.


  METHOD convert_values_to_display.
    CHECK: mv_mode = c_value_mode,
           mf_error = abap_false,
           mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input,
           mr_v_join_source_tab->* IS NOT INITIAL,
           mr_v_join_source_field->* IS NOT INITIAL.

*.. Try to convert entered value to internal format
    TRY.
        IF mr_v_join_cond_value1->* IS NOT INITIAL OR
           mr_v_join_cond_value2->* IS NOT INITIAL.

          zcl_dbbr_data_converter=>convert_selopt_to_disp_format(
            EXPORTING
              iv_tabname             = |{ mr_v_join_source_tab->* }|
              iv_fieldname           = mr_v_join_source_field->*
            CHANGING
              cv_value1              = mr_v_join_cond_value1->*
              cv_value2              = mr_v_join_cond_value2->*
          ).
        ENDIF.
      CATCH zcx_dbbr_conversion_exc INTO DATA(lx_conv_error).
        lx_conv_error->print_message(
            iv_msg_type  = 'E'
            if_to_screen = abap_false
        ).
        zcx_dbbr_validation_exception=>raise_from_sy(
            iv_parameter = c_p_join_cond_value1
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD convert_values_to_internal.
    CHECK mv_mode = c_value_mode.
    CHECK mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input OR
          mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>system_value_input.


    IF mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.

*.... Try to convert entered value to internal format
      TRY.
          IF mr_v_join_cond_value1->* IS NOT INITIAL OR
             mr_v_join_cond_value2->* IS NOT INITIAL.

            zcl_dbbr_data_converter=>convert_selopt_to_int_format(
              EXPORTING
                iv_tabname             = |{ mr_v_join_source_tab->* }|
                iv_fieldname           = mr_v_join_source_field->*
                if_print_error_message = abap_false
              CHANGING
                cv_value1              = mr_v_join_cond_value1->*
                cv_value2              = mr_v_join_cond_value2->*
            ).
          ENDIF.
        CATCH zcx_dbbr_conversion_exc INTO DATA(lx_conv_error).
          lx_conv_error->print_message(
              iv_msg_type  = 'S'
              if_to_screen = abap_false
          ).
          zcx_dbbr_validation_exception=>raise_from_sy(
              iv_parameter = c_p_join_cond_value1
          ).
      ENDTRY.

    ELSE.
      TRANSLATE mr_v_join_cond_value1->* to UPPER CASE.
      TRANSLATE mr_v_join_cond_value2->* to UPPER CASE.
    ENDIF.
  ENDMETHOD.


  METHOD fill_comparator_list.
    DATA(lt_list) = st_comparator_fix_vals.
    IF mv_mode = c_field_mode.
*.... For simplfication reasons only equals will be allowed in the first step
      lt_list = VALUE #( ( key = '=' text = 'Equals' ) ).
*.... For experienced users other options could be possible as well
*      DELETE lt_list WHERE key = zif_dbbr_c_operator=>like
*                        OR key = zif_dbbr_c_operator=>not_like
*                        OR key = zif_dbbr_c_operator=>between.
    ELSE.
      CASE mr_v_join_cond_val_type->*.

        WHEN zif_dbbr_c_join_cond_val_type=>typed_input.

*          DELETE lt_list WHERE key <> zif_dbbr_c_operator=>between
*                           AND key <> zif_dbbr_c_operator=>equals
*                           AND key <> zif_dbbr_c_operator=>not_equals.
*
**........ current operator value has to be updated as well
*          IF mr_v_join_cond_comp1->* <> zif_dbbr_c_operator=>between AND
*             mr_v_join_cond_comp1->* <> zif_dbbr_c_operator=>equals AND
*             mr_v_join_cond_comp1->* <> zif_dbbr_c_operator=>not_equals.
*            mr_v_join_cond_comp1->* = zif_dbbr_c_operator=>equals.
*          ENDIF.

        WHEN zif_dbbr_c_join_cond_val_type=>system_value_input.
          DELETE lt_list WHERE key <> zif_dbbr_c_operator=>not_equals
                           AND key <> zif_dbbr_c_operator=>equals.

*........ current operator value has to be updated as well
          IF mr_v_join_cond_comp1->* <> zif_dbbr_c_operator=>equals AND
             mr_v_join_cond_comp1->* <> zif_dbbr_c_operator=>not_equals.
            mr_v_join_cond_comp1->* = zif_dbbr_c_operator=>equals.
          ENDIF.

      ENDCASE.
    ENDIF.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = CONV vrm_id( c_p_join_cond_comparator1 )
        values = lt_list.
  ENDMETHOD.


  METHOD fill_field_attribute_fields.
    DATA: lv_fieldname          TYPE fieldname,
          lv_length_fieldname   TYPE dynfnam,
          lv_datatype_fieldname TYPE dynfnam,
          lv_tabname            TYPE tabname.

    FIELD-SYMBOLS: <lv_length>   TYPE ddleng,
                   <lv_datatype> TYPE datatype_d.

    IF if_source = abap_true.
      ASSIGN mr_v_join_source_fld_datatype->* TO <lv_datatype>.
      ASSIGN mr_v_join_source_fld_length->* TO <lv_length>.
      lv_length_fieldname = c_p_join_source_fld_length.
      lv_datatype_fieldname = c_p_join_source_fld_datatype.
    ELSE.
      ASSIGN mr_v_join_trgt_field_datatype->* TO <lv_datatype>.
      ASSIGN mr_v_join_trgt_fld_length->* TO <lv_length>.
      lv_length_fieldname = c_p_join_trgt_fld_length.
      lv_datatype_fieldname = c_p_join_trgt_fld_datatype.
    ENDIF.

    DATA(lr_screen_field_manager) = COND #( WHEN ir_screen_field_manager IS BOUND THEN
                                                ir_screen_field_manager
                                            ELSE
                                                NEW zcl_uitb_screen_field_manager( get_report_id( ) ) ).

    IF ir_screen_field_manager IS INITIAL.
      lr_screen_field_manager->read_values( ).
    ENDIF.

    IF iv_fieldname IS NOT INITIAL AND iv_tabname IS NOT INITIAL.
      lv_fieldname = iv_fieldname.
      lv_tabname = iv_tabname.
    ELSE.
      lr_screen_field_manager->get_value(
        EXPORTING iv_fieldname = COND #( WHEN if_source = abap_true THEN
                                           c_p_join_src_field
                                         ELSE
                                           c_p_join_trgt_field )
        IMPORTING ev_value     = lv_fieldname
      ).
      lr_screen_field_manager->get_value(
        EXPORTING iv_fieldname = COND #( WHEN if_source = abap_true THEN
                                           c_p_join_src_tab
                                         ELSE
                                           c_p_join_trgt_tab )
        IMPORTING ev_value     = lv_tabname
      ).
    ENDIF.

    IF lv_fieldname IS INITIAL OR lv_tabname IS INITIAL.
      CLEAR: <lv_datatype>,
             <lv_length>.
    ELSE.

*.... Read field information
      DATA(ls_field_info) = zcl_dbbr_dictionary_helper=>get_table_field_info(
        iv_tablename = lv_tabname
        iv_fieldname = lv_fieldname
      ).
      <lv_length> = |{ ls_field_info-leng }|.
      <lv_datatype> = ls_field_info-datatype.
    ENDIF.

*... Update fields on dynpro
    lr_screen_field_manager->set_field_value(
        iv_name  = lv_datatype_fieldname
        iv_value = <lv_datatype>
    ).
    lr_screen_field_manager->set_field_value(
        iv_name  = lv_length_fieldname
        iv_value = <lv_length>
    ).
    lr_screen_field_manager->update_fields( ).
  ENDMETHOD.


  METHOD get_updated_condition.
    IF mv_mode = c_field_mode.
      es_field_condition = ms_field_condition.
    ELSE.
      es_filter_condition = ms_filter_condition.
    ENDIF.
  ENDMETHOD.


  METHOD send_new_condition_via_event.
    IF mv_mode = c_field_mode.
      RAISE EVENT created_field_condition
        EXPORTING
          es_field = ms_field_condition.

      CLEAR: mr_v_join_source_field->*,
             mr_v_join_source_fld_datatype->*,
             mr_v_join_source_fld_length->*,
             mr_v_join_target_field->*,
             mr_v_join_target_tab->*,
             mr_v_join_target_offset->*,
             mr_v_join_target_offset_length->*.
    ELSE.
      RAISE EVENT created_filter_condition
        EXPORTING
          es_filter = ms_filter_condition.

      CLEAR: mr_v_join_source_field->*,
             mr_v_join_source_tab->*,
             mr_v_join_cond_value1->*,
             mr_v_join_cond_value2->*.

      mr_v_join_cond_comp1->* = '='.
      mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
    ENDIF.

    CLEAR: mf_saved,
           ms_field_condition,
           ms_filter_condition.

*... Reset cursor
    mr_cursor->set_field( c_p_join_src_field ).
    mr_cursor->request_update( ).
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


  METHOD transfer_values.
    IF mv_mode = c_field_mode.
      ms_field_condition = VALUE zdbbr_joinfld(
          field             = mr_v_join_source_field->*
          ref_field         = mr_v_join_target_field->*
          ref_table         = mr_v_join_target_tab->*
          operator          = mr_v_join_cond_comp1->*
          off_offset        = mr_v_join_target_offset->*
          off_length        = mr_v_join_target_offset_length->*
      ).
    ELSE.
      ms_filter_condition = VALUE zdbbr_joinfil(
          tabname           = mr_v_join_source_tab->*
          fieldname         = mr_v_join_source_field->*
          operator          = mr_v_join_cond_comp1->*
          value_type        = mr_v_join_cond_val_type->*
          value             = mr_v_join_cond_value1->*
          value2            = mr_v_join_cond_value2->*
      ).
    ENDIF.
  ENDMETHOD.


  METHOD transfer_values_to_screen.
    IF mv_mode = c_field_mode.
      mr_v_join_source_field->*         = ms_field_condition-field.
      mr_v_join_target_field->*         = ms_field_condition-ref_field.
      mr_v_join_target_tab->*           = ms_field_condition-ref_table.
      mr_v_join_cond_comp1->*           = ms_field_condition-operator.
      mr_v_join_target_offset->*        = ms_field_condition-off_offset.
      mr_v_join_target_offset_length->* = ms_field_condition-off_length.
    ELSE.
      mr_v_join_source_tab->*   = ms_filter_condition-tabname.
      mr_v_join_source_field->* = ms_filter_condition-fieldname.
      mr_v_join_cond_comp1->*   = ms_filter_condition-operator.
      mr_v_join_cond_val_type->* = ms_filter_condition-value_type.
      mr_v_join_cond_value1->*  = ms_filter_condition-value.
      mr_v_join_cond_value2->*  = ms_filter_condition-value2.
    ENDIF.
  ENDMETHOD.


  METHOD validate.
    DATA: lv_value1 TYPE zdbbr_value,
          lv_value2 TYPE zdbbr_value.

    IF mr_v_join_source_field->* IS INITIAL.
      zcx_dbbr_validation_exception=>raise_with_text(
        iv_text      = |Source Field must have a value|
        iv_parameter = c_p_join_src_field
      ).
    ENDIF.

    TRANSLATE mr_v_join_source_field->* TO UPPER CASE.
    TRANSLATE mr_v_join_source_tab->* TO UPPER CASE.

    CASE mv_mode.

      WHEN c_field_mode.
        TRANSLATE mr_v_join_target_tab->* TO UPPER CASE.
        TRANSLATE mr_v_join_target_field->* TO UPPER CASE.

*...... Validate that the source field exists in the current join table
        DATA(ls_source) = zcl_dbbr_dictionary_helper=>validate_table_field(
            iv_table_field = mr_v_join_source_field->*
            iv_table_name  = |{ mr_v_join_source_tab->* }|
            iv_dynfname    = c_p_join_src_field
        ).
        IF mf_allow_offset = abap_true.
          mr_v_join_source_fld_datatype->* = ls_source-datatype.
          mr_v_join_source_fld_length->* = ls_source-leng.
        ENDIF.

*...... Target field and table have to be filled as well
        IF mr_v_join_target_tab->* IS INITIAL.
          zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |Target Table/Field must be filled (Use F4 for Target Field|
            iv_parameter = c_p_join_trgt_field
          ).
        ELSE.
          IF mr_v_join_target_tab->* NOT IN mt_target_table_list.
            zcx_dbbr_validation_exception=>raise_with_text(
                iv_text      = |Target Table { mr_v_join_target_tab->* } is not in list of valid tables.|
              iv_parameter = c_p_join_trgt_tab
            ).
          ENDIF.
        ENDIF.

        IF mr_v_join_target_field->* IS INITIAL.
          zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |Target Field must be filled|
            iv_parameter = c_p_join_trgt_field
          ).
        ENDIF.

*...... Both fields are filled, now check if the field and tables match
        DATA(ls_target) = zcl_dbbr_dictionary_helper=>validate_table_field(
            iv_table_field = mr_v_join_target_field->*
            iv_table_name  = |{ mr_v_join_target_tab->* }|
            iv_dynfname    = c_p_join_trgt_field
        ).
        IF mf_allow_offset = abap_true.
          mr_v_join_trgt_field_datatype->* = ls_target-datatype.
          mr_v_join_trgt_fld_length->* = ls_target-leng.
        ENDIF.

      WHEN c_value_mode.

*...... Source Table has to be filled as well
        IF mr_v_join_source_tab->* IS INITIAL.
          zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |Source Table/Field must be filled (Use F4 for Source Field|
            iv_parameter = c_p_join_src_field
          ).
        ELSE.
          IF mr_v_join_source_tab->* NOT IN mt_target_table_list.
            zcx_dbbr_validation_exception=>raise_with_text(
              iv_text      = |Source Table { mr_v_join_source_tab->* } is not in list of valid tables.|
              iv_parameter = c_p_join_src_tab
            ).
          ENDIF.
        ENDIF.
*...... Validate that the source field exists in the current join table
        zcl_dbbr_dictionary_helper=>validate_table_field(
            iv_table_field = mr_v_join_source_field->*
            iv_table_name  = |{ mr_v_join_source_tab->* }|
            iv_dynfname    = c_p_join_src_field
        ).

        CASE mr_v_join_cond_val_type->*..

          WHEN zif_dbbr_c_join_cond_val_type=>system_value_input.
            validate_system_field( ).

          WHEN zif_dbbr_c_join_cond_val_type=>typed_input.
            convert_values_to_internal( ).

          WHEN zif_dbbr_c_join_cond_val_type=>parameter_input.
            validate_parameter( ).
        ENDCASE.

      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  method VALIDATE_PARAMETER.
  endmethod.


  METHOD validate_system_field.
    TYPES: BEGIN OF lty_values,
             value     TYPE zdbbr_value,
             fieldname TYPE dynfnam,
           END OF lty_values.

    DATA: lv_value  TYPE zdbbr_value,
          lt_values TYPE STANDARD TABLE OF lty_values.

    FIELD-SYMBOLS: <lv_system_value> TYPE any.

    CHECK mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>system_value_input.

    clear mr_v_join_cond_value2->*.
    TRANSLATE mr_v_join_cond_value1->* to UPPER CASE.

    lt_values = VALUE #(
      ( value     = mr_v_join_cond_value1->*
        fieldname = c_p_join_cond_value1 )
    ).
    IF mr_v_join_cond_comp1->* = zif_dbbr_c_operator=>between.
      lt_values = VALUE #( BASE lt_values
       ( value     = mr_v_join_cond_value2->*
         fieldname = c_p_join_cond_value2 )
      ).
    ENDIF.

    LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<ls_value>).
*.... check if field is filled
      IF <ls_value>-value IS INITIAL.
        zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |No valid system field was supplied|
            iv_parameter = <ls_value>-fieldname
        ).
      ELSEIF <ls_value>-value NP 'SY-*'.
*...... Only allow fields from SY-structure
        zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |Only Fields from SY - structure are allowed here|
            iv_parameter = <ls_value>-fieldname
        ).
      ELSE.
*...... check if system field exists at all
        ASSIGN (<ls_value>-value) TO <lv_system_value>.
        IF sy-subrc <> 0.
          zcx_dbbr_validation_exception=>raise_with_text(
              iv_text      = |System Field { <ls_value>-value } does not exist|
              iv_parameter = <ls_value>-fieldname
          ).
        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    mf_first_call = abap_true.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = c_r_edit_join_cond_view
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


  METHOD zif_uitb_screen_controller~determine_cursor.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_maintain_join_cond.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    DATA(lv_function) = cv_function_code.
    CLEAR cv_function_code.

    mr_cursor = zcl_uitb_cursor=>get_cursor( ).


    TRY.
*...... convert values to internal format
        IF lv_function <> 'CMP1CHNG' AND
           lv_function <> 'VALTYPCHANGED'.
          validate( ).
        ENDIF.

        CASE lv_function.

          WHEN 'VALTYPCHANGED'.
            IF mr_v_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>system_value_input.
              clear mf_value2_visible.
            ENDIF.
            fill_comparator_list( ).

          WHEN 'SAVE' OR 'SAVENEW'.
*.......... transfer the entered values
            transfer_values( ).
            mf_saved = abap_true.
            IF lv_function = 'SAVE'.
              zcl_dbbr_screen_helper=>leave_screen( ).
            ELSE.
              send_new_condition_via_event( ).
            ENDIF.

          WHEN 'CMP1CHNG'.
*.......... The comparison operator changed
            mf_value2_visible = xsdbool( mr_v_join_cond_comp1->* = zif_dbbr_c_operator=>between ).
            IF mf_value2_visible = abap_false.
*............ clear value2 if between is no longer chosen
              CLEAR: mr_v_join_cond_value2->*.
            ENDIF.

        ENDCASE.

        mf_error = abap_false.
      CATCH zcx_dbbr_validation_exception INTO DATA(lx_valid).
        mf_error = abap_true.
        IF lx_valid->parameter_name IS NOT INITIAL.
          mr_cursor->set_field( lx_valid->parameter_name ).
          mr_cursor->refresh( ).
        ENDIF.
        lx_valid->print_message( iv_msg_type = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    DATA: lv_values_active       TYPE screen-active,
          lv_value2_active       TYPE screen-active VALUE 0,
          lv_valtype_required    TYPE screen-required,
          lv_target_field_active TYPE screen-active.

    zif_uitb_screen_controller~set_status( ).

    IF mr_cursor IS BOUND AND mr_cursor->is_update_requested( ).
      mr_cursor->refresh( ).
    ENDIF.

    convert_values_to_display( ).

    IF mf_first_call = abap_true.
      fill_comparator_list( ).
      CLEAR mf_first_call.
    ENDIF.


    IF mv_mode = c_field_mode.
      lv_values_active = 0.
      lv_valtype_required = '0'.
      lv_target_field_active = 1.
    ELSE.
      lv_values_active = 1.
      lv_valtype_required = '1'.
      lv_target_field_active = 0.
    ENDIF.

    IF mf_value2_visible = abap_true.
      lv_value2_active = 1.
    ENDIF.

    LOOP AT SCREEN.
      IF screen-name = c_p_join_trgt_fld_datatype OR
         screen-name = c_p_join_source_fld_datatype OR
         screen-name = c_p_join_trgt_fld_length OR
         screen-name = c_p_join_source_fld_length.
        screen-input = 0.
        screen-value_help = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = c_p_join_trgt_tab OR
         screen-name = c_p_join_src_tab.
        IF screen-name = c_p_join_src_tab AND
           mv_mode = c_field_mode.
          screen-input = 0.
        ENDIF.
        screen-value_help = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = c_p_join_src_field OR
         screen-name = c_p_join_trgt_field.
        screen-required = '2'.
        MODIFY SCREEN.
      ENDIF.

      IF screen-group1 = 'VAL' OR
         screen-group1 = 'VL2'.

        IF screen-group1 = 'VL2'.
          screen-active = lv_value2_active.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.

        screen-active = lv_values_active.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = c_p_join_cond_value_type.
        screen-required = lv_valtype_required.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = c_p_join_cond_value2.
        IF mf_value2_visible = abap_true.
          screen-required = '2'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF screen-group1 = 'OFF'.
        screen-active = COND #( WHEN mf_allow_offset = abap_true THEN 1 ELSE 0 ).
        MODIFY SCREEN.
      ENDIF.

      IF screen-group1 = 'TRG'.
        screen-active = lv_target_field_active.
        MODIFY SCREEN.
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
