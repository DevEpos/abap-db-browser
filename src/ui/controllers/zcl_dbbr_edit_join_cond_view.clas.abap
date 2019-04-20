CLASS zcl_dbbr_edit_join_cond_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    ALIASES show
      FOR zif_uitb_screen_controller~call_screen .
    ALIASES was_not_cancelled
      FOR zif_uitb_screen_controller~was_not_cancelled .

    CONSTANTS c_field_mode TYPE i VALUE 1 ##NO_TEXT.
    CONSTANTS c_value_mode TYPE i VALUE 2 ##NO_TEXT.

    EVENTS created_filter_condition
      EXPORTING
        VALUE(es_filter) TYPE zdbbr_joinfil .
    EVENTS created_field_condition
      EXPORTING
        VALUE(es_field) TYPE zdbbr_joinfld .

    CLASS-METHODS class_constructor .
    METHODS call_source_field_f4 .
    METHODS call_target_field_f4 .
    METHODS call_value1_f4 .
    METHODS call_value2_f4 .
    METHODS constructor
      IMPORTING
        !if_is_new             TYPE abap_bool DEFAULT abap_true
        !is_filter_condition   TYPE zdbbr_joinfil OPTIONAL
        !is_field_condition    TYPE zdbbr_joinfld OPTIONAL
        !iv_mode               TYPE i DEFAULT c_field_mode
        !if_allow_offset       TYPE abap_bool OPTIONAL
        !iv_source_table       TYPE tabname OPTIONAL
        is_source_entity       TYPE zdbbr_joint OPTIONAL
        !it_target_entity_list TYPE zdbbr_entity_t OPTIONAL .
    METHODS get_updated_condition
      EXPORTING
        !es_field_condition  TYPE zdbbr_joinfld
        !es_filter_condition TYPE zdbbr_joinfil .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mf_first_call
      FOR zif_uitb_screen_controller~mf_first_call .
    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    "! type - syst_title
    CONSTANTS c_v_join_condition_title TYPE dynfnam VALUE 'JOINCOND' ##NO_TEXT.
    "! Type - fieldname
    CONSTANTS c_p_join_src_field TYPE dynfnam VALUE 'P_SRCFLD' ##NO_TEXT.
    "! Type - zdbbr_entity_id
    CONSTANTS c_p_join_src_tab TYPE dynfnam VALUE 'P_SRCTAB' ##NO_TEXT.
    "! Type - voperator
    CONSTANTS c_p_join_cond_comparator1 TYPE dynfnam VALUE 'P_COMP1' ##NO_TEXT.
    "! Type - char length 60
    CONSTANTS c_v_type_for_join_cond_val TYPE dynfnam VALUE 'GV_TYPE_OF_VALUE' ##NO_TEXT.
    "! Type - dynamic from gv_type_of_value
    CONSTANTS c_p_join_cond_value1 TYPE dynfnam VALUE 'P_VAL1' ##NO_TEXT.
    "! Type - dynamic from gv_type_of_value
    CONSTANTS c_p_join_cond_value2 TYPE dynfnam VALUE 'P_VAL2' ##NO_TEXT.
    "! Type - fieldname
    CONSTANTS c_p_join_trgt_field TYPE dynfnam VALUE 'P_TRGFLD' ##NO_TEXT.
    "! Type - doffset
    CONSTANTS c_p_join_trgt_offset TYPE dynfnam VALUE 'P_TFLDOF' ##NO_TEXT.
    "! Type - ddleng
    CONSTANTS c_p_join_trgt_offset_length TYPE dynfnam VALUE 'P_TFLDOL' ##NO_TEXT.
    "! Type datatype_d
    CONSTANTS c_p_join_trgt_fld_datatype TYPE dynfnam VALUE 'P_TRGDTP' ##NO_TEXT.
    "! Type datatype_d
    CONSTANTS c_p_join_source_fld_datatype TYPE dynfnam VALUE 'P_SRCDTP' ##NO_TEXT.
    "! Type ddleng
    CONSTANTS c_p_join_trgt_fld_length TYPE dynfnam VALUE 'P_TRGLNG' ##NO_TEXT.
    "! Type ddleng
    CONSTANTS c_p_join_source_fld_length TYPE dynfnam VALUE 'P_SRCLNG' ##NO_TEXT.
    "! Type - zdbbr_entity_id
    CONSTANTS c_p_join_trgt_tab TYPE dynfnam VALUE 'P_TRGTAB' ##NO_TEXT.
    CONSTANTS c_p_join_cond_value_type TYPE dynfnam VALUE 'P_VALTY' ##NO_TEXT.
    CONSTANTS c_r_edit_join_cond_view TYPE dynfnam VALUE 'GR_EDIT_JOIN_COND_VIEW' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_functions,
        comparator1_changed TYPE ui_func VALUE 'CMP1CHNG' ##NO_TEXT,
        value_type_changed  TYPE ui_func VALUE 'VALTYPCHANGED' ##NO_TEXT,
        save_and_continue   TYPE ui_func VALUE 'SAVENEW' ##NO_TEXT,
        save                TYPE ui_func VALUE 'SAVE' ##NO_TEXT,
      END OF c_functions.

    DATA ms_source_entity TYPE zdbbr_joint.
    DATA mf_saved TYPE abap_bool .
    DATA mf_is_new TYPE abap_bool .
    DATA mr_join_source_field TYPE REF TO fieldname .
    DATA mr_join_source_tab TYPE REF TO zdbbr_entity_id .
    DATA mr_join_cond_comp1 TYPE REF TO voperator .
    DATA mr_join_cond_val_type TYPE REF TO zdbbr_join_cond_value_type .
    DATA mr_join_cond_value1 TYPE REF TO zdbbr_value .
    DATA mr_join_cond_value2 TYPE REF TO zdbbr_value .
    DATA mr_join_target_field TYPE REF TO fieldname .
    DATA mr_join_target_tab TYPE REF TO zdbbr_entity_id .
    DATA mr_join_target_offset TYPE REF TO doffset .
    DATA mr_join_target_offset_length TYPE REF TO ddleng .
    DATA mv_source_entity TYPE zdbbr_entity_id.
    DATA mv_mode TYPE i .
    CLASS-DATA st_comparator_fix_vals TYPE vrm_values .
    DATA mf_value2_visible TYPE abap_bool .
    DATA mr_title TYPE REF TO syst_title .
    DATA mv_value_field_length TYPE i .
    DATA mv_value_field_rollname TYPE rollname .
    DATA mt_target_entity_list TYPE zdbbr_entity_t .
    DATA mf_allow_offset TYPE abap_bool .
    DATA mr_join_trgt_field_datatype TYPE REF TO datatype_d .
    DATA mr_join_source_fld_datatype TYPE REF TO datatype_d .
    DATA mr_join_trgt_fld_length TYPE REF TO ddleng .
    DATA mr_join_source_fld_length TYPE REF TO ddleng .
    DATA ms_field_condition TYPE zdbbr_joinfld .
    DATA ms_filter_condition TYPE zdbbr_joinfil .
    DATA mo_cursor TYPE REF TO zcl_uitb_cursor .
    DATA mr_save_func TYPE REF TO smp_dyntxt .
    DATA mr_save_new_func TYPE REF TO smp_dyntxt .
    DATA mf_error TYPE abap_bool .

    METHODS validate_parameter .
    METHODS call_built_in_f4_for_value
      IMPORTING
        !iv_dynp_fieldname TYPE dynfnam
      CHANGING
        !cv_value          TYPE zdbbr_value .
    METHODS clear_field_attributes .
    METHODS convert_values_to_internal .
    METHODS convert_values_to_display .
    METHODS fill_comparator_list .
    METHODS fill_field_attribute_fields
      IMPORTING
        !if_source               TYPE abap_bool OPTIONAL
        !io_screen_field_manager TYPE REF TO zcl_uitb_screen_field_manager OPTIONAL
        !iv_fieldname            TYPE fieldname OPTIONAL
        !iv_tabname              TYPE zdbbr_entity_id OPTIONAL .
    METHODS send_new_condition_via_event .
    METHODS set_functions .
    METHODS transfer_values .
    METHODS transfer_values_to_screen .
    METHODS validate .
    METHODS validate_system_field .
    METHODS get_entity_for_alias
      IMPORTING
        iv_alias            TYPE zdbbr_entity_alias
      RETURNING
        VALUE(rv_entity_id) TYPE zdbbr_entity_id
      RAISING
        cx_sy_itab_line_not_found.
ENDCLASS.



CLASS zcl_dbbr_edit_join_cond_view IMPLEMENTATION.

  METHOD constructor.
    DEFINE read_cached_field.
      &1 = CAST #( lr_data_cache->get_data_ref( |{ &2 }| ) ).
    END-OF-DEFINITION.

    mv_mode = iv_mode.
    mf_is_new = if_is_new.
    mt_target_entity_list = it_target_entity_list.
    mf_allow_offset = if_allow_offset.
    ms_source_entity = is_source_entity.
    mo_cursor = zcl_uitb_cursor=>get_cursor( ).

    " init global data references from cache
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    read_cached_field:
      mr_join_source_field          c_p_join_src_field,
      mr_join_source_tab            c_p_join_src_tab,
      mr_join_cond_comp1            c_p_join_cond_comparator1,
      mr_join_cond_value1           c_p_join_cond_value1,
      mr_join_cond_value2           c_p_join_cond_value2,
      mr_join_target_field          c_p_join_trgt_field,
      mr_join_target_tab            c_p_join_trgt_tab,
      mr_title                      c_v_join_condition_title,
      mr_join_cond_val_type         c_p_join_cond_value_type,
      mr_join_target_offset         c_p_join_trgt_offset,
      mr_join_target_offset_length  c_p_join_trgt_offset_length,
      mr_join_trgt_field_datatype   c_p_join_trgt_fld_datatype,
      mr_join_source_fld_datatype   c_p_join_source_fld_datatype,
      mr_join_trgt_fld_length       c_p_join_trgt_fld_length,
      mr_join_source_fld_length     c_p_join_source_fld_length,
      mr_save_func                  zif_dbbr_main_report_var_ids=>c_s_save_function,
      mr_save_new_func              zif_dbbr_main_report_var_ids=>c_s_save_and_stay_function.

*.. clear all screen values first
    CLEAR: mr_join_source_field->*,
           mr_join_source_tab->*,
           mr_join_cond_comp1->*,
           mr_join_cond_value1->*,
           mr_join_cond_value2->*,
           mr_join_target_field->*,
           mr_join_target_tab->*,
           mr_title->*,
           mr_join_cond_val_type->*,
           mr_join_target_offset->*,
           mr_join_target_offset_length->*.

    mo_cursor->set_field( c_p_join_src_field ).
    mo_cursor->request_update( ).

    IF mv_mode = c_field_mode.
      IF if_is_new = abap_true.
        mr_title->* = 'Create new Field Condition'(001).
      ELSE.
        mr_title->* = 'Change Field Condition'(002).
      ENDIF.
    ELSE.
      IF if_is_new = abap_true.
        mr_title->* = 'Create new Filter Condition'(003).
      ELSE.
        mr_title->* = 'Change Filter Condition'(004).
      ENDIF.
    ENDIF.

*... update current screen fields
    IF mf_is_new = abap_true.
      IF mv_mode = c_value_mode.
        mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
      ELSE.
        mr_join_source_tab->* = is_source_entity-add_table_alias.
        mv_source_entity = is_source_entity-add_table.
      ENDIF.
      mr_join_cond_comp1->* = '='.
    ELSE.
      ms_field_condition = is_field_condition.
      ms_filter_condition = is_filter_condition.
      IF mv_mode = c_field_mode.
        mr_join_source_tab->* = is_source_entity-add_table_alias.
        mv_source_entity = is_source_entity-add_table.
      ENDIF.
      transfer_values_to_screen( ).
*.... Call validate to fill additional values
      validate( ).
    ENDIF.
  ENDMETHOD.

  METHOD call_built_in_f4_for_value.
    DATA: lv_tabname   TYPE tabname,
          lv_value     TYPE zdbbr_value,
          lv_fieldname TYPE fieldname.

    DATA(lo_screen_field_manager) = NEW zcl_uitb_screen_field_manager(
        iv_repid = zif_dbbr_c_report_id=>main
    ).

    lo_screen_field_manager->read_values(
      IMPORTING et_field_values     = DATA(lt_field_values)
    ).

    lv_fieldname = to_upper( lt_field_values[ fieldname = c_p_join_src_field ]-fieldvalue ).
    lv_tabname = to_upper( lt_field_values[ fieldname = c_p_join_src_tab ]-fieldvalue ).

    CHECK: lv_fieldname IS NOT INITIAL,
           lv_tabname IS NOT INITIAL.

    zcl_dbbr_f4_helper=>call_built_in_f4(
      EXPORTING
        iv_tablename            = get_entity_for_alias( lv_tabname )
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
          iv_tablename          = mv_source_entity
          iv_dynpname_fieldname = |{ c_p_join_src_field }|
        CHANGING
          cv_fieldname          = mr_join_source_field->*
      ).
      IF mf_allow_offset = abap_true.
        fill_field_attribute_fields(
          if_source    = abap_true
          iv_tabname   = mv_source_entity
          iv_fieldname = mr_join_source_field->*
        ).
      ENDIF.
    ELSE.
*.... call join f4 field value help like in old join definition screen
      DATA(lr_join_field_f4) = NEW zcl_dbbr_tabfield_tree_f4(
        iv_screen_title     = 'Field F4 for Source'
        io_tree_node_filler = NEW zcl_dbbr_table_treeno_fill(
           it_join_tables = mt_target_entity_list
        )
      ).

      lr_join_field_f4->display_value_help(
        IMPORTING ev_chosen_field            = DATA(lv_field)
                  ev_chosen_table            = DATA(lv_table)
                  ev_chosen_table_alias      = DATA(lv_table_alias) ).

      IF lv_field IS NOT INITIAL AND
         lv_table IS NOT INITIAL.
        mr_join_source_field->* = lv_field.
*...... update target field via screen field manager
        DATA(lr_screen_field_manager) = NEW zcl_uitb_screen_field_manager( get_report_id( ) ).
        lr_screen_field_manager->read_values( ).
        lr_screen_field_manager->set_field_value(
            iv_name             = |{ c_p_join_src_tab }|
            iv_value            = lv_table_alias
            if_immediate_update = abap_true
        ).
        mr_join_source_tab->* = lv_table_alias.

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

    DATA(lo_screen_field_manager) = NEW zcl_uitb_screen_field_manager( get_report_id( ) ).
    lo_screen_field_manager->read_values( ).
    lo_screen_field_manager->get_value( EXPORTING iv_fieldname = c_p_join_src_field
                                        IMPORTING ev_value     = lv_source_field    ).
    lo_screen_field_manager->get_value( EXPORTING iv_fieldname = c_p_join_src_tab
                                        IMPORTING ev_value     = lv_source_tab    ).

    IF lv_source_field IS NOT INITIAL AND
       lv_source_tab   IS NOT INITIAL.

      IF mv_mode = c_field_mode.
        lv_source_tab = mv_source_entity.
      ELSE.
        lv_source_tab = get_entity_for_alias( lv_source_tab ).
      ENDIF.

      DATA(ls_source_field_dfies) = zcl_dbbr_dictionary_helper=>get_table_field_info(
          iv_tablename = lv_source_tab
          iv_fieldname = to_upper( lv_source_field )
      ).
    ENDIF.

    DATA(lr_join_field_f4) = NEW zcl_dbbr_tabfield_tree_f4(
        iv_screen_title     = 'Field F4 for Target'
        io_tree_node_filler = NEW zcl_dbbr_table_treeno_fill(
          it_join_tables     = mt_target_entity_list
          is_join_field_info = ls_source_field_dfies
        )
    ).
*... Call the Value help for the Target field
    lr_join_field_f4->display_value_help(
      IMPORTING
        ev_chosen_field            = DATA(lv_field)
        ev_chosen_table            = DATA(lv_table)
        ev_chosen_table_alias      = DATA(lv_alias)
    ).
    IF lv_field IS NOT INITIAL AND
       lv_table IS NOT INITIAL.
      mr_join_target_field->* = lv_field.
*.... update target field via screen field manager
      lo_screen_field_manager->set_field_value(
          iv_name             = |{ c_p_join_trgt_tab }|
          iv_value            = lv_alias
      ).
      mr_join_target_tab->* = lv_alias.

      IF mf_allow_offset = abap_true.
        fill_field_attribute_fields(
          iv_tabname              = |{ lv_table }|
          io_screen_field_manager = lo_screen_field_manager
          iv_fieldname            = lv_field
        ).
      ELSE.
        lo_screen_field_manager->update_fields( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD call_value1_f4.
    IF mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
      call_built_in_f4_for_value(
        EXPORTING iv_dynp_fieldname = |{ c_p_join_cond_value1 }|
        CHANGING  cv_value          = mr_join_cond_value1->*
      ).
    ENDIF.
  ENDMETHOD.


  METHOD call_value2_f4.
    IF mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
      call_built_in_f4_for_value(
        EXPORTING iv_dynp_fieldname = |{ c_p_join_cond_value2 }|
        CHANGING  cv_value          = mr_join_cond_value2->*
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


  METHOD convert_values_to_display.
    CHECK: mv_mode = c_value_mode,
           mf_error = abap_false,
           mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input,
           mr_join_source_tab->* IS NOT INITIAL,
           mr_join_source_field->* IS NOT INITIAL.

*.. Try to convert entered value to internal format
    TRY.
        IF mr_join_cond_value1->* IS NOT INITIAL OR
           mr_join_cond_value2->* IS NOT INITIAL.

          zcl_dbbr_data_converter=>convert_selopt_to_disp_format(
            EXPORTING
              iv_tabname             = get_entity_for_alias( mr_join_source_tab->* )
              iv_fieldname           = mr_join_source_field->*
            CHANGING
              cv_value1              = mr_join_cond_value1->*
              cv_value2              = mr_join_cond_value2->*
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
    CHECK mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input OR
          mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>system_value_input.


    IF mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.

*.... Try to convert entered value to internal format
      TRY.
          IF mr_join_cond_value1->* IS NOT INITIAL OR
             mr_join_cond_value2->* IS NOT INITIAL.

            zcl_dbbr_data_converter=>convert_selopt_to_int_format(
              EXPORTING
                iv_tabname             = get_entity_for_alias( mr_join_source_tab->* )
                iv_fieldname           = mr_join_source_field->*
                if_print_error_message = abap_false
              CHANGING
                cv_value1              = mr_join_cond_value1->*
                cv_value2              = mr_join_cond_value2->*
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
      TRANSLATE mr_join_cond_value1->* TO UPPER CASE.
      TRANSLATE mr_join_cond_value2->* TO UPPER CASE.
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
      CASE mr_join_cond_val_type->*.

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
          IF mr_join_cond_comp1->* <> zif_dbbr_c_operator=>equals AND
             mr_join_cond_comp1->* <> zif_dbbr_c_operator=>not_equals.
            mr_join_cond_comp1->* = zif_dbbr_c_operator=>equals.
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
      ASSIGN mr_join_source_fld_datatype->* TO <lv_datatype>.
      ASSIGN mr_join_source_fld_length->* TO <lv_length>.
      lv_length_fieldname = c_p_join_source_fld_length.
      lv_datatype_fieldname = c_p_join_source_fld_datatype.
    ELSE.
      ASSIGN mr_join_trgt_field_datatype->* TO <lv_datatype>.
      ASSIGN mr_join_trgt_fld_length->* TO <lv_length>.
      lv_length_fieldname = c_p_join_trgt_fld_length.
      lv_datatype_fieldname = c_p_join_trgt_fld_datatype.
    ENDIF.

    DATA(lr_screen_field_manager) = COND #( WHEN io_screen_field_manager IS BOUND THEN
                                                io_screen_field_manager
                                            ELSE
                                                NEW zcl_uitb_screen_field_manager( get_report_id( ) ) ).

    IF io_screen_field_manager IS INITIAL.
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
*.... Use table instead of alias to get field information
      IF if_source = abap_true AND mv_mode = c_field_mode.
        lv_tabname = mv_source_entity.
      ELSE.
        lv_tabname = get_entity_for_alias( lv_tabname ).
      ENDIF.
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

  METHOD get_entity_for_alias.
    rv_entity_id = mt_target_entity_list[ entity_alias = to_upper( iv_alias ) ]-entity_id.
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

      CLEAR: mr_join_source_field->*,
             mr_join_source_fld_datatype->*,
             mr_join_source_fld_length->*,
             mr_join_target_field->*,
             mr_join_target_tab->*,
             mr_join_target_offset->*,
             mr_join_target_offset_length->*.
    ELSE.
      RAISE EVENT created_filter_condition
        EXPORTING
          es_filter = ms_filter_condition.

      CLEAR: mr_join_source_field->*,
             mr_join_source_tab->*,
             mr_join_cond_value1->*,
             mr_join_cond_value2->*.

      mr_join_cond_comp1->* = '='.
      mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>typed_input.
    ENDIF.

    CLEAR: mf_saved,
           ms_field_condition,
           ms_filter_condition.

*... Reset cursor
    mo_cursor->set_field( c_p_join_src_field ).
    mo_cursor->request_update( ).
  ENDMETHOD.


  METHOD set_functions.
    IF mf_is_new = abap_true.
      mr_save_func->icon_id = icon_create.
      mr_save_func->icon_text = |{ 'Create'(014) }|.
      mr_save_func->text = |{ 'Create and close'(015) }|.

      mr_save_new_func->icon_id = icon_create.
      mr_save_new_func->icon_text = |{ 'Create +'(016) }|.
      mr_save_new_func->text = |{ 'Create and continue with another entry'(017) }|.
    ELSE.
      mr_save_func->icon_id = icon_system_save.
      mr_save_func->icon_text = |{ 'Save'(018) }|.
    ENDIF.
  ENDMETHOD.


  METHOD transfer_values.
    IF mv_mode = c_field_mode.
      ms_field_condition = VALUE zdbbr_joinfld(
          field             = mr_join_source_field->*
          ref_field         = mr_join_target_field->*
          ref_table_alias   = mr_join_target_tab->*
          ref_table         = get_entity_for_alias( mr_join_target_tab->* )
          operator          = mr_join_cond_comp1->*
          off_offset        = mr_join_target_offset->*
          off_length        = mr_join_target_offset_length->*
      ).
    ELSE.
      ms_filter_condition = VALUE zdbbr_joinfil(
          tabname           = get_entity_for_alias( mr_join_source_tab->* )
          tabname_alias     = mr_join_source_tab->*
          fieldname         = mr_join_source_field->*
          operator          = mr_join_cond_comp1->*
          value_type        = mr_join_cond_val_type->*
          value             = mr_join_cond_value1->*
          value2            = mr_join_cond_value2->*
      ).
    ENDIF.
  ENDMETHOD.


  METHOD transfer_values_to_screen.
    IF mv_mode = c_field_mode.
      mr_join_source_field->*         = ms_field_condition-field.
      mr_join_target_field->*         = ms_field_condition-ref_field.
      mr_join_target_tab->*           = ms_field_condition-ref_table_alias.
      mr_join_cond_comp1->*           = ms_field_condition-operator.
      mr_join_target_offset->*        = ms_field_condition-off_offset.
      mr_join_target_offset_length->* = ms_field_condition-off_length.
    ELSE.
      mr_join_source_tab->*   = ms_filter_condition-tabname_alias.
      mr_join_source_field->* = ms_filter_condition-fieldname.
      mr_join_cond_comp1->*   = ms_filter_condition-operator.
      mr_join_cond_val_type->* = ms_filter_condition-value_type.
      mr_join_cond_value1->*  = ms_filter_condition-value.
      mr_join_cond_value2->*  = ms_filter_condition-value2.
    ENDIF.
  ENDMETHOD.


  METHOD validate.
    DATA: lv_value1 TYPE zdbbr_value,
          lv_value2 TYPE zdbbr_value.

    IF mr_join_source_field->* IS INITIAL.
      zcx_dbbr_validation_exception=>raise_with_text(
        iv_text      = |{ 'Source Field must have a value'(019) }|
        iv_parameter = c_p_join_src_field
      ).
    ENDIF.

    TRANSLATE mr_join_source_field->* TO UPPER CASE.
    TRANSLATE mr_join_source_tab->* TO UPPER CASE.

    CASE mv_mode.

      WHEN c_field_mode.
        TRANSLATE mr_join_target_tab->* TO UPPER CASE.
        TRANSLATE mr_join_target_field->* TO UPPER CASE.

*...... Validate that the source field exists in the current join table
        DATA(ls_source) = zcl_dbbr_dictionary_helper=>validate_table_field(
            iv_table_field = mr_join_source_field->*
            iv_table_name  = |{ mv_source_entity }|
            iv_dynfname    = c_p_join_src_field
        ).
        IF mf_allow_offset = abap_true.
          mr_join_source_fld_datatype->* = ls_source-datatype.
          mr_join_source_fld_length->* = ls_source-leng.
        ENDIF.

*...... Target field and table have to be filled as well
        IF mr_join_target_tab->* IS INITIAL.
          zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |{ 'Target Table/Field must be filled (Use F4 for Target Field'(020) }|
            iv_parameter = c_p_join_trgt_field
          ).
        ENDIF.

        IF mr_join_target_field->* IS INITIAL.
          zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |Target Field must be filled|
            iv_parameter = c_p_join_trgt_field
          ).
        ENDIF.

*...... Retrieve alias of target entity
        TRY.
            DATA(lv_target_entity) = get_entity_for_alias( mr_join_target_tab->* ).
          CATCH cx_sy_itab_line_not_found.
            zcx_dbbr_validation_exception=>raise_with_text(
                iv_text      = |There is no entity with the alias { mr_join_target_tab->* }|
                iv_parameter = c_p_join_trgt_tab
            ).
        ENDTRY.

*...... Both fields are filled, now check if the field and tables match
        DATA(ls_target) = zcl_dbbr_dictionary_helper=>validate_table_field(
            iv_table_field = mr_join_target_field->*
            iv_table_name  = lv_target_entity
            iv_dynfname    = c_p_join_trgt_field
        ).
        IF mf_allow_offset = abap_true.
          mr_join_trgt_field_datatype->* = ls_target-datatype.
          mr_join_trgt_fld_length->* = ls_target-leng.
        ENDIF.

      WHEN c_value_mode.

*...... Source Table has to be filled as well
        IF mr_join_source_tab->* IS INITIAL.
          zcx_dbbr_validation_exception=>raise_with_text(
            iv_text      = |Source Table/Field must be filled (Use F4 for Source Field)|
            iv_parameter = c_p_join_src_field
          ).
        ENDIF.

*...... Retrieve alias of source entity
        TRY.
            DATA(lv_source_entity) = get_entity_for_alias( mr_join_source_tab->* ).
          CATCH cx_sy_itab_line_not_found.
            zcx_dbbr_validation_exception=>raise_with_text(
                iv_text      = |There is no entity with the alias { mr_join_source_tab->* }|
                iv_parameter = c_p_join_src_tab
            ).
        ENDTRY.

*...... Validate that the source field exists in the current join table
        zcl_dbbr_dictionary_helper=>validate_table_field(
            iv_table_field = mr_join_source_field->*
            iv_table_name  = lv_source_entity
            iv_dynfname    = c_p_join_src_field
        ).

        CASE mr_join_cond_val_type->*..

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


  METHOD validate_parameter.
  ENDMETHOD.


  METHOD validate_system_field.
    TYPES: BEGIN OF lty_values,
             value     TYPE zdbbr_value,
             fieldname TYPE dynfnam,
           END OF lty_values.

    DATA: lv_value  TYPE zdbbr_value,
          lt_values TYPE STANDARD TABLE OF lty_values.

    FIELD-SYMBOLS: <lv_system_value> TYPE any.

    CHECK mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>system_value_input.

    CLEAR mr_join_cond_value2->*.
    TRANSLATE mr_join_cond_value1->* TO UPPER CASE.

    lt_values = VALUE #(
      ( value     = mr_join_cond_value1->*
        fieldname = c_p_join_cond_value1 )
    ).
    IF mr_join_cond_comp1->* = zif_dbbr_c_operator=>between.
      lt_values = VALUE #( BASE lt_values
       ( value     = mr_join_cond_value2->*
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

    mo_cursor = zcl_uitb_cursor=>get_cursor( ).

    TRY.
*...... convert values to internal format
        IF lv_function <> c_functions-comparator1_changed AND
           lv_function <> c_functions-value_type_changed.
          validate( ).
        ENDIF.

        CASE lv_function.

          WHEN c_functions-value_type_changed.
            IF mr_join_cond_val_type->* = zif_dbbr_c_join_cond_val_type=>system_value_input.
              CLEAR mf_value2_visible.
            ENDIF.
            fill_comparator_list( ).

          WHEN c_functions-save OR c_functions-save_and_continue.
*.......... transfer the entered values
            transfer_values( ).
            mf_saved = abap_true.
            IF lv_function = c_functions-save.
              zcl_dbbr_screen_helper=>leave_screen( ).
            ELSE.
              send_new_condition_via_event( ).
            ENDIF.

          WHEN c_functions-comparator1_changed.
*.......... The comparison operator changed
            mf_value2_visible = xsdbool( mr_join_cond_comp1->* = zif_dbbr_c_operator=>between ).
            IF mf_value2_visible = abap_false.
*............ clear value2 if between is no longer chosen
              CLEAR: mr_join_cond_value2->*.
            ENDIF.

        ENDCASE.

        mf_error = abap_false.
      CATCH zcx_dbbr_validation_exception INTO DATA(lx_valid).
        mf_error = abap_true.
        IF lx_valid->parameter_name IS NOT INITIAL.
          mo_cursor->set_field( lx_valid->parameter_name ).
          mo_cursor->refresh( ).
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

    IF mo_cursor IS BOUND AND mo_cursor->is_update_requested( ).
      mo_cursor->refresh( ).
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
        it_excluding_functions = COND #(
          WHEN mf_is_new = abap_false THEN
             VALUE #( ( c_functions-save_and_continue ) )
        )
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_saved.
  ENDMETHOD.

ENDCLASS.
