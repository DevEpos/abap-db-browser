class ZCL_DBBR_SYSTEM_HELPER definition
  public
  create public .

public section.

  class-methods PRINT_SYSTEM_MESSAGE .
  class-methods SET_LANGUAGE .
  class-methods GET_CDS_VIEW_TEXT_TABLE
    returning
      value(RESULT) type TABNAME .
  class-methods SET_LOCALE_LANGUAGE .
  class-methods CREATE_GUID_22
    returning
      value(RV_GUID_22) type GUID_22 .
  class-methods GENERATE_SUBROUTINE
    importing
      !IT_LINES type STRING_TABLE
    returning
      value(RV_SUBR_PROGNAME) type PROGNAME .
  class-methods CHECK_ABAP_SYNTAX
    importing
      !IT_LINES type STRING_TABLE .
  class-methods GET_SYSTEM_VALUE
    importing
      !IV_SYSTEM_VALUE_TYPE type ZDBBR_SYST_VALUE_TYPE
    exporting
      !EV_SYSTEM_VALUE type ANY .
  class-methods SYST_VALUE_ALLWD_FOR_INTTYP
    importing
      !IV_INTTYPE type INTTYPE
    returning
      value(RF_POSSIBLE) type BOOLEAN .
  class-methods GET_CURRENT_METHOD_NAME
    importing
      !IV_STACK_LEVEL type I default 2
    returning
      value(RV_METHOD_NAME) type STRING .
  class-methods GET_DISP_VAL_FOR_SYSTEM_VA
    importing
      !IV_SYSTEM_VAR_NAME type STRING
    returning
      value(RV_DISPLAY_VALUE) type ZDBBR_VALUE .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF mty_abap_callstack_entry,
        mainprogram TYPE dbgsrepid,
        include     TYPE dbgsrepid,
        line        TYPE dbglinno,
        eventtype   TYPE dbglevtype,
        event       TYPE dbglevent,
        flag_system TYPE c LENGTH 1,
      END OF mty_abap_callstack_entry .
  types:
    mtt_abap_callstack TYPE STANDARD TABLE OF mty_abap_callstack_entry WITH DEFAULT KEY .

  class-data SV_CDS_VIEW_TEXT_TABLE type TABNAME .
  class-data SF_IS_750 type ABAP_BOOL .

  class-methods RAISE_GENERAL_ERROR
    importing
      !IV_MESSAGE type STRING .
ENDCLASS.



CLASS ZCL_DBBR_SYSTEM_HELPER IMPLEMENTATION.


  METHOD check_abap_syntax.
    DATA: lv_line TYPE i,
          lv_word TYPE string.

    SELECT SINGLE * FROM trdir
    INTO @DATA(dir)
    WHERE name = @sy-repid.

    DATA: lv_message TYPE string.

    SYNTAX-CHECK FOR it_lines MESSAGE lv_message
                                LINE lv_line
                                WORD lv_word
                                DIRECTORY ENTRY dir.

    IF lv_message IS NOT INITIAL.
      raise_general_error( EXPORTING iv_message = lv_message ).
    ENDIF.
  ENDMETHOD.


  METHOD create_guid_22.
    TRY.
        rv_guid_22 = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
      CATCH cx_uuid_error INTO DATA(lr_uuid_exc).
        MESSAGE lr_uuid_exc TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD generate_subroutine.
    GENERATE SUBROUTINE POOL it_lines NAME DATA(lv_prog) MESSAGE DATA(lv_message).
    IF lv_message IS NOT INITIAL.
      raise_general_error( EXPORTING iv_message = lv_message ).
    ENDIF.
  ENDMETHOD.


  METHOD get_cds_view_text_table.
    result = cond #(
      when sy-saprl >= 750 then 'DDDDLSRC02BT' else 'DD02BT'
    ).
  ENDMETHOD.


  METHOD get_current_method_name.
    DATA: lt_tab_callstack TYPE mtt_abap_callstack.

    CALL 'ABAP_CALLSTACK'
    ID 'DEPTH' FIELD -10
    ID 'CALLSTACK' FIELD lt_tab_callstack.

    " count of stack has to be greater/equal 2 to get previous method call
    IF lines( lt_tab_callstack ) < iv_stack_level.
      RETURN.
    ENDIF.

    rv_method_name = lt_tab_callstack[ iv_stack_level ]-event.
  ENDMETHOD.


  METHOD get_disp_val_for_system_va.
    CHECK iv_system_var_name IS NOT INITIAL.
    CHECK iv_system_var_name CP 'SY-*'.

    ASSIGN (iv_system_var_name) TO FIELD-SYMBOL(<lv_system_variable>).
    CHECK sy-subrc = 0.
    check <lv_system_variable> is not INITIAL.

*.. get descriptor for system field
    DATA(lr_system_type_descr) = cast cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_data( p_data = <lv_system_variable> )
    ).

    CHECK lr_system_type_descr is bound.

    lr_system_type_descr->get_ddic_field(
      EXPORTING
        p_langu      = SY-LANGU    " Current Language
      RECEIVING
        p_flddescr   = data(ls_field)    " Field Description
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        others       = 3
    ).
    IF sy-subrc = 0.
      data(lr_converter) = new zcl_dbbr_data_converter( ).
      rv_display_value = <lv_system_variable>.
      lr_converter->convert_values_to_disp_format(
        EXPORTING
          iv_rollname = ls_field-tabname
        CHANGING
          cv_value1   = rv_display_value
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_system_value.
    CASE iv_system_value_type.
      WHEN zif_dbbr_global_consts=>gc_system_value_types-date.
        ev_system_value = sy-datum.

      WHEN zif_dbbr_global_consts=>gc_system_value_types-time.
        ev_system_value = sy-timlo.

      WHEN zif_dbbr_global_consts=>gc_system_value_types-user.
        ev_system_value = sy-uname.

      WHEN zif_dbbr_global_consts=>gc_system_value_types-language.
        ev_system_value = sy-langu.

      WHEN OTHERS.
        IF ev_system_value IS SUPPLIED.
          CLEAR ev_system_value.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD print_system_message.

    MESSAGE ID sy-msgid
        TYPE   sy-msgty
        NUMBER sy-msgno
        WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDMETHOD.


  METHOD raise_general_error.
    zcl_dbbr_appl_util=>split_string_for_message(
      EXPORTING iv_string = iv_message
      IMPORTING ev_msgv1  = DATA(lv_msgv1)
                ev_msgv2  = DATA(lv_msgv2)
                ev_msgv3  = DATA(lv_msgv3)
                ev_msgv4  = DATA(lv_msgv4)
    ).
    RAISE EXCEPTION TYPE zcx_dbbr_exception
      EXPORTING
        textid = zcx_dbbr_exception=>general_error
        msgv1  = lv_msgv1
        msgv2  = lv_msgv2
        msgv3  = lv_msgv3
        msgv4  = lv_msgv4.
  ENDMETHOD.


  METHOD set_language.
    SET LANGUAGE 'EN'.
  ENDMETHOD.


  METHOD set_locale_language.
    SET LOCALE LANGUAGE 'E'.
  ENDMETHOD.


  METHOD syst_value_allwd_for_inttyp.
    IF iv_inttype = cl_abap_typedescr=>typekind_char OR
       iv_inttype = cl_abap_typedescr=>typekind_date OR
       iv_inttype = cl_abap_typedescr=>typekind_time.

      rf_possible = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
