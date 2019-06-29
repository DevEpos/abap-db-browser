CLASS zcl_dbbr_system_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA gv_original_system_langu TYPE sy-langu READ-ONLY.
    CLASS-METHODS print_system_message .
    CLASS-METHODS set_language .
    CLASS-METHODS get_cds_view_text_table
      RETURNING
        VALUE(result) TYPE tabname .
    CLASS-METHODS set_locale_language .
    CLASS-METHODS create_guid_22
      RETURNING
        VALUE(rv_guid_22) TYPE guid_22 .
    CLASS-METHODS generate_subroutine
      IMPORTING
        !it_lines               TYPE string_table
      RETURNING
        VALUE(rv_subr_progname) TYPE progname .
    CLASS-METHODS check_abap_syntax
      IMPORTING
        !it_lines TYPE string_table .
    CLASS-METHODS get_system_language
      RETURNING
        VALUE(result) TYPE sy-langu.
    CLASS-METHODS get_system_value
      IMPORTING
        !iv_system_value_type TYPE zdbbr_syst_value_type
      EXPORTING
        !ev_system_value      TYPE any .
    CLASS-METHODS syst_value_allwd_for_inttyp
      IMPORTING
        !iv_inttype        TYPE inttype
      RETURNING
        VALUE(rf_possible) TYPE boolean .
    CLASS-METHODS get_current_method_name
      IMPORTING
        !iv_stack_level       TYPE i DEFAULT 2
      RETURNING
        VALUE(rv_method_name) TYPE string .
    CLASS-METHODS get_disp_val_for_system_va
      IMPORTING
        !iv_system_var_name     TYPE string
      RETURNING
        VALUE(rv_display_value) TYPE zdbbr_value .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mty_abap_callstack_entry,
        mainprogram TYPE dbgsrepid,
        include     TYPE dbgsrepid,
        line        TYPE dbglinno,
        eventtype   TYPE dbglevtype,
        event       TYPE dbglevent,
        flag_system TYPE c LENGTH 1,
      END OF mty_abap_callstack_entry .
    TYPES:
      mtt_abap_callstack TYPE STANDARD TABLE OF mty_abap_callstack_entry WITH DEFAULT KEY .

    CLASS-DATA sv_cds_view_text_table TYPE tabname .
    CLASS-DATA sf_is_750 TYPE abap_bool .

    CLASS-METHODS raise_general_error
      IMPORTING
        !iv_message TYPE string .
ENDCLASS.



CLASS zcl_dbbr_system_helper IMPLEMENTATION.


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
    result = COND #(
      WHEN sy-saprl >= 750 THEN 'DDDDLSRC02BT' ELSE 'DD02BT'
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
    CHECK <lv_system_variable> IS NOT INITIAL.

*.. get descriptor for system field
    DATA(lr_system_type_descr) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_data( p_data = <lv_system_variable> )
    ).

    CHECK lr_system_type_descr IS BOUND.

    lr_system_type_descr->get_ddic_field(
      EXPORTING
        p_langu      = sy-langu    " Current Language
      RECEIVING
        p_flddescr   = DATA(ls_field)    " Field Description
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3
    ).
    IF sy-subrc = 0.
      DATA(lr_converter) = NEW zcl_dbbr_data_converter( ).
      rv_display_value = <lv_system_variable>.
*.... System language was changed always to English because of GUI Text translations missing
*..... in other languages -> use the cached original language to get the true sy-langu value
      IF iv_system_var_name = 'SY-LANGU'.
        rv_display_value = COND #( WHEN gv_original_system_langu IS NOT INITIAL THEN gv_original_system_langu ELSE sy-langu ).
      ENDIF.
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
    IF gv_original_system_langu IS INITIAL.
      gv_original_system_langu = sy-langu.
    ENDIF.
    SET LOCALE LANGUAGE 'E'.
  ENDMETHOD.


  METHOD syst_value_allwd_for_inttyp.
    IF iv_inttype = cl_abap_typedescr=>typekind_char OR
       iv_inttype = cl_abap_typedescr=>typekind_date OR
       iv_inttype = cl_abap_typedescr=>typekind_time.

      rf_possible = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_system_language.
    result = COND #( WHEN gv_original_system_langu IS NOT INITIAL THEN gv_original_system_langu ELSE sy-langu ).
  ENDMETHOD.

ENDCLASS.
