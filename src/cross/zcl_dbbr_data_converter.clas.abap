class ZCL_DBBR_DATA_CONVERTER definition
  public
  create public .

public section.

  class-methods PERFORM_ALPHA_CONVERSION_INPUT
    importing
      !IV_TABNAME type TABNAME
      !IV_FIELDNAME type FIELDNAME
      !IV_VALUE type CLIKE
    exporting
      !EV_OUTPUT type CLIKE .
  class-methods CONVERT_VALUES_TO_DISP_FORMAT
    importing
      !IV_ROLLNAME type ROLLNAME optional
      !IV_TYPE type INTTYPE optional
      !IV_LENGTH type I default 0
      !IV_DECIMALS type I default 0
    changing
      !CV_VALUE1 type ZDBBR_VALUE optional
      !CV_VALUE2 type ZDBBR_VALUE optional .
  class-methods CONVERT_SELOPT_TO_DISP_FORMAT
    importing
      !IV_TABNAME type TABNAME
      !IV_FIELDNAME type FIELDNAME
    changing
      !CV_VALUE1 type SE16N_VALUE optional
      !CV_VALUE2 type SE16N_VALUE optional .
  class-methods CONVERT_VALUES_TO_INT_FORMAT
    importing
      !IV_ROLLNAME type ROLLNAME optional
      !IV_TYPE type INTTYPE optional
      !IV_LENGTH type I default 0
      !IV_DECIMALS type I default 0
      !IF_PRINT_ERROR_MESSAGE type ABAP_BOOL default ABAP_TRUE
    changing
      !CV_VALUE1 type ZDBBR_VALUE optional
      !CV_VALUE2 type ZDBBR_VALUE optional .
  class-methods CONVERT_SELOPT_TO_INT_FORMAT
    importing
      !IV_TABNAME type TABNAME
      !IV_FIELDNAME type FIELDNAME
      !IF_PRINT_ERROR_MESSAGE type ABAP_BOOL default ABAP_TRUE
    changing
      !CV_VALUE1 type SE16N_VALUE optional
      !CV_VALUE2 type SE16N_VALUE optional .
  class-methods CONVERT_DATES_TO_INT_FORMAT
    importing
      !IV_TABNAME type TABNAME
      !IV_FIELDNAME type FIELDNAME
      !IF_PRINT_ERROR_MESSAGE type ABAP_BOOL default ABAP_TRUE
    changing
      !CV_VALUE1 type SE16N_VALUE
      !CV_VALUE2 type SE16N_VALUE .
  class-methods CONVERT_DATES_TO_OUT_FORMAT
    importing
      !IV_TABNAME type TABNAME
      !IV_FIELDNAME type FIELDNAME
    changing
      !CV_VALUE1 type SE16N_VALUE optional
      !CV_VALUE2 type SE16N_VALUE optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_DATA_CONVERTER IMPLEMENTATION.


  METHOD convert_dates_to_int_format.
*&---------------------------------------------------------------------*
*& Description: Conversion of dates into internal format
*& Special case: check if entered value equals to sy-datum
*&---------------------------------------------------------------------*
    data(lf_convert_value1) = xsdbool( cv_value1 <> 'SY-DATUM' ).
    data(lf_convert_value2) = xsdbool( cv_value2 <> 'SY-DATUM' ).
  ENDMETHOD.


  METHOD convert_dates_to_out_format.
*&---------------------------------------------------------------------*
*& Description: Conversion of dates into internal format
*& Special case: check if entered value equals to sy-datum
*&---------------------------------------------------------------------*
    CHECK cv_value1 <> space OR cv_value2 <> space.

    DATA(lr_converter) = NEW cl_fobu_input_util(
        tabname   = iv_tabname
        fieldname = iv_fieldname
    ).

    IF cv_value1 <> space.
      IF cv_value1 = 'SY-DATUM'.
        cv_value1 = |{ sy-datum date = user }|.
      ELSE.
        lr_converter->output_convert( EXPORTING field_value_int = cv_value1
                                      IMPORTING field_value_ext = cv_value1 ).
      ENDIF.
    ENDIF.

    IF cv_value2 <> space.
      IF cv_value2 = 'SY-DATUM'.
        cv_value2 = |{ sy-datum date = user }|.
      ELSE.
        lr_converter->output_convert( EXPORTING field_value_int = cv_value2
                                      IMPORTING field_value_ext = cv_value2 ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD convert_selopt_to_disp_format.

*&---------------------------------------------------------------------*
*& Description: Converts input values to display format
*&---------------------------------------------------------------------*
    CHECK cv_value1 <> space OR cv_value2 <> space.

    CHECK: iv_tabname IS NOT INITIAL,
           iv_fieldname IS NOT INITIAL.

    DATA(lr_converter) = NEW cl_fobu_input_util(
        tabname   = iv_tabname
        fieldname = iv_fieldname
    ).

    IF cv_value1 <> space.
      lr_converter->output_convert( EXPORTING field_value_int = cv_value1
                                    IMPORTING field_value_ext = cv_value1 ).
    ENDIF.

    IF cv_value2 <> space.
      lr_converter->output_convert( EXPORTING field_value_int = cv_value2
                                    IMPORTING field_value_ext = cv_value2 ).
    ENDIF.

  ENDMETHOD.


  METHOD convert_selopt_to_int_format.
*&---------------------------------------------------------------------*
*& Description: Converts select parameters in internal format
*&---------------------------------------------------------------------*
    TYPES: lty_value_ref TYPE REF TO data.
    DATA: lt_ref_table TYPE TABLE OF lty_value_ref.

    FIELD-SYMBOLS: <lv_value> LIKE cv_value1.

    CHECK cv_value1 <> space OR cv_value2 <> space.

    CHECK: iv_tabname IS NOT INITIAL,
           iv_fieldname IS NOT INITIAL.

    "" build ref table
    lt_ref_table = VALUE #( ( REF #( cv_value1 ) )
                            ( REF #( cv_value2 ) ) ).

    DATA(lr_converter) = NEW cl_fobu_input_util(
        tabname   = iv_tabname
        fieldname = iv_fieldname
    ).

    """ convert requested values into internal format - if possible.
    LOOP AT lt_ref_table ASSIGNING FIELD-SYMBOL(<lv_ref_value>).
      ASSIGN <lv_ref_value>->* TO <lv_value>.
      IF <lv_value> <> space.
        lr_converter->input_convert( EXPORTING  field_value_ext   = <lv_value>
                                     IMPORTING  field_value_int_c = <lv_value>
                                     EXCEPTIONS illegal_value     = 1 ).
        IF sy-subrc = 1.
          IF if_print_error_message = abap_true.
            zcl_dbbr_system_helper=>print_system_message( ).
          ELSE.
            DATA(lr_conversion_exc) = zcx_dbbr_conversion_exc=>create_from_sy( ).
            RAISE EXCEPTION lr_conversion_exc.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD CONVERT_VALUES_TO_DISP_FORMAT.

*&---------------------------------------------------------------------*
*& Description: Converts input values to display format
*&---------------------------------------------------------------------*
    CHECK cv_value1 <> space OR cv_value2 <> space.

    DATA(lr_converter) = NEW cl_fobu_input_util(
        typename = cond #( when iv_rollname is not initial then iv_rollname else iv_type )
        length   = iv_length
        decimals = iv_decimals
    ).

    IF cv_value1 <> space.
      lr_converter->output_convert( EXPORTING field_value_int = cv_value1
                                    IMPORTING field_value_ext = cv_value1 ).
    ENDIF.

    IF cv_value2 <> space.
      lr_converter->output_convert( EXPORTING field_value_int = cv_value2
                                    IMPORTING field_value_ext = cv_value2 ).
    ENDIF.

  ENDMETHOD.


  METHOD CONVERT_VALUES_TO_INT_FORMAT.
*&---------------------------------------------------------------------*
*& Description: Converts select parameters in internal format
*&---------------------------------------------------------------------*
    TYPES: lty_value_ref TYPE REF TO data.
    DATA: lt_ref_table TYPE TABLE OF lty_value_ref.

    FIELD-SYMBOLS: <lv_value> LIKE cv_value1.

    CHECK cv_value1 <> space OR cv_value2 <> space.

    "" build ref table
    lt_ref_table = VALUE #( ( REF #( cv_value1 ) )
                            ( REF #( cv_value2 ) ) ).

    DATA(lr_converter) = NEW cl_fobu_input_util(
        typename = cond #( when iv_rollname is not initial then iv_rollname else iv_type )
        length   = iv_length
        decimals = iv_decimals
    ).

    """ convert requested values into internal format - if possible.
    LOOP AT lt_ref_table ASSIGNING FIELD-SYMBOL(<lv_ref_value>).
      ASSIGN <lv_ref_value>->* TO <lv_value>.
      IF <lv_value> <> space.
        lr_converter->input_convert( EXPORTING  field_value_ext   = <lv_value>
                                     IMPORTING  field_value_int_c = <lv_value>
                                     EXCEPTIONS illegal_value     = 1 ).
        IF sy-subrc = 1.
          IF if_print_error_message = abap_true.
            ZCL_DBBR_system_helper=>print_system_message( ).
          ELSE.
            DATA(lr_conversion_exc) = ZCX_DBBR_conversion_exc=>create_from_sy( ).
            RAISE EXCEPTION lr_conversion_exc.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD perform_alpha_conversion_input.
*&---------------------------------------------------------------------*
*& Description: Performs alpha conversion of input data
*&---------------------------------------------------------------------*
    DATA: lr_input TYPE REF TO data.
    FIELD-SYMBOLS: <lv_input> TYPE any.

    " create type described by name
    DATA(lr_type_descr) = CAST cl_abap_datadescr(
        cl_abap_typedescr=>describe_by_name(
            cond #( when iv_fieldname is initial then iv_tabname else iv_tabname && '-' && iv_fieldname )
        )
    ).
    CREATE DATA lr_input TYPE HANDLE lr_type_descr.

    ASSIGN lr_input->* TO <lv_input>.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_value
      IMPORTING
        output = <lv_input>.

    ev_output = <lv_input>.

  ENDMETHOD.
ENDCLASS.
