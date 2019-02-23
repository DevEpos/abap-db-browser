"! <p class="shorttext synchronized" lang="en">Base table controller for low/high like input table controls</p>
CLASS zcl_dbbr_base_select_tc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.
  PROTECTED SECTION.

    DATA mr_selfield_line TYPE REF TO zdbbr_selfield .

    "! <p class="shorttext synchronized" lang="en">Check interval validity of selection field</p>
    "!
    "! @parameter iv_save_low | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_save_high | <p class="shorttext synchronized" lang="en"></p>
    METHODS check_interval_validity
      IMPORTING
        !iv_save_low  TYPE zdbbr_selfield-low
        !iv_save_high TYPE zdbbr_selfield-high .
    "! <p class="shorttext synchronized" lang="en">Convert selfield low/high to internal format</p>
    "!
    "! @parameter if_no_uppercase_conversion | <p class="shorttext synchronized" lang="en"></p>
    METHODS conv_selfields_to_internal
      IMPORTING
        !if_no_uppercase_conversion TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Fills the sign in the selection field</p>
    "!
    METHODS fill_selopt_sign .
    "! <p class="shorttext synchronized" lang="en">Retrieve current selscreen util instance</p>
    "!
    "! @parameter rr_util | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_util
      RETURNING
        VALUE(rr_util) TYPE REF TO zcl_dbbr_selscreen_util.
    "! <p class="shorttext synchronized" lang="en">Checks if custom timestamp conversion should be done</p>
    "!
    "! @parameter rf_custom_timst_conv | <p class="shorttext synchronized" lang="en"></p>
    METHODS is_custom_timest_conv
      RETURNING
        VALUE(rf_custom_timst_conv) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Check if uppercase conversion should be prevented</p>
    "!
    "! @parameter rf_no_uppercase_conv | <p class="shorttext synchronized" lang="en"></p>
    METHODS is_uppercase_conv_disabled
      RETURNING
        VALUE(rf_no_uppercase_conv) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Perform pre conversion checks</p>
    "!
    "! @parameter is_selfield | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_no_uppercase_conv | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter cv_value | <p class="shorttext synchronized" lang="en"></p>
    METHODS precheck_selfield_value
      IMPORTING
        is_selfield          TYPE zdbbr_selfield
        if_no_uppercase_conv TYPE abap_bool OPTIONAL
      CHANGING
        cv_value             TYPE zdbbr_value .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_base_select_tc IMPLEMENTATION.

  METHOD is_custom_timest_conv.

    rf_custom_timst_conv = xsdbool(
       ( mr_selfield_line->domname = zif_dbbr_global_consts=>c_domain_names-timestamp OR
         mr_selfield_line->domname = zif_dbbr_global_consts=>c_domain_names-timestamp_long ) AND
       get_util( )->mo_data->mr_s_global_data->settings-disable_date_to_times_conv = abap_false
    ).

  ENDMETHOD.


  METHOD check_interval_validity.

*.. check if low-value greater than high value
*.. to only allow CHAR is the easiest solution. Another option would
*.. be the coding of the documentation for CREATE DATA - TYPE abap_type
    IF mr_selfield_line->low > mr_selfield_line->high AND
       mr_selfield_line->high <> space           AND
       ( mr_selfield_line->datatype = 'CHAR' OR
         mr_selfield_line->datatype = 'DATS' OR
         mr_selfield_line->datatype = 'LANG' OR
         mr_selfield_line->datatype = 'CUKY' OR
         mr_selfield_line->datatype = 'CLNT' OR
         mr_selfield_line->datatype = 'NUMC' OR
         mr_selfield_line->datatype = 'TIMS' ).
      mr_selfield_line->low  = iv_save_low.
      mr_selfield_line->high = iv_save_high.
      MESSAGE e650(db).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD conv_selfields_to_internal.

    DATA(lf_custom_timst_conv) = is_custom_timest_conv( ).

    IF if_no_uppercase_conversion = abap_false.
      IF mr_selfield_line->is_parameter = abap_true.
        IF lf_custom_timst_conv = abap_true.
          mr_selfield_line->low = zcl_dbbr_date_time_util=>convert_to_timestamp_internal(
              iv_value     = mr_selfield_line->low
              if_is_date   = abap_true
              iv_rollname  = mr_selfield_line->rollname
              iv_domain    = mr_selfield_line->domname
          ).
        ELSE.
          zcl_dbbr_data_converter=>convert_values_to_int_format(
            EXPORTING
              iv_rollname            = mr_selfield_line->rollname
              iv_type                = mr_selfield_line->inttype
              iv_length              = CONV #( mr_selfield_line->intlen )
              iv_decimals            = CONV #( mr_selfield_line->decimals )
              if_print_error_message = abap_false
            CHANGING
              cv_value1              = mr_selfield_line->low
          ).
        ENDIF.
      ELSE.
        IF lf_custom_timst_conv = abap_true.
          mr_selfield_line->low = zcl_dbbr_date_time_util=>convert_to_timestamp_internal(
              iv_value     = mr_selfield_line->low
              if_is_date   = abap_true
              iv_rollname  = mr_selfield_line->rollname
              iv_domain    = mr_selfield_line->domname
          ).
          mr_selfield_line->high = zcl_dbbr_date_time_util=>convert_to_timestamp_internal(
              iv_value     = mr_selfield_line->high
              if_is_date   = abap_true
              iv_rollname  = mr_selfield_line->rollname
              iv_domain    = mr_selfield_line->domname
          ).
        ELSE.
          zcl_dbbr_data_converter=>convert_selopt_to_int_format(
            EXPORTING iv_tabname   = mr_selfield_line->tabname
                      iv_fieldname = mr_selfield_line->fieldname
                      if_print_error_message = abap_false
            CHANGING  cv_value1    = mr_selfield_line->low
                      cv_value2    = mr_selfield_line->high
          ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD fill_selopt_sign.

    IF mr_selfield_line->sign = space.
      mr_selfield_line->sign = zif_dbbr_global_consts=>gc_options-i.
    ENDIF.

  ENDMETHOD.

  METHOD get_util.
    rr_util = zcl_dbbr_selscreen_util_fac=>get_util_instance( ).
  ENDMETHOD.


  METHOD is_uppercase_conv_disabled.

    IF mr_selfield_line->convexit = space AND
       ( mr_selfield_line->inttype = cl_abap_typedescr=>typekind_char OR
         mr_selfield_line->inttype = cl_abap_typedescr=>typekind_string ) AND
       mr_selfield_line->lowercase = abap_true.
      rf_no_uppercase_conv = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD precheck_selfield_value.
    CHECK cv_value <> space.

*.. check if input length is 45 or more
    zcl_dbbr_input_validator=>check_and_crop_input( CHANGING cv_value = cv_value ).

*.. as the screen field is upper case sensitive I have to convert
*.. all other fields
    IF is_selfield-lowercase <> abap_true AND
      if_no_uppercase_conv = abap_false.
      TRANSLATE cv_value TO UPPER CASE.
    ENDIF.

*.. was sy-datum entered?
    IF ( is_selfield-datatype = 'DATS' OR
       ( is_selfield-domname = zif_dbbr_global_consts=>c_domain_names-timestamp OR
         is_selfield-domname = zif_dbbr_global_consts=>c_domain_names-timestamp_long ) AND
       get_util( )->mo_data->mr_s_global_data->settings-disable_date_to_times_conv = abap_false )
         AND cv_value = 'SY-DATUM'.
      cv_value = |{ sy-datum DATE = USER }|.
    ENDIF.

    IF is_selfield-datatype = 'CHAR' AND
       is_selfield-intlen >= 12 AND
       cv_value = 'SY-UNAME'.
      cv_value = sy-uname.
    ENDIF.

    IF is_selfield-datatype = 'LANG' AND cv_value = 'SY-LANGU'.
      cv_value = sy-langu.
    ENDIF.

  ENDMETHOD.



ENDCLASS.
