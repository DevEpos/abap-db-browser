"! <p class="shorttext synchronized" lang="en">Date Time Util</p>
CLASS zcl_dbbr_date_time_util DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Convert entered value into internal timestamp format</p>
    "!
    "! @parameter iv_value | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_is_date | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_domain | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_value | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS convert_to_timestamp_internal
      IMPORTING
        iv_value        TYPE any
        if_is_date      TYPE abap_bool OPTIONAL
        iv_time_zone    TYPE timezone OPTIONAL
        iv_rollname     TYPE rollname
        iv_domain       TYPE domname
      RETURNING
        VALUE(rv_value) TYPE zdbbr_value.
    "! <p class="shorttext synchronized" lang="en">Convert value into internal date format</p>
    "!
    "! @parameter iv_value | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_date | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS convert_to_date_internal
      IMPORTING
        iv_value       TYPE any
      RETURNING
        VALUE(rv_date) TYPE dats.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_date_time_util IMPLEMENTATION.
  METHOD convert_to_timestamp_internal.
    DATA: lv_time       TYPE t,
          lv_message    TYPE string,
          lx_exception  TYPE REF TO zcx_dbbr_conversion_exc,
          lv_timestamp  TYPE timestamp,
          lv_timestampl TYPE timestampl.

    CHECK iv_value IS NOT INITIAL.

*.. Try to convert entered value into date
    IF if_is_date = abap_true.
      TRY.
          DATA(lv_date) = convert_to_date_internal( iv_value ).
        CATCH zcx_dbbr_conversion_exc.
*........ Try conversion as the given value is in timestamp display format
          rv_value = iv_value.
          zcl_dbbr_data_converter=>convert_values_to_int_format(
            EXPORTING
              iv_rollname            = iv_rollname
              if_print_error_message = abap_false
            CHANGING
              cv_value1   = rv_value
          ).
          return.
      ENDTRY.


      IF iv_domain = zif_dbbr_global_consts=>c_domain_names-timestamp.
        CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE iv_time_zone.
        IF sy-subrc = 0 OR sy-subrc = 4.
          rv_value = lv_timestamp.
          CONDENSE rv_value.
        ELSE.
          MESSAGE e019(zdbbr_exception) WITH |{ 'Timestamp could not be created'(001) } | INTO lv_message.
          lx_exception = zcx_dbbr_conversion_exc=>create_from_sy( ).
          RAISE EXCEPTION lx_exception.
        ENDIF.
      ELSEIF iv_domain = zif_dbbr_global_consts=>c_domain_names-timestamp_long.
        CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestampl TIME ZONE iv_time_zone.
        IF sy-subrc = 0 OR sy-subrc = 4.
          rv_value = lv_timestampl.
          CONDENSE rv_value.
        ELSE.
          MESSAGE e019(zdbbr_exception) WITH |{ 'Timestamp could not be created'(001) } | INTO lv_message.
          DATA(lr_exception) = zcx_dbbr_conversion_exc=>create_from_sy( ).
          RAISE EXCEPTION lr_exception.
        ENDIF.
      ENDIF.
    ELSE.
      rv_value = iv_value.
      zcl_dbbr_data_converter=>convert_values_to_int_format(
        EXPORTING
          iv_rollname            = iv_rollname
          if_print_error_message = abap_false
        CHANGING
          cv_value1   = rv_value
      ).
    ENDIF.

  ENDMETHOD.

  METHOD convert_to_date_internal.

    DATA(lv_value) = CONV zdbbr_value( iv_value ).

    zcl_dbbr_data_converter=>convert_values_to_int_format(
      EXPORTING
        iv_rollname            = 'DATS'
        if_print_error_message = abap_false
      CHANGING
        cv_value1   = lv_value
    ).

    rv_date = lv_value.
  ENDMETHOD.

ENDCLASS.
