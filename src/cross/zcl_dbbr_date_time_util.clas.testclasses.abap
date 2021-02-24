CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_convert_timestamp FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD test_convert_timestamp.
    TRY.
        DATA(lv_converted_date) = zcl_dbbr_date_time_util=>convert_to_timestamp_internal(
          EXPORTING
            iv_value     = '04.01.2019'
            if_is_date   = abap_true
*        iv_time_zone = SY-ZONLO
            iv_rollname  = '/DRY/LASTCHANGEDATETIME'
            iv_domain    = zif_dbbr_c_global=>c_domain_names-timestamp_long
        ).
      CATCH ZCX_SAT_CONVERSION_EXC INTO DATA(lx_conv_exc).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
