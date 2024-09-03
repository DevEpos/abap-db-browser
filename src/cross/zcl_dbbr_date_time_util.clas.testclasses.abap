CLASS ltcl_abap_unit DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_convert_timestamp FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD test_convert_timestamp.
    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(lv_converted_date) = zcl_dbbr_date_time_util=>convert_to_timestamp_internal(
                                      iv_value    = '04.01.2019'
                                      if_is_date  = abap_true
*                                      iv_time_zone = SY-ZONLO
                                      iv_rollname = '/DRY/LASTCHANGEDATETIME'
                                      iv_domain   = zif_dbbr_c_global=>c_domain_names-timestamp_long ).
      CATCH zcx_sat_conversion_exc INTO DATA(lx_conv_exc). " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
