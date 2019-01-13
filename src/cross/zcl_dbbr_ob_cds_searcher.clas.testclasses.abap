*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_dbbr_ob_cds_searcher.

    METHODS:
      test_no_query FOR TESTING,
      test_description_option_search FOR TESTING,
      test_from_option_search FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD test_no_query.
    TRY.
        mr_cut = NEW #( ir_query = zcl_dbbr_object_search_query=>parse_query_string( iv_query = '' iv_search_type = zif_dbbr_c_object_browser_mode=>cds_view ) ).
        DATA(lt_result) = mr_cut->zif_dbbr_object_searcher~search( ).
      CATCH zcx_dbbr_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act  = lx_search_error
        msg  = 'No Query supplied'
        quit = if_aunit_constants=>method
    ).

  ENDMETHOD.

  METHOD test_description_option_search.

  ENDMETHOD.

  METHOD test_from_option_search.

  ENDMETHOD.

ENDCLASS.
