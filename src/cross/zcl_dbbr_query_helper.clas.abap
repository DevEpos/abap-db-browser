CLASS zcl_dbbr_query_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_query_name
      IMPORTING
        !iv_query_name TYPE zsat_query_name
        !if_global     TYPE boolean .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_query_helper IMPLEMENTATION.

  METHOD check_query_name.
    IF matches( val = iv_query_name regex = '[^$].*' ) AND if_global = abap_true.
      RAISE EXCEPTION TYPE zcx_dbbr_exception
        EXPORTING
          textid = zcx_dbbr_exception=>query_global_name_error
          msgv1  = '$'.
    ELSEIF matches( val = iv_query_name regex = '\$.*' ) AND if_global = abap_false.
      RAISE EXCEPTION TYPE zcx_dbbr_exception
        EXPORTING
          textid = zcx_dbbr_exception=>query_not_global_name_error
          msgv1  = '$'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
