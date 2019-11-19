CLASS zcl_dbbr_query_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_query_name
      IMPORTING
        !iv_query_name TYPE zsat_query_name
        !if_global     TYPE boolean .
    CLASS-METHODS call_query_f4
      IMPORTING
        !iv_repid            TYPE sy-repid
      RETURNING
        VALUE(rv_query_name) TYPE zsat_query_name .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_query_helper IMPLEMENTATION.


  METHOD call_query_f4.
    TYPES: BEGIN OF lty_search_value,
             query_name    TYPE zsat_query_name,
             primary_table TYPE tabname,
             description   TYPE ddtext,
             created_by    TYPE zsat_created_by,
           END OF lty_search_value.

    DATA: lt_values TYPE STANDARD TABLE OF lty_search_value,
          lt_return TYPE STANDARD TABLE OF ddshretval.

    DATA(lr_query_factory) = NEW zcl_dbbr_query_factory( ).

    lr_query_factory->find_queries( IMPORTING et_queries = DATA(lt_queries) ).

    lt_values = VALUE #( FOR query IN lt_queries ( CORRESPONDING #( query ) ) ).

    DATA(lv_dynprog) = iv_repid.
    DATA(lv_dynnr) = sy-dynnr.

    " key for personal value list
    DATA(lv_pers_value_list_key) = CONV ddshpvkey( lv_dynprog && lv_dynnr ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'query_NAME'
        window_title    = 'Skriptauswahl'
        value_org       = 'S'
        dynpprog        = lv_dynprog
        dynpnr          = lv_dynnr
        pvalkey         = lv_pers_value_list_key
      TABLES
        value_tab       = lt_values
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc = 0 AND lt_return IS NOT INITIAL.
      rv_query_name = lt_values[ query_name = lt_return[ 1 ]-fieldval ]-query_name.
    ENDIF.
  ENDMETHOD.


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
