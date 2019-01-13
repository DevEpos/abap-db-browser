class ZCL_DBBR_WHERE_CLAUSE_BUILDER definition
  public
  final
  create public .

public section.

    "! Creates where condition table connected
    "! with AND keyword
    "! @parameter if_pool | 'X' if pool table is used
    "! @parameter it_and_seltab | List of or conditions to be connected via AND
    "! @parameter rt_where | Lines of conditions for SQL where clause
  class-methods CREATE_AND_CONDITION
    importing
      !IF_POOL type ABAP_BOOL optional
      !IT_AND_SELTAB type ZDBBR_AND_SELTAB_T
    returning
      value(RT_WHERE) type STRING_TABLE .
  class-methods GET_OPTION
    importing
      !IV_SIGN type DDSIGN
      !IV_OPTION type DDOPTION
      !IV_HIGH type CLIKE
      !IF_ESCAPE_CHAR type ABAP_BOOL optional
      !IF_POOL type ABAP_BOOL optional
    changing
      !CV_OPTION type DDOPTION
      !CV_LOW type CLIKE .
    "! Creates where condition table connected with OR keyword
    "!
    "! @parameter if_pool | 'X' if pool table is used
    "! @parameter it_or_seltab | List of conditions to be connected via OR
    "! @parameter rt_where | Lines of conditions for SQL where clause
  class-methods CREATE_OR_CONDITION
    importing
      !IF_POOL type ABAP_BOOL optional
      !IT_OR_SELTAB type ZDBBR_OR_SELTAB_SQL_T
    returning
      value(RT_WHERE) type STRING_TABLE .
  PROTECTED SECTION.
private section.

  class-methods CREATE_CONDITION
    importing
      !IF_POOL type ABAP_BOOL optional
      !IF_OLD_CALL type ABAP_BOOL optional
      !IF_ESCAPE_CHAR type ABAP_BOOL optional
      !IF_PRIMARY_TABLE type ABAP_BOOL optional
      !IT_SEL type ZDBBR_SELTAB_T
    returning
      value(RT_WHERE) type STRING_TABLE .
ENDCLASS.



CLASS ZCL_DBBR_WHERE_CLAUSE_BUILDER IMPLEMENTATION.


  METHOD create_and_condition.
    DATA(lf_multi_and) = xsdbool( lines( it_and_seltab ) > 1 ).
    " Table it_or_seltab contains a counter and a deep table that contains
    " the selection criteria for this counter.

    IF lf_multi_and = abap_true.
      rt_where = VALUE #( ( `(` ) ).
    ENDIF.

    LOOP AT it_and_seltab ASSIGNING FIELD-SYMBOL(<lt_or_seltab>).
      IF sy-tabix > 1.
        rt_where = VALUE #( BASE rt_where (  `) AND (` ) ).
      ENDIF.

      DATA(lf_multi_or) = xsdbool( lines( <lt_or_seltab> ) > 1 ).
      IF lf_multi_or = abap_true.
        rt_where = VALUE #( BASE rt_where ( `(` ) ).
      ENDIF.

      LOOP AT <lt_or_seltab> ASSIGNING FIELD-SYMBOL(<ls_or_seltab>).
        IF sy-tabix > 1.
          rt_where = VALUE #( BASE rt_where (  `) OR (` ) ).
        ENDIF.

        rt_where = VALUE #(
          BASE rt_where
          ( LINES OF create_condition(
             if_pool          = if_pool
             if_primary_table = abap_true
             it_sel           = <ls_or_seltab>-values )
          )
        ).
      ENDLOOP.

      IF lf_multi_or = abap_true.
        rt_where = VALUE #( BASE rt_where ( `)` ) ).
      ENDIF.

    ENDLOOP.

    IF lf_multi_and = abap_true.
      rt_where = VALUE #( BASE rt_where ( `)` ) ).
    ENDIF.

  ENDMETHOD.


  METHOD create_condition.
    DATA: lt_field_ranges  TYPE rsds_trange,
          ls_field_ranges  TYPE rsds_range,
          ls_rsds_trange   TYPE rsds_frange,
          ls_selopt        TYPE rsdsselopt,
          lt_where_clauses TYPE rsds_twhere,
          ls_where_clauses TYPE rsds_where,
          lv_old_field     TYPE fieldname.

    CHECK: it_sel IS NOT INITIAL.

    LOOP AT it_sel ASSIGNING FIELD-SYMBOL(<ls_selfield>).
      " new fieldname
      IF <ls_selfield>-field <> lv_old_field AND
         lv_old_field <> space.
        ls_field_ranges-frange_t = VALUE #( BASE ls_field_ranges-frange_t ( ls_rsds_trange ) ).
        CLEAR ls_rsds_trange-selopt_t.
      ENDIF.

      ls_rsds_trange-fieldname = <ls_selfield>-field.

      ls_selopt = VALUE #(
        sign   = COND #( WHEN <ls_selfield>-sign = space THEN 'I' ELSE <ls_selfield>-sign )
        low    = <ls_selfield>-low
        high   = <ls_selfield>-high
        option = <ls_selfield>-option
      ).

      get_option(
        EXPORTING
          iv_sign        = ls_selopt-sign
          iv_option      = <ls_selfield>-option
          iv_high        = <ls_selfield>-high
          if_escape_char = if_escape_char
          if_pool        = if_pool
        CHANGING
          cv_option      = ls_selopt-option
          cv_low         = ls_selopt-low
      ).

      ls_rsds_trange-selopt_t = VALUE #( BASE ls_rsds_trange-selopt_t ( ls_selopt ) ).
      lv_old_field = <ls_selfield>-field.
    ENDLOOP.

    ls_field_ranges-frange_t = VALUE #( BASE ls_field_ranges-frange_t ( ls_rsds_trange ) ).
    lt_field_ranges = VALUE #( ( ls_field_ranges ) ).

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = lt_field_ranges
      IMPORTING
        where_clauses = lt_where_clauses.
    ls_where_clauses = lt_where_clauses[ 1 ].

    LOOP AT ls_where_clauses-where_tab ASSIGNING FIELD-SYMBOL(<ls_where_line>).
      rt_where = VALUE #( BASE rt_where ( CONV #( <ls_where_line>-line ) ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create_or_condition.
    DATA(lt_or_seltab) = it_or_seltab.

    " first check if every line really contains selections
    DELETE lt_or_seltab WHERE values IS INITIAL.
    IF lt_or_seltab IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lf_multi) = xsdbool( lines( lt_or_seltab ) > 1 ).
    " Table it_or_seltab contains a counter and a deep table that contains
    " the selection criteria for this counter.

    IF lf_multi = abap_true.
      rt_where = VALUE #( ( `(` ) ).
    ENDIF.

    LOOP AT lt_or_seltab ASSIGNING FIELD-SYMBOL(<ls_or_seltab>).
      IF sy-tabix > 1.
        rt_where = VALUE #( BASE rt_where (  `) or (` ) ).
      ENDIF.
      rt_where = VALUE #(
        BASE rt_where
        ( LINES OF create_condition(
           if_pool          = if_pool
           if_primary_table = abap_true
           it_sel           = <ls_or_seltab>-values )
        )
      ).

    ENDLOOP.
    IF lf_multi = abap_true.
      rt_where = VALUE #( BASE rt_where ( `)` ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_option.

    DATA: lf_low_cp TYPE abap_bool.

    DATA(lv_sign) = COND #( WHEN iv_sign = space THEN 'I' ELSE iv_sign ).
    DATA(lf_escape_char) = if_escape_char.

    IF ( cv_low CS '*' OR
       cv_low CS '+') AND ( lf_escape_char <> abap_true ).
      lf_low_cp = abap_true.
    ENDIF.

    IF if_pool    = abap_true AND
       cv_low     CS '_' AND
       lf_low_cp = abap_true.
      sy-subrc = 0.
      WHILE sy-subrc = 0.
        REPLACE '_' WITH '+' INTO cv_low.
      ENDWHILE.
    ENDIF.

    IF if_pool    = abap_true AND
       cv_low     CS '%' AND
       lf_low_cp = abap_true.
      sy-subrc = 0.
      WHILE sy-subrc = 0.
        REPLACE '%' WITH '+' INTO cv_low.
      ENDWHILE.
    ENDIF.

    IF iv_option = space.
      IF iv_high <> space AND
         cv_low <> iv_high.
        cv_option = 'BT'.
      ENDIF.
      IF lf_low_cp = abap_true AND iv_high IS INITIAL.
        cv_option = 'CP'.
      ENDIF.
      IF cv_option = space.
        cv_option = 'EQ'.
      ENDIF.
    ELSE.
      cv_option = iv_option.
    ENDIF.

    IF cv_option = space.
      cv_option = 'EQ'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
