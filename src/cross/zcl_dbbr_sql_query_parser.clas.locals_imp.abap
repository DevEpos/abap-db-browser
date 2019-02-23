*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_token_parser IMPLEMENTATION.

  METHOD constructor.
    mt_token = it_token.
    mv_count = lines( mt_token ).
    mv_current_index = 1.
    ms_current_token = mt_token[ mv_current_index ].
  ENDMETHOD.

  METHOD update_from_current.
    mt_token[ mv_current_index ] = ms_current_token.
  ENDMETHOD.


  METHOD is_next_token.

    CHECK has_next_token( ).

    DATA(lv_token) = mt_token[ mv_current_index + 1 ]-value.

    IF iv_next_token CS ','.
      rf_is_next = token_matches( iv_check_list     = iv_next_token
                                  iv_token_to_check = lv_token ).
    ELSE.
      rf_is_next = xsdbool( lv_token = iv_next_token ).
    ENDIF.

  ENDMETHOD.

  METHOD is_previous_token.

    CHECK has_previous_token( ).

    DATA(lv_token) = mt_token[ mv_current_index - 1 ]-value.

    IF iv_previous_token CS ','.
      rf_is_previous = token_matches( iv_check_list     = iv_previous_token
                                      iv_token_to_check = lv_token ).
    ELSE.
      rf_is_previous = xsdbool( lv_token = iv_previous_token ).
    ENDIF.

  ENDMETHOD.

  METHOD next_token.
    CHECK has_next_token( ).

    ADD 1 TO mv_current_index.
    ms_current_token = mt_token[ mv_current_index ].
  ENDMETHOD.

  METHOD has_next_token.
    rf_has_next = xsdbool( mv_current_index + 1 <= mv_count ).
  ENDMETHOD.

  METHOD has_previous_token.
    rf_has_previous = xsdbool( mv_current_index - 1 >= 1 ).
  ENDMETHOD.

  METHOD get_token.
    DATA: lt_token_list  TYPE string_table,
          lt_token_range TYPE RANGE OF string.

    IF iv_token CS ','.
      SPLIT iv_token AT ',' INTO TABLE lt_token_list.
      lt_token_range = VALUE #(
        LET i = 'I' eq = 'EQ' IN
        FOR token IN lt_token_list ( sign = i option = eq low = token )
      ).
      LOOP AT mt_token ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE value IN lt_token_range.
        DATA(lv_index) = sy-tabix.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        mv_current_index = lv_index.
        ms_current_token = <ls_token>.
        rf_exists = abap_true.
      ENDIF.
    ELSE.
      DATA(lv_token_index) = line_index( mt_token[ value = iv_token ] ).
      IF lv_token_index > 0.
        mv_current_index = lv_token_index.
        ms_current_token = mt_token[ lv_token_index ].
        rf_exists = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD set_index_to_first.
    mv_current_index = 1.
    ms_current_token = mt_token[ 1 ].
  ENDMETHOD.

  METHOD delete_current.
    CHECK mv_current_index > 0 AND mv_current_index <= mv_count.

    DELETE mt_token INDEX mv_current_index.

    mv_count = lines( mt_token ).
    IF mv_current_index > mv_count.
      mv_current_index = mv_count.
    ENDIF.

    ms_current_token = mt_token[ mv_current_index ].
  ENDMETHOD.

  METHOD delete_next.
    CHECK has_next_token( ).

    DELETE mt_token INDEX mv_current_index + 1.
  ENDMETHOD.

  METHOD previous_token.
    CHECK mv_current_index > 1.

    SUBTRACT 1 FROM mv_current_index.
    ms_current_token = mt_token[ mv_current_index ].
  ENDMETHOD.

  METHOD delete_previous.
    CHECK mv_current_index - 1 >= 1.

    DELETE mt_token INDEX mv_current_index - 1.
    SUBTRACT 1 FROM mv_current_index.
  ENDMETHOD.

  METHOD token_matches.
    DATA(lv_token_check) = replace( val = iv_check_list sub = ',' with = '|' occ = 0 ).
    lv_token_check = |({ lv_token_check })|.

    rf_matches = xsdbool( count( val = iv_token_to_check regex = lv_token_check  ) > 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_query_param_parser IMPLEMENTATION.

  METHOD lif_statement_parser~parse.
    mr_parameter = NEW #( ).

*.. First token is always the DATA token so jump immediately to second
    next_token( ).
    parse_name( ).
    parse_type( ).
    parse_length( ).
    parse_decimals( ).
    parse_value( ).

    rr_data = mr_parameter.
  ENDMETHOD.

  METHOD parse_name.
    IF ms_current_token-value CS '('. " length declaration inside parenthesis
      DATA(lv_parenthesis_left) = find( val = ms_current_token-value sub = '(' ).
      DATA(lv_parenthesis_right) = find( val = ms_current_token-value sub = ')' ).
      mr_parameter->name = substring( val = ms_current_token-value len = lv_parenthesis_left ).
      mr_parameter->length = substring( val = ms_current_token-value
                                   off = lv_parenthesis_left + 1
                                   len = lv_parenthesis_right - lv_parenthesis_left - 1 ).
    ELSE.
      mr_parameter->name = ms_current_token-value.
    ENDIF.

    mr_parameter->line_in_query = ms_current_token-row.
  ENDMETHOD.

  METHOD parse_decimals.
    CHECK get_token( 'DECIMALS' ).
    next_token( ).

    mr_parameter->decimals = ms_current_token-value.

    data(lv_val) = sana_tok_alias_def.

  ENDMETHOD.

  METHOD parse_length.
    CHECK mr_parameter->length IS NOT INITIAL.

    IF get_token( 'LENGTH' ).
      next_token( ).
      mr_parameter->length = ms_current_token-value.
    ELSEIF mr_parameter->length IS INITIAL AND
           mr_parameter->type = cl_abap_typedescr=>typekind_char.
      mr_parameter->length = 1.
    ENDIF.

  ENDMETHOD.

  METHOD parse_type.
    IF get_token( 'TYPE' ).
      IF is_next_token( 'RANGE' ).
*...... skip an extra token to set current token to type of range
        next_token( ).
        next_token( ).
        mr_parameter->is_range = abap_true.
      ENDIF.
      next_token( ).
      mr_parameter->type = ms_current_token-value.
    ELSE.
*... no concrete type specified so the default type 'C' will be used
      mr_parameter->type = cl_abap_typedescr=>typekind_char.
    ENDIF.
  ENDMETHOD.

  METHOD parse_value.
    CHECK: get_token( 'VALUE' ),
           NOT is_next_token( 'IS' ).

    next_token( ).

    mr_parameter->default_value = replace( val = ms_current_token-value sub = `'` occ = 0 with = space ).
    mr_parameter->default_value_raw = ms_current_token-value.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_query_token_simplifier IMPLEMENTATION.

  METHOD simplify.
    simplify_by_clause( iv_clause     = 'ORDER'
                        iv_simplified = zcl_dbbr_sql_query_parser=>c_keywords-order_by ).
    simplify_by_clause( iv_clause     = 'GROUP'
                        iv_simplified = zcl_dbbr_sql_query_parser=>c_keywords-group_by ).
    simplify_joins( ).
    simplify_conditions( ).
    rt_tokens = mt_token.
  ENDMETHOD.


  METHOD simplify_by_clause.
    WHILE get_token( iv_clause ).
      IF NOT is_next_token( 'BY' ).
        EXIT.
      ENDIF.

      ms_current_token-value =
      ms_current_token-value_no_modifier = iv_simplified .

      update_from_current( ).
      delete_next( ).
    ENDWHILE.
  ENDMETHOD.


  METHOD simplify_joins.

    WHILE get_token( 'JOIN' ).
      previous_token( ).

      IF ms_current_token-value = 'INNER'.
        ms_current_token-value = zcl_dbbr_sql_query_parser=>c_keywords-inner_join.
        ms_current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
        delete_next( ).
      ELSEIF ms_current_token-value = 'OUTER'.
        delete_next( ).
        previous_token( ).
        delete_next( ).
        ms_current_token-value = |{ ms_current_token-value } OUTER JOIN|.
        ms_current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
*..... Abbreviated form of outer join
      ELSEIF ms_current_token-value = 'LEFT' OR
             ms_current_token-value = 'RIGHT'.
        ms_current_token-value = |{ ms_current_token-value } OUTER JOIN|.
        ms_current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
        delete_next( ).
      ELSEIF ms_current_token-value = 'CROSS'.
        ms_current_token-value = |{ ms_current_token-value } JOIN|.
        ms_current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
        delete_next( ).
      ELSE.
*...... This is also an inner join
        next_token( ).
        ms_current_token-value = zcl_dbbr_sql_query_parser=>c_keywords-inner_join.
        ms_current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
      ENDIF.

    ENDWHILE.
  ENDMETHOD.


  METHOD simplify_conditions.

    WHILE get_token( 'NULL,INITIAL' ).
      IF is_previous_token( 'NOT' ).
        delete_previous( ).
        delete_previous( ).
        ms_current_token-value_no_modifier = ms_current_token-value.
        ms_current_token-value = |IS NOT { ms_current_token-value }|.
        update_from_current( ).
      ELSE.
        delete_previous( ).
        ms_current_token-value_no_modifier = ms_current_token-value.
        ms_current_token-value = |IS { ms_current_token-value }|.
        update_from_current( ).
      ENDIF.
    ENDWHILE.

    WHILE get_token( 'NOT' ).

      next_token( ).

      IF ms_current_token-value = 'EXISTS' OR
         ms_current_token-value = 'IN' OR
         ms_current_token-value = 'BETWEEN' OR
         ms_current_token-value = 'LIKE'.
        ms_current_token-value_no_modifier = ms_current_token-value.
        ms_current_token-value = |NOT { ms_current_token-value }|.
        update_from_current( ).
        delete_previous( ).
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
