"! <p class="shorttext synchronized" lang="en">Parser for SQL Query</p>
CLASS zcl_dbbr_sql_query_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_parameter.
        INCLUDE TYPE zdbbr_queryp.
    TYPES is_used       TYPE abap_bool.
    TYPES line_in_query TYPE i.
    TYPES: END OF ty_s_parameter.
    TYPES: ty_t_parameter TYPE STANDARD TABLE OF ty_s_parameter WITH KEY name.

    TYPES: ty_sql_statement_type TYPE c LENGTH 1.
    TYPES:
      BEGIN OF ty_s_token,
        value             TYPE string,
        value_no_modifier TYPE string,
        type              TYPE token_type,
        row               TYPE i,
        col               TYPE i,
      END OF ty_s_token.
    TYPES: ty_t_token TYPE STANDARD TABLE OF ty_s_token WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_statement,
        first_token   TYPE string,
        is_main_query TYPE abap_bool,
        terminator    TYPE stmnt_term,
        type          TYPE stmnt_type,
        tokens        TYPE ty_t_token,
      END OF ty_s_statement.
    TYPES: ty_t_statement TYPE STANDARD TABLE OF ty_s_statement WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF c_keywords,
        select           TYPE string VALUE 'SELECT',
        count            TYPE string VALUE 'COUNT',
        count_pattern    TYPE string VALUE 'COUNT*',
        count_star       TYPE string VALUE 'COUNT(*)',
        data             TYPE string VALUE 'DATA',
        where            TYPE string VALUE 'WHERE',
        fields           TYPE string VALUE 'FIELDS',
        order_by         TYPE string VALUE 'ORDER BY',
        group_by         TYPE string VALUE 'GROUP BY',
        with             TYPE string VALUE 'WITH',
        inner_join       TYPE string VALUE 'INNER JOIN',
        left_outer_join  TYPE string VALUE 'LEFT OUTER JOIN',
        right_outer_join TYPE string VALUE 'RIGHT OUTER JOIN',
        union            TYPE string VALUE 'UNION',
        cross_join       TYPE string VALUE 'CROSS JOIN',
      END OF c_keywords.

    CONSTANTS:
      BEGIN OF c_token_class,
        join TYPE i VALUE 2,
      END OF c_token_class.
    CONSTANTS:
      BEGIN OF c_literals,
        star_value TYPE string VALUE '*',
      END OF c_literals.
    CONSTANTS:
      BEGIN OF c_statement_type,
        computation  TYPE stmnt_type VALUE 'C',
        keyword      TYPE stmnt_type VALUE 'K',
        line_comment TYPE stmnt_type VALUE 'P',
        comment      TYPE stmnt_type VALUE 'S',
        unknown      TYPE stmnt_type VALUE 'U',
      END OF c_statement_type.

    "! <p class="shorttext synchronized" lang="en">Create new parser</p>
    "!
    METHODS constructor
      IMPORTING
        iv_query TYPE string.
    CLASS-METHODS class_constructor.
    "! <p class="shorttext synchronized" lang="en">Parse the query</p>
    "!
    METHODS parse
      RETURNING
        VALUE(ro_query) TYPE REF TO zcl_dbbr_sql_query
      RAISING
        zcx_dbbr_sql_query_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gt_invalid_keyword_range TYPE RANGE OF string.

    DATA mv_raw_query TYPE string.
    DATA mv_query_type TYPE string.
    DATA mv_executable_query TYPE string.
    DATA mv_select_query_end_offset TYPE i.
    DATA mo_sql_query TYPE REF TO zcl_dbbr_sql_query.
    DATA mt_stmnt TYPE ty_t_statement.
    DATA ms_select_stmnt TYPE ty_s_statement.
    DATA mt_parameter TYPE ty_t_parameter.
    DATA mf_union TYPE abap_bool.
    DATA mt_param_range TYPE RANGE OF string.
    DATA mv_select_stmnt_index TYPE i.
    DATA mt_stmnt_raw TYPE sstmnt_tab.
    DATA mt_token_raw TYPE stokesx_tab.
    DATA mt_query_lines TYPE STANDARD TABLE OF string.
    DATA mv_select_query_end_row TYPE i.
    DATA mf_single_result TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Tokenize the query statement</p>
    "!
    METHODS tokenize
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Process the tokens to get details of the query</p>
    "!
    METHODS parse_query
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Do some prelimary validation on the query</p>
    "!
    METHODS pre_validate_statements
      RAISING
        zcx_dbbr_sql_query_error.

    "! <p class="shorttext synchronized" lang="en">Check for invalid tokens before hand</p>
    "!
    METHODS pre_validate_tokens
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Combines tokens with statements for easier processing</p>
    "!
    METHODS combine_stmnt_with_tokens.
    "! <p class="shorttext synchronized" lang="en">Checks ABAP syntax of query</p>
    "!
    METHODS check_syntax
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Insert INTO TABLE clause into select stmnt.</p>
    "!
    METHODS insert_into_table_clause
      CHANGING
        ct_query_lines TYPE string_table
      RAISING
        zcx_dbbr_sql_query_error.
    "! <p class="shorttext synchronized" lang="en">Extract parameters from query string</p>
    "!
    METHODS extract_parameters.
    "! <p class="shorttext synchronized" lang="en">Check if all parameters are used in query</p>
    "!
    METHODS check_parameters_where_used.
    "! <p class="shorttext synchronized" lang="en">Simplifation of some tokens</p>
    "! Parses tokens in SELECT/WITH clause as combines them into one token where
    "! it is senseful. <br>
    "! Example: <br>
    "! Token 'ORDER' and token 'BY' will be combined into Token 'ORDER BY'
    METHODS simplify_tokens.
    "! <p class="shorttext synchronized" lang="en">Determines the properties of the main statement in the query</p>
    "! This are needed to properly create the subroutine program for the data
    "! selection
    METHODS determine_main_stmnt_props.
ENDCLASS.



CLASS zcl_dbbr_sql_query_parser IMPLEMENTATION.
  METHOD constructor.
    mv_raw_query = iv_query.
    SPLIT mv_raw_query AT cl_abap_char_utilities=>cr_lf INTO TABLE mt_query_lines.
  ENDMETHOD.

  METHOD class_constructor.
    gt_invalid_keyword_range = VALUE #(
      LET i = 'I' eq = 'EQ' IN
      ( sign = i option = eq low = `INSERT` )
      ( sign = i option = eq low = `MODIFY` )
      ( sign = i option = eq low = `DELETE` )
      ( sign = i option = eq low = `UP` )
      ( sign = i option = eq low = `UPDATE` )
      ( sign = i option = eq low = `INTO` )
      ( sign = i option = eq low = `SINGLE` )
      ( sign = i option = eq low = `REF` )
      ( sign = i option = eq low = `ENDSELECT` )
      ( sign = i option = eq low = `ENDWITH` )
      ( sign = i option = eq low = `READ` )
      ( sign = i option = eq low = `LOOP` )
      ( sign = i option = eq low = `FIELD-SYMBOLS` )
      ( sign = i option = eq low = `ASSIGN` )
    ).
  ENDMETHOD.

  METHOD parse.
    CHECK mv_raw_query IS NOT INITIAL.

    tokenize( ).
    pre_validate_tokens( ).
    pre_validate_statements( ).

    combine_stmnt_with_tokens( ).

*.. Check the type of the query
    determine_main_stmnt_props( ).

    check_syntax( ).

    extract_parameters( ).
    check_parameters_where_used( ).

*.. Syntax check is done, now the actual parsing/tokenization
*.. of the query is performed
    IF lines( mt_stmnt ) = 1.
      ms_select_stmnt = mt_stmnt[ 1 ].
      CLEAR mt_stmnt.

      simplify_tokens( ).
      parse_query( ).
    ELSEIF mt_stmnt IS INITIAL.
      RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
        EXPORTING
          textid = zcx_dbbr_sql_query_error=>no_select_statement.
    ENDIF.

*.. Parse without error
    mo_sql_query = NEW zcl_dbbr_sql_query(
      is_query      = VALUE zdbbr_sql_query(
        source                   = mv_raw_query
        select_source            = mv_executable_query
        last_row_in_select_stmnt = mv_select_query_end_row
        last_row_offset          = mv_select_query_end_offset
        main_select_stmnt_type   = mv_query_type
        is_single_result_query   = mf_single_result
      )
      it_parameters = VALUE #( FOR param IN mt_parameter WHERE ( is_used = abap_true ) ( CORRESPONDING #( param ) )  )
    ).

    ro_query = mo_sql_query.

  ENDMETHOD.


  METHOD tokenize.

    DATA: lv_message TYPE string,
          lv_word    TYPE char80,
          lv_line    TYPE i.

    SCAN ABAP-SOURCE mt_query_lines TOKENS INTO     mt_token_raw
                                    STATEMENTS INTO mt_stmnt_raw
                                    MESSAGE INTO    lv_message
                                    WORD INTO       lv_word
                                    LINE INTO       lv_line
                                    WITH ANALYSIS.

    IF lv_message IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
        EXPORTING
          textid = zcx_dbbr_application_exc=>general_error
          msgv1  = |{ lv_message }|
          msgv2  = |{ lv_word }|
          msgv3  = |{ lv_line }|.
    ENDIF.

    LOOP AT mt_stmnt_raw ASSIGNING FIELD-SYMBOL(<ls_stmnt>).

      CALL FUNCTION 'RS_QUALIFY_ABAP_TOKENS_STR'
        EXPORTING
          statement_type = <ls_stmnt>-type
          index_from     = <ls_stmnt>-from
          index_to       = <ls_stmnt>-to
        CHANGING
          stokesx_tab    = mt_token_raw
        EXCEPTIONS
          OTHERS         = 0.

    ENDLOOP.

  ENDMETHOD.


  METHOD pre_validate_tokens.
    LOOP AT mt_token_raw ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE str IN gt_invalid_keyword_range.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
        EXPORTING
          textid      = zcx_dbbr_sql_query_error=>invalid_token
          msgv1       = |{ <ls_token>-str }|
          line_number = <ls_token>-row.
    ENDIF.
  ENDMETHOD.

  METHOD pre_validate_statements.
    DATA: lv_select_stmnt_count TYPE i.

    FIELD-SYMBOLS: <ls_token> TYPE stokesx.


    DATA(lv_stmnt_count) = lines( mt_stmnt_raw ).

    LOOP AT mt_stmnt_raw ASSIGNING FIELD-SYMBOL(<ls_stmnt>).
      DATA(lv_tabix) = sy-tabix.

      ASSIGN mt_token_raw[ <ls_stmnt>-from ] TO FIELD-SYMBOL(<ls_first_token>).

      IF <ls_first_token>-str = c_keywords-select OR
         <ls_first_token>-str = c_keywords-with.
        ADD 1 TO lv_select_stmnt_count.
        mv_select_stmnt_index = <ls_first_token>-row.
      ELSEIF <ls_first_token>-str = c_keywords-data.
        LOOP AT mt_token_raw ASSIGNING <ls_token> FROM <ls_stmnt>-from TO <ls_stmnt>-to WHERE str = 'LIKE' OR
                                                                                              str = 'TABLE'.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
            EXPORTING
              textid      = zcx_dbbr_sql_query_error=>invalid_token_in_data_declare
              msgv1       = |{ <ls_token>-str }|
              line_number = <ls_token>-row.
        ENDIF.

      ELSEIF <ls_first_token>-str <> c_keywords-data AND
             <ls_first_token>-str <> c_keywords-with AND
             <ls_first_token>-str <> c_keywords-select.
        RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
          EXPORTING
            textid      = zcx_dbbr_sql_query_error=>invalid_statement
            msgv1       = |{ <ls_first_token>-str }|
            line_number = <ls_first_token>-row.
      ENDIF.

      IF lv_tabix = lv_stmnt_count AND
         <ls_first_token>-str <> c_keywords-select AND
         <ls_first_token>-str <> c_keywords-with.
        zcx_dbbr_sql_query_error=>raise_with_text(
            iv_text        = |Last statement has to be a SELECT or WITH statment|
            iv_line_number = <ls_first_token>-row
        ).
      ENDIF.
    ENDLOOP.

    IF lv_select_stmnt_count > 1.
      RAISE EXCEPTION TYPE zcx_dbbr_sql_query_error
        EXPORTING
          textid = zcx_dbbr_sql_query_error=>too_many_select_stmnt.
    ENDIF.

  ENDMETHOD.


  METHOD combine_stmnt_with_tokens.
*.. Combine statements and tokens
    LOOP AT mt_stmnt_raw ASSIGNING FIELD-SYMBOL(<ls_stmnt>).
      DATA(ls_statement) = VALUE ty_s_statement(
          terminator  = <ls_stmnt>-terminator
          type        = <ls_stmnt>-type
          tokens      = VALUE ty_t_token(
            FOR token IN mt_token_raw FROM <ls_stmnt>-from TO <ls_stmnt>-to
            ( value             = token-str
              value_no_modifier = token-str
              row               = token-row
              col               = token-col
              type              = token-type )
          )
      ).

      ls_statement-first_token = ls_statement-tokens[ 1 ]-value.
      ls_statement-is_main_query = xsdbool( ls_statement-first_token = c_keywords-select OR
                                            ls_statement-first_token = c_keywords-with ).

      mt_stmnt = VALUE #( BASE mt_stmnt ( ls_statement ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD parse_query.

  ENDMETHOD.


  METHOD simplify_tokens.
    ms_select_stmnt-tokens = NEW lcl_query_token_simplifier( ms_select_stmnt-tokens )->simplify( ).
  ENDMETHOD.

  METHOD extract_parameters.
    LOOP AT mt_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>) WHERE first_token = c_keywords-data.

      DATA(ls_parameter) = CAST ty_s_parameter( NEW lcl_query_param_parser( <ls_stmnt>-tokens )->parse( ) ).
      IF ls_parameter->name IS NOT INITIAL.
        mt_parameter = VALUE #( BASE mt_parameter ( ls_parameter->* ) ).
      ENDIF.

      DELETE mt_stmnt.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_parameters_where_used.
    DATA: lt_param_check_range TYPE RANGE OF string.

    CHECK mt_parameter IS NOT INITIAL.

    DATA(lo_protocol) = zcl_uitb_protocol=>get_instance( ).
    ASSIGN mt_stmnt[ is_main_query = abap_true ] TO FIELD-SYMBOL(<ls_select>).
    CHECK sy-subrc = 0.

    LOOP AT <ls_select>-tokens ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE type = sana_tok_field.
      LOOP AT mt_parameter ASSIGNING FIELD-SYMBOL(<ls_parameter>) WHERE is_used = abap_false.
        IF <ls_token>-value CP '*' && <ls_parameter>-name  && '*'.
          <ls_parameter>-is_used = abap_true.
        ENDIF.
      ENDLOOP.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF line_exists( mt_parameter[ is_used = abap_false ] ).
      LOOP AT mt_parameter ASSIGNING <ls_parameter> WHERE is_used = abap_false.
        lo_protocol->add_warning(
            iv_message     = |Parameter { <ls_parameter>-name } is not used in query|
            iv_line_number = CONV #( <ls_parameter>-line_in_query )
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD insert_into_table_clause.
*.. Find select/with statement
    ASSIGN mt_stmnt[ is_main_query = abap_true ] TO FIELD-SYMBOL(<ls_select_stmnt>).
    CHECK sy-subrc = 0.

    DATA(ls_last_token) = <ls_select_stmnt>-tokens[ lines( <ls_select_stmnt>-tokens ) ].

*.. find position in query lines
    ASSIGN ct_query_lines[ ls_last_token-row ] TO FIELD-SYMBOL(<lv_query_line>).

    mv_select_query_end_offset = ls_last_token-col + strlen( ls_last_token-value ).
    mv_select_query_end_row = ls_last_token-row.
    IF mf_single_result = abap_true.
      <lv_query_line> = |{ <lv_query_line>(mv_select_query_end_offset) } INTO @DATA(result).|.
    ELSE.
      <lv_query_line> = |{ <lv_query_line>(mv_select_query_end_offset) } INTO TABLE @DATA(result).|.
    ENDIF.
  ENDMETHOD.

  METHOD check_syntax.
    DATA: lv_line    TYPE i,
          lv_word    TYPE string,
          lv_message TYPE string.

    SELECT SINGLE * FROM trdir
    INTO @DATA(dir)
    WHERE name = @sy-cprog.

    DATA(lt_query_lines) = mt_query_lines.

    insert_into_table_clause( CHANGING ct_query_lines = lt_query_lines  ).

    DATA(lt_source_code) = VALUE string_table(
      ( |REPORT ZCHECK_QUERY.| )
      ( LINES OF lt_query_lines )
    ).

    SYNTAX-CHECK FOR lt_source_code MESSAGE         lv_message
                                    LINE            lv_line
                                    WORD            lv_word
                                    DIRECTORY ENTRY dir.

    IF lv_message IS NOT INITIAL AND sy-subrc <> 0.
      zcx_dbbr_sql_query_error=>raise_with_text(
          iv_text        = lv_message
          iv_line_number = lv_line - 1
      ).
    ENDIF.

*.. Remove all lines but the select statement
    IF mv_select_stmnt_index > 1.
      DELETE mt_query_lines FROM 1 TO mv_select_stmnt_index - 1.
      mv_select_query_end_row = mv_select_query_end_row - mv_select_stmnt_index + 1.
    ENDIF.

    CONCATENATE LINES OF mt_query_lines INTO mv_executable_query SEPARATED BY cl_abap_char_utilities=>cr_lf.

  ENDMETHOD.


  METHOD determine_main_stmnt_props.
    DATA: lt_aggregate_func_range TYPE RANGE OF string,
          lv_select_index         TYPE sy-tabix.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF mt_token_raw.

    mf_single_result = abap_false.

*.. Determine the last select statement in the query
    IF line_exists( mt_token_raw[ str = c_keywords-with ] ).
      lv_select_index = lines( mt_token_raw ).
      WHILE lv_select_index > 0.
        IF mt_token_raw[ lv_select_index ]-str = c_keywords-select.
*........ If it is the last main select, the preceding token has to be a parenthesis
          IF mt_token_raw[ lv_select_index - 1 ]-str = ')'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_select_index = lv_select_index - 1.
      ENDWHILE.

    ELSE.
      lv_select_index = line_index( mt_token_raw[ str = c_keywords-select ] ).
    ENDIF.

*.. Check if INTO TABLE is possible for the query by checking if COUNT is available without
*... any active aggregation functions
    LOOP AT mt_token_raw ASSIGNING <ls_token> FROM lv_select_index WHERE str CP c_keywords-count_pattern.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      lt_aggregate_func_range = VALUE #(
        ( sign = 'I' option = 'EQ' low = 'GROUP' )
        ( sign = 'I' option = 'EQ' low = 'SUM' )
        ( sign = 'I' option = 'EQ' low = 'MAX' )
        ( sign = 'I' option = 'EQ' low = 'MIN' )
        ( sign = 'I' option = 'EQ' low = 'AVG' )
      ).
      LOOP AT mt_token_raw ASSIGNING <ls_token> FROM lv_select_index WHERE str IN lt_aggregate_func_range.
        EXIT.
      ENDLOOP.
      mf_single_result = xsdbool( sy-subrc <> 0 ).
    ENDIF.

    LOOP AT mt_token_raw ASSIGNING <ls_token> FROM lv_select_index WHERE str = c_keywords-union.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      mv_query_type = c_keywords-union.
    ELSE.
      mv_query_type = c_keywords-select.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
