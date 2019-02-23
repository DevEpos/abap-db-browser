*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF lty_s_field,
    value TYPE string,
    alias TYPE string,
  END OF lty_s_field.
TYPES: lty_t_field TYPE STANDARD TABLE OF lty_s_field WITH EMPTY KEY.

TYPES:
  BEGIN OF lty_s_select_part,
    fields TYPE lty_t_field,
  END OF lty_s_select_part.
TYPES:
  BEGIN OF lty_s_where_comp,
    value  TYPE string,
    and_or TYPE string,
  END OF lty_s_where_comp.
TYPES: lty_t_where_comp TYPE STANDARD TABLE OF lty_s_where_comp WITH EMPTY KEY.

TYPES:
  BEGIN OF lty_s_where_part,
    components TYPE lty_t_where_comp,
  END OF lty_s_where_part.

TYPES:
  BEGIN OF lty_s_query,
    select_part TYPE lty_s_select_part,
    where_part  TYPE lty_s_where_part,
  END OF lty_s_query.


INTERFACE lif_statement_parser.
  METHODS parse
    RETURNING
      VALUE(rr_data) TYPE REF TO data.
ENDINTERFACE.

CLASS lcl_token_parser DEFINITION
 ABSTRACT.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_token TYPE zcl_dbbr_sql_query_parser=>ty_t_token.

  PROTECTED SECTION.
    DATA mt_token TYPE zcl_dbbr_sql_query_parser=>ty_t_token.
    DATA mv_count TYPE i.
    DATA mv_current_index TYPE i.
    DATA ms_current_token TYPE zcl_dbbr_sql_query_parser=>ty_s_token.
    "! <p class="shorttext synchronized" lang="en">Navigate to next token in the list</p>
    "!
    METHODS next_token.
    "! <p class="shorttext synchronized" lang="en">Delete the current token</p>
    "!
    METHODS delete_current.
    "! <p class="shorttext synchronized" lang="en">Set index to first token in the list</p>
    "!
    METHODS set_index_to_first.
    "! <p class="shorttext synchronized" lang="en">Navigate to the previous token in the list</p>
    "!
    METHODS previous_token.
    "! <p class="shorttext synchronized" lang="en">Retrieve the next token for the given value</p>
    "!
    METHODS get_token
      IMPORTING
        iv_token         TYPE string
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Checks if there is another token after the current one</p>
    "!
    METHODS has_next_token
      RETURNING
        VALUE(rf_has_next) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Checks if there is another token before the current one</p>
    "!
    METHODS has_previous_token
      RETURNING
        VALUE(rf_has_previous) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Check if the next token has the given value</p>
    "!
    METHODS is_next_token
      IMPORTING
        iv_next_token     TYPE string
      RETURNING
        VALUE(rf_is_next) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Check if the previous token has the given value</p>
    "!
    METHODS is_previous_token
      IMPORTING
        iv_previous_token     TYPE string
      RETURNING
        VALUE(rf_is_previous) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Updates the current token from the current working structure</p>
    "!
    METHODS update_from_current.
    "! <p class="shorttext synchronized" lang="en">Deletes the next token in the list</p>
    "!
    METHODS delete_next.
    "! <p class="shorttext synchronized" lang="en">Delete the previous token in the list</p>
    "!
    METHODS delete_previous.

    "! <p class="shorttext synchronized" lang="en">Checks if the token matches a token in a comma separated token list</p>
    "!
    METHODS token_matches
      IMPORTING
        iv_check_list     TYPE string
        iv_token_to_check TYPE string
      RETURNING
        VALUE(rf_matches) TYPE abap_bool.
ENDCLASS.

CLASS lcl_query_token_simplifier DEFINITION
INHERITING FROM lcl_token_parser.

  PUBLIC SECTION.
    METHODS simplify
      RETURNING
        VALUE(rt_tokens) TYPE zcl_dbbr_sql_query_parser=>ty_t_token.
  PROTECTED SECTION.


  PRIVATE SECTION.


    METHODS simplify_by_clause
      IMPORTING
        iv_clause     TYPE string
        iv_simplified TYPE string.
    METHODS simplify_joins.
    METHODS simplify_conditions.
ENDCLASS.

CLASS lcl_query_param_parser DEFINITION
 INHERITING FROM lcl_token_parser.

  PUBLIC SECTION.
    INTERFACES lif_statement_parser.
    ALIASES parse
      FOR lif_statement_parser~parse.

  PRIVATE SECTION.
    DATA mr_parameter TYPE REF TO zcl_dbbr_sql_query_parser=>ty_s_parameter.
    METHODS parse_type.
    METHODS parse_length.
    METHODS parse_value.
    METHODS parse_decimals.
    METHODS parse_name.

ENDCLASS.
