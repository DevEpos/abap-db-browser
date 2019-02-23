"! <p class="shorttext synchronized" lang="en">Query For object search</p>
CLASS zcl_dbbr_object_search_query DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_dbbr_c_object_browser.
    INTERFACES zif_dbbr_ty_object_browser.
    ALIASES:
      c_search_option FOR zif_dbbr_c_object_browser~c_search_option,
      ty_search_option_values FOR zif_dbbr_ty_object_browser~ty_search_option_values.

    DATA mv_search_string TYPE string READ-ONLY.
    DATA mv_search_option TYPE ddoption READ-ONLY.
    DATA mv_type TYPE zdbbr_obj_browser_mode READ-ONLY.
    DATA mv_query TYPE string READ-ONLY.
    DATA mv_max_rows TYPE sy-tabix READ-ONLY.
    DATA mt_search_options TYPE zif_dbbr_ty_object_browser=>tt_search_option_values READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">Parses the given search string</p>
    "! The given string is processed to extract additional search parameters
    "!
    CLASS-METHODS parse_query_string
      IMPORTING
        iv_query        TYPE string
        iv_search_type  TYPE zdbbr_obj_browser_mode
      RETURNING
        VALUE(rr_query) TYPE REF TO zcl_dbbr_object_search_query
      RAISING
        zcx_dbbr_object_search.

    "! <p class="shorttext synchronized" lang="en">CLASS CONSTRUCTOR</p>
    "!
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized" lang="en">Has the query options?</p>
    "!
    METHODS has_options
      RETURNING
        VALUE(result) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Has the query a search string</p>
    "!
    METHODS has_search_string
      RETURNING
        VALUE(result) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Retrieves the options of the query</p>
    "!
    METHODS get_options
      RETURNING
        VALUE(rt_options) TYPE zif_dbbr_ty_object_browser=>tt_search_option_values.
    "! <p class="shorttext synchronized" lang="en">Add search option to query</p>
    "!
    METHODS set_option
      IMPORTING
        is_option TYPE zif_dbbr_ty_object_browser=>ty_search_option_values.
    "! <p class="shorttext synchronized" lang="en">Get a specific search option</p>
    "!
    METHODS get_option
      IMPORTING
        iv_option        TYPE string
      RETURNING
        VALUE(rs_option) TYPE zif_dbbr_ty_object_browser=>ty_search_option_values.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_allowed_option_by_type,
             type   TYPE char1,
             option TYPE string,
           END OF ty_allowed_option_by_type.
    TYPES: BEGIN OF ty_option_setting,
             option      TYPE string,
             single      TYPE abap_bool,
             key_value   TYPE abap_bool,
             no_negation TYPE abap_bool,
           END OF ty_option_setting.
    CLASS-DATA gt_search_option_map TYPE STANDARD TABLE OF ty_allowed_option_by_type.
    CLASS-DATA gt_search_option_setting TYPE STANDARD TABLE OF ty_option_setting.
    TYPES:
      ty_lt_allowed_options TYPE RANGE OF string.
    CONSTANTS c_option_separator TYPE string VALUE ':' ##NO_TEXT.
    CONSTANTS c_value_separator TYPE string VALUE ',' ##NO_TEXT.
    CONSTANTS c_negation_operator TYPE string VALUE '!' ##no_text.
    CONSTANTS c_negation_operator2 TYPE string VALUE '<>' ##no_text.
    CONSTANTS c_key_value_pair_separator TYPE string VALUE '=' ##no_text.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        iv_query          TYPE string
        iv_type           TYPE zdbbr_obj_browser_mode
        iv_search_string  TYPE string
        it_search_options TYPE zif_dbbr_ty_object_browser=>tt_search_option_values.
    "! <p class="shorttext synchronized" lang="en">Validate option value</p>
    "!
    CLASS-METHODS validate_option_value
      IMPORTING
        iv_option      TYPE string
        iv_search_type TYPE zdbbr_obj_browser_mode
        iv_value       TYPE string
      RAISING
        zcx_dbbr_object_search.
    "! <p class="shorttext synchronized" lang="en">Converts option value for correct selection</p>
    "!
    CLASS-METHODS convert_option_value
      IMPORTING
        iv_option TYPE string
      CHANGING
        cv_value  TYPE string.
    "! <p class="shorttext synchronized" lang="en">Extract search option from token</p>
    "! Splits token at character ':' and collects the option and its value
    "! if it exists in the list of allowed search options
    "!
    CLASS-METHODS extract_option
      IMPORTING
        it_allowed_options       TYPE ty_lt_allowed_options
        iv_search_type           TYPE zdbbr_obj_browser_mode
        iv_token                 TYPE string
      CHANGING
        VALUE(cs_search_options) TYPE zif_dbbr_ty_object_browser=>ty_search
      RAISING
        zcx_dbbr_object_search.
    "! <p class="shorttext synchronized" lang="en">Adds value for option</p>
    "!
    CLASS-METHODS add_option_value
      IMPORTING
        is_option  TYPE ty_option_setting
        iv_value   TYPE string
      CHANGING
        ct_options TYPE zif_dbbr_ty_object_browser=>tt_search_option_values.
ENDCLASS.



CLASS zcl_dbbr_object_search_query IMPLEMENTATION.

  METHOD class_constructor.
*.. Settings for search options
    gt_search_option_setting = VALUE #(
      ( option = c_search_option-by_owner )
      ( option = c_search_option-by_api )
      ( option = c_search_option-by_params single = abap_true no_negation = abap_true )
      ( option = c_search_option-by_select_from )
      ( option = c_search_option-by_association )
      ( option = c_search_option-by_anno key_value = abap_true )
      ( option = c_search_option-by_field )
      ( option = c_search_option-by_type )
      ( option = c_search_option-by_package )
      ( option = c_search_option-by_description )
      ( option = c_search_option-by_table )
      ( option = c_search_option-max_rows single = abap_true no_negation = abap_true )
    ).

*.. Create mapping between search type and allowed search options
    gt_search_option_map = VALUE #(
*.... CDS View options)
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_owner       )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_api         )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_params      )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_select_from )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_association )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_anno        )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_field       )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_type        )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_package     )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-by_description )
      ( type = zif_dbbr_c_object_browser_mode=>cds_view option = c_search_option-max_rows       )
*.... Database table/view options
      ( type = zif_dbbr_c_object_browser_mode=>database_table_view option = c_search_option-by_owner       )
      ( type = zif_dbbr_c_object_browser_mode=>database_table_view option = c_search_option-by_package     )
      ( type = zif_dbbr_c_object_browser_mode=>database_table_view option = c_search_option-by_type        )
      ( type = zif_dbbr_c_object_browser_mode=>database_table_view option = c_search_option-by_field       )
      ( type = zif_dbbr_c_object_browser_mode=>database_table_view option = c_search_option-by_description )
      ( type = zif_dbbr_c_object_browser_mode=>database_table_view option = c_search_option-max_rows       )
*.... Package options
      ( type = zif_dbbr_c_object_browser_mode=>package option = c_search_option-by_owner       )
      ( type = zif_dbbr_c_object_browser_mode=>package option = c_search_option-by_description )
      ( type = zif_dbbr_c_object_browser_mode=>package option = c_search_option-max_rows       )
*.... Query options
      ( type = zif_dbbr_c_object_browser_mode=>query option = c_search_option-by_owner )
      ( type = zif_dbbr_c_object_browser_mode=>query option = c_search_option-by_select_from )
      ( type = zif_dbbr_c_object_browser_mode=>query option = c_search_option-by_description )
      ( type = zif_dbbr_c_object_browser_mode=>query option = c_search_option-max_rows )
    ).
  ENDMETHOD.

  METHOD parse_query_string.
    DATA: lt_query_tokens       TYPE TABLE OF string,
          ls_query              TYPE zif_dbbr_ty_object_browser=>ty_search,
          lt_allowed_options    TYPE RANGE OF string,
          lv_option             TYPE string,
          lv_value              TYPE string,
          lf_query_string_found TYPE abap_bool.

    FIELD-SYMBOLS: <ls_option_value> TYPE ty_search_option_values.

    IF iv_query IS INITIAL.
      RAISE EXCEPTION TYPE zcx_dbbr_object_search
        EXPORTING
          textid = zcx_dbbr_object_search=>no_query_string.
    ENDIF.

*.. Get all possible options for the given search type
    lt_allowed_options = VALUE #(
        FOR option IN gt_search_option_map
        WHERE ( type = iv_search_type )
        ( sign = 'I' option = 'EQ' low = option-option )
    ).

    SPLIT iv_query AT space INTO TABLE lt_query_tokens.

    LOOP AT lt_query_tokens ASSIGNING FIELD-SYMBOL(<lv_token>).
      CLEAR: lv_option, lv_value.

      IF <lv_token> CS c_option_separator.
        extract_option(
          EXPORTING
            it_allowed_options = lt_allowed_options
            iv_search_type     = iv_search_type
            iv_token           = <lv_token>
          CHANGING
            cs_search_options = ls_query
        ).
      ELSE.
        IF lf_query_string_found = abap_true.
          RAISE EXCEPTION TYPE zcx_dbbr_object_search
            EXPORTING
              textid = zcx_dbbr_object_search=>invalid_query_parameter
              msgv1  = |{ <lv_token> }|.

        ENDIF.

        ls_query-query = <lv_token>.
        lf_query_string_found = abap_true.
      ENDIF.
    ENDLOOP.

    rr_query = NEW #(
      iv_search_string  = ls_query-query
      iv_type           = iv_search_type
      iv_query          = iv_query
      it_search_options = ls_query-options
    ).

  ENDMETHOD.

  METHOD constructor.
    mv_query = iv_query.
    mv_type = iv_type.
    mv_search_string = to_upper( iv_search_string ).
    IF iv_search_string IS NOT INITIAL.
      mv_search_option = COND #( WHEN iv_search_string CS '*' THEN 'CP' ELSE 'EQ' ).
    ENDIF.
    mt_search_options = it_search_options.

    mv_max_rows = 50.
    IF mt_search_options IS NOT INITIAL.
      mv_max_rows = VALUE #( mt_search_options[ option = zif_dbbr_c_object_browser=>c_search_option-max_rows ]-value_range[ 1 ]-low DEFAULT 50 ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_option_value.
    IF sy-saprl >= 751. " upper() function in CDS view
      TRANSLATE cv_value TO UPPER CASE.
    ELSE.
      CASE iv_option.

        WHEN zif_dbbr_c_object_browser=>c_search_option-by_description OR
             zif_dbbr_c_object_browser=>c_search_option-by_anno.

        WHEN OTHERS.
          TRANSLATE cv_value TO UPPER CASE.
      ENDCASE.
    ENDIF.

    IF iv_option = zif_dbbr_c_object_browser=>c_search_option-by_owner.
      IF cv_value = 'SY-UNAME' OR cv_value = 'ME'.
        cv_value = sy-uname.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD add_option_value.
    DATA: lv_value2 TYPE string.

    ASSIGN ct_options[ option = is_option-option ] TO FIELD-SYMBOL(<ls_option>).
    IF sy-subrc <> 0.
      INSERT VALUE #(
        option = is_option-option
      ) INTO TABLE ct_options ASSIGNING <ls_option>.
    ENDIF.

    DATA(lv_value) = iv_value.
    DATA(lv_sign) = 'I'.

    IF is_option-no_negation = abap_false.
      IF lv_value CP '!*'.
        lv_value = lv_value+1.
        lv_sign = 'E'.
      ENDIF.

      IF lv_value CP '<>*'.
        lv_value = lv_value+2.
        lv_sign = 'E'.
      ENDIF.
    ENDIF.

*.. Consider key-value options in a special way
    IF is_option-key_value = abap_true.
      IF lv_value CS c_key_value_pair_separator.
        SPLIT lv_value AT c_key_value_pair_separator INTO lv_value lv_value2.
        TRANSLATE lv_value TO UPPER CASE.
        IF sy-saprl >= 751. " upper() function available!
          TRANSLATE lv_value2 TO UPPER CASE.
        ENDIF.
      ELSE.
        TRANSLATE lv_value TO UPPER CASE.
      ENDIF.
    ENDIF.

    <ls_option>-value_range = VALUE #(
        BASE <ls_option>-value_range
        ( sign = lv_sign  option = COND #( WHEN iv_value CS '*' THEN 'CP' ELSE 'EQ ' ) low = lv_value high = lv_value2 )
    ).
  ENDMETHOD.


  METHOD extract_option.

    DATA: lv_option     TYPE string,
          lv_value_list TYPE string,
          lt_values     TYPE zdbbr_string_t.

    SPLIT iv_token AT c_option_separator INTO lv_option lv_value_list.
    TRANSLATE lv_option TO UPPER CASE.
    IF NOT line_exists( gt_search_option_map[ type = iv_search_type option = lv_option ] ).
      RAISE EXCEPTION TYPE zcx_dbbr_object_search
        EXPORTING
          textid = zcx_dbbr_object_search=>invalid_query_option
          msgv1  = |{ lv_option }|.
    ENDIF.
    ASSIGN gt_search_option_setting[ option = lv_option ] TO FIELD-SYMBOL(<ls_option_info>).

*.. Get all included values for this option
    IF lv_value_list CS c_value_separator.
      IF <ls_option_info>-single = abap_true.
        RAISE EXCEPTION TYPE zcx_dbbr_object_search
          EXPORTING
            textid = zcx_dbbr_object_search=>no_intervals_for_option
            msgv1  = |{ SWITCH #(
                          lv_option
                          WHEN zif_dbbr_c_object_browser=>c_search_option-max_rows THEN
                            |{ lv_option }({ 'Max Rows'(001) })|
                          ELSE lv_option
                        ) }|.
      ENDIF.

      SPLIT lv_value_list AT c_value_separator INTO TABLE lt_values.
      LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<lv_value>).
        convert_option_value( EXPORTING iv_option = lv_option CHANGING cv_value = <lv_value> ).
        validate_option_value( iv_option = lv_option iv_search_type = iv_search_type iv_value = <lv_value> ).
        add_option_value(
          EXPORTING is_option  = <ls_option_info>
                    iv_value   = <lv_value>
          CHANGING  ct_options = cs_search_options-options
        ).
      ENDLOOP.
    ELSE.
*.... Only a single value is included in the query
      convert_option_value( EXPORTING iv_option = lv_option CHANGING cv_value = lv_value_list ).
      validate_option_value( iv_option = lv_option iv_search_type = iv_search_type iv_value = lv_value_list ).
      add_option_value(
        EXPORTING is_option  = <ls_option_info>
                  iv_value   = lv_value_list
        CHANGING  ct_options = cs_search_options-options
      ).
    ENDIF.

*... delete duplicate entries
    LOOP AT cs_search_options-options ASSIGNING FIELD-SYMBOL(<ls_option>).
      SORT <ls_option>-value_range BY sign option low high.
      DELETE ADJACENT DUPLICATES FROM <ls_option>-value_range COMPARING sign option low high.
    ENDLOOP.

  ENDMETHOD.

  METHOD validate_option_value.
    cl_query_option_validator=>create_validator( iv_search_type )->validate( iv_option = iv_option iv_value = iv_value ).
  ENDMETHOD.

  METHOD get_option.
    rs_option = VALUE #( mt_search_options[ option = iv_option ] OPTIONAL ).
  ENDMETHOD.

  METHOD set_option.
    ASSIGN mt_search_options[ option = is_option-option ] TO FIELD-SYMBOL(<ls_option>).
    IF sy-subrc = 0.
      <ls_option>-value_range = is_option-value_range.
    ELSE.
      mt_search_options = VALUE #( BASE mt_search_options ( is_option ) ).
    ENDIF.

    IF is_option-option = zif_dbbr_c_object_browser=>c_search_option-max_rows.
      mv_max_rows = VALUE #( mt_search_options[ option = zif_dbbr_c_object_browser=>c_search_option-max_rows ]-value_range[ 1 ]-low DEFAULT 50 ).
    ENDIF.

  ENDMETHOD.

  METHOD get_options.
    rt_options = mt_search_options.
  ENDMETHOD.

  METHOD has_options.
    result = xsdbool( mt_search_options IS NOT INITIAL ).
  ENDMETHOD.

  METHOD has_search_string.
    result = xsdbool( mv_search_string IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.
