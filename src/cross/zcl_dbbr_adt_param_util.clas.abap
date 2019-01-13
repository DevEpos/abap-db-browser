CLASS zcl_dbbr_adt_param_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_parameter,
             param_id    TYPE string,
             param_value TYPE string,
           END OF ty_parameter.
    TYPES: tt_parameter TYPE STANDARD TABLE OF ty_parameter WITH KEY param_id.

    METHODS constructor
      IMPORTING
        iv_parameters TYPE string .

    "! Retrieve parameters
    "! @parameter result |
    METHODS get_parameters
      RETURNING
        VALUE(result) TYPE tt_parameter.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_parameter_separator TYPE c VALUE ';'.       "#EC NOTEXT
    CONSTANTS c_key_value_separator TYPE c VALUE '='.       "#EC NOTEXT
    DATA mv_aie_tcode_parameters TYPE string .
    DATA mt_parameters TYPE tt_parameter.

    METHODS set_parameter
      CHANGING
        !cv_parameter_string TYPE string .
ENDCLASS.



CLASS zcl_dbbr_adt_param_util IMPLEMENTATION.

  METHOD constructor.

    mv_aie_tcode_parameters = cl_http_utility=>unescape_url( escaped = iv_parameters options = 1 ).
    SHIFT mv_aie_tcode_parameters LEFT DELETING LEADING space.
    SHIFT mv_aie_tcode_parameters RIGHT DELETING TRAILING space.

  ENDMETHOD.


  METHOD set_parameter.

    DATA: lv_rest          TYPE string, lv_token TYPE string,
          lv_param_value   TYPE c LENGTH 255,
          lv_parameter_key TYPE c LENGTH 20.

    lv_rest = substring_after( val = cv_parameter_string regex = c_parameter_separator ).
    lv_token = substring_before( val = cv_parameter_string regex = c_parameter_separator ).
    IF lv_token IS INITIAL.
      lv_token = cv_parameter_string.
    ENDIF.

    IF NOT lv_token IS INITIAL.

      lv_parameter_key = substring_before( val = lv_token regex = c_key_value_separator ).

      lv_param_value = substring_after( val = lv_token regex = c_key_value_separator ).

      mt_parameters = VALUE #(
        BASE mt_parameters
        ( param_id    = lv_parameter_key
          param_value = lv_param_value )
      ).

    ENDIF.

    cv_parameter_string = lv_rest.

  ENDMETHOD.


  METHOD get_parameters.

    DATA(lv_rest) = mv_aie_tcode_parameters.

    WHILE strlen( lv_rest ) > 0.

      set_parameter(
        CHANGING
          cv_parameter_string = lv_rest
      ).

    ENDWHILE.

    result = mt_parameters.

  ENDMETHOD.
ENDCLASS.
