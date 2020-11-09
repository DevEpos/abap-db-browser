CLASS zcl_dbbr_fe_token_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS is_type_keyword
      IMPORTING
        is_token      TYPE zif_dbbr_fe_types=>ty_token
      RETURNING
        VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_string
      IMPORTING
        is_token      TYPE zif_dbbr_fe_types=>ty_token
      RETURNING
        VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_definition_keyword
      IMPORTING
        iv_token      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
    CLASS-METHODS is_function_keyword
      IMPORTING
        iv_token      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
    CLASS-METHODS get_validator
      IMPORTING
        iv_token            TYPE string
      RETURNING
        VALUE(rr_validator) TYPE REF TO zif_dbbr_token_validator.
    CLASS-METHODS is_subroutine_relevant
      IMPORTING
        iv_token           TYPE string
      RETURNING
        VALUE(rf_relevant) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_token_validator IMPLEMENTATION.


  METHOD get_validator.
    CASE iv_token.

      WHEN zif_dbbr_c_fe_keywords=>define_description.
        rr_validator = NEW zcl_dbbr_fe_text_def_tv( ).

      WHEN zif_dbbr_c_fe_keywords=>define_icon.
        rr_validator = NEW zcl_dbbr_fe_icon_def_tv( ).

      WHEN zif_dbbr_c_fe_keywords=>define_icon_quick.
        rr_validator = NEW zcl_dbbr_fe_icon_tt_tv( ).

      WHEN zif_dbbr_c_fe_keywords=>set_icon_value.
        rr_validator = NEW zcl_dbbr_fe_set_icon_tv( ).

      WHEN zif_dbbr_c_fe_keywords=>define_unit.
        rr_validator = new zcl_dbbr_fe_unit_def_tv( ).

      WHEN zif_dbbr_c_fe_keywords=>set_row_color.
        rr_validator = NEW zcl_dbbr_fe_set_row_color_tv( ).

      WHEN zif_dbbr_c_fe_keywords=>set_cell_color.
        rr_validator = NEW zcl_dbbr_fe_set_cell_color_tv( ).

      WHEN zif_dbbr_c_fe_keywords=>define_field.
        rr_validator = NEW zcl_dbbr_fe_field_def_tv( ).

      WHEN OTHERS.
        " raise exception
    ENDCASE.
  ENDMETHOD.


  METHOD is_definition_keyword.
    CASE iv_token.
      WHEN zif_dbbr_c_fe_keywords=>define_description OR
           zif_dbbr_c_fe_keywords=>define_icon OR
           zif_dbbr_c_fe_keywords=>define_icon_quick OR
           zif_dbbr_c_fe_keywords=>define_field OR
           zif_dbbr_c_fe_keywords=>define_unit OR
           zif_dbbr_c_fe_keywords=>set_icon_value OR
           zif_dbbr_c_fe_keywords=>set_row_color OR
           zif_dbbr_c_fe_keywords=>set_cell_color.
        result = abap_true.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD is_function_keyword.
    CASE iv_token.
      WHEN zif_dbbr_c_fe_keywords=>set_icon_value OR
           zif_dbbr_c_fe_keywords=>set_row_color OR
           zif_dbbr_c_fe_keywords=>set_cell_color.
        result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD is_string.
    result = xsdbool( is_token-type = 'S' ).
  ENDMETHOD.


  METHOD is_subroutine_relevant.
    CASE iv_token.
      WHEN zif_dbbr_c_fe_keywords=>define_icon OR
           zif_dbbr_c_fe_keywords=>define_unit OR
           zif_dbbr_c_fe_keywords=>define_icon_quick OR
           zif_dbbr_c_fe_keywords=>define_field OR
           zif_dbbr_c_fe_keywords=>set_icon_value OR
           zif_dbbr_c_fe_keywords=>set_row_color OR
           zif_dbbr_c_fe_keywords=>set_cell_color.
        rf_relevant = abap_true.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD is_type_keyword.
    result = xsdbool( to_upper( is_token-str ) = zif_dbbr_c_fe_abap_keywords=>type ).
  ENDMETHOD.
ENDCLASS.
