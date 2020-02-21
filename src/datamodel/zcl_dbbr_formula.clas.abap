CLASS zcl_dbbr_formula DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_dbbr_fe_validator .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_formula TYPE string
        if_is_valid TYPE abap_bool DEFAULT abap_true.
    METHODS set_valid
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS add_stmnt
      IMPORTING
        is_statement TYPE zif_dbbr_fe_types=>ty_statement.
    METHODS set_stmnt
      IMPORTING
        !it_stmnt TYPE zif_dbbr_fe_types=>tt_statement .
    METHODS add_field
      IMPORTING
        !is_field TYPE zif_dbbr_fe_types=>ty_form_field
      RAISING
        zcx_dbbr_formula_exception .
    METHODS get_field
      IMPORTING
        !iv_field            TYPE fieldname
      RETURNING
        VALUE(rs_form_field) TYPE zif_dbbr_fe_types=>ty_form_field
      RAISING
        zcx_dbbr_formula_exception .
    METHODS is_formula_field
      IMPORTING
        !iv_field     TYPE fieldname
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS is_valid
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_statements
      EXPORTING
        !et_statements TYPE zif_dbbr_fe_types=>tt_statement .
    METHODS get_formula_fields
      EXPORTING
        !et_fields TYPE zif_dbbr_fe_types=>tt_form_field .
    METHODS has_executable_code
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_formula_field_count
      RETURNING
        VALUE(rv_count) TYPE sy-tabix .
    METHODS get_formula_string
      RETURNING
        VALUE(result) TYPE string .
    METHODS is_color_column_needed
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_color_column_needed
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mf_color_column_needed TYPE abap_bool.
    DATA mv_formula_string TYPE string .
    DATA mt_formula_stmnt TYPE zif_dbbr_fe_types=>tt_statement .
    DATA mt_defined_fields TYPE zif_dbbr_fe_types=>tt_form_field .
    DATA mv_executable_stmnt_count TYPE i.
    DATA mf_valid TYPE abap_bool.
    METHODS evaluate_stmnt
      IMPORTING
        is_stmnt TYPE zif_dbbr_fe_types=>ty_statement.
    METHODS update_description_of_field
      IMPORTING
        !iv_fieldname         TYPE fieldname
        !iv_short_description TYPE scrtext_m OPTIONAL
        !iv_long_description  TYPE scrtext_l OPTIONAL
      RAISING
        zcx_dbbr_formula_exception .
    METHODS update_unit_field_of_field
      IMPORTING
        iv_fieldname TYPE fieldname
        iv_unit      TYPE string
      RAISING
        zcx_dbbr_formula_exception .
ENDCLASS.



CLASS zcl_dbbr_formula IMPLEMENTATION.


  METHOD add_field.
    IF is_field-is_description = abap_true.
      update_description_of_field(
         iv_fieldname         = is_field-field
         iv_short_description = is_field-short_description
         iv_long_description  = is_field-long_description
      ).
    ELSEIF is_field-is_unit = abap_true.
      update_unit_field_of_field(
          iv_fieldname = is_field-field
          iv_unit      = is_field-unit_field
      ).
    ELSE.
      INSERT is_field INTO TABLE mt_defined_fields.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
          EXPORTING
            textid = zcx_dbbr_formula_exception=>duplicate_form_field
            msgv1  = |{ is_field-field }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mv_formula_string = iv_formula.
    mf_valid = if_is_valid.
  ENDMETHOD.


  METHOD get_field.
    TRY.
        rs_form_field = mt_defined_fields[ field = iv_field ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
          EXPORTING
            textid = zcx_dbbr_formula_exception=>form_field_not_defined
            msgv1  = |{ iv_field }|.
    ENDTRY.
  ENDMETHOD.


  METHOD get_formula_fields.
    et_fields = mt_defined_fields.
  ENDMETHOD.


  METHOD get_formula_field_count.
    rv_count = lines( mt_defined_fields ).
  ENDMETHOD.


  METHOD get_formula_string.
    result = mv_formula_string.
  ENDMETHOD.


  METHOD get_statements.
    et_statements = mt_formula_stmnt.
  ENDMETHOD.


  METHOD is_formula_field.
    result = xsdbool( line_exists( mt_defined_fields[ field = iv_field ] ) ).
  ENDMETHOD.


  METHOD set_stmnt.
    mt_formula_stmnt = it_stmnt.
  ENDMETHOD.


  METHOD update_description_of_field.
    TRY.
        DATA(lr_defined_field) = REF #( mt_defined_fields[ field = iv_fieldname ] ).
        lr_defined_field->short_description = iv_short_description.
        lr_defined_field->long_description = iv_long_description.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
          EXPORTING
            textid = zcx_dbbr_formula_exception=>form_field_not_defined
            msgv1  = |{ iv_fieldname }|.
    ENDTRY.
  ENDMETHOD.

  METHOD update_unit_field_of_field.
    TRY.
        DATA(lr_defined_field) = REF #( mt_defined_fields[ field = iv_fieldname ] ).
        lr_defined_field->unit_field = iv_unit.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_dbbr_formula_exception
          EXPORTING
            textid = zcx_dbbr_formula_exception=>form_field_not_defined
            msgv1  = |{ iv_fieldname }|.
    ENDTRY.
  ENDMETHOD.

  METHOD set_color_column_needed.
    mf_color_column_needed = value.
  ENDMETHOD.

  METHOD is_color_column_needed.
    result = mf_color_column_needed.
  ENDMETHOD.

  METHOD has_executable_code.
    result = xsdbool( mv_executable_stmnt_count > 0 ).
  ENDMETHOD.

  METHOD add_stmnt.
    mt_formula_stmnt = VALUE #( BASE mt_formula_stmnt ( is_statement ) ).

    evaluate_stmnt( is_statement ).
  ENDMETHOD.

  METHOD set_valid.
    mf_valid = value.
  ENDMETHOD.

  METHOD is_valid.
    result = mf_valid.
  ENDMETHOD.


  METHOD evaluate_stmnt.
    IF is_stmnt-is_function_call = abap_true OR
       is_stmnt-type = 'U'. " definition of formula field
      ADD 1 TO mv_executable_stmnt_count.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
