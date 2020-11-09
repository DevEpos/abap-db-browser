CLASS zcl_dbbr_fe_custom_sv DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_statement_validator .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS validate_token_count
      IMPORTING
        !is_statement TYPE zif_dbbr_fe_types=>ty_statement
      RAISING
        zcx_dbbr_fe_stmnt_valid_exc .
ENDCLASS.



CLASS zcl_dbbr_fe_custom_sv IMPLEMENTATION.


  METHOD validate_token_count.

    DATA: lv_template TYPE string,
          lf_error    TYPE abap_bool.

    CASE is_statement-first_token_str.

      WHEN zif_dbbr_c_fe_keywords=>define_field.
        IF is_statement-token_count <> 4.
          lv_template = zcl_dbbr_fe_templates=>gv_form_field_tmplt.
          lf_error = abap_true.
        ENDIF.

      WHEN zif_dbbr_c_fe_keywords=>define_unit.
        IF is_statement-token_count <> 3.
          lv_template = zcl_dbbr_fe_templates=>gv_form_unit_tmplt.
          lf_error = abap_true.
        ENDIF.

      WHEN zif_dbbr_c_fe_keywords=>define_icon.
        IF is_statement-token_count <> 2.
          lv_template = zcl_dbbr_fe_templates=>gv_icon_field_tmplt.
          lf_error = abap_true.
        ENDIF.

      WHEN zif_dbbr_c_fe_keywords=>define_icon_quick.
        IF is_statement-token_count <> 2.
          lv_template = zcl_dbbr_fe_templates=>gv_icon_tt_field_tmplt.
          lf_error = abap_true.
        ENDIF.

      WHEN zif_dbbr_c_fe_keywords=>set_icon_value.
        IF is_statement-token_count <> 4.
          lv_template = zcl_dbbr_fe_templates=>gv_set_icon_tmplt.
          lf_error = abap_true.
        ENDIF.

      WHEN zif_dbbr_c_fe_keywords=>define_description.
        IF is_statement-token_count <> 3 AND
           is_statement-token_count <> 4.
          lv_template = zcl_dbbr_fe_templates=>gv_text_for_field_tmplt.
          lf_error = abap_true.
        ENDIF.

      WHEN zif_dbbr_c_fe_keywords=>set_row_color.
        IF is_statement-token_count <> 2.
          lv_template = zcl_dbbr_fe_templates=>gv_set_row_color_template.
          lf_error = abap_true.
        ENDIF.

      WHEN zif_dbbr_c_fe_keywords=>set_cell_color.
        IF is_statement-token_count <> 3.
          lv_template = zcl_dbbr_fe_templates=>gv_set_cell_color_template.
          lf_error = abap_true.
        ENDIF.

    ENDCASE.

    IF lf_error = abap_true.
      RAISE EXCEPTION TYPE zcx_dbbr_fe_stmnt_valid_exc
        EXPORTING
          textid      = zcx_dbbr_fe_stmnt_valid_exc=>formfield_not_tmplt_conform
          invalid_row = is_statement-trow
          msgv1       = |{ is_statement-first_token_str }|
          msgv2       = |{ lv_template }|.
    ENDIF.

  ENDMETHOD.


  METHOD zif_dbbr_statement_validator~validate.


    IF NOT zcl_dbbr_fe_token_validator=>is_definition_keyword( cs_statement-first_token_str ).
      RAISE EXCEPTION TYPE zcx_dbbr_fe_stmnt_valid_exc
        EXPORTING
          textid      = zcx_dbbr_fe_stmnt_valid_exc=>unknown_keyword_found
          invalid_row = cs_statement-tokens[ 1 ]-row
          msgv1       = |{ cs_statement-first_token_str }|.
    ELSE.
      cs_statement-is_form_stmnt = abap_true.
      cs_statement-is_function_call = zcl_dbbr_fe_token_validator=>is_function_keyword( cs_statement-first_token_str ).

      IF cs_statement-is_function_call = abap_true.
        cs_statement-is_form_stmnt = abap_false.
      ENDIF.

      cs_statement-is_text_form = xsdbool( cs_statement-first_token_str = zif_dbbr_c_fe_keywords=>define_description ).
    ENDIF.

    validate_token_count( cs_statement ).

    " keyword is valid, get the correct token validator
    DATA(lr_validator) = zcl_dbbr_fe_token_validator=>get_validator( cs_statement-first_token_str ).

    LOOP AT cs_statement-tokens ASSIGNING FIELD-SYMBOL(<ls_token>) FROM 2.
      lr_validator->validate( CHANGING cs_token = <ls_token> ).
    ENDLOOP.

    cs_statement-exclude_from_subroutine = xsdbool( NOT zcl_dbbr_fe_token_validator=>is_subroutine_relevant( cs_statement-first_token_str ) ).

  ENDMETHOD.
ENDCLASS.
