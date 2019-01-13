class ZCL_DBBR_FE_FIELD_DEF_TV definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_TOKEN_VALIDATOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_FIELD_DEF_TV IMPLEMENTATION.


  method ZIF_DBBR_TOKEN_VALIDATOR~VALIDATE.

    DATA: lv_error_string TYPE string.

    " - Definition of a calculation field:
    "       $DEF <formula field> TYPE <type name>.
    CASE cs_token-id.

      WHEN 2.
        " has to be an identifier
        IF cs_token-type <> 'I'.
          lv_error_string = |{ text-pos } 2 { text-e02 }|.
        ELSEIF strlen( cs_token-str ) > 28.
          lv_error_string = |{ text-e08 }|.
        ELSE.
          cs_token-is_formula_field = abap_true.
        ENDIF.

      WHEN 3.
        IF NOT ZCL_DBBR_fe_token_validator=>is_type_keyword( cs_token ).
          lv_error_string = |{ text-pos } 3 { text-e03 }|.
        ENDIF.

      WHEN 4.
        " use rtti to validate the given data type name
        cl_abap_typedescr=>describe_by_name(
          EXPORTING
            p_name         = cs_token-str
          RECEIVING
            p_descr_ref    = DATA(lr_type_of_form)
          EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2
        ).
        IF sy-subrc <> 0.
          lv_error_string = |{ text-pos } 4 { text-e04 }: { cs_token-str }|.
        ELSEIF NOT lr_type_of_form->is_ddic_type( ).
          lv_error_string = |{ cs_token-str } { text-e07 }|.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

    IF lv_error_string IS NOT INITIAL.
      ZCL_DBBR_appl_util=>split_string_for_message(
        EXPORTING iv_string = lv_error_string
        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                  ev_msgv2  = DATA(lv_msgv2)
                  ev_msgv3  = DATA(lv_msgv3)
                  ev_msgv4  = DATA(lv_msgv4)
      ).
      RAISE EXCEPTION TYPE ZCX_DBBR_fe_stmnt_valid_exc
        EXPORTING
          textid = ZCX_DBBR_fe_stmnt_valid_exc=>form_def_token_error
          invalid_row = cs_token-row
          msgv1  = lv_msgv1
          msgv2  = lv_msgv2
          msgv3  = lv_msgv3
          msgv4  = lv_msgv4.
    ENDIF.

  endmethod.
ENDCLASS.
