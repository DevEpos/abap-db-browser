"! <p class="shorttext synchronized" lang="en">Validates tokens of unit definition</p>
CLASS zcl_dbbr_fe_unit_def_tv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_token_validator .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_unit_def_tv IMPLEMENTATION.

  METHOD zif_dbbr_token_validator~validate.

    DATA: lv_error_string TYPE string.

    " - Assignment of Unit/Currency field to formula field
    "       $UNIT <formula field> <column name>.
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
        IF cs_token-type <> 'I'.
          lv_error_string = |{ TEXT-pos } 3 { TEXT-e02 }|.
        ELSEIF cs_token-str NP 'ROW-*'.
          lv_error_string = |{ TEXT-pos } 3 { text-e03 }|.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

    IF lv_error_string IS NOT INITIAL.
      zcl_sat_message_helper=>split_string_for_message(
        EXPORTING iv_string = lv_error_string
        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                  ev_msgv2  = DATA(lv_msgv2)
                  ev_msgv3  = DATA(lv_msgv3)
                  ev_msgv4  = DATA(lv_msgv4)
      ).
      RAISE EXCEPTION TYPE zcx_dbbr_fe_stmnt_valid_exc
        EXPORTING
          textid      = zcx_dbbr_fe_stmnt_valid_exc=>form_def_token_error
          invalid_row = cs_token-row
          msgv1       = lv_msgv1
          msgv2       = lv_msgv2
          msgv3       = lv_msgv3
          msgv4       = lv_msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
