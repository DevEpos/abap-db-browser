class ZCL_DBBR_FE_SET_ROW_COLOR_TV definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_TOKEN_VALIDATOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_SET_ROW_COLOR_TV IMPLEMENTATION.


  method ZIF_DBBR_TOKEN_VALIDATOR~VALIDATE.

    DATA: lv_error_string TYPE string.

    CASE cs_token-id.

      WHEN 2.
        IF cs_token-type <> 'S'.
          lv_error_string = |{ text-pos } 2 { text-e01 }|.
        ELSEIF NOT matches( val = cs_token-str regex = |['`].\{4\}['`]| ).
          " validate if the string length is correct
          lv_error_string = |{ text-pos } 2 { text-e11 }|.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    IF lv_error_string IS NOT INITIAL.
      ZCL_SAT_MESSAGE_HELPER=>split_string_for_message(
        EXPORTING iv_string = lv_error_string
        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                  ev_msgv2  = DATA(lv_msgv2)
                  ev_msgv3  = DATA(lv_msgv3)
                  ev_msgv4  = DATA(lv_msgv4)
      ).
      RAISE EXCEPTION TYPE ZCX_DBBR_fe_stmnt_valid_exc
        EXPORTING
          textid = ZCX_DBBR_fe_stmnt_valid_exc=>wrong_keyword_syntax
          invalid_row = cs_token-row
          msgv1  = |{ zif_dbbr_c_fe_keywords=>set_row_color }|
          msgv2  = lv_msgv2
          msgv3  = lv_msgv3
          msgv4  = lv_msgv4.
    ENDIF.

  endmethod.
ENDCLASS.
