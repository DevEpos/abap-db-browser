class ZCL_DBBR_FE_TEXT_DEF_TV definition
  public
  create public .

public section.

  interfaces ZIF_DBBR_TOKEN_VALIDATOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBBR_FE_TEXT_DEF_TV IMPLEMENTATION.


  method ZIF_DBBR_TOKEN_VALIDATOR~VALIDATE.

    DATA: lv_error_string TYPE string.

    " - Definition of column names for formula field
    "       $TEXT <formula field> '<short text>' ['<long text'].
    CASE cs_token-id.
      WHEN 1. " nothing to do here.
      WHEN 2.
        " has to be an identifier
        IF cs_token-type <> 'I'.
          lv_error_string = |{ text-pos } 2 { text-e02 }|.
        ENDIF.

      WHEN 3.
        IF cs_token-type <> 'S'.
          lv_error_string = |{ text-pos } 3 { text-e01 }|.
        ELSEIF NOT matches( val = cs_token-str regex = |['`].\{1,20\}['`]| ).
          " validate if the string length is correct
          lv_error_string = |{ text-pos } 3 { text-e05 }|.
        ENDIF.

      WHEN 4.
        IF cs_token-type <> 'S'.
          lv_error_string = |{ text-pos } 4 { text-e01 }|.
        ELSEIF NOT matches( val = cs_token-str regex = |['`].\{1,40\}['`]| ).
          " validate if the string length is correct
          lv_error_string = |{ text-pos } 3 { text-e06 }|.
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
          textid = ZCX_DBBR_fe_stmnt_valid_exc=>text_def_token_error
          invalid_row = cs_token-row
          msgv1  = lv_msgv1
          msgv2  = lv_msgv2
          msgv3  = lv_msgv3
          msgv4  = lv_msgv4.
    ENDIF.

  endmethod.
ENDCLASS.
