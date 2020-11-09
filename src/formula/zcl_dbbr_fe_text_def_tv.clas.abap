"! <p class="shorttext synchronized" lang="en">Validates tokens of formula description</p>
CLASS zcl_dbbr_fe_text_def_tv DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_token_validator .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_text_def_tv IMPLEMENTATION.


  METHOD zif_dbbr_token_validator~validate.

    DATA: lv_error_string TYPE string.

    " - Definition of column names for formula field
    "       $TEXT <formula field> '<short text>' ['<long text'].
    CASE cs_token-id.
      WHEN 1. " nothing to do here.
      WHEN 2.
        " has to be an identifier
        IF cs_token-type <> 'I'.
          lv_error_string = |{ TEXT-pos } 2 { TEXT-e02 }|.
        ENDIF.

      WHEN 3.
        IF cs_token-type <> 'S'.
          lv_error_string = |{ TEXT-pos } 3 { TEXT-e01 }|.
        ELSEIF NOT matches( val = cs_token-str regex = |['`].\{1,20\}['`]| ).
          " validate if the string length is correct
          lv_error_string = |{ TEXT-pos } 3 { TEXT-e05 }|.
        ENDIF.

      WHEN 4.
        IF cs_token-type <> 'S'.
          lv_error_string = |{ TEXT-pos } 4 { TEXT-e01 }|.
        ELSEIF NOT matches( val = cs_token-str regex = |['`].\{1,40\}['`]| ).
          " validate if the string length is correct
          lv_error_string = |{ TEXT-pos } 3 { TEXT-e06 }|.
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
          textid      = zcx_dbbr_fe_stmnt_valid_exc=>text_def_token_error
          invalid_row = cs_token-row
          msgv1       = lv_msgv1
          msgv2       = lv_msgv2
          msgv3       = lv_msgv3
          msgv4       = lv_msgv4.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
