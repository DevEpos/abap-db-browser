"! <p class="shorttext synchronized" lang="en">Validates tokens of SET_ICON call</p>
CLASS zcl_dbbr_fe_set_icon_tv DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_token_validator .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_set_icon_tv IMPLEMENTATION.

  METHOD zif_dbbr_token_validator~validate.

    " $SET_ICON_TT  IKONE   ICON_SPACE         space.
    DATA: lv_error_string TYPE string.

    CASE cs_token-id.

      WHEN 2.
        " has to be an identifier
        IF cs_token-type <> 'I'.
          lv_error_string = |{ TEXT-pos } 2 { TEXT-e02 }|.
        ELSEIF strlen( cs_token-str ) > 28.
          lv_error_string = |{ TEXT-e08 }|.
        ELSE.
          cs_token-is_formula_field = abap_true.
        ENDIF.

      WHEN 3.
        IF cs_token-type <> 'I'.
          lv_error_string = |{ TEXT-pos } 3 { TEXT-e02 }|.
        ELSE.
          " validate the given token
          zcl_dbbr_icon_handler=>create_icon( iv_icon_name = CONV #( cs_token-str ) ).
        ENDIF.

      WHEN 4.
        IF cs_token-str = 'SPACE'.
          RETURN.
        ENDIF.

        IF cs_token-type <> 'S'.
          lv_error_string = |{ TEXT-pos } 4 { TEXT-e09 }|.
        ELSEIF NOT matches( val = cs_token-str regex = |['`].\{0,30\}['`]| ).
          " validate if the string length is correct
          lv_error_string = |{ TEXT-pos } 4 { TEXT-e10 }|.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    IF lv_error_string IS NOT INITIAL.
      zcl_sat_message_helper=>split_string_for_message(
        EXPORTING iv_string = lv_error_string
        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                  ev_msgv2  = DATA(lv_msgv2)
                  ev_msgv3  = DATA(lv_msgv3)
      ).
      RAISE EXCEPTION TYPE zcx_dbbr_fe_stmnt_valid_exc
        EXPORTING
          textid      = zcx_dbbr_fe_stmnt_valid_exc=>wrong_keyword_syntax
          invalid_row = cs_token-row
          msgv1       = |{ '$SET_ICON' }|
          msgv2       = lv_msgv1
          msgv3       = lv_msgv2
          msgv4       = lv_msgv3.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
