CLASS zcl_dbbr_appl_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_input_val TYPE STANDARD TABLE OF sval WITH EMPTY KEY .

    CLASS-METHODS get_program_variables
      IMPORTING
        !iv_repid           TYPE sy-repid
      RETURNING
        VALUE(rt_variables) TYPE scompotab .
    CLASS-METHODS build_selopt
      IMPORTING
        !iv_value              TYPE any
      RETURNING
        VALUE(rt_selopt_table) TYPE zdbbr_selopt_itab .
    CLASS-METHODS build_selopt_tab_from_table
      IMPORTING
        !it_table_data       TYPE STANDARD TABLE
        !iv_compname_for_low TYPE fieldname
      RETURNING
        VALUE(rt_selopt)     TYPE zdbbr_selopt_itab .
    CLASS-METHODS get_tab_line_count
      IMPORTING
        !it_criteria         TYPE zdbbr_where_criteria_itab
        !it_table            TYPE STANDARD TABLE
        !if_with_or          TYPE boolean OPTIONAL
      RETURNING
        VALUE(rv_line_count) TYPE sy-tabix .
    CLASS-METHODS split_string_for_message
      IMPORTING
        !iv_string TYPE string
      EXPORTING
        !ev_msgv1  TYPE sy-msgv1
        !ev_msgv2  TYPE sy-msgv2
        !ev_msgv3  TYPE sy-msgv3
        !ev_msgv4  TYPE sy-msgv4 .
    CLASS-METHODS get_docu_text
      IMPORTING
        !iv_class      TYPE dokhl-id DEFAULT 'DT'
        !iv_object     TYPE dokhl-object
      RETURNING
        VALUE(rv_text) TYPE string .
    CLASS-METHODS get_current_datetime
      EXPORTING
        !ev_time TYPE t
        !ev_date TYPE d .
    CLASS-METHODS print_exc_message
      IMPORTING
        !is_textid        TYPE scx_t100key
        !if_to_screen     TYPE abap_bool DEFAULT abap_true
        !ir_previous      TYPE REF TO cx_root
        !iv_display_type  TYPE syst_msgty DEFAULT 'E'
        !iv_message_type  TYPE syst_msgty DEFAULT 'E'
        !ir_exc_message   TYPE REF TO zif_dbbr_exception_message
        !iv_msgv1         TYPE sy-msgv1 OPTIONAL
        !iv_msgv2         TYPE sy-msgv2 OPTIONAL
        !iv_msgv3         TYPE sy-msgv3 OPTIONAL
        !iv_msgv4         TYPE sy-msgv4 OPTIONAL
      RETURNING
        VALUE(rv_message) TYPE string .
    CLASS-METHODS convert_string_tab_to_string
      IMPORTING
        !it_lines        TYPE string_table
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS popup_to_confirm
      IMPORTING
        !iv_title                 TYPE string DEFAULT space
        !if_suppress_query        TYPE abap_bool OPTIONAL
        !iv_query                 TYPE string
        !if_display_cancel_button TYPE boolean DEFAULT abap_true
        !iv_text_button1          TYPE char12 DEFAULT 'Yes'(001)
        !iv_text_button2          TYPE char12 DEFAULT 'No'(002)
        !iv_quickinfo_button1     TYPE text132 OPTIONAL
        !iv_quickinfo_button2     TYPE text132 OPTIONAL
        !iv_icon_type             TYPE iconname
      RETURNING
        VALUE(rv_result)          TYPE char1 .
    CLASS-METHODS popup_get_value
      IMPORTING
        is_field      TYPE sval
        !iv_title     TYPE string OPTIONAL
      EXPORTING
        ef_cancelled  TYPE abap_bool
      RETURNING
        VALUE(result) TYPE spo_value.
    CLASS-METHODS popup_get_values
      IMPORTING
        !iv_title           TYPE string OPTIONAL
      CHANGING
        ct_fields           TYPE tt_input_val
      RETURNING
        VALUE(rf_cancelled) TYPE abap_bool.
    CLASS-METHODS set_descr_lang_ref
      IMPORTING
        !ir_v_descr_language TYPE REF TO langu .
    CLASS-METHODS get_description_language
      RETURNING
        VALUE(result) TYPE langu .
    CLASS-METHODS translate_first_letter
      IMPORTING
        !iv_input  TYPE clike
      EXPORTING
        !ev_output TYPE clike .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA mr_v_description_language TYPE REF TO langu.
ENDCLASS.



CLASS zcl_dbbr_appl_util IMPLEMENTATION.


  METHOD build_selopt.
*&---------------------------------------------------------------------*
*& Author: stockbal     Date: 2017/02/08
*&---------------------------------------------------------------------*
*& Description: Creates selopt table for a single value
*&---------------------------------------------------------------------*
    CHECK iv_value IS NOT INITIAL.

    DATA(lv_option) = COND #( WHEN contains( val = iv_value sub = '*' ) THEN
                                'CP'
                              ELSE
                                'EQ' ).

    rt_selopt_table = VALUE #( ( sign = 'I' option = lv_option low = iv_value ) ).
  ENDMETHOD.


  METHOD build_selopt_tab_from_table.
    LOOP AT it_table_data ASSIGNING FIELD-SYMBOL(<ls_line>).
      ASSIGN COMPONENT iv_compname_for_low OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_low_val>).
      APPEND VALUE zdbbr_selopt(
          sign   = 'I'
          option = 'EQ'
          low    = <lv_low_val>
      ) TO rt_selopt.
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_string_tab_to_string.
    LOOP AT it_lines ASSIGNING FIELD-SYMBOL(<lv_line>).
      IF rv_string IS INITIAL.
        rv_string = <lv_line>.
      ELSE.
        rv_string = |{ rv_string }{ cl_abap_char_utilities=>cr_lf }{ <lv_line> }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_current_datetime.
    DATA lv_timestamp_long TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp_long.
    CONVERT TIME STAMP lv_timestamp_long TIME ZONE 'UTC' INTO TIME ev_time.
    CONVERT TIME STAMP lv_timestamp_long TIME ZONE 'UTC' INTO DATE ev_date.
  ENDMETHOD.


  METHOD get_description_language.
    IF mr_v_description_language IS BOUND AND mr_v_description_language->* IS NOT INITIAL.
      result = mr_v_description_language->*.
    ELSE.
      result = sy-langu.
    ENDIF.
  ENDMETHOD.


  METHOD get_docu_text.
    DATA: lt_lines TYPE STANDARD TABLE OF tline.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = iv_class
        langu             = sy-langu
        object            = iv_object
*       typ               = 'E'    " Dokutyp
*       version           = 0
*       version_active_or_last = 'L'    " Hoechste od. letzte aktive Version
*       print_param_get   = 'X'
*    IMPORTING
*       dokstate          =     " Status des Dokubausteines
*       doktitle          =
*       head              =     " SAPscript-Informationen zum Dokubaustein
*       doktyp            =
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      " convert lines to string
      LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
        IF rv_text IS INITIAL.
          rv_text = <ls_line>-tdline.
        ELSE.
          rv_text = |{ rv_text }{ cl_abap_char_utilities=>cr_lf }{ <ls_line>-tdline }|.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_program_variables.
    DATA: lt_includes TYPE STANDARD TABLE OF d010inc,
          lv_type     TYPE char1.

    FIELD-SYMBOLS: <lv_global_data_var> TYPE any.

    " 1) read global data of function group ZDBBR
    CALL FUNCTION 'RS_PROGRAM_INDEX'
      EXPORTING
        pg_name      = iv_repid
      TABLES
        compo        = rt_variables
        inc          = lt_includes
      EXCEPTIONS
        syntax_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    DELETE rt_variables WHERE name CP '<*' OR
                               type <> 'D'.
  ENDMETHOD.


  METHOD get_tab_line_count.
    DATA: lt_where TYPE STANDARD TABLE OF string.

    DATA(lv_crit_count) = lines( it_criteria ).

    " build where clause
    LOOP AT it_criteria ASSIGNING FIELD-SYMBOL(<ls_criteria>).
      APPEND |{ <ls_criteria>-crit } { <ls_criteria>-compare } { cl_abap_dyn_prg=>quote( <ls_criteria>-value ) }| TO lt_where.
      IF sy-tabix <> lv_crit_count.
        APPEND COND string( WHEN if_with_or = abap_true THEN 'OR' ELSE 'AND' ) TO lt_where.
      ENDIF.
    ENDLOOP.

    TRY.
        LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_line>) WHERE (lt_where).
          ADD 1 TO rv_line_count.
        ENDLOOP.
      CATCH  cx_sy_itab_dyn_loop.
        rv_line_count = -1.
    ENDTRY.
  ENDMETHOD.

  METHOD popup_get_value.
    DATA: lv_rcode(1),
          lt_fields TYPE TABLE OF sval.

    result = is_field-value.

    lt_fields = VALUE #( ( is_field ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = iv_title
        start_column = '12'
        start_row    = '5'
      IMPORTING
        returncode   = lv_rcode
      TABLES
        fields       = lt_fields
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0 OR lv_rcode = 'A'.
      ef_cancelled = abap_true.
    ELSE.
      result = CONV string( lt_fields[ 1 ]-value ).
    ENDIF.

  ENDMETHOD.


  METHOD popup_get_values.
    DATA: lv_rcode(1).

    CLEAR rf_cancelled.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = iv_title
        start_column = '12'
        start_row    = '5'
      IMPORTING
        returncode   = lv_rcode
      TABLES
        fields       = ct_fields
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0 OR lv_rcode = 'A'.
      rf_cancelled = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD popup_to_confirm.
    IF if_suppress_query = abap_true.
      rv_result = '1'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_query    " Fragetext im Popup
        text_button_1         = iv_text_button1
        text_button_2         = iv_text_button2
        display_cancel_button = if_display_cancel_button
        popup_type            = iv_icon_type
        iv_quickinfo_button_1 = iv_quickinfo_button1
        iv_quickinfo_button_2 = iv_quickinfo_button2
      IMPORTING
        answer                = rv_result.

  ENDMETHOD.


  METHOD print_exc_message.
    IF is_textid-msgid = 'SY' AND is_textid-msgno = 530.

      " try to print message of previous exception
      IF ir_previous IS BOUND.
        TRY.
            DATA(lr_exception_message) = CAST zif_dbbr_exception_message( ir_previous ).
            IF if_to_screen = abap_true.
              rv_message = lr_exception_message->print(
                  iv_msg_type     = iv_message_type
                  iv_display_type = iv_display_type
                  if_to_screen    = if_to_screen
              ).
            ELSE.
              rv_message = lr_exception_message->get_message( ).
            ENDIF.
          CATCH cx_sy_move_cast_error.
*.......... Return message from non db browser exception
            IF if_to_screen = abap_true.
              MESSAGE ir_previous->get_text( ) TYPE iv_message_type DISPLAY LIKE iv_display_type.
            ELSE.
              rv_message = ir_previous->get_text( ).
            ENDIF.
        ENDTRY.
      ENDIF.
    ELSE.

      IF if_to_screen = abap_true.
        MESSAGE ID is_textid-msgid
                TYPE   iv_message_type
                NUMBER is_textid-msgno
                WITH   iv_msgv1
                       iv_msgv2
                       iv_msgv3
                       iv_msgv4
                DISPLAY LIKE iv_display_type.
      ELSE.
        MESSAGE ID is_textid-msgid
                TYPE   'E'
                NUMBER is_textid-msgno
                WITH   iv_msgv1
                       iv_msgv2
                       iv_msgv3
                       iv_msgv4
                INTO rv_message.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_descr_lang_ref.
    mr_v_description_language = ir_v_descr_language.
  ENDMETHOD.


  METHOD split_string_for_message.

    DATA: lv_off TYPE i.
    DATA: lv_string TYPE string.

    lv_string = iv_string.

    ev_msgv1 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv2 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv3 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv4 = lv_string.

    IF strlen( lv_string ) > 50.
      FIND ALL OCCURRENCES OF REGEX '.\s.' IN SECTION LENGTH 47 OF ev_msgv4 MATCH OFFSET lv_off.
      IF sy-subrc = 0.
        lv_off = lv_off + 1.
        ev_msgv4 = ev_msgv4(lv_off).

        ev_msgv4 = |{ ev_msgv4 }...|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD translate_first_letter.
    DATA: lv_pos TYPE sytabix.

    ev_output = iv_input.

    " if first letter is a * take second letter
    IF ev_output(1) = '*'.
      lv_pos = 1.
    ELSE.
      lv_pos = 0.
    ENDIF.

    IF ev_output+lv_pos(1) CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ'.
      TRANSLATE ev_output+lv_pos(1) TO LOWER CASE.
    ELSEIF ev_output+lv_pos(1) CA 'abcdefghijklmnopqrstuvwxyzäöü'.
      TRANSLATE ev_output+lv_pos(1) TO UPPER CASE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
