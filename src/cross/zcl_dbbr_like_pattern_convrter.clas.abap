"! <p class="shorttext synchronized" lang="en">Converter for LIKE pattern SQL &lt;-&gt; SAP</p>
CLASS zcl_dbbr_like_pattern_convrter DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPE-POOLS abap.
    TYPES escape_char TYPE c LENGTH 1.

    "! Convert escape characters from SAP to SQL
    CLASS-METHODS conv_sap_to_sql_pattern
      IMPORTING
        iv_sap_pattern   TYPE clike
      EXPORTING
        ev_sql_pattern   TYPE string
        ef_escape_needed TYPE abap_bool
      EXCEPTIONS
        closing_escape.

    CLASS-METHODS class_constructor.

    "! Convert escape characters from SQL to SAP
    CLASS-METHODS conv_sql_to_sap_pattern
      IMPORTING
        iv_sql_pattern   TYPE clike
        iv_escape        TYPE escape_char
      EXPORTING
        ev_sap_pattern   TYPE string
        ef_escape_needed TYPE abap_bool
      EXCEPTIONS
        closing_escape.
  PRIVATE SECTION.
    CONSTANTS:
      c_sap_any_single   TYPE c  VALUE '+',
      c_sap_any_sequence TYPE c  VALUE '*',

      c_sap_escape       TYPE c  VALUE '#',

      c_sql_any_single   TYPE c  VALUE '_',
      c_sql_any_sequence TYPE c  VALUE '%'.

    CONSTANTS c_string_space TYPE string VALUE ` `.
    CLASS-DATA gf_is_nuc TYPE abap_bool VALUE abap_true.
ENDCLASS.



CLASS zcl_dbbr_like_pattern_convrter IMPLEMENTATION.

  METHOD class_constructor.
    IF cl_abap_char_utilities=>charsize > 1.
      gf_is_nuc = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD conv_sap_to_sql_pattern.
    DATA: lv_last_pos     TYPE i,
          lv_input_maxpos TYPE i,
          lv_curr_pos     TYPE i VALUE 0,
          lv_continue_off TYPE i,
          lv_charlen      TYPE i.

    CLEAR ev_sql_pattern.

    lv_input_maxpos = strlen( iv_sap_pattern ).
    SUBTRACT 1 FROM lv_input_maxpos.

    lv_last_pos = strlen( iv_sap_pattern ) - 1.

    WHILE lv_curr_pos <= lv_last_pos.
      lv_continue_off = 1.
      lv_charlen = charlen( iv_sap_pattern+lv_curr_pos ).

      IF gf_is_nuc = abap_true AND lv_charlen > 1.
*...... multibyte character copy charlen bytes
        CONCATENATE ev_sql_pattern iv_sap_pattern+lv_curr_pos(lv_charlen) INTO ev_sql_pattern.
        ADD 1 TO lv_continue_off.
        ADD lv_continue_off TO lv_curr_pos.
        CONTINUE.
      ENDIF.

      CASE iv_sap_pattern+lv_curr_pos(1).

        WHEN c_sap_any_single.
          CONCATENATE ev_sql_pattern c_sql_any_single INTO ev_sql_pattern.

        WHEN c_sap_any_sequence.
          CONCATENATE ev_sql_pattern c_sql_any_sequence INTO ev_sql_pattern.

        WHEN c_sap_escape.
          DATA next_char TYPE c.
          DATA next_char_pos TYPE i.
          DATA next_char_len TYPE i.

          next_char_pos = lv_curr_pos + 1.
          next_char_len = charlen( iv_sap_pattern+next_char_pos ).
          IF gf_is_nuc = abap_true AND next_char_len > 1.
*.......... next character is multibyte => copy charlen bytes
            CONCATENATE ev_sql_pattern iv_sap_pattern+next_char_pos(next_char_len) INTO ev_sql_pattern.
            lv_curr_pos = lv_curr_pos + 1 + next_char_len. " escape and next multibyte char processed
            CONTINUE.
          ENDIF.

          IF lv_curr_pos < lv_last_pos.
*.......... character after escape is single byte
            next_char_pos = lv_curr_pos + 1.
            ADD 1 TO lv_continue_off.
*.......... Character after escape is sqlMeta or sapEscape
            next_char = iv_sap_pattern+next_char_pos(1).
            IF  next_char = c_sql_any_single OR
                next_char = c_sql_any_sequence OR
                next_char = c_sap_escape.
              ef_escape_needed = abap_true.
              CONCATENATE ev_sql_pattern c_sap_escape INTO ev_sql_pattern.
            ENDIF.
            CONCATENATE ev_sql_pattern next_char INTO ev_sql_pattern.
          ELSE.
            IF lv_last_pos < lv_input_maxpos.
              CONCATENATE ev_sql_pattern c_string_space INTO ev_sql_pattern.
            ELSE.
              RAISE closing_escape.
            ENDIF.
          ENDIF.

        WHEN c_sql_any_single OR
             c_sql_any_sequence.
          ef_escape_needed = abap_true.
          CONCATENATE ev_sql_pattern c_sap_escape iv_sap_pattern+lv_curr_pos(1) INTO ev_sql_pattern.

        WHEN OTHERS.
          CONCATENATE ev_sql_pattern iv_sap_pattern+lv_curr_pos(1) INTO ev_sql_pattern RESPECTING BLANKS.
      ENDCASE.
*
      lv_curr_pos = lv_curr_pos + lv_continue_off.
    ENDWHILE.

  ENDMETHOD.                    "sap_to_sql_patte_uc

  METHOD conv_sql_to_sap_pattern.
    DATA: curr_pos         TYPE i VALUE 0,
          lv_last_pos      TYPE i,
          lv_input_maxpos  TYPE i,
          lv_charlen       TYPE i,
          lv_continue_off  TYPE i,

          lv_next_char     TYPE c,
          lv_next_char_pos TYPE i,
          lv_next_char_len TYPE i.

    lv_input_maxpos = strlen( iv_sql_pattern ).
    SUBTRACT 1 FROM lv_input_maxpos.
    lv_last_pos = strlen( iv_sql_pattern ) - 1.

    WHILE curr_pos <= lv_last_pos.
      lv_continue_off = 1.

      lv_charlen = charlen( iv_sql_pattern ).

      IF gf_is_nuc = abap_true AND lv_charlen > 1.
        CONCATENATE ev_sap_pattern iv_sql_pattern+curr_pos(lv_charlen) INTO ev_sap_pattern.
        ADD 1 TO lv_continue_off.
        ADD lv_continue_off TO curr_pos.
        CONTINUE.
      ENDIF.

      CASE iv_sql_pattern+curr_pos(1).

        WHEN c_sql_any_single.
          CONCATENATE ev_sap_pattern c_sap_any_single INTO ev_sap_pattern.

        WHEN c_sql_any_sequence.
          CONCATENATE ev_sap_pattern c_sap_any_sequence INTO ev_sap_pattern.

        WHEN iv_escape.


          lv_next_char_pos = curr_pos + 1.
          lv_next_char_len = charlen( iv_sql_pattern+lv_next_char_pos ).
          IF gf_is_nuc = abap_true AND lv_next_char_len > 1.
*.......... next character is multibyte => copy charlen bytes
            CONCATENATE ev_sap_pattern iv_sql_pattern+lv_next_char_pos(lv_next_char_len) INTO ev_sap_pattern.
            curr_pos = curr_pos + 1 + lv_next_char_len. " escape and next multibyte char processed
            CONTINUE.
          ENDIF.
          IF curr_pos < lv_last_pos.
*.......... character after escape is single byte
            lv_next_char_pos = curr_pos + 1.
            ADD 1 TO lv_continue_off.
*.......... Character after escape is sqlMeta or sapEscape
            lv_next_char = iv_sql_pattern+lv_next_char_pos(1).
            IF  lv_next_char = c_sap_any_single OR
                lv_next_char = c_sap_any_sequence OR
                lv_next_char = iv_escape.
              ef_escape_needed = abap_true.
              CONCATENATE ev_sap_pattern c_sap_escape INTO ev_sap_pattern.
            ENDIF.
            CONCATENATE ev_sap_pattern lv_next_char INTO ev_sap_pattern.
          ELSE.
            IF lv_last_pos < lv_input_maxpos.
              CONCATENATE ev_sap_pattern c_string_space INTO ev_sap_pattern.
            ELSE.
              RAISE closing_escape.
            ENDIF.
          ENDIF.

        WHEN c_sap_any_single OR
             c_sap_any_sequence.
          ef_escape_needed = abap_true.
          CONCATENATE ev_sap_pattern c_sap_escape iv_sql_pattern+curr_pos(1) INTO ev_sap_pattern.

        WHEN OTHERS.
          CONCATENATE ev_sap_pattern iv_sql_pattern+curr_pos(1) INTO ev_sap_pattern RESPECTING BLANKS.
      ENDCASE.

      curr_pos = curr_pos + lv_continue_off.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
