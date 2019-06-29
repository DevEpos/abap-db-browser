CLASS zcl_dbbr_altcoltext_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS save_altcoltext
      IMPORTING
        is_altcoltext_data TYPE zdbbr_altcoltext_data.
    METHODS find_alternative_texts
      IMPORTING
        it_tabname_selopt           TYPE zdbbr_selopt_itab
      RETURNING
        VALUE(rt_alternative_texts) TYPE zdbbr_altcoltext_data_itab.
    METHODS find_alternative_text
      IMPORTING
        iv_tabname                 TYPE tabname
        iv_fieldname               TYPE fieldname
      RETURNING
        VALUE(rs_alternative_text) TYPE zdbbr_altcoltext_data.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_altcoltext_factory IMPLEMENTATION.

  METHOD save_altcoltext.
    " delete alternative text if none is defined
    IF is_altcoltext_data-alt_long_text = space AND
       is_altcoltext_data-alt_short_text = space.
      DELETE FROM zdbbr_altcolt WHERE tabname = is_altcoltext_data-tabname
                                   AND fieldname = is_altcoltext_data-fieldname.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ELSE. " at least one entry is filled
      MODIFY zdbbr_altcolt FROM is_altcoltext_data.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD find_alternative_texts.
    DATA(lv_language) = zcl_dbbr_system_helper=>get_system_language( ).

    SELECT * FROM zdbbr_altcolt INTO CORRESPONDING FIELDS OF TABLE rt_alternative_texts
      WHERE tabname IN it_tabname_selopt
        AND language = lv_language.
  ENDMETHOD.

  METHOD find_alternative_text.
    DATA(lv_language) = zcl_dbbr_system_helper=>get_system_language( ).

    SELECT SINGLE * FROM zdbbr_altcolt INTO CORRESPONDING FIELDS OF rs_alternative_text
      WHERE tabname   = iv_tabname
        AND fieldname = iv_fieldname
        AND language  = lv_language.
  ENDMETHOD.

ENDCLASS.
