CLASS ZCL_DBBR_formula_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS save_formula_def
      IMPORTING
        iv_formula_string TYPE string
        iv_description    TYPE ddtext.
    METHODS delete_formulas_for_user
      IMPORTING
        iv_created_by TYPE sy-uname DEFAULT sy-uname.
    METHODS delete_formula_by_id
      IMPORTING
        iv_id TYPE guid_22.
    METHODS get_formulas
      IMPORTING
        iv_created_by          TYPE sy-uname optional
      RETURNING
        VALUE(rt_formula_defs) TYPE ZIF_DBBR_fe_types=>tt_formula_defs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_formula_factory IMPLEMENTATION.


  METHOD get_formulas.
    data: lt_created_by_range type range of ZDBBR_created_by.

    lt_created_by_range = cond #(
      when iv_created_by is not initial then
        value #( ( sign = 'I' option = 'EQ' low = iv_created_by ) )
      else
        value #( ( sign = 'E' option = 'EQ' low = sy-uname ) )
    ).

    SELECT * FROM ZDBBR_ffdef INTO CORRESPONDING FIELDS OF TABLE rt_formula_defs
      WHERE created_by in lt_created_by_range
      ORDER BY created_date DESCENDING
               created_time DESCENDING.
  ENDMETHOD.


  METHOD save_formula_def.
    DATA(ls_formula) = VALUE ZDBBR_ffdef(
        id             = ZCL_DBBR_system_helper=>create_guid_22( )
        formula_string = iv_formula_string
        description    = iv_description
        created_by     = sy-uname
        created_date   = sy-datum
        created_time   = sy-timlo
    ).

    INSERT ZDBBR_ffdef FROM ls_formula.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD delete_formulas_for_user.
    DELETE FROM ZDBBR_ffdef WHERE created_by = iv_created_by.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD delete_formula_by_id.
    DELETE FROM ZDBBR_ffdef WHERE id = iv_id.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
