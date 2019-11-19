CLASS zcl_dbbr_formula_factory DEFINITION
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
        iv_created_by          TYPE sy-uname OPTIONAL
      RETURNING
        VALUE(rt_formula_defs) TYPE zif_dbbr_fe_types=>tt_formula_defs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_formula_factory IMPLEMENTATION.


  METHOD get_formulas.
    DATA: lt_created_by_range TYPE RANGE OF zsat_created_by.

    lt_created_by_range = COND #(
      WHEN iv_created_by IS NOT INITIAL THEN
        VALUE #( ( sign = 'I' option = 'EQ' low = iv_created_by ) )
      ELSE
        VALUE #( ( sign = 'E' option = 'EQ' low = sy-uname ) )
    ).

    SELECT * FROM zdbbr_ffdef INTO CORRESPONDING FIELDS OF TABLE rt_formula_defs
      WHERE created_by IN lt_created_by_range
      ORDER BY created_date DESCENDING
               created_time DESCENDING.
  ENDMETHOD.


  METHOD save_formula_def.
    DATA(ls_formula) = VALUE zdbbr_ffdef(
        id             = zcl_sat_system_helper=>create_guid_22( )
        formula_string = iv_formula_string
        description    = iv_description
        created_by     = sy-uname
        created_date   = sy-datum
        created_time   = sy-timlo
    ).

    INSERT zdbbr_ffdef FROM ls_formula.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD delete_formulas_for_user.
    DELETE FROM zdbbr_ffdef WHERE created_by = iv_created_by.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD delete_formula_by_id.
    DELETE FROM zdbbr_ffdef WHERE id = iv_id.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
