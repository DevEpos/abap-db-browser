CLASS zcl_dbbr_input_validator DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_and_crop_input
      CHANGING
        !cv_value TYPE zsat_value .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_input_validator IMPLEMENTATION.


  METHOD check_and_crop_input.
*&---------------------------------------------------------------------*
*& Description: Checks if input has the correct length
*&---------------------------------------------------------------------*

    DATA(lv_length) = strlen( cv_value ).
    IF lv_length > 45.
      DATA(lv_cropped) = CONV zsat_value( cv_value(44) ).
      lv_cropped+44(1) = '*'.
      CLEAR cv_value.
      cv_value = lv_cropped.

      MESSAGE i104(zdbbr_info).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
