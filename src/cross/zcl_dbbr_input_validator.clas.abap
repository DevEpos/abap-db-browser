class ZCL_DBBR_INPUT_VALIDATOR definition
  public
  create public .

public section.

  class-methods CHECK_AND_CROP_INPUT
    changing
      !CV_VALUE type SE16N_VALUE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_INPUT_VALIDATOR IMPLEMENTATION.


  METHOD check_and_crop_input.
*&---------------------------------------------------------------------*
*& Description: Checks if input has the correct length
*&---------------------------------------------------------------------*

    DATA(lv_length) = strlen( cv_value ).
    IF lv_length > 45.
      DATA(lv_cropped) = CONV se16n_value( cv_value(44) ).
      lv_cropped+44(1) = '*'.
      CLEAR cv_value.
      cv_value = lv_cropped.

      MESSAGE i450(wusl).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
