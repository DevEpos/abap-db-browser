"! <p class="shorttext synchronized" lang="en">Utility Class handling CDS Parameters</p>
CLASS zcl_dbbr_cds_param_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Concatenate parameters and its values to a string</p>
    "!
    "! @parameter it_param_values | <p class="shorttext synchronized" lang="en">List of parameters with values</p>
    "! @parameter iv_param_indentation | <p class="shorttext synchronized" lang="en">Number of spaces for indentation</p>
    "! @parameter if_sep_param_by_newline | <p class="shorttext synchronized" lang="en">Every param/value in new line</p>
    "! @parameter rv_params | <p class="shorttext synchronized" lang="en">Concatenated parameter/value combinations</p>
    CLASS-METHODS build_param_string
      IMPORTING
        it_param_values         TYPE zif_sat_ty_global=>ty_t_cds_param_value
        iv_param_indentation    TYPE int1
        if_sep_param_by_newline TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_params)        TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_cds_param_util IMPLEMENTATION.

  METHOD build_param_string.

    CHECK it_param_values IS NOT INITIAL.

    DATA(lv_spaces) = `  `.
    DO iv_param_indentation TIMES.
      lv_spaces = lv_spaces && ` `.
    ENDDO.

    DATA(lv_param_seperation) = COND #(
      WHEN if_sep_param_by_newline = abap_true THEN |{ cl_abap_char_utilities=>cr_lf }{ lv_spaces }|
      ELSE | | ).

    DATA(lv_params) = REDUCE string(
      INIT value = `` sep = ``
      FOR param IN it_param_values
      NEXT value = |{ value }{ sep }{ param-name } = { cl_abap_dyn_prg=>quote( param-value ) }|
           sep = `,` && lv_param_seperation ).

    rv_params = |( { lv_params } )|.

  ENDMETHOD.

ENDCLASS.
