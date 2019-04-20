"! <p class="shorttext synchronized" lang="en">Helper for data selection (SQL)</p>
CLASS zcl_dbbr_selection_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Read DB table size</p>
    CLASS-METHODS read_db_size
      IMPORTING
        !iv_db_tab    TYPE tabname
      RETURNING
        VALUE(result) TYPE sy-tabix .
    "! <p class="shorttext synchronized" lang="en">Creates additional where conditions for ignore case</p>
    CLASS-METHODS create_ignore_case_cond
      IMPORTING
        !is_selfield TYPE zdbbr_selfield
      CHANGING
        !ct_selfield TYPE zdbbr_selfield_itab .
    "! <p class="shorttext synchronized" lang="en">Append parameters string to table clause</p>
    CLASS-METHODS append_params_to_table_string
      IMPORTING
        it_parameters TYPE zdbbr_table_parameter_t
      CHANGING
        cv_table_part TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_selection_helper IMPLEMENTATION.

  METHOD append_params_to_table_string.
    cv_table_part = REDUCE string(
       INIT value = |{ cv_table_part }( | sep = ``
       FOR param IN it_parameters
       NEXT value = |{ value }{ sep }{ param-param_name } = { cl_abap_dyn_prg=>quote( param-param_value ) }| sep = `, `
    ) && | )|.
  ENDMETHOD.

  METHOD create_ignore_case_cond.
  ENDMETHOD.


  METHOD read_db_size.
    SELECT COUNT( * ) FROM (iv_db_tab) INTO result.
  ENDMETHOD.
ENDCLASS.
