"! <p class="shorttext synchronized" lang="en">Database query</p>
CLASS zcl_dbbr_sql_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">SQL Query information</p>
    DATA ms_data TYPE zdbbr_sql_query READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">List of parameter definitions</p>
    DATA mt_parameters TYPE zdbbr_query_parameter_t READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">Create new query instance</p>
    METHODS constructor
      IMPORTING
        is_query      TYPE zdbbr_sql_query
        it_parameters TYPE zdbbr_query_parameter_t.
    "! <p class="shorttext synchronized" lang="en">Set value for a certain parameter</p>
    "!
    METHODS set_parameter_value
      IMPORTING
        iv_name        TYPE fieldname
        iv_value       TYPE zdbbr_value OPTIONAL
        it_value_range TYPE zuitb_generic_range_itab OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_sql_query IMPLEMENTATION.

  METHOD constructor.
    ms_data = is_query.
    mt_parameters = it_parameters.
  ENDMETHOD.


  METHOD set_parameter_value.
    ASSIGN mt_parameters[ name = iv_name ] TO FIELD-SYMBOL(<ls_param>).
    CHECK sy-subrc = 0.

    IF iv_value IS SUPPLIED.
      <ls_param>-value = iv_value.
    ELSEIF it_value_range IS SUPPLIED.
      <ls_param>-value_list = it_value_range.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
