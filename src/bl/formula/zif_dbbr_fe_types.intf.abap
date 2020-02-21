"! <p class="shorttext synchronized" lang="en">Type definitions for formula editor</p>
INTERFACE zif_dbbr_fe_types
  PUBLIC .
  TYPES:
    "! <p class="shorttext synchronized" lang="en">Formula Field definition</p>
    BEGIN OF ty_form_field,
      field             TYPE fieldname,
      type_ref_tab      TYPE tabname,
      type_ref_field    TYPE fieldname,
      type_name         TYPE string,
      is_color          TYPE abap_bool,
      is_description    TYPE abap_bool,
      is_unit           TYPE abap_bool,
      unit_field        TYPE string,
      type              TYPE zdbbr_formfield_type,
      short_description TYPE zdbbr_ff_list_output_medium,
      long_description  TYPE zdbbr_ff_list_output_long,
    END OF ty_form_field.

  TYPES: tt_form_field TYPE HASHED TABLE OF ty_form_field WITH UNIQUE KEY field.

  TYPES: tt_formula_defs TYPE STANDARD TABLE OF zdbbr_ffdef WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_form_calc_field,
      fieldname TYPE fieldname,
      tabname   TYPE tabname,
    END OF ty_form_calc_field.

  TYPES: tt_form_calc_field TYPE STANDARD TABLE OF ty_form_calc_field WITH DEFAULT KEY.
  TYPES: tt_form_calc_field_unique TYPE SORTED TABLE OF ty_form_calc_field WITH UNIQUE KEY fieldname tabname.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Token inside Formula Statement</p>
    BEGIN OF ty_token,
      id               TYPE sy-tabix,
      str              TYPE string,
      row              TYPE token_row,
      col              TYPE token_col,
      type             TYPE token_type,
      is_row_field     TYPE abap_bool,
      is_formula_field TYPE abap_bool,
    END OF ty_token.

  TYPES: tt_token TYPE STANDARD TABLE OF ty_token WITH DEFAULT KEY
                                                  WITH UNIQUE SORTED KEY idkey COMPONENTS id.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Statement inside formula</p>
    BEGIN OF ty_statement,
*           from                    TYPE stmnt_from,
*           to                      TYPE stmnt_to,
      number                  TYPE stmnt_nr,
      trow                    TYPE stmnt_trow,
*           tcol                    TYPE stmnt_tcol,
*           prefixlen               TYPE stmnt_plen,
      type                    TYPE stmnt_type,
      terminator              TYPE stmnt_term,
      stringform              TYPE string,
      stringform_subroutine   TYPE string,
      exclude_from_subroutine TYPE abap_bool,
      is_form_stmnt           TYPE abap_bool,
      is_text_form            TYPE abap_bool,
      is_function_call        TYPE abap_bool,
      needs_row_fields        TYPE abap_bool,
      first_token_str         TYPE string,
      token_count             TYPE sy-tabix,
      tokens                  TYPE tt_token,
    END OF ty_statement.

  TYPES: tt_statement TYPE STANDARD TABLE OF ty_statement WITH DEFAULT KEY.
ENDINTERFACE.
