INTERFACE ZIF_DBBR_fe_field_extractor
  PUBLIC .
  METHODS extract_field
    IMPORTING
      is_statement    TYPE ZIF_DBBR_fe_types=>ty_statement
    RETURNING
      VALUE(rs_field) TYPE ZIF_DBBR_fe_types=>ty_form_field.
ENDINTERFACE.
