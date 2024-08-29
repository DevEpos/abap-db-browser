INTERFACE zif_dbbr_fe_field_extractor
  PUBLIC.
  METHODS extract_field
    IMPORTING
      is_statement    TYPE zif_dbbr_fe_types=>ty_statement
    RETURNING
      VALUE(rs_field) TYPE zif_dbbr_fe_types=>ty_form_field.
ENDINTERFACE.
