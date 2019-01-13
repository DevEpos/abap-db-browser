CLASS ZCL_DBBR_tablefield DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_table_field_id
      RETURNING
        VALUE(rv_table_field_id) TYPE ZDBBR_tabfield_id.
    METHODS get_tabname
      RETURNING
        VALUE(rv_tabname) TYPE tabname.
    METHODS get_fieldname
      RETURNING
        VALUE(rv_fieldname) TYPE fieldname.
    METHODS get_ref_id
      RETURNING
        VALUE(rv_ref_id) TYPE guid_22.
    METHODS is_sort_active
      RETURNING
        VALUE(rf_sort_active) TYPE boolean.
    METHODS get_sort_direction
      RETURNING
        VALUE(rv_sort_direction) TYPE ZDBBR_sort_direction.
    METHODS get_sort_order
      RETURNING
        VALUE(rv_sort_order) TYPE numc4.
    METHODS is_selection_active
      RETURNING
        VALUE(rf_selection_active) TYPE boolean.
    METHODS get_selection_order
      RETURNING
        VALUE(rv_selection_order) TYPE numc4.
    METHODS is_output_active
      RETURNING
        VALUE(rf_output_active) TYPE boolean.
    METHODS get_output_order
      RETURNING
        VALUE(rv_output_order) TYPE numc4.
    METHODS get_is_formula_field
      RETURNING
        VALUE(rv_is_formula_field) TYPE boolean.
    METHODS is_text_field
      RETURNING
        VALUE(rf_is_text_field) TYPE boolean.
    METHODS is_numeric
      RETURNING
        VALUE(rf_is_numeric) TYPE boolean.
    METHODS get_alias
      RETURNING
        VALUE(rv_alias) TYPE ZDBBR_table_alias.
    METHODS is_key
      RETURNING
        VALUE(rf_is_key) TYPE keyflag.
    METHODS is_foreign_key
      RETURNING
        VALUE(rf_is_foreign_key) TYPE keyflag.
    METHODS get_field_ddtext
      RETURNING
        VALUE(rv_field_ddtext) TYPE ddtext.
    METHODS get_sql_fieldname
      RETURNING
        VALUE(rv_sql_fieldname) TYPE ZDBBR_fieldname_with_alias.
    METHODS get_alv_fieldname
      RETURNING
        VALUE(rv_alv_fieldname) TYPE fieldname.
    METHODS get_ddic_order
      RETURNING
        VALUE(rv_ddic_order) TYPE tabfdpos.
    METHODS get_f4_available
      RETURNING
        VALUE(rv_f4_available) TYPE ddf4avail.
    METHODS get_rollname
      RETURNING
        VALUE(rv_rollname) TYPE rollname.
    METHODS get_domname
      RETURNING
        VALUE(rv_domname) TYPE domname.
    METHODS get_std_short_text
      RETURNING
        VALUE(rv_std_short_text) TYPE scrtext_m.
    METHODS get_std_long_text
      RETURNING
        VALUE(rv_std_long_text) TYPE scrtext_l.
    METHODS get_alt_short_text
      RETURNING
        VALUE(rv_alt_short_text) TYPE scrtext_m.
    METHODS get_alt_long_text
      RETURNING
        VALUE(rv_alt_long_text) TYPE scrtext_l.
    METHODS has_text_field
      RETURNING
        VALUE(rf_has_text_field) TYPE boolean.
    METHODS has_active_text_field
      RETURNING
        VALUE(rf_has_active_text_field) TYPE boolean.
    METHODS get_reference_alv_fieldname
      RETURNING
        VALUE(rv_reference_alv_fieldname) TYPE fieldname.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_table_field_id TYPE ZDBBR_tabfield_id .
    DATA mv_tabname TYPE tabname.
    DATA mv_fieldname TYPE fieldname.
    DATA mv_ref_id TYPE guid_22.
    DATA mv_sort_active TYPE boolean.
    DATA mv_sort_direction TYPE ZDBBR_sort_direction.
    DATA mv_sort_order TYPE numc4.
    DATA mv_selection_active TYPE boolean.
    DATA mv_selection_order TYPE numc4.
    DATA mv_output_active TYPE boolean.
    DATA mv_output_order TYPE numc4.
    DATA mf_is_formula_field TYPE boolean.
    DATA mf_is_text_field TYPE boolean.
    DATA mf_is_numeric TYPE boolean.
    DATA mv_alias TYPE ZDBBR_table_alias.
    DATA mf_is_key TYPE keyflag.
    DATA mf_is_foreign_key TYPE keyflag.
    DATA mv_field_ddtext TYPE ddtext.
    DATA mv_sql_fieldname TYPE ZDBBR_fieldname_with_alias.
    DATA mv_alv_fieldname TYPE fieldname.
    DATA mv_ddic_order TYPE tabfdpos.
    DATA mv_f4_available TYPE ddf4avail.
    DATA mv_rollname TYPE rollname.
    DATA mv_domname TYPE domname.
    DATA mv_std_short_text TYPE scrtext_m.
    DATA mv_std_long_text TYPE scrtext_l.
    DATA mv_alt_short_text TYPE scrtext_m.
    DATA mv_alt_long_text TYPE scrtext_l.
    DATA mv_has_text_field TYPE boolean.
    DATA mv_has_active_text_field TYPE boolean.
    DATA mv_reference_alv_fieldname TYPE fieldname.
ENDCLASS.



CLASS ZCL_DBBR_tablefield IMPLEMENTATION.
  METHOD get_table_field_id.
    rv_table_field_id = mv_table_field_id.
  ENDMETHOD.
  METHOD get_tabname.
    rv_tabname = mv_tabname.
  ENDMETHOD.
  METHOD get_fieldname.
    rv_fieldname = mv_fieldname.
  ENDMETHOD.
  METHOD get_ref_id.
    rv_ref_id = mv_ref_id.
  ENDMETHOD.
  METHOD is_sort_active.
    rf_sort_active = mv_sort_active.
  ENDMETHOD.
  METHOD get_sort_direction.
    rv_sort_direction = mv_sort_direction.
  ENDMETHOD.
  METHOD get_sort_order.
    rv_sort_order = mv_sort_order.
  ENDMETHOD.
  METHOD is_selection_active.
    rf_selection_active = mv_selection_active.
  ENDMETHOD.
  METHOD get_selection_order.
    rv_selection_order = mv_selection_order.
  ENDMETHOD.
  METHOD is_output_active.
    rf_output_active = mv_output_active.
  ENDMETHOD.
  METHOD get_output_order.
    rv_output_order = mv_output_order.
  ENDMETHOD.
  METHOD get_is_formula_field.
    rv_is_formula_field = mf_is_formula_field.
  ENDMETHOD.
  METHOD is_text_field.
    rf_is_text_field = mf_is_text_field.
  ENDMETHOD.
  METHOD is_numeric.
    rf_is_numeric = mf_is_numeric.
  ENDMETHOD.
  METHOD get_alias.
    rv_alias = mv_alias.
  ENDMETHOD.
  METHOD is_key.
    rf_is_key = mf_is_key.
  ENDMETHOD.
  METHOD is_foreign_key.
    rf_is_foreign_key = mf_is_foreign_key.
  ENDMETHOD.
  METHOD get_field_ddtext.
    rv_field_ddtext = mv_field_ddtext.
  ENDMETHOD.
  METHOD get_sql_fieldname.
    rv_sql_fieldname = mv_sql_fieldname.
  ENDMETHOD.
  METHOD get_alv_fieldname.
    rv_alv_fieldname = mv_alv_fieldname.
  ENDMETHOD.
  METHOD get_ddic_order.
    rv_ddic_order = mv_ddic_order.
  ENDMETHOD.
  METHOD get_f4_available.
    rv_f4_available = mv_f4_available.
  ENDMETHOD.
  METHOD get_rollname.
    rv_rollname = mv_rollname.
  ENDMETHOD.
  METHOD get_domname.
    rv_domname = mv_domname.
  ENDMETHOD.
  METHOD get_std_short_text.
    rv_std_short_text = mv_std_short_text.
  ENDMETHOD.
  METHOD get_std_long_text.
    rv_std_long_text = mv_std_long_text.
  ENDMETHOD.
  METHOD get_alt_short_text.
    rv_alt_short_text = mv_alt_short_text.
  ENDMETHOD.
  METHOD get_alt_long_text.
    rv_alt_long_text = mv_alt_long_text.
  ENDMETHOD.
  METHOD has_text_field.
    rf_has_text_field = mv_has_text_field.
  ENDMETHOD.
  METHOD has_active_text_field.
    rf_has_active_text_field = mv_has_active_text_field.
  ENDMETHOD.
  METHOD get_reference_alv_fieldname.
    rv_reference_alv_fieldname = mv_reference_alv_fieldname.
  ENDMETHOD.
ENDCLASS.
