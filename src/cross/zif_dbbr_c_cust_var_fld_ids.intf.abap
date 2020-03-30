"! <p class="shorttext synchronized" lang="en">IDs for custom variant fields</p>
INTERFACE zif_dbbr_c_cust_var_fld_ids
  PUBLIC .

  CONSTANTS custom_field_table TYPE tabname VALUE '#GS_DATA' ##NO_TEXT.
  CONSTANTS grouping_minimum TYPE fieldname VALUE 'GROUPING_MINIMUM' ##NO_TEXT.
  CONSTANTS max_lines  TYPE fieldname VALUE 'MAX_LINES' ##NO_TEXT.
ENDINTERFACE.
