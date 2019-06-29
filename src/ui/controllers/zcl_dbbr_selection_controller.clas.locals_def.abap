*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF lty_s_col_selection,
    idx            TYPE sy-tabix,
    tech_fieldname TYPE fieldname,
    fieldname      TYPE fieldname,
    description    TYPE ddtext,
    filtered       TYPE abap_bool,
  END OF lty_s_col_selection.
TYPES: lty_t_col_selection TYPE STANDARD TABLE OF lty_s_col_selection WITH EMPTY KEY.
TYPES:
  BEGIN OF mty_salv_sort,
    columnname TYPE lvc_fname,
    position   TYPE i,
    sequence   TYPE salv_de_sort_sequence,
  END OF mty_salv_sort.

TYPES: mtt_salv_sort TYPE STANDARD TABLE OF mty_salv_sort.

CLASS lcl_choose_col_view DEFINITION
  INHERITING FROM zcl_uitb_selection_dialog.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_col TYPE lty_t_col_selection.
    METHODS get_chosen_column
      RETURNING
        VALUE(rv_col) TYPE fieldname.
  PROTECTED SECTION.
    METHODS:
      get_output_table REDEFINITION,
      matches_filter REDEFINITION,
      set_selected_element REDEFINITION,
      adjust_column REDEFINITION,
      create_content REDEFINITION.
  PRIVATE SECTION.
    DATA mt_col_filtered TYPE lty_t_col_selection.
    DATA mt_col TYPE lty_t_col_selection.
    DATA mv_chosen_field TYPE fieldname.
ENDCLASS.

CLASS lcl_detail_viewer DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_util           TYPE REF TO zcl_dbbr_selection_util
        is_technical_info TYPE zdbbr_tech_info
        io_tabfields_all  TYPE REF TO zcl_dbbr_tabfield_list
        it_fieldcat       TYPE lvc_t_fcat.
    METHODS show_details
      IMPORTING
        is_line TYPE any.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_selfield,
        fieldname         TYPE scrtext_l,
        value             TYPE se16n_value,
        value_unconverted TYPE se16n_value,
        tech_fieldname    TYPE fieldname,
        hidden            TYPE abap_bool,
      END OF ty_s_selfield.
    DATA mt_selfield TYPE STANDARD TABLE OF ty_s_selfield.
    DATA mo_util TYPE REF TO zcl_dbbr_selection_util.
    DATA mf_empty_hidden TYPE abap_bool.
    DATA mo_alv TYPE REF TO zcl_uitb_alv.
    DATA mo_tabfields_all TYPE REF TO zcl_dbbr_tabfield_list.
    DATA ms_technical_info TYPE zdbbr_tech_info.
    DATA mt_fieldcat TYPE lvc_t_fcat.

    METHODS on_function
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          ev_function
          ev_tag.
ENDCLASS.
