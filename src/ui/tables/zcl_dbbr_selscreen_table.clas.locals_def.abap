*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
    TYPES:
      BEGIN OF lty_s_col_selection,
        idx            TYPE sy-tabix,
        tabname        TYPE tabname,
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

    CLASS lcl_find_table_field_view DEFINITION
      INHERITING FROM zcl_uitb_selection_dialog.
      PUBLIC SECTION.
        METHODS constructor
          IMPORTING
            if_hide_tabname_field type abap_bool
            it_col TYPE lty_t_col_selection.
        METHODS get_chosen_field_index
          RETURNING
            VALUE(rv_col_index) TYPE i.
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
        data mf_hide_tabname_field type abap_bool.
        DATA mv_chosen_index TYPE i.
    ENDCLASS.
