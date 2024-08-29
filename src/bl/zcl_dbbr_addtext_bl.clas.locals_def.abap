*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_text_field_reader DEFINITION
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS determine_text_fields
      IMPORTING
        iv_entity      TYPE zsat_entity_id
        iv_entity_type TYPE zsat_entity_type
      CHANGING
        ct_addtext     TYPE zdbbr_addtext_data_itab.
ENDCLASS.


CLASS lcl_text_field_reader_base DEFINITION ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_entity TYPE tabname.

    METHODS determine_text_fields
      CHANGING
        ct_addtext TYPE zdbbr_addtext_data_itab.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF c_shlporigin,
        explicit         TYPE shlporigin VALUE 'X',
        checktable       TYPE shlporigin VALUE 'P',
        data_type        TYPE shlporigin VALUE 'T',
        explicit_at_dtel TYPE shlporigin VALUE 'D',
        fixed_values     TYPE shlporigin VALUE 'F',
      END OF c_shlporigin.

    TYPES: BEGIN OF ty_field_data,
             fieldname TYPE fieldname,
             rollname  TYPE rollname,
             domname   TYPE domname,
           END OF ty_field_data.

    TYPES: BEGIN OF ty_field_w_shlp,
             fieldname TYPE fieldname,
             shlpname  TYPE dd30l-shlpname,
             shlpfield TYPE shlpfield,
             texttab   TYPE dd30l-texttab,
           END OF ty_field_w_shlp.

    TYPES: BEGIN OF ty_field_w_fixed_val_shlp,
             fieldname TYPE fieldname,
           END OF ty_field_w_fixed_val_shlp.

    TYPES: BEGIN OF ty_dtel_shlp,
             rollname            TYPE rollname,
             shlpname            TYPE shlpname,
             shlpfield           TYPE shlpfield,
             entitytab           TYPE entitytab,
             dtel_text_table     TYPE tabname,
             dtel_text_table_key TYPE tabname,
             texttab             TYPE dd30l-texttab,
           END OF ty_dtel_shlp,

           ty_t_dtel_shlp TYPE TABLE OF ty_dtel_shlp WITH EMPTY KEY.

    TYPES: BEGIN OF ty_text_table_field,
             tabname   TYPE tabname,
             fieldname TYPE fieldname,
             rollname  TYPE rollname,
             datatype  TYPE dd03l-datatype,
             keyflag   TYPE dd03l-keyflag,
           END OF ty_text_table_field.

    DATA mv_entity TYPE zsat_entity_id.
    DATA mt_fields TYPE TABLE OF ty_field_data.
    DATA mt_field_with_shlp TYPE SORTED TABLE OF ty_field_w_shlp WITH UNIQUE KEY fieldname.
    DATA mt_field_with_fix_val_shlp TYPE SORTED TABLE OF ty_field_w_fixed_val_shlp WITH UNIQUE KEY fieldname.
    DATA mt_dtel_with_shlp TYPE SORTED TABLE OF ty_dtel_shlp WITH UNIQUE KEY rollname.
    DATA mt_text_table_field TYPE SORTED TABLE OF ty_text_table_field WITH UNIQUE KEY tabname fieldname
                                                                        WITH NON-UNIQUE SORTED KEY lang COMPONENTS tabname datatype.

    METHODS determine_f_w_shlp.
    METHODS determine_f_w_fixed_val_shlp ABSTRACT.
    METHODS determine_f_w_dtel_shlp.
    METHODS determine_text_table_fields.

    METHODS fill_text_field_infos
      CHANGING
        cs_text_field TYPE zdbbr_addtext_data.

    METHODS determine_fields ABSTRACT.

    METHODS select_f_w_dtel_shlp ABSTRACT
      RETURNING
        VALUE(rt_result) TYPE ty_t_dtel_shlp.
ENDCLASS.


CLASS lcl_cds_text_field_reader DEFINITION
  INHERITING FROM lcl_text_field_reader_base
  CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS determine_fields             REDEFINITION.
    METHODS determine_f_w_fixed_val_shlp REDEFINITION.
    METHODS select_f_w_dtel_shlp         REDEFINITION.

ENDCLASS.


CLASS lcl_table_text_field_reader DEFINITION
  INHERITING FROM lcl_text_field_reader_base
  CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS determine_fields             REDEFINITION.
    METHODS determine_f_w_fixed_val_shlp REDEFINITION.
    METHODS select_f_w_dtel_shlp         REDEFINITION.

ENDCLASS.
