CLASS zcl_dbbr_help_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_help_id,
        "! Help id for the Object Search
        object_search TYPE i VALUE 1,
        main_entry    TYPE i VALUE 2,
        join_manager  TYPE i VALUE 3,
      END OF c_help_id.

    CONSTANTS c_help_ucomm TYPE ui_func VALUE 'APPHELP'.

    "! <p class="shorttext synchronized" lang="en">Shows the help for the supplied help index</p>
    CLASS-METHODS show_help
      IMPORTING
        iv_help_id TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_doku_class,
        data_element TYPE doku_class VALUE 'DE',
        general_text TYPE doku_class VALUE 'TX',
      END OF c_doku_class.

    CLASS-METHODS call_rollname_help
      IMPORTING
        iv_rollname TYPE rollname.
    CLASS-METHODS call_general_help
      IMPORTING
        iv_help_object_id TYPE doku_obj.
    CLASS-METHODS help_show_test.
ENDCLASS.



CLASS zcl_dbbr_help_repository IMPLEMENTATION.

  METHOD show_help.
    CASE iv_help_id.

      WHEN c_help_id-object_search.
        call_general_help( 'ZDBBR_OB_HELP' ).

      WHEN c_help_id-main_entry.
        call_general_help( 'ZDBBR_MAIN_HELP' ).

      WHEN c_help_id-join_manager.
        call_general_help( 'ZDBBR_JOIN_MANAGER_HELP' ).
    ENDCASE.
  ENDMETHOD.


  METHOD call_rollname_help.
    DATA: lt_links TYPE STANDARD TABLE OF tline.

    DATA(lv_object) = CONV doku_obj( iv_rollname ).

    CALL FUNCTION 'HELP_OBJECT_SHOW'
      EXPORTING
        dokclass         = c_doku_class-data_element
        dokname          = lv_object
      TABLES
        links            = lt_links
      EXCEPTIONS
        object_not_found = 1
        sapscript_error  = 2
        OTHERS           = 3.
  ENDMETHOD.

  METHOD call_general_help.
    DATA: lt_links TYPE STANDARD TABLE OF tline.

    CALL FUNCTION 'HELP_OBJECT_SHOW'
      EXPORTING
        dokclass         = c_doku_class-general_text
        dokname          = iv_help_object_id
      TABLES
        links            = lt_links
      EXCEPTIONS
        object_not_found = 1
        sapscript_error  = 2
        OTHERS           = 3.
  ENDMETHOD.

  METHOD help_show_test.
    zcl_uitb_popup_help_viewer=>create(
        iv_title        = 'Test Help Viewer'
        ir_html_content = NEW zcl_uitb_html_content(
        )->add_heading(
            iv_level = 2
            iv_text  = 'Definition'
        )->start_table(
        )->start_table_row(
        )->add_table_header_cell(
            'Column 1'
        )->add_table_header_cell(
            'Column 2'
        )->add_table_header_cell(
            'Column 3'
        )->add_table_header_cell(
            'Column 4'
        )->add_table_header_cell(
            'Column 5'
        )->add_table_header_cell(
            'Column 6'
        )->add_table_header_cell(
            'Column 7'
        )->end_table_row(
        )->start_table_row(
        )->add_table_body_cell(
            'Data 1'
        )->add_table_body_cell(
            'Data 2'
        )->add_table_body_cell(
            'Data 3'
        )->add_table_body_cell(
            'Data 4'
        )->add_table_body_cell(
            'Data 5'
        )->add_table_body_cell(
            'Data 6'
        )->add_table_body_cell(
            'Data 7'
        )->end_table_row(
        )->end_table( )
    )->show( ).
  ENDMETHOD.

ENDCLASS.
