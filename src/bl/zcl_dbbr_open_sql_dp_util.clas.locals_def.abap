*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_upper_case_settings DEFINITION
INHERITING FROM cl_pretty_printer_wb_settings.

  PUBLIC SECTION.
    METHODS if_pretty_printer_settings~get_case_mode
         REDEFINITION.
ENDCLASS.
