CLASS zcl_dbbr_pretty_printer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS format_source
      IMPORTING
        !ir_settings   TYPE REF TO if_pretty_printer_settings OPTIONAL
      CHANGING
        ct_source      TYPE rswsourcet
      RAISING
        cx_sedi_pretty_printer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_pretty_printer IMPLEMENTATION.


  METHOD format_source.
    DATA:
      lt_buffer            TYPE rswsourcet,
      lf_indentation_wrong TYPE i.

    DATA(lr_settings) = COND #( WHEN ir_settings IS NOT INITIAL THEN ir_settings ELSE NEW cl_pretty_printer_wb_settings( ) ).

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo                  = abap_false
        settings                = lr_settings
      IMPORTING
        indentation_maybe_wrong = lf_indentation_wrong
      TABLES
        otext                   = ct_source
        ntext                   = lt_buffer
      EXCEPTIONS
        OTHERS                  = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sedi_pretty_printer.
    ENDIF.

    ct_source = lt_buffer.

  ENDMETHOD.


ENDCLASS.

