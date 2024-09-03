CLASS zcl_dbbr_addtext_exporter DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_exporter FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS create_internal_table_ref REDEFINITION.
    METHODS build_data_for_export     REDEFINITION.
    METHODS write_success_message     REDEFINITION.
    METHODS write_error_message       REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_dbbr_addtext_exporter IMPLEMENTATION.
  METHOD constructor.
    super->constructor( 'DB-Browser-AdditionalTexts' ).
  ENDMETHOD.

  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_addtext_itab( ).
  ENDMETHOD.

  METHOD build_data_for_export.
    FIELD-SYMBOLS <lt_table> TYPE zdbbr_addtext_itab.

    ASSIGN mr_source_data_tab->* TO <lt_table>.

    DATA(lr_addtext_f) = NEW zcl_dbbr_addtext_factory( ).

    <lt_table> = lr_addtext_f->get_all( ).
    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_addtext>).
      CLEAR <ls_addtext>-addtext_id.
    ENDLOOP.
  ENDMETHOD.

  METHOD write_success_message.
    MESSAGE |Additional Texts were successfully exported to { mv_full_export_path }| TYPE 'S'.
  ENDMETHOD.

  METHOD write_error_message.
    MESSAGE |An Error occurred during the export of Additional Texts| TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.
ENDCLASS.
