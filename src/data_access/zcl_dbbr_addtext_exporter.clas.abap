CLASS ZCL_DBBR_addtext_exporter DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_exporter
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS: create_internal_table_ref REDEFINITION,
      build_data_for_export REDEFINITION,
      write_success_message REDEFINITION,
      write_error_message REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_addtext_exporter IMPLEMENTATION.
  METHOD constructor.
    super->constructor( 'DB-Browser-AdditionalTexts' ).
  ENDMETHOD.

  METHOD create_internal_table_ref.
    rr_table_ref = NEW ZDBBR_addtext_itab( ).
  ENDMETHOD.

  METHOD build_data_for_export.
    FIELD-SYMBOLS: <lt_table> TYPE ZDBBR_addtext_itab.

    ASSIGN mr_source_data_tab->* TO <lt_table>.

    DATA(lr_addtext_f) = NEW ZCL_DBBR_addtext_factory( ).

    <lt_table> = lr_addtext_f->get_all( ).
    loop at <lt_table> ASSIGNING FIELD-SYMBOL(<ls_addtext>).
        clear: <ls_addtext>-addtext_id.
    ENDLOOP.
  ENDMETHOD.

  METHOD write_success_message.
    MESSAGE |Additional Texts were successfully exported to { mv_full_export_path }| TYPE 'S'.
  ENDMETHOD.

  METHOD write_error_message.
    MESSAGE |An Error occurred during the export of Additional Texts| TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.

ENDCLASS.
