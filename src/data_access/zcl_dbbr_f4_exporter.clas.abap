CLASS zcl_dbbr_f4_exporter DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_exporter
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_f4_id TYPE zdbbr_id_itab.
  PROTECTED SECTION.

    METHODS build_data_for_export
        REDEFINITION .
    METHODS create_internal_table_ref
        REDEFINITION .
    METHODS write_success_message
        REDEFINITION .
    METHODS write_error_message
        REDEFINITION .
  PRIVATE SECTION.
    DATA mt_f4_id TYPE zdbbr_id_itab.
ENDCLASS.



CLASS zcl_dbbr_f4_exporter IMPLEMENTATION.

  METHOD constructor.
    super->constructor( 'DB-Browser-F4' ).

    mt_f4_id = it_f4_id.
  ENDMETHOD.

  METHOD build_data_for_export.
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_f4_export_itab.

    ASSIGN mr_source_data_tab->* TO <lt_table>.

    LOOP AT mt_f4_id ASSIGNING FIELD-SYMBOL(<lv_f4_id>).
      zcl_dbbr_custom_f4_factory=>get_f4(
        EXPORTING
          iv_f4_id          = <lv_f4_id>
        IMPORTING
          es_f4_data        = DATA(ls_f4_data)
          et_f4_assignments = DATA(lt_f4_assgnmt)
      ).

      DATA(ls_f4_export) = CORRESPONDING zdbbr_f4_export( ls_f4_data ).
      ls_f4_export-assignments = CORRESPONDING #( lt_f4_assgnmt ).

      <lt_table> = VALUE #(
        BASE <lt_table>
        ( ls_f4_export )
      ).

      CLEAR: ls_f4_data,
             lt_f4_assgnmt,
             ls_f4_export.
    ENDLOOP.

    CLEAR mt_f4_id.
  ENDMETHOD.


  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_f4_export_itab( ).
  ENDMETHOD.


  METHOD write_error_message.
    MESSAGE s038(zdbbr_exception) DISPLAY LIKE 'E'.
  ENDMETHOD.


  METHOD write_success_message.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_source_data_tab->* TO <lt_table>.

    MESSAGE s048(zdbbr_info) WITH |{ lines( <lt_table> ) }|
                                   mv_full_export_path.
  ENDMETHOD.

ENDCLASS.
