CLASS zcl_dbbr_f4_importer DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_importer
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.

    METHODS create_internal_table_ref
        REDEFINITION .
    METHODS persist_import_data
        REDEFINITION .
    METHODS process_import_data
        REDEFINITION .
    METHODS write_no_data_found_message
        REDEFINITION .
    METHODS write_start_message
        REDEFINITION .
  PRIVATE SECTION.
    METHODS edit_f4_data.
ENDCLASS.



CLASS zcl_dbbr_f4_importer IMPLEMENTATION.

  METHOD constructor.
    super->constructor( 'DB-Browser-F4' ).
  ENDMETHOD.

  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_f4_export_itab( ).
  ENDMETHOD.


  METHOD persist_import_data.
    DATA: lv_import_count TYPE i,
          lv_dummy        TYPE string ##needed.
    FIELD-SYMBOLS: <lt_f4> TYPE zdbbr_f4_export_itab.

    ASSIGN mr_import_data->* TO <lt_f4>.

    LOOP AT <lt_f4> ASSIGNING FIELD-SYMBOL(<ls_f4_export>).
      DATA(lv_f4_id) = zcl_dbbr_custom_f4_factory=>save_custom( CORRESPONDING #( <ls_f4_export> ) ).

      DATA(lt_f4_assignments) = VALUE zdbbr_f4_assignment_itab(
        FOR assgnmnt IN <ls_f4_export>-assignments
        ( entity_id  = assgnmnt-entity_id
          fieldname  = assgnmnt-fieldname
          ref_f4_id  = lv_f4_id )
      ).
      zcl_dbbr_custom_f4_factory=>update_f4_assignments( it_f4_assignments = lt_f4_assignments ).

      ADD 1 TO lv_import_count.
    ENDLOOP.

    MESSAGE i049(zdbbr_info) WITH |{ lv_import_count }| INTO lv_dummy.
    mr_log->add_from_sy(
      if_newobj = abap_false
      iv_level  = zif_uitb_c_protocol_level=>info
    ).
  ENDMETHOD.


  METHOD process_import_data.
    edit_f4_data( ).

    " @TODO: check if search help already exists
  ENDMETHOD.

  METHOD edit_f4_data.
    FIELD-SYMBOLS: <lt_f4> TYPE zdbbr_f4_export_itab.

    ASSIGN mr_import_data->* TO <lt_f4>.

    LOOP AT <lt_f4> ASSIGNING FIELD-SYMBOL(<ls_f4>).
      CLEAR: <ls_f4>-ref_join_id,
             <ls_f4>-f4_id.

      LOOP AT <ls_f4>-fields ASSIGNING FIELD-SYMBOL(<ls_f4_field>).
        CLEAR: <ls_f4_field>-ref_f4_id.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD write_no_data_found_message ##needed.
  ENDMETHOD.


  METHOD write_start_message.
    MESSAGE i042(zdbbr_info) WITH 'F4-Hilfen' INTO DATA(lv_dummy).
    mr_log->add_from_sy( if_newobj = abap_false
                         iv_level  = zif_uitb_c_protocol_level=>success ).
  ENDMETHOD.
ENDCLASS.
