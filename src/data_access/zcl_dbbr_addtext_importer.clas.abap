CLASS zcl_dbbr_addtext_importer DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_importer FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS create_internal_table_ref   REDEFINITION.
    METHODS write_no_data_found_message REDEFINITION.
    METHODS process_import_data         REDEFINITION.
    METHODS persist_import_data         REDEFINITION.
    METHODS write_start_message         REDEFINITION.

  PRIVATE SECTION.
    DATA mr_addtext_f TYPE REF TO zcl_dbbr_addtext_factory.
ENDCLASS.


CLASS zcl_dbbr_addtext_importer IMPLEMENTATION.
  METHOD constructor.
    super->constructor( 'SE16N-Advanced-AdditionalTexts' ).

    mr_addtext_f = NEW zcl_dbbr_addtext_factory( ).
  ENDMETHOD.

  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_addtext_itab( ).
  ENDMETHOD.

  METHOD write_no_data_found_message.
    MESSAGE w020(zdbbr_exception) INTO DATA(lv_dummy) ##NEEDED.
    mr_log->add_from_sy( if_newobj = abap_false
                         iv_level  = zif_uitb_c_protocol_level=>warning ).
  ENDMETHOD.

  METHOD process_import_data.
    ASSIGN CAST zdbbr_addtext_itab( mr_import_data )->* TO FIELD-SYMBOL(<lt_addtexts>).

    LOOP AT <lt_addtexts> ASSIGNING FIELD-SYMBOL(<ls_addtext>).
      IF mr_addtext_f->exists_for_bus_key( iv_id_table       = <ls_addtext>-id_table
                                           iv_id_field1      = <ls_addtext>-id_field
                                           iv_id_field2      = <ls_addtext>-id_field2
                                           iv_cond_field     = <ls_addtext>-condition_field
                                           iv_cond_value     = <ls_addtext>-condition_value
                                           iv_cond_operation = <ls_addtext>-condition_op ).

        DATA(lv_bus_key) = |{ <ls_addtext>-id_table },| &&
                            |{ <ls_addtext>-id_field },| &&
                            |{ <ls_addtext>-id_field2 },| &&
                            |{ <ls_addtext>-condition_field },| &&
                            |{ <ls_addtext>-condition_value },| &&
                            <ls_addtext>-condition_op.

        zcl_sat_message_helper=>split_string_for_message( EXPORTING iv_string = lv_bus_key
                                                          IMPORTING ev_msgv1  = DATA(lv_msgv1)
                                                                    ev_msgv2  = DATA(lv_msgv2)
                                                                    ev_msgv3  = DATA(lv_msgv3)
                                                                    ev_msgv4  = DATA(lv_msgv4) ).

        MESSAGE w040(zdbbr_exception) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO DATA(lv_exists_msg) ##NEEDED.
        mr_log->add_from_sy( if_newobj = abap_false
                             iv_level  = zif_uitb_c_protocol_level=>warning ).
        DELETE <lt_addtexts>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD persist_import_data.
    LOOP AT CAST zdbbr_addtext_itab( mr_import_data )->* ASSIGNING FIELD-SYMBOL(<ls_addtext>).
      mr_addtext_f->save_add_text( CHANGING cs_addtext_data = <ls_addtext> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD write_start_message.
    MESSAGE i050(zdbbr_info) WITH 'Zusatztexten' INTO DATA(lv_dummy) ##NEEDED.
    mr_log->add_from_sy( if_newobj = abap_false
                         iv_level  = zif_uitb_c_protocol_level=>success ).
  ENDMETHOD.
ENDCLASS.
