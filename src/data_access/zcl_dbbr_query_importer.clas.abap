CLASS zcl_dbbr_query_importer DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_importer
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS: create_internal_table_ref REDEFINITION,
      write_no_data_found_message REDEFINITION,
      process_import_data REDEFINITION,
      persist_import_data REDEFINITION.
    METHODS: write_start_message REDEFINITION.
  PRIVATE SECTION.
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory .

    METHODS validate_query_names .
    METHODS edit_queries .
ENDCLASS.



CLASS zcl_dbbr_query_importer IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_default_file_name = 'DB-Browser-querys' ).
    mr_query_f = NEW #( ).
  ENDMETHOD.


  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_query_data_itab( ).
  ENDMETHOD.


  METHOD edit_queries.
    FIELD-SYMBOLS: <lt_queries> TYPE zdbbr_query_data_itab.
    ASSIGN mr_import_data->* TO <lt_queries>.

    LOOP AT <lt_queries> ASSIGNING FIELD-SYMBOL(<ls_query>).
      " create new unique query id
      DATA(lv_new_query_id) = zcl_dbbr_system_helper=>create_guid_22( ).

      <ls_query>-query_id = lv_new_query_id.

      ASSIGN <ls_query>-join_def TO FIELD-SYMBOL(<ls_join_def>).
      ASSIGN <ls_query>-variants TO FIELD-SYMBOL(<lt_variants>).
      ASSIGN <ls_query>-jump_fields TO FIELD-SYMBOL(<lt_jump_fields>).
      ASSIGN <ls_query>-parameters TO FIELD-SYMBOL(<lt_parameters>).

      " 1) clear join definition of ids
      CLEAR: <ls_join_def>-join_id,
             <ls_query>-ref_join_id.

      " 4) clear variants of ids
      LOOP AT <lt_variants> ASSIGNING FIELD-SYMBOL(<ls_variant_data>).
        <ls_variant_data>-entity_id = lv_new_query_id.
        CLEAR: <ls_variant_data>-variant_id.
      ENDLOOP.

      " 5) clear jump fields of ids
      LOOP AT <lt_jump_fields> ASSIGNING FIELD-SYMBOL(<ls_jump_field>).
        <ls_jump_field>-ref_query_id = lv_new_query_id.
        CLEAR <ls_jump_field>-jumpdest_id.
      ENDLOOP.

      LOOP AT <lt_parameters> ASSIGNING FIELD-SYMBOL(<ls_param>).
        <ls_param>-ref_query_id = lv_new_query_id.
      ENDLOOP.

      IF <ls_query>-source IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN <ls_query>-source WITH cl_abap_char_utilities=>cr_lf.
      ENDIF.

      IF <ls_query>-formula IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN <ls_query>-formula WITH cl_abap_char_utilities=>cr_lf.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD persist_import_data.
    DATA: lv_import_count TYPE i.
    FIELD-SYMBOLS: <lt_queries> TYPE zdbbr_query_data_itab.

    ASSIGN mr_import_data->* TO <lt_queries>.

    LOOP AT <lt_queries> ASSIGNING FIELD-SYMBOL(<ls_query>).
      " does a query with this name already exist?
      mr_query_f->save_query( is_query = <ls_query> ).
      MESSAGE i041(zdbbr_info) WITH <ls_query>-query_name INTO DATA(lv_dummy).
      mr_log->add_from_sy(
          if_newobj = abap_false
          iv_level  = zif_uitb_c_protocol_level=>info
      ).
      ADD 1 TO lv_import_count.
    ENDLOOP.

    MESSAGE i039(zdbbr_info) WITH |{ lv_import_count }| INTO lv_dummy.
    mr_log->add_from_sy(
          if_newobj = abap_false
          iv_level  = zif_uitb_c_protocol_level=>info
      ).

  ENDMETHOD.


  METHOD process_import_data.
    edit_queries( ).

    validate_query_names( ).
  ENDMETHOD.


  METHOD validate_query_names.
    FIELD-SYMBOLS: <lt_queries> TYPE zdbbr_query_data_itab.

    ASSIGN mr_import_data->* TO <lt_queries>.

    LOOP AT <lt_queries> ASSIGNING FIELD-SYMBOL(<ls_queries>).
      " does a query with this name already exist?
      DATA(lv_existing_query_id) = mr_query_f->get_query_id( <ls_queries>-query_name ).

      IF lv_existing_query_id IS NOT INITIAL.

        DATA(lv_answer) = zcl_dbbr_appl_util=>popup_to_confirm(
            iv_title     = 'Overwrite?'
            iv_query     = |A query with the name: '{ <ls_queries>-query_name }'| &&
                           |, Creator: '{ <ls_queries>-created_by }'| &&
                           |, Global: '{ COND string( WHEN <ls_queries>-is_global = abap_true THEN 'Yes' ELSE 'No' ) }'| &&
                           ` already exists. Do you want to overwrite it?`
            iv_icon_type = 'ICON_MESSAGE_INFO'
        ).

        IF lv_answer = '2' OR lv_answer = 'A'.
          MESSAGE w010(zdbbr_exception) WITH |'{ <ls_queries>-query_name }'|
                                              |'{ <ls_queries>-created_by }'|
                                              |'{ COND string( WHEN <ls_queries>-is_global = abap_true THEN 'Yes' ELSE 'No' ) }'|
                                        INTO DATA(lv_error).
          mr_log->add_from_sy(
              if_newobj = abap_false
              iv_level  = zif_uitb_c_protocol_level=>warning
          ).
          DELETE <lt_queries>.
        ELSE.
          mr_query_f->delete_query_by_id( lv_existing_query_id ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD write_no_data_found_message.
    MESSAGE w040(zdbbr_info) INTO DATA(lv_dummy).
    mr_log->add_from_sy(
        if_newobj = abap_false
        iv_level  = zif_uitb_c_protocol_level=>warning
    ).
  ENDMETHOD.


  METHOD write_start_message.
    MESSAGE i042(zdbbr_info) WITH 'querys' INTO DATA(lv_dummy).
    mr_log->add_from_sy( if_newobj = abap_false
                         iv_level  = zif_uitb_c_protocol_level=>success ).
  ENDMETHOD.
ENDCLASS.
