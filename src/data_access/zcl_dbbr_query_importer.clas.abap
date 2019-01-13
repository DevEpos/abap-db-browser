CLASS zcl_dbbr_query_importer DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_importer
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_query_name_so TYPE zif_dbbr_global_types=>ty_query_name_range OPTIONAL
        it_created_by_so  TYPE zif_dbbr_global_types=>ty_created_by_range OPTIONAL
        it_description_so TYPE zif_dbbr_global_types=>ty_ddtext_range OPTIONAL
        it_first_table_so TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_any_tables_so  TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_all_tables_so  TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_only_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_none_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL.
  PROTECTED SECTION.
    METHODS: create_internal_table_ref REDEFINITION,
      write_no_data_found_message REDEFINITION,
      filter_data REDEFINITION,
      process_import_data REDEFINITION,
      persist_import_data REDEFINITION.
    METHODS: write_start_message REDEFINITION.
  PRIVATE SECTION.
    DATA mt_query_name_so TYPE zif_dbbr_global_types=>ty_query_name_range.
    DATA mt_created_by_so TYPE zif_dbbr_global_types=>ty_created_by_range .
    DATA mt_first_table_so TYPE zif_dbbr_global_types=>ty_tabname16_range .
    DATA mt_any_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range .
    DATA mt_all_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range .
    DATA mt_only_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range .
    DATA mt_none_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range .
    DATA mt_description_so TYPE zif_dbbr_global_types=>ty_ddtext_range .
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory .

    METHODS validate_query_names .
    METHODS edit_queries .
ENDCLASS.



CLASS ZCL_DBBR_query_IMPORTER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_default_file_name = 'DB-Browser-querys' ).

    mt_query_name_so = it_query_name_so.
    mt_created_by_so  = it_created_by_so.
    mt_first_table_so = it_first_table_so.
    mt_any_tables_so  = it_any_tables_so.
    mt_all_tables_so  = it_all_tables_so.
    mt_only_tables_so = it_only_tables_so.
    mt_none_tables_so = it_none_tables_so.
    mt_description_so = it_description_so.

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
    ENDLOOP.
  ENDMETHOD.


  METHOD filter_data.
    FIELD-SYMBOLS: <lt_queries> TYPE zdbbr_query_data_itab.
    ASSIGN mr_import_data->* TO <lt_queries>.

    FIELD-SYMBOLS: <ls_table_selopt> LIKE LINE OF mt_all_tables_so.

    DELETE <lt_queries> WHERE primary_table NOT IN mt_first_table_so.
    DELETE <lt_queries> WHERE created_by NOT IN mt_created_by_so.

    IF <lt_queries> IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT <lt_queries> ASSIGNING FIELD-SYMBOL(<ls_query>).

      " only include query that match the passed names
      IF <ls_query>-query_name NOT IN mt_query_name_so.
        DELETE <lt_queries>.
        CONTINUE.
      ENDIF.

      DATA(lf_one) = abap_false.
      DATA(lf_all) = abap_true.
      DATA(lf_only) = abap_false.
      DATA(lf_none) = abap_true.
      DATA(lf_query_ok) = abap_true.

      LOOP AT <ls_query>-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
        IF mt_any_tables_so IS NOT INITIAL AND <ls_table>-tabname IN mt_any_tables_so.
          lf_one = abap_true.
          EXIT.
        ENDIF.

        IF mt_all_tables_so IS NOT INITIAL AND <ls_table>-tabname NOT IN mt_all_tables_so.
          lf_all = abap_false.
          EXIT.
        ENDIF.

        IF mt_only_tables_so IS NOT INITIAL AND <ls_table>-tabname NOT IN mt_only_tables_so.
          lf_only = abap_false.
          EXIT.
        ENDIF.

        IF mt_none_tables_so IS NOT INITIAL AND <ls_table>-tabname IN mt_none_tables_so.
          lf_none = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF mt_any_tables_so IS NOT INITIAL AND lf_one = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF mt_all_tables_so IS NOT INITIAL AND lf_all = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF mt_only_tables_so IS NOT INITIAL AND lf_only = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF mt_none_tables_so IS NOT INITIAL AND lf_none = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF lf_query_ok = abap_false.
        DELETE <lt_queries>.
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
