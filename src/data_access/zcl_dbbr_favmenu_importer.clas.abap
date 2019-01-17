CLASS zcl_dbbr_favmenu_importer DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_generic_importer
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
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
    DATA mr_favmenu_f TYPE REF TO zcl_dbbr_favmenu_factory.
ENDCLASS.



CLASS zcl_dbbr_favmenu_importer IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_default_file_name = 'DB-Browser-Favorites' ).
    mr_favmenu_f = NEW #( ).
  ENDMETHOD.


  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_favmenu_itab( ).
  ENDMETHOD.


  METHOD persist_import_data.
    ASSIGN CAST zdbbr_favmenu_itab( mr_import_data )->* TO FIELD-SYMBOL(<lt_favorites>).

    mr_favmenu_f->update_favorites( <lt_favorites> ).

    MESSAGE i045(zdbbr_info) WITH |{ lines( <lt_favorites> ) }| INTO DATA(lv_dummy).
    mr_log->add_from_sy(
        if_newobj = abap_false
        iv_level  = zif_uitb_c_protocol_level=>info
    ).
  ENDMETHOD.


  METHOD process_import_data.
    ASSIGN CAST zdbbr_favmenu_itab( mr_import_data )->* TO FIELD-SYMBOL(<lt_favorites>).

    " check what kind of data the import file contained
    DATA(lf_private_favs_in_file) = xsdbool( line_exists( <lt_favorites>[ uname = sy-uname ] ) ).
    DATA(lf_global_favs_in_file) = xsdbool( line_exists( <lt_favorites>[ uname = space ] ) ).

    DATA(lf_private_favs_in_db) = mr_favmenu_f->user_favorites_exist( ).
    DATA(lf_global_favs_in_db) = mr_favmenu_f->global_favorites_exist( ).

    IF lf_private_favs_in_file = abap_true AND
       lf_private_favs_in_db = abap_true.

      mr_favmenu_f->delete_user_favorites( ).
    ENDIF.

    IF lf_global_favs_in_file = abap_true AND
       lf_global_favs_in_db = abap_true.

      mr_favmenu_f->delete_global_favorites( ).
    ENDIF.
  ENDMETHOD.


  METHOD write_no_data_found_message.
    MESSAGE w020(zdbbr_exception) INTO DATA(lv_dummy).
    mr_log->add_from_sy(
        if_newobj = abap_false
        iv_level  = zif_uitb_c_protocol_level=>warning
    ).
  ENDMETHOD.


  METHOD write_start_message.
    MESSAGE i042(zdbbr_info) WITH 'Favoriten' INTO DATA(lv_dummy).
    mr_log->add_from_sy( if_newobj = abap_false
                         iv_level  = zif_uitb_c_protocol_level=>success ).
  ENDMETHOD.
ENDCLASS.
