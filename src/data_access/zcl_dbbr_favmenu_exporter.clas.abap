CLASS zcl_dbbr_favmenu_exporter DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_uitb_generic_exporter
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        if_global_favs  TYPE boolean
        if_private_favs TYPE boolean.
  PROTECTED       SECTION.
    METHODS: build_data_for_export REDEFINITION,
      write_success_message REDEFINITION,
      write_error_message REDEFINITION.
    METHODS: create_internal_table_ref REDEFINITION.
  PRIVATE SECTION.
    DATA mf_global_favs TYPE boolean.
    DATA mf_private_favs TYPE boolean.
ENDCLASS.



CLASS zcl_dbbr_favmenu_exporter IMPLEMENTATION.


  METHOD build_data_for_export.
    FIELD-SYMBOLS: <lt_favorites> TYPE zdbbr_favmenu_itab.

    ASSIGN mr_source_data_tab->* TO <lt_favorites>.

    DATA(lr_favmenu_f) = NEW zcl_dbbr_favmenu_factory( ).

    IF mf_private_favs = abap_true.
      lr_favmenu_f->get_favorites(
        EXPORTING if_global_favorites = abap_false
        IMPORTING et_favmenu_entries  = DATA(lt_favorites)
      ).
      APPEND LINES OF lt_favorites TO <lt_favorites>.
    ENDIF.

    IF mf_global_favs = abap_true.
      lr_favmenu_f->get_favorites(
        EXPORTING if_global_favorites = abap_true
        IMPORTING et_favmenu_entries  = lt_favorites
      ).
      APPEND LINES OF lt_favorites TO <lt_favorites>.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( iv_default_file_name = 'DB-Browser-Favorites' ).

    mf_global_favs = if_global_favs.
    mf_private_favs = if_private_favs.
  ENDMETHOD.


  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_favmenu_itab( ).
  ENDMETHOD.


  METHOD write_error_message.
    MESSAGE |An error occurred during the export of Favorites| TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.


  METHOD write_success_message.
    MESSAGE |Favorites have been successfully imported to { mv_full_export_path }| TYPE 'S'.
  ENDMETHOD.
ENDCLASS.
