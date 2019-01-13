CLASS zcl_dbbr_query_exporter DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_uitb_generic_exporter
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_file_name    TYPE string OPTIONAL
        !it_query_info TYPE zdbbr_query_info_ui_itab .
  PROTECTED SECTION.
    METHODS: build_data_for_export REDEFINITION,
      write_success_message REDEFINITION,
      write_error_message REDEFINITION.
    METHODS: create_internal_table_ref REDEFINITION.
  PRIVATE SECTION.
    DATA mt_query_info TYPE zdbbr_query_info_ui_itab .

ENDCLASS.



CLASS zcl_dbbr_query_exporter IMPLEMENTATION.


  METHOD build_data_for_export.
    FIELD-SYMBOLS: <lt_table> TYPE zdbbr_query_data_itab.

    ASSIGN mr_source_data_tab->* TO <lt_table>.

    DATA(lr_query_f) = NEW zcl_dbbr_query_factory( ).

    LOOP AT mt_query_info ASSIGNING FIELD-SYMBOL(<ls_query>).
      APPEND lr_query_f->get_query_by_id(
        EXPORTING
          iv_query_id              = <ls_query>-query_id
          if_load_jump_destinations = abap_true
          if_load_formulas          = abap_true
      ) TO <lt_table>.
    ENDLOOP.

    CLEAR mt_query_info.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
        iv_default_file_name = COND #(
          WHEN iv_file_name IS NOT INITIAL THEN iv_file_name ELSE 'DB-Browser-querys'
        )
    ).
    mt_query_info = it_query_info.
  ENDMETHOD.


  METHOD create_internal_table_ref.
    rr_table_ref = NEW zdbbr_query_data_itab( ).
  ENDMETHOD.


  METHOD write_error_message.
    MESSAGE s009(zdbbr_exception) DISPLAY LIKE 'E'.
  ENDMETHOD.


  METHOD write_success_message.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_source_data_tab->* TO <lt_table>.

    MESSAGE s038(zdbbr_info) WITH |{ lines( <lt_table> ) }|
                                   mv_full_export_path.
  ENDMETHOD.
ENDCLASS.
