CLASS zcl_dbbr_protocol_alv DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_fullscreen_alv_table FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_messages TYPE STANDARD TABLE.

  PROTECTED SECTION.
    METHODS select_data         REDEFINITION.
    METHODS get_report_id       REDEFINITION.
    METHODS get_status          REDEFINITION.
    METHODS on_user_command     REDEFINITION.
    METHODS get_title           REDEFINITION.
    METHODS adjust_columns      REDEFINITION.
    METHODS adjust_functions    REDEFINITION.
    METHODS get_table_reference REDEFINITION.

  PRIVATE SECTION.
    DATA mt_protocol TYPE STANDARD TABLE OF zdbbr_protocol_info.
ENDCLASS.


CLASS zcl_dbbr_protocol_alv IMPLEMENTATION.
  METHOD adjust_columns.
    DATA lr_column TYPE REF TO cl_salv_column.

    lr_column ?= ir_columns->get_column( 'TYPE_ICON' ).
    lr_column->set_short_text( 'Typ' ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( space ).

    lr_column ?= ir_columns->get_column( 'MESSAGE' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( 'Nachricht' ).
    lr_column->set_long_text( space ).
  ENDMETHOD.

  METHOD adjust_functions.
    super->adjust_functions( ir_functions = ir_functions ).

    IF mf_show_as_dialog = abap_true.
      ir_functions->set_all( abap_false ).
    ELSEIF mf_show_docked = abap_true.
      ir_functions->set_all( abap_false ).
      ir_functions->add_function( name     = 'CLOSE'
                                  icon     = '@3X@'
                                  tooltip  = 'Schließen'
                                  position = if_salv_c_function_position=>right_of_salv_functions ). " Funktion Positionierung
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

    mt_protocol = it_messages.
  ENDMETHOD.

  METHOD get_report_id.
    rv_repid = zif_dbbr_c_report_id=>main.
  ENDMETHOD.

  METHOD get_status.
    rv_status = 'PROTOCOL'.
  ENDMETHOD.

  METHOD get_table_reference.
    rr_table_ref = REF #( mt_protocol ).
  ENDMETHOD.

  METHOD get_title.
    rv_title = 'Messages'.
  ENDMETHOD.

  METHOD on_user_command.
    IF e_salv_function = 'CLOSE'.
      close( ).
    ENDIF.
  ENDMETHOD.

  METHOD select_data ##NEEDED.
    " nothing to select
  ENDMETHOD.
ENDCLASS.
