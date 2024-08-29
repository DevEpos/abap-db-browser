CLASS zcl_dbbr_protocol DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_dbbr_protocol.

    METHODS add_info
      IMPORTING
        iv_message TYPE string.

    METHODS add_info_from_sy.

    METHODS add_exception_message
      IMPORTING
        ir_exception_msg TYPE REF TO zif_sat_exception_message.

    METHODS add_warning_from_sy.
    METHODS add_error_from_sy.

    METHODS add_warning
      IMPORTING
        iv_message TYPE string.

    METHODS add_error
      IMPORTING
        iv_message TYPE string.

    METHODS show_protocol
      IMPORTING
        if_show_fullscreen TYPE boolean OPTIONAL
        if_show_as_dialog  TYPE boolean OPTIONAL.

    METHODS close_protocol.
    METHODS clear.

  PRIVATE SECTION.
    CLASS-DATA sr_instance TYPE REF TO zcl_dbbr_protocol.

    DATA mt_messages TYPE STANDARD TABLE OF zdbbr_protocol_info.
    DATA mr_alv TYPE REF TO zcl_dbbr_protocol_alv.

    METHODS get_sy_message
      RETURNING
        VALUE(rv_message) TYPE string.
ENDCLASS.


CLASS zcl_dbbr_protocol IMPLEMENTATION.
  METHOD add_error.
    APPEND VALUE #( type_icon = icon_red_light
                    message   = iv_message )
           TO mt_messages.
  ENDMETHOD.

  METHOD add_error_from_sy.
    DATA(lv_message) = get_sy_message( ).

    IF lv_message IS NOT INITIAL.
      add_error( lv_message ).
    ENDIF.
  ENDMETHOD.

  METHOD add_exception_message.
    add_error( iv_message = ir_exception_msg->get_message( ) ).
  ENDMETHOD.

  METHOD add_info.
    APPEND VALUE #( type_icon = icon_green_light
                    message   = iv_message )
           TO mt_messages.
  ENDMETHOD.

  METHOD add_info_from_sy.
    DATA(lv_message) = get_sy_message( ).

    IF lv_message IS NOT INITIAL.
      add_info( lv_message ).
    ENDIF.
  ENDMETHOD.

  METHOD add_warning.
    APPEND VALUE #( type_icon = icon_yellow_light
                    message   = iv_message )
           TO mt_messages.
  ENDMETHOD.

  METHOD add_warning_from_sy.
    DATA(lv_message) = get_sy_message( ).

    IF lv_message IS NOT INITIAL.
      add_warning( lv_message ).
    ENDIF.
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_messages.
  ENDMETHOD.

  METHOD close_protocol.
    IF mr_alv IS NOT INITIAL.
      mr_alv->close( ).
      CLEAR mr_alv.
    ENDIF.
  ENDMETHOD.

  METHOD get_instance.
    IF sr_instance IS INITIAL.
      sr_instance = NEW #( ).
    ENDIF.

    rr_instance = sr_instance.
  ENDMETHOD.

  METHOD get_sy_message.
    MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4 INTO rv_message.
  ENDMETHOD.

  METHOD show_protocol.
    CHECK mt_messages IS NOT INITIAL.

    close_protocol( ).

    mr_alv = NEW zcl_dbbr_protocol_alv( mt_messages ).

    IF if_show_fullscreen = abap_true.
      mr_alv->show( ).
    ELSEIF if_show_as_dialog = abap_true.
      mr_alv->show( if_show_as_dialog = abap_true ).
    ELSE.
      mr_alv->show_docked( iv_dock_at    = cl_gui_docking_container=>dock_at_bottom
                           iv_dock_ratio = 20 ).
    ENDIF.

    clear( ).
  ENDMETHOD.
ENDCLASS.
