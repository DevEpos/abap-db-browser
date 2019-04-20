"! <p class="shorttext synchronized" lang="en">Value Help for CDS View fields</p>
CLASS zcl_dbbr_cds_field_value_help DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_modal_dialog
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        iv_entity TYPE zdbbr_cds_view_name
        iv_field  TYPE fieldname.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
  PRIVATE SECTION.
    DATA mo_main_splitter TYPE REF TO zcl_uitb_gui_splitter_cont.
    DATA mv_cds_view TYPE zdbbr_cds_view_name.
    DATA mo_filter_alv TYPE REF TO zcl_dbbr_f4_alv_filter.
    DATA mv_fieldname TYPE fieldname.
ENDCLASS.



CLASS zcl_dbbr_cds_field_value_help IMPLEMENTATION.

  METHOD constructor.

    super->constructor( iv_title = |{ 'Value Help for ' }{ iv_entity }-{ iv_field }| ).
    mv_cds_view = iv_entity.
    mv_fieldname = iv_field.

  ENDMETHOD.

  METHOD create_content.
    create_control_toolbar(
      EXPORTING
        io_parent    = io_container
        it_button    = VALUE #(
          ( function  = 'TAKE'
            icon      = icon_okay
            quickinfo = |{ 'Transfer values' }| )
        )
      IMPORTING
        eo_toolbar   = DATA(mo_toolbar)
        eo_client    = DATA(lo_container)
    ).

    mo_main_splitter = NEW zcl_uitb_gui_splitter_cont(
      iv_elements = 2
      iv_size     = '30:70'
      io_parent   = lo_container
    ).

    DATA(lo_filter_container) = mo_main_splitter->get_container( 1 ).
    mo_filter_alv = NEW zcl_dbbr_f4_alv_filter(
        io_parent   = lo_filter_container
        it_selfield = VALUE #( )
    ).
    DATA(lo_alv_container) = mo_main_splitter->get_container( 2 ).
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.

  ENDMETHOD.

ENDCLASS.
