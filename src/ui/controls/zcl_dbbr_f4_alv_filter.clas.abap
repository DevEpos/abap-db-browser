"! <p class="shorttext synchronized" lang="en">ALV Filter for F4 Value help</p>
CLASS zcl_dbbr_f4_alv_filter DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_gui_control.

    TYPES:
      BEGIN OF ty_s_f4_filter,
        field_text  TYPE ddtext,
        option      TYPE ddoption,
        option_icon TYPE iconbutton,
        tabname     TYPE tabname,
        fieldname   TYPE fieldname,
      END OF ty_s_f4_filter.

    TYPES: ty_t_f4_filter TYPE STANDARD TABLE OF ty_s_f4_filter.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        it_selfield TYPE zdbbr_selfield_itab
        io_parent   TYPE REF TO cl_gui_container.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_parent TYPE REF TO cl_gui_container.
    DATA mo_alv TYPE REF TO zcl_uitb_alv.
    DATA mt_filter TYPE ty_t_f4_filter.

    "! <p class="shorttext synchronized" lang="en">Creates the ALV for the filtering</p>
    METHODS create_alv.
ENDCLASS.



CLASS zcl_dbbr_f4_alv_filter IMPLEMENTATION.

  METHOD constructor.
    mo_parent = io_parent.

    LOOP AT it_selfield ASSIGNING FIELD-SYMBOL(<ls_selfield>).
      mt_filter = VALUE #(
        ( fieldname = <ls_selfield>-fieldname
          field_text = <ls_selfield>-description
          )
      ).
    ENDLOOP.

    create_alv( ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    IF mo_alv IS BOUND.
      mo_alv->zif_uitb_gui_control~focus( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    IF mo_alv IS BOUND.
      rf_has_focus = mo_alv->zif_uitb_gui_control~has_focus( ).
    ENDIF.
  ENDMETHOD.


  METHOD create_alv.
  data: lo_col type ref to zcl_uitb_alv_column.

    mo_alv = zcl_uitb_alv=>create_alv(
       ir_data                 = REF #( mt_filter )
       ir_container            = mo_parent
       if_editable             = abap_true
    ).

    TRY.
        DATA(lo_disp_settings) = mo_alv->get_display_settings( ).
        lo_disp_settings->hide_toolbar( ).
        lo_disp_settings->set_row_marks( abap_false ).

        DATA(lo_cols) = mo_alv->get_columns( ).
        lo_cols->set_optimized( ).

        lo_cols->get_column( 'OPTION' )->set_technical( ).

        lo_col = lo_cols->get_column( 'OPTION_ICON' ).
        lo_col->set_descriptions( iv_long = 'Opt.' ).
        lo_col->set_icon( ).

        mo_alv->display( ).
      CATCH zcx_uitb_alv_error.
        "handle exception
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
