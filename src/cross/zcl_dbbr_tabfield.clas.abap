CLASS ZCL_DBBR_tabfield DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        ir_table_field_info TYPE REF TO ZDBBR_tabfield_info_ui
        ir_text_field_info type ref to ZDBBR_tabfield_info_ui optional
        !iv_mode                   TYPE ZDBBR_field_chooser_mode .
    METHODS set_custom_active
      IMPORTING
        !if_active TYPE boolean .
    METHODS set_custom_order
      IMPORTING
        !iv_order TYPE numc3 .
    METHODS set_text_active
      IMPORTING
        !if_active TYPE boolean .
    methods set_text_output_order
    importing
    iv_order type numc3.
    METHODS get_tabfield_info
      RETURNING
        VALUE(rs_info) TYPE ZDBBR_tabfield_info_ui .
    METHODS get_tabfield_ref
      RETURNING
        VALUE(rr_info) TYPE REF TO ZDBBR_tabfield_info_ui.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mr_table_field_info TYPE REF TO ZDBBR_tabfield_info_ui,
          mv_mode             TYPE ZDBBR_field_chooser_mode,
          mr_text_field_info TYPE REF TO ZDBBR_tabfield_info_ui.
ENDCLASS.



CLASS ZCL_DBBR_tabfield IMPLEMENTATION.


  METHOD constructor.
    mr_table_field_info = ir_table_field_info.
    mr_text_field_info = ir_text_field_info.
    mv_mode = iv_mode.
  ENDMETHOD.


  METHOD get_tabfield_info.
    FIELD-SYMBOLS: <ls_tabfield> TYPE ZDBBR_tabfield_info_ui.
    ASSIGN mr_table_field_info->* TO <ls_tabfield>.

    rs_info = <ls_tabfield>.
  ENDMETHOD.


  METHOD set_custom_active.
    FIELD-SYMBOLS: <ls_tabfield> TYPE ZDBBR_tabfield_info_ui.
    ASSIGN mr_table_field_info->* TO <ls_tabfield>.

    CASE mv_mode.
      WHEN zif_dbbr_c_global=>c_field_chooser_modes-output.
        <ls_tabfield>-output_active = if_active.
      WHEN zif_dbbr_c_global=>c_field_chooser_modes-selection.
        <ls_tabfield>-selection_active = if_active.
    ENDCASE.
  ENDMETHOD.


  METHOD set_custom_order.
    FIELD-SYMBOLS: <ls_tabfield> TYPE ZDBBR_tabfield_info_ui.
    ASSIGN mr_table_field_info->* TO <ls_tabfield>.

    CASE mv_mode.
      WHEN zif_dbbr_c_global=>c_field_chooser_modes-output.
        <ls_tabfield>-output_order = iv_order.
      WHEN zif_dbbr_c_global=>c_field_chooser_modes-selection.
        <ls_tabfield>-selection_order = iv_order.
    ENDCASE.

  ENDMETHOD.


  METHOD set_text_active.
    CHECK mr_text_field_info IS NOT INITIAL.

    mr_text_field_info->output_active = if_active.
  ENDMETHOD.

  METHOD get_tabfield_ref.
    rr_info = cast #( mr_table_field_info ).
  ENDMETHOD.

  METHOD set_text_output_order.
    check mr_text_field_info is not initial.

    mr_text_field_info->output_order = iv_order.
  ENDMETHOD.

ENDCLASS.
