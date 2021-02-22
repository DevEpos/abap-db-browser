"! <p class="shorttext synchronized" lang="en">Class for calling dialogs</p>
CLASS zcl_dbbr_dialogs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Show user settings dialog for DB Browser</p>
    CLASS-METHODS show_user_settings
      IMPORTING
        if_disable_save TYPE abap_bool OPTIONAL
        iv_start_dynnr  TYPE sy-dynnr OPTIONAL
        iv_start_tab    TYPE string OPTIONAL
      CHANGING
        !cs_settings    TYPE zdbbr_user_settings_a .
    "! <p class="shorttext synchronized" lang="en">Shows the multi input selection screen for a table field</p>
    CLASS-METHODS show_multi_select
      IMPORTING
        io_custom_f4_map  TYPE REF TO zcl_dbbr_custom_f4_map
        is_screen_size    TYPE zif_uitb_screen_controller=>ty_s_screen_size OPTIONAL
      EXPORTING
        ef_changed        TYPE abap_bool
      CHANGING
        cs_selfield       TYPE zdbbr_selfield
        ct_selfield_multi TYPE zdbbr_selfield_itab .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_dialogs IMPLEMENTATION.

  METHOD show_multi_select.
    CHECK cs_selfield-is_parameter = abap_false OR
          ( cs_selfield-is_parameter = abap_true AND
            cs_selfield-is_range_param = abap_true ).

    DATA(lo_multi_select_table) = NEW zcl_dbbr_multi_select_table( ).
    lo_multi_select_table->init_table(
        is_template = cs_selfield
        it_multi    = ct_selfield_multi
    ).
    " create the controller
    DATA(lo_multi_select_controller) = NEW zcl_dbbr_multi_select_ctlr(
      io_custom_f4_map        = io_custom_f4_map
      ir_selection_table      = lo_multi_select_table
    ).

    lo_multi_select_controller->zif_uitb_screen_controller~call_screen( is_screen_size = is_screen_size ).

    ef_changed = lo_multi_select_controller->should_data_be_transferred( ).

    CHECK ef_changed = abap_true.

    lo_multi_select_controller->transfer_data(
      CHANGING
        cs_selfield       = cs_selfield
        ct_selfield_multi = ct_selfield_multi
    ).
  ENDMETHOD.


  METHOD show_user_settings.
    DATA(lr_user_settings) = NEW zcl_dbbr_user_settings_sc(
        is_user_settings = zcl_dbbr_usersettings_factory=>get_settings( )
        if_disable_save  = if_disable_save
        iv_start_tab     = iv_start_tab
        iv_start_dynnr   = iv_start_dynnr
    ).
    lr_user_settings->zif_uitb_screen_controller~call_screen( ).

    IF lr_user_settings->zif_uitb_screen_controller~was_not_cancelled( ).
      cs_settings = lr_user_settings->get_settings( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
