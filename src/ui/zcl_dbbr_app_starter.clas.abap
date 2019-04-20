"! <p class="shorttext synchronized" lang="en">Starts screen for DB Browser</p>
CLASS zcl_dbbr_app_starter DEFINITION
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
        !iv_current_line   TYPE sy-tabix
        !ir_custom_f4_map  TYPE REF TO zcl_dbbr_custom_f4_map
      CHANGING
        !ct_selfield       TYPE zdbbr_selfield_itab
        !ct_selfield_multi TYPE zdbbr_selfield_itab .
    "! <p class="shorttext synchronized" lang="en">Start new DB Browser selection from memory</p>
    CLASS-METHODS start_selection_from_memory
      RETURNING
        VALUE(result) TYPE REF TO zcl_dbbr_selection_controller .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_app_starter IMPLEMENTATION.


  METHOD show_multi_select.
    " get current line for index
    ASSIGN ct_selfield[ iv_current_line ] TO FIELD-SYMBOL(<ls_selfield>).
    IF sy-subrc <> 0. " line was not found
      RETURN.
    ENDIF.

    CHECK <ls_selfield>-is_parameter = abap_false OR
          ( <ls_selfield>-is_parameter = abap_true AND
            <ls_selfield>-is_range_param = abap_true ).

    DATA(lr_multi_select_table) = NEW zcl_dbbr_multi_select_table( ).
    lr_multi_select_table->init_table(
        is_template = <ls_selfield>
        it_multi    = ct_selfield_multi
    ).
    " create the controller
    DATA(lr_multi_select_controller) = NEW zcl_dbbr_multi_select_ctlr(
      ir_custom_f4_map        = ir_custom_f4_map
      ir_selection_table      = lr_multi_select_table
    ).

    lr_multi_select_controller->zif_uitb_screen_controller~call_screen( ).

    lr_multi_select_controller->transfer_data(
      CHANGING
        cs_selfield       = <ls_selfield>
        ct_selfield_multi = ct_selfield_multi
    ).
  ENDMETHOD.


  METHOD show_user_settings.
    DATA(lr_user_settings) = NEW zcl_dbbr_user_settings_sc(
        is_user_settings = cs_settings
        if_disable_save  = if_disable_save
        iv_start_tab     = iv_start_tab
        iv_start_dynnr   = iv_start_dynnr
    ).
    lr_user_settings->zif_uitb_screen_controller~call_screen( ).

    IF lr_user_settings->zif_uitb_screen_controller~was_not_cancelled( ).
      cs_settings = lr_user_settings->get_settings( ).
    ENDIF.
  ENDMETHOD.


  METHOD start_selection_from_memory.
    DATA: ls_controller_data TYPE zdbbr_sel_ctrl_serialized,
          lr_t_for_all_data  TYPE REF TO data,
          lv_mem_id          TYPE char32.

    FIELD-SYMBOLS: <lt_for_all_data> TYPE table.

    lv_mem_id = zif_dbbr_c_report_id=>main && sy-uname.
    IMPORT
      serialized = ls_controller_data
    FROM MEMORY ID lv_mem_id.

*... clear memory
    FREE MEMORY ID lv_mem_id.

    CHECK ls_controller_data IS NOT INITIAL.

    IF ls_controller_data-navigation_info IS NOT INITIAL.
      lr_t_for_all_data = zcl_dbbr_dictionary_helper=>build_dynamic_std_table(
        VALUE #(
          FOR assoc_field IN ls_controller_data-navigation_info-fields
          ( tabname   = ls_controller_data-navigation_info-ref_cds_view
            fieldname = assoc_field-name )
        )
      ).
      ASSIGN lr_t_for_all_data->* TO <lt_for_all_data>.

      lv_mem_id = lv_mem_id && 'FORALLTAB'.
      IMPORT
        data = <lt_for_all_data>
      FROM MEMORY ID lv_mem_id.
    ENDIF.

*... clear memory
    FREE MEMORY ID lv_mem_id.

*... create controller instance
    DATA(lr_selection_controller) = zcl_dbbr_selection_controller=>create_controller_from_data(
      is_controller_serialized = ls_controller_data
      ir_t_for_all_data        = lr_t_for_all_data
      if_not_first_screen_call = abap_true
    ).
    result = lr_selection_controller.
    lr_selection_controller->execute_selection( ).
  ENDMETHOD.
ENDCLASS.
