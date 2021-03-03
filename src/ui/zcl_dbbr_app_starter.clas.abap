"! <p class="shorttext synchronized" lang="en">Starts screen for DB Browser</p>
CLASS zcl_dbbr_app_starter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Start new DB Browser selection from memory</p>
    CLASS-METHODS start_selection_from_memory
      RETURNING
        VALUE(result) TYPE REF TO zcl_dbbr_selection_controller .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_app_starter IMPLEMENTATION.

  METHOD start_selection_from_memory.
    DATA: ls_controller_data TYPE zif_dbbr_ty_global=>ty_sel_ctrl_serialized,
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

***    IF ls_controller_data-navigation_info IS NOT INITIAL.
***      lr_t_for_all_data = zcl_dbbr_ddic_util=>build_dynamic_std_table(
***        VALUE #(
***          FOR assoc_field IN ls_controller_data-navigation_info-fields
***          ( tabname   = ls_controller_data-navigation_info-ref_cds_view
***            fieldname = assoc_field-name )
***        )
***      ).
***      ASSIGN lr_t_for_all_data->* TO <lt_for_all_data>.
***
***      lv_mem_id = lv_mem_id && 'FORALLTAB'.
***      IMPORT
***        data = <lt_for_all_data>
***      FROM MEMORY ID lv_mem_id.
***    ENDIF.
***
****... clear memory
***    FREE MEMORY ID lv_mem_id.

*... create controller instance
    DATA(lr_selection_controller) = zcl_dbbr_selection_controller=>create_controller_from_data(
      is_controller_serialized = ls_controller_data
*      ir_t_for_all_data        = lr_t_for_all_data
      if_not_first_screen_call = abap_true
    ).
    result = lr_selection_controller.
    lr_selection_controller->execute_selection( ).
  ENDMETHOD.

ENDCLASS.
