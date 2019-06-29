"! <p class="shorttext synchronized" lang="en">Utility for Handling Default Entity Variant</p>
CLASS zcl_dbbr_default_variant_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Loads defaultVariant</p>
    CLASS-METHODS load_default_variant
      IMPORTING
        if_show_loaded_message TYPE abap_bool OPTIONAL
        io_data                TYPE REF TO zcl_dbbr_selscreen_data
        io_selscreen_util      TYPE REF TO zcl_dbbr_selscreen_util
      RETURNING
        VALUE(rf_loaded)       TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Deletes default Variant</p>
    CLASS-METHODS delete_default_variant
      IMPORTING
        io_selscreen_util TYPE REF TO zcl_dbbr_selscreen_util.
    "! <p class="shorttext synchronized" lang="en">Creates default variant</p>
    CLASS-METHODS create_default_variant
      IMPORTING
        io_data           TYPE REF TO zcl_dbbr_selscreen_data
        io_selscreen_util TYPE REF TO zcl_dbbr_selscreen_util.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_default_variant_util IMPLEMENTATION.

  METHOD load_default_variant.
    io_selscreen_util->get_entity_information(
      IMPORTING ev_type       = DATA(lv_entity_type)
                ev_entity_raw = DATA(lv_entity_name)
                ev_entity_id  = DATA(lv_entity_id)
    ).
    DATA(ls_variant) = zcl_dbbr_variant_loader=>create_default_variant_loader(
        iv_entity_type       = lv_entity_type
        iv_entity_id         = lv_entity_id
        ir_t_multi_or        = io_data->get_multi_or_all( )
        ir_t_selfields       = io_data->mr_t_table_data
        ir_t_selfields_multi = io_data->mr_t_selfields_multi
        ir_tabfields         = io_data->mo_tabfield_list
        ir_s_global_data     = io_data->mr_s_global_data
        ir_tabfields_grouped = io_data->mo_tabfield_aggr_list
    )->load( ).

    rf_loaded = xsdbool( ls_variant IS NOT INITIAL ).

    IF rf_loaded = abap_true.
      IF if_show_loaded_message = abap_true.
        MESSAGE s097(zdbbr_info).
      ENDIF.
    ELSE.
      IF if_show_loaded_message = abap_true.
        MESSAGE s098(zdbbr_info) WITH lv_entity_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD delete_default_variant.
    io_selscreen_util->get_entity_information(
      IMPORTING ev_type       = DATA(lv_entity_type)
                ev_entity_raw = DATA(lv_entity_name)
                ev_entity_id  = DATA(lv_entity_id)
    ).
    DATA(lf_deleted) = zcl_dbbr_variant_factory=>delete_default_variant(
        iv_entity_id   = lv_entity_id
        iv_entity_type = lv_entity_type
    ).
    IF lf_deleted = abap_true.
      MESSAGE s096(zdbbr_info) WITH lv_entity_name.
    ELSE.
      MESSAGE s098(zdbbr_info) WITH lv_entity_name.
    ENDIF.
  ENDMETHOD.

  METHOD create_default_variant.
*...if there is an active join-saving is prevented
    IF io_data->get_mode( ) = zif_dbbr_c_selscreen_mode=>table AND
       io_data->is_join_active( ).
      MESSAGE e044(zdbbr_exception) DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    io_selscreen_util->get_entity_information(
      IMPORTING ev_entity_raw = DATA(lv_entity_name)
                ev_entity_id  = DATA(lv_entity_id)
    ).

    DATA(lr_variant_controller) = NEW zcl_dbbr_variant_controller(
        iv_screen_mode          = io_data->get_mode( )
        if_default_variant_mode = abap_true
        iv_mode                 = zcl_dbbr_variant_controller=>c_modes-save
        iv_entity_id            = lv_entity_id
        iv_entity_name          = lv_entity_name
        ir_t_multi_or           = io_data->get_multi_or_all( )
        ir_s_global_data        = io_data->mr_s_global_data
        ir_t_selfields          = io_data->mr_t_table_data
        ir_t_selfields_multi    = io_data->mr_t_selfields_multi
        io_tabfields            = io_data->mo_tabfield_list
        io_tabfields_grpd       = io_data->mo_tabfield_aggr_list
    ).

    lr_variant_controller->zif_uitb_screen_controller~call_screen( ).
  ENDMETHOD.

ENDCLASS.
