"! <p class="shorttext synchronized" lang="en">Utility class for Variants of an Entity</p>
CLASS zcl_dbbr_variant_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Choose variant for given entity</p>
    CLASS-METHODS select_and_load_variant
      IMPORTING
        iv_entity_id         TYPE zdbbr_entity_id
        iv_entity_name       TYPE zdbbr_entity_id_raw OPTIONAL
        iv_entity_type       TYPE zdbbr_entity_type
        ir_s_global_data     TYPE REF TO zdbbr_global_data
        ir_t_selfields       TYPE REF TO zdbbr_selfield_itab OPTIONAL
        ir_t_selfields_multi TYPE REF TO zdbbr_selfield_itab OPTIONAL
        ir_t_multi_or        TYPE REF TO zdbbr_or_seltab_itab OPTIONAL
        io_tabfields         TYPE REF TO zcl_dbbr_tabfield_list
        io_tabfields_grpd    TYPE REF TO zcl_dbbr_tabfield_list .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_variant_util IMPLEMENTATION.

  METHOD select_and_load_variant.
    DATA(lo_variant_f4) = NEW zcl_dbbr_variant_f4_view(
      iv_entity_id   = iv_entity_id
      iv_entity_name = iv_entity_name
      iv_entity_type = iv_entity_type
    ).

    DATA(lv_variant_id) = lo_variant_f4->choose_variant( ).
    CHECK lv_variant_id IS NOT INITIAL.

    DATA(lo_variant_loader) = zcl_dbbr_variant_loader=>create_variant_loader(
        iv_variant_id        = lv_variant_id
        iv_entity_id         = iv_entity_id
        iv_entity_type       = iv_entity_type
        ir_t_multi_or        = ir_t_multi_or
        ir_t_selfields       = ir_t_selfields
        ir_t_selfields_multi = ir_t_selfields_multi
        ir_tabfields         = io_tabfields
        ir_s_global_data     = ir_s_global_data
        ir_tabfields_grouped = io_tabfields_grpd
    ).

    TRY.
        DATA(ls_variant_info) = lo_variant_loader->load( ).
      CATCH zcx_dbbr_validation_exception INTO DATA(lr_valid_error).
        lr_valid_error->show_message( ).
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
