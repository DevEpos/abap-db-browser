"! <p class="shorttext synchronized" lang="en">F4 View for choosing variants</p>
CLASS zcl_dbbr_variant_f4_view DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_selection_dialog
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        iv_entity_id   TYPE zdbbr_entity_id
        iv_entity_name TYPE zdbbr_entity_id_raw OPTIONAL
        iv_entity_type TYPE zdbbr_entity_type.
    "! <p class="shorttext synchronized" lang="en">Display ALV popup to show variant options</p>
    METHODS choose_variant
      RETURNING
        VALUE(rv_variant_id) TYPE zdbbr_variant_id.
  PROTECTED SECTION.
    METHODS adjust_column
        REDEFINITION.
    METHODS get_output_table
        REDEFINITION.
    METHODS get_filtered_data
        REDEFINITION.
    METHODS set_selected_element
        REDEFINITION.
  PRIVATE SECTION.

    DATA mt_variant TYPE zdbbr_variant_info_itab.
    DATA mv_entity_id TYPE zdbbr_entity_id.
    DATA mv_entity_type TYPE zdbbr_entity_type.
    DATA mv_variant_id TYPE zdbbr_variant_id.

    "! <p class="shorttext synchronized" lang="en">Finds variants for the current entity and filter value</p>
    METHODS find_variants
      IMPORTING
        iv_name_filter    TYPE string OPTIONAL
      RETURNING
        VALUE(rt_variant) TYPE zdbbr_variant_info_itab.

ENDCLASS.



CLASS zcl_dbbr_variant_f4_view IMPLEMENTATION.
  METHOD constructor.
    DATA(lv_title) = |{ 'Choose Variant' }| &&
                     COND #( WHEN iv_entity_name IS NOT INITIAL THEN | for { iv_entity_name }| ).
    super->constructor(
        iv_title         = lv_title
        iv_filter_prompt = 'Filter by Variant Name'
    ).
    mv_entity_id = iv_entity_id.
    mv_entity_type = iv_entity_type.
  ENDMETHOD.


  METHOD choose_variant.
    CLEAR: mv_variant_id,
           mt_variant.

    mt_variant = find_variants( ).
    IF mt_variant IS INITIAL.
      MESSAGE |No Variants found| TYPE 'S'.
      RETURN.
    ENDIF.

    show(
        iv_top    = 2
        iv_left   = 20
        iv_width  = 90
        iv_height = 20
    ).

    CLEAR mo_alv.

    rv_variant_id = mv_variant_id.
  ENDMETHOD.


  METHOD find_variants.
    zcl_dbbr_variant_factory=>find_variants(
      EXPORTING
        iv_variant_name = |*{ to_upper( iv_name_filter ) }*|
        iv_entity_id    = mv_entity_id
        iv_entity_type  = mv_entity_type
      IMPORTING
        et_variant_info = rt_variant
    ).
  ENDMETHOD.

  METHOD adjust_column.
    CASE io_column->get_name( ).
      WHEN 'VARIANT_NAME'.
        io_column->set_hotspot( ).
        io_column->set_optimized( ).
      WHEN 'DESCRIPTION'.
        io_column->set_optimized( ).

      WHEN 'HAS_OUTPUT_FIELDS' OR
           'HAS_SORT_FIELDS' OR
           'HAS_CRITERIA'.
        io_column->set_optimized( ).
        io_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).

      WHEN OTHERS.
        io_column->set_technical( ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_output_table.
    rr_table = REF #( mt_variant ).
  ENDMETHOD.

  METHOD get_filtered_data.
    rr_data = NEW zdbbr_variant_info_itab( find_variants( iv_name_filter ) ).
  ENDMETHOD.

  METHOD set_selected_element.
    mv_variant_id = mt_variant[ iv_row ]-variant_id.
  ENDMETHOD.

ENDCLASS.
