class ZCL_DBBR_VARIANT_CREATOR definition
  public
  final
  create public .

public section.

  class-methods CREATE_VARIANT
    importing
      !IV_VARIANT_ID type ZDBBR_VARIANT_ID optional
      !IV_VARIANT_NAME type ZDBBR_VARIANT_NAME optional
      !IV_ENTITY_ID type ZDBBR_ENTITY_ID
      !IV_ENTITY_TYPE type ZDBBR_ENTITY_TYPE
      !IV_VARIANT_DESCRIPTION type DDTEXT
      !IF_HAS_OUTPUT_FIELDS type ABAP_BOOL optional
      !IF_HAS_SORT_FIELDS type ABAP_BOOL optional
      !IT_SELFIELDS type ZDBBR_SELFIELD_ITAB
      !IT_MULTI_SELFIELDS type ZDBBR_SELFIELD_ITAB optional
      !IT_MULTI_OR type ZDBBR_OR_SELTAB_ITAB optional
    returning
      value(RS_VARIANT) type ZDBBR_VARIANT_DATA .
  PROTECTED SECTION.
private section.

  class-methods ADD_SPECIAL_VARIANT_DATA
    importing
      !IV_LAYOUT_DATA_TYPE type ZDBBR_VARIANT_DATATYPE
      !IV_LAYOUT_DATA_LOW type ANY optional
      !IV_LAYOUT_DATA_HIGH type ANY optional
    changing
      !CS_LAYOUT_DATA type ZDBBR_VARDATA
      !CT_LAYOUT_DATA type ZDBBR_VARDATA_ITAB
      !CV_LINE_COUNTER type SY-TABIX .
  class-methods ADD_VALUE_VARIANT_DATA
    importing
      !IS_SELFIELD type ZDBBR_SELFIELD
      !IT_MULTI type ZDBBR_SELFIELD_ITAB
    changing
      !CS_LAYOUT_DATA type ZDBBR_VARDATA
      !CT_LAYOUT_DATA type ZDBBR_VARDATA_ITAB
      !CV_LINE_COUNTER type SY-TABIX .
  class-methods ADD_MULTI_OR_VARIANT_DATA
    importing
      !IS_SELFIELD_INFO type ZDBBR_SELFIELD_INFO
    changing
      !CS_LAYOUT_DATA type ZDBBR_VARDATA
      !CT_LAYOUT_DATA type ZDBBR_VARDATA_ITAB .
ENDCLASS.



CLASS ZCL_DBBR_VARIANT_CREATOR IMPLEMENTATION.


  METHOD add_multi_or_variant_data.
    IF is_selfield_info-low IS NOT INITIAL OR
       is_selfield_info-high IS NOT INITIAL OR
       is_selfield_info-option IS NOT INITIAL.

      DATA(lv_line_counter) = 1.
      cs_layout_data-counter   = lv_line_counter.
      cs_layout_data-low_val   = is_selfield_info-low.
      cs_layout_data-high_val  = is_selfield_info-high.
      cs_layout_data-sign_val  = is_selfield_info-sign.
      cs_layout_data-data_type = is_selfield_info-option.
      APPEND cs_layout_data TO ct_layout_data.
    ENDIF.

    " get multi select values
    LOOP AT is_selfield_info-multi_values ASSIGNING FIELD-SYMBOL(<ls_select_multi>)
       WHERE ( low IS NOT INITIAL OR high IS NOT INITIAL ).
      ADD 1 TO lv_line_counter.
      cs_layout_data-counter   = lv_line_counter.
      cs_layout_data-low_val   = <ls_select_multi>-low.
      cs_layout_data-high_val  = <ls_select_multi>-high.
      cs_layout_data-sign_val  = <ls_select_multi>-sign.
      cs_layout_data-data_type = <ls_select_multi>-option.
      APPEND cs_layout_data TO ct_layout_data.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_special_variant_data.

*&---------------------------------------------------------------------*
*& Description: Adds a special line to the layout data table
*&---------------------------------------------------------------------*
    ADD 1 TO cv_line_counter.
    cs_layout_data-counter = cv_line_counter.
    CLEAR: cs_layout_data-high_val,
           cs_layout_data-low_val.
    IF iv_layout_data_low IS SUPPLIED.
      cs_layout_data-low_val = iv_layout_data_low.
    ENDIF.
    IF iv_layout_data_high IS SUPPLIED.
      cs_layout_data-high_val = iv_layout_data_high.
    ENDIF.
    cs_layout_data-sign_val = zif_dbbr_global_consts=>gc_options-i.
    cs_layout_data-data_type = iv_layout_data_type.
    APPEND cs_layout_data TO ct_layout_data.

  ENDMETHOD.


  METHOD add_value_variant_data.
*&---------------------------------------------------------------------*
*& Description: Adds value data of current selection field to variant
*&---------------------------------------------------------------------*

    " check if system field value should be used
    IF is_selfield-system_value_type IS NOT INITIAL.
      cs_layout_data-counter = 1.
      cs_layout_data-system_value_type = is_selfield-system_value_type.
      cs_layout_data-sign_val  = is_selfield-sign.
      cs_layout_data-data_type = is_selfield-option.
      APPEND cs_layout_data TO ct_layout_data.
    ENDIF.

    IF is_selfield-low IS NOT INITIAL OR
       is_selfield-high IS NOT INITIAL OR
       is_selfield-option IS NOT INITIAL.

      ADD 1 TO cv_line_counter.
      cs_layout_data-counter   = cv_line_counter.
      cs_layout_data-low_val   = is_selfield-low.
      cs_layout_data-high_val  = is_selfield-high.
      cs_layout_data-sign_val  = is_selfield-sign.
      cs_layout_data-data_type = is_selfield-option.
      APPEND cs_layout_data TO ct_layout_data.
    ENDIF.

    " get multi select values
    IF is_selfield-push = abap_true.
      LOOP AT it_multi ASSIGNING FIELD-SYMBOL(<ls_select_multi>)
         WHERE tabname = is_selfield-tabname AND
               fieldname = is_selfield-fieldname AND
               ( low IS NOT INITIAL OR high IS NOT INITIAL ).
        ADD 1 TO cv_line_counter.
        cs_layout_data-counter   = cv_line_counter.
        cs_layout_data-low_val   = <ls_select_multi>-low.
        cs_layout_data-high_val  = <ls_select_multi>-high.
        cs_layout_data-sign_val  = <ls_select_multi>-sign.
        cs_layout_data-data_type = <ls_select_multi>-option.
        APPEND cs_layout_data TO ct_layout_data.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD create_variant.
    " 2) create variant data
    rs_variant = VALUE zdbbr_variant_data(
        variant_id          = iv_variant_id
        variant_name        = iv_variant_name
        entity_id           = iv_entity_id
        entity_type         = iv_entity_type
        created_by          = sy-uname
        description         = iv_variant_description
        has_output_fields   = if_has_output_fields
        has_sort_fields     = if_has_sort_fields
        has_multi_or_values = xsdbool( it_multi_or IS NOT INITIAL )
    ).

    " save global data, like ALV-layout, technical setting, ...
    " create fields for layout saving
    LOOP AT it_selfields ASSIGNING FIELD-SYMBOL(<ls_selection_field>)
      WHERE is_table_header = abap_false.
      DATA(lv_counter) = 0.

      " fill default values
      DATA(ls_vardata_entry) = VALUE zdbbr_vardata(
          tabname         = <ls_selection_field>-tabname
          fieldname       = <ls_selection_field>-fieldname
      ).

      add_value_variant_data(
        EXPORTING is_selfield     = <ls_selection_field>
                  it_multi        = it_multi_selfields
        CHANGING  cs_layout_data  = ls_vardata_entry
                  ct_layout_data  = rs_variant-variant_data
                  cv_line_counter = lv_counter
      ).

      " group by?
      IF <ls_selection_field>-group_by = abap_true.
        add_special_variant_data(
          EXPORTING iv_layout_data_type = zif_dbbr_global_consts=>gc_variant_datatypes-group_by
          CHANGING  cs_layout_data      = ls_vardata_entry
                    ct_layout_data      = rs_variant-variant_data
                    cv_line_counter     = lv_counter              ).
      ENDIF.

      " aggregation ?
      IF <ls_selection_field>-aggregation <> space.
        add_special_variant_data(
          EXPORTING iv_layout_data_type = zif_dbbr_global_consts=>gc_variant_datatypes-aggregation
                    iv_layout_data_low  = <ls_selection_field>-aggregation
          CHANGING  cs_layout_data      = ls_vardata_entry
                    ct_layout_data      = rs_variant-variant_data
                    cv_line_counter     = lv_counter
        ).
      ENDIF.

    ENDLOOP.

*.. add multi or data to variant
    LOOP AT it_multi_or ASSIGNING FIELD-SYMBOL(<ls_multi_or_tuple>).

      ls_vardata_entry = VALUE zdbbr_vardata(
          tuple_id = <ls_multi_or_tuple>-pos
      ).

      " 1) process single tuple data
      LOOP AT <ls_multi_or_tuple>-values ASSIGNING FIELD-SYMBOL(<ls_multi_or_tuple_data>).
        ls_vardata_entry-tabname = <ls_multi_or_tuple_data>-tabname.
        ls_vardata_entry-fieldname = <ls_multi_or_tuple_data>-fieldname.

        add_multi_or_variant_data(
          EXPORTING
            is_selfield_info = <ls_multi_or_tuple_data>
          CHANGING
            cs_layout_data   = ls_vardata_entry
            ct_layout_data   = rs_variant-variant_data
        ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
