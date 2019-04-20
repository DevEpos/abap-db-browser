CLASS zcl_dbbr_variant_loader DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_variant_name      TYPE zdbbr_variant_name OPTIONAL
        !iv_variant_id        TYPE zdbbr_variant_id OPTIONAL
        !iv_entity_type       TYPE zdbbr_entity_type OPTIONAL
        !iv_entity_id         TYPE zdbbr_entity_id OPTIONAL
        !ir_t_multi_or        TYPE REF TO zdbbr_or_seltab_itab
        !ir_t_selfields       TYPE REF TO zdbbr_selfield_itab
        !ir_t_selfields_multi TYPE REF TO zdbbr_selfield_itab
        !ir_tabfields         TYPE REF TO zcl_dbbr_tabfield_list
        !ir_s_global_data     TYPE REF TO zdbbr_global_data
        !ir_tabfields_grouped TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS load_variant
      IMPORTING
        !if_no_message    TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_variant) TYPE zdbbr_variant_info .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_variant_name TYPE zdbbr_variant_name .
    DATA mv_variant_id TYPE zdbbr_variant_id .
    DATA mr_t_multi_or TYPE REF TO zdbbr_or_seltab_itab .
    DATA mr_t_selfields TYPE REF TO zdbbr_selfield_itab .
    DATA mr_t_selfields_multi TYPE REF TO zdbbr_selfield_itab .
    DATA mr_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mr_tabfields_grouped TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mr_s_global_data TYPE REF TO zdbbr_global_data .
    DATA mv_entity_type TYPE zdbbr_entity_type .
    DATA mv_entity_id TYPE zdbbr_entity_id .

    METHODS fill_selscreen_with_vardata
      IMPORTING
        !it_variant_data TYPE zdbbr_vardata_itab .
    METHODS fill_selscreen_with_tupledata
      IMPORTING
        !it_variant_data TYPE zdbbr_vardata_itab .
ENDCLASS.



CLASS zcl_dbbr_variant_loader IMPLEMENTATION.


  METHOD constructor.
    mv_variant_name = iv_variant_name.
    mv_variant_id = iv_variant_id.
    mv_entity_id = iv_entity_id.
    mv_entity_type = iv_entity_type.
    mr_t_multi_or = ir_t_multi_or.
    mr_t_selfields = ir_t_selfields.
    mr_t_selfields_multi = ir_t_selfields_multi.
    mr_tabfields = ir_tabfields.
    mr_tabfields_grouped = ir_tabfields_grouped.
    mr_s_global_data = ir_s_global_data.
  ENDMETHOD.


  METHOD fill_selscreen_with_tupledata.

    DATA(ls_multi_or) = VALUE zdbbr_or_seltab(
        pos    = it_variant_data[ 1 ]-tuple_id
    ).

    LOOP AT it_variant_data ASSIGNING FIELD-SYMBOL(<ls_vardata>)
         GROUP BY ( tabname_alias = <ls_vardata>-tabname
                    fieldname     = <ls_vardata>-fieldname )
         ASSIGNING FIELD-SYMBOL(<ls_vardata_group>).

      DATA(lf_first_iteration) = abap_true.

      TRY.
          DATA(lr_field) = mr_tabfields->get_field_ref(
              iv_tabname_alias = <ls_vardata_group>-tabname_alias
              iv_fieldname     = <ls_vardata_group>-fieldname
          ).
          data(lv_tabname) = lr_field->tabname.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      LOOP AT GROUP <ls_vardata_group> ASSIGNING FIELD-SYMBOL(<ls_vardata_group_entry>).
        IF lf_first_iteration = abap_true.
          DATA(ls_selfield_info) = VALUE zdbbr_selfield_info(
              tabname_alias = <ls_vardata_group_entry>-tabname
              tabname       = lv_tabname
              fieldname     = <ls_vardata_group_entry>-fieldname
              sign          = <ls_vardata_group_entry>-sign_val
              option        = <ls_vardata_group_entry>-data_type
              low           = <ls_vardata_group_entry>-low_val
              high          = <ls_vardata_group_entry>-high_val
          ).
          CLEAR lf_first_iteration.
        ELSE.
          APPEND VALUE zdbbr_selfield_value(
               sign   = <ls_vardata_group_entry>-sign_val
               option = <ls_vardata_group_entry>-data_type
               low    = <ls_vardata_group_entry>-low_val
               high   = <ls_vardata_group_entry>-high_val
          ) TO ls_selfield_info-multi_values.
        ENDIF.
      ENDLOOP.

      APPEND ls_selfield_info TO ls_multi_or-values.
    ENDLOOP.

    APPEND ls_multi_or TO mr_t_multi_or->*.
  ENDMETHOD.


  METHOD fill_selscreen_with_vardata.

    LOOP AT it_variant_data ASSIGNING FIELD-SYMBOL(<ls_vardata>)
      GROUP BY ( tabname_alias = <ls_vardata>-tabname
                 fieldname     = <ls_vardata>-fieldname )
      ASSIGNING FIELD-SYMBOL(<ls_vardata_group>).

      DATA(lt_vardata_group) = VALUE zdbbr_vardata_itab( FOR data IN GROUP <ls_vardata_group> ( data ) ).
      SORT lt_vardata_group BY counter.

      " read correct selection field
      ASSIGN mr_t_selfields->*[ tabname_alias = <ls_vardata_group>-tabname_alias
                                fieldname     = <ls_vardata_group>-fieldname ] TO FIELD-SYMBOL(<ls_selfield>).
      IF sy-subrc = 0.
        DATA(lf_first_selvalue) = abap_true.

        LOOP AT lt_vardata_group ASSIGNING FIELD-SYMBOL(<ls_vardata_group_entry>).
          CASE <ls_vardata_group_entry>-data_type.

            WHEN zif_dbbr_global_consts=>gc_variant_datatypes-aggregation.
              <ls_selfield>-aggregation = <ls_vardata_group_entry>-low_val.

            WHEN zif_dbbr_global_consts=>gc_variant_datatypes-group_by.
              <ls_selfield>-group_by = abap_true.
            WHEN OTHERS. " normal selection field
              IF lf_first_selvalue = abap_true.

                IF <ls_vardata_group_entry>-system_value_type IS NOT INITIAL.
                  zcl_dbbr_system_helper=>get_system_value( EXPORTING iv_system_value_type = <ls_vardata_group_entry>-system_value_type
                                                             IMPORTING ev_system_value      = <ls_selfield>-low ).
                  <ls_selfield>-system_value_type = <ls_vardata_group_entry>-system_value_type.
                  <ls_selfield>-sign  = <ls_vardata_group_entry>-sign_val.
                  <ls_selfield>-option = <ls_vardata_group_entry>-data_type.
                ELSE.

                  <ls_selfield>-sign = <ls_vardata_group_entry>-sign_val.
                  <ls_selfield>-option = <ls_vardata_group_entry>-data_type.
                  <ls_selfield>-low = <ls_vardata_group_entry>-low_val.
                  <ls_selfield>-high = <ls_vardata_group_entry>-high_val.
                  lf_first_selvalue = abap_false.
                ENDIF.
              ELSE. " further values will be added to multi selection field table
                DATA(ls_multi_select) = <ls_selfield>.
                ls_multi_select-low = <ls_vardata_group_entry>-low_val.
                ls_multi_select-high = <ls_vardata_group_entry>-high_val.
                ls_multi_select-sign = <ls_vardata_group_entry>-sign_val.
                ls_multi_select-option = <ls_vardata_group_entry>-data_type.
                APPEND ls_multi_select TO mr_t_selfields_multi->*.
                " obviously there is more than one value for this selection field
                " so the push optin will be active
                <ls_selfield>-push = abap_true.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD load_variant.

    DATA: ls_alv_variant TYPE disvariant.

    DATA(lr_variant_factory) = NEW zcl_dbbr_variant_factory( ).

    lr_variant_factory->get_variant(
      EXPORTING
        iv_variant_name    = mv_variant_name
        iv_variant_id      = mv_variant_id
        iv_entity_id       = mv_entity_id
        iv_entity_type     = mv_entity_type
        if_load_completely = abap_true
      IMPORTING
        es_variant         = DATA(ls_existing_variant)
    ).

    IF ls_existing_variant IS INITIAL.
      MESSAGE e030(zdbbr_info) WITH mv_variant_name INTO DATA(lv_dummy).
      zcx_dbbr_validation_exception=>raise_from_sy( ).
    ENDIF.

*... if variant holds output fields clear all active formulas
    IF ls_existing_variant-has_output_fields = abap_true.
      mr_tabfields->delete_formula_fields( ).
      mr_tabfields_grouped->delete_formula_fields( ).
    ENDIF.

    SORT ls_existing_variant-variant_data BY tuple_id tabname fieldname counter.

*... check if variant stores grouping information
    LOOP AT ls_existing_variant-variant_data ASSIGNING FIELD-SYMBOL(<ls_vard>) WHERE data_type = zif_dbbr_global_consts=>gc_variant_datatypes-group_by OR
                                                                                     data_type = zif_dbbr_global_consts=>gc_variant_datatypes-aggregation.
      DATA(lf_grouping_is_active) = abap_true.
      mr_tabfields_grouped->clear( ).
      EXIT.
    ENDLOOP.

*... clear any filled aggregation data before selection table is filled with variant data
    LOOP AT mr_t_selfields->* ASSIGNING FIELD-SYMBOL(<ls_table_line>).
      CLEAR: <ls_table_line>-group_by,
             <ls_table_line>-aggregation,
             <ls_table_line>-sign,
             <ls_table_line>-option,
             <ls_table_line>-low,
             <ls_table_line>-high.
    ENDLOOP.

*... fill selection screen with variant data
    LOOP AT ls_existing_variant-variant_data ASSIGNING FIELD-SYMBOL(<ls_vardata>)
      GROUP BY ( tuple = <ls_vardata>-tuple_id )
      ASSIGNING FIELD-SYMBOL(<ls_vardata_tuple>).

      IF <ls_vardata_tuple>-tuple IS NOT INITIAL.
        fill_selscreen_with_tupledata( VALUE #( FOR data IN GROUP <ls_vardata_tuple> ( data ) ) ).
      ELSE.
        fill_selscreen_with_vardata( VALUE #( FOR data IN GROUP <ls_vardata_tuple> ( data ) ) ).
      ENDIF.
    ENDLOOP.

    IF lf_grouping_is_active = abap_false AND
       ( ls_existing_variant-has_output_fields = abap_true OR
         ls_existing_variant-has_sort_fields = abap_true ).
      mr_tabfields->clear_active_flag(
        if_clear_output = ls_existing_variant-has_output_fields
        if_clear_sort   = ls_existing_variant-has_sort_fields
      ).
    ENDIF.


*... fill output and sorting fields from variant
    IF ls_existing_variant-has_output_fields = abap_true OR ls_existing_variant-has_sort_fields = abap_true.

      LOOP AT ls_existing_variant-fields ASSIGNING FIELD-SYMBOL(<ls_tabfield>) WHERE is_formula_field = abap_false.

        TRY .
            DATA(lr_field_stru) = mr_tabfields->get_field_ref(
               iv_tabname_alias     = <ls_tabfield>-tabname_alias
               iv_fieldname         = <ls_tabfield>-fieldname
               if_is_text_field     = <ls_tabfield>-is_text_field
            ).
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.

        IF lf_grouping_is_active = abap_true.
          DATA(ls_field) = lr_field_stru->*.
          ASSIGN ls_field TO FIELD-SYMBOL(<ls_field>).

        ELSE.
          ASSIGN lr_field_stru->* TO <ls_field>.
        ENDIF.

        IF ls_existing_variant-has_output_fields = abap_true.
          <ls_field>-output_active = <ls_tabfield>-output_active.
          <ls_field>-output_order = <ls_tabfield>-output_order.
          <ls_field>-is_text_field = <ls_tabfield>-is_text_field.
        ENDIF.

        IF ls_existing_variant-has_sort_fields = abap_true.
          <ls_field>-sort_active = <ls_tabfield>-sort_active.
          <ls_field>-sort_direction = <ls_tabfield>-sort_direction.
          <ls_field>-sort_order = <ls_tabfield>-sort_order.
        ENDIF.

        IF lf_grouping_is_active = abap_true.
          mr_tabfields_grouped->append_tabfield_info( <ls_field> ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF if_no_message = abap_false.
      MESSAGE s036(zdbbr_info) WITH ls_existing_variant-variant_name.
    ENDIF.

    rs_variant = CORRESPONDING #( ls_existing_variant ).

  ENDMETHOD.
ENDCLASS.
