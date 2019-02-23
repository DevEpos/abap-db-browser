"! <p class="shorttext synchronized" lang="en">Creation of output structure</p>
CLASS zcl_dbbr_output_tab_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Create dynamic table for data output</p>
    "!
    CLASS-METHODS create_dyn_comp_tab
      IMPORTING
        ir_tabfields       TYPE REF TO zcl_dbbr_tabfield_list
        if_active_grouping TYPE abap_bool OPTIONAL
        it_add_texts       TYPE zdbbr_additional_text_itab OPTIONAL
        is_tech_info       TYPE zdbbr_tech_info
      RETURNING
        VALUE(rt_comp_tab) TYPE zdbbr_abap_comp_type_itab.

    "! <p class="shorttext synchronized" lang="en">Create/enhance table components for output table</p>
    "!
    CLASS-METHODS enhance_output_table_comps
      IMPORTING
        ir_table      TYPE REF TO data
      EXPORTING
        eo_line_type  TYPE REF TO cl_abap_structdescr
        eo_table_type TYPE REF TO cl_abap_tabledescr.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS add_text_comp_to_dyntab
      IMPORTING
        ir_current_field TYPE REF TO zdbbr_tabfield_info_ui
        it_add_texts     TYPE zdbbr_additional_text_itab
      CHANGING
        ct_comp_type_tab TYPE zdbbr_abap_comp_type_itab.

ENDCLASS.



CLASS zcl_dbbr_output_tab_builder IMPLEMENTATION.


  METHOD add_text_comp_to_dyntab.
    DATA(ls_ddtext_text_field_def) = VALUE zdbbr_abap_comp_type(
        name        = ir_current_field->alv_fieldname
        simple_name = ir_current_field->fieldname
        table_alias = ir_current_field->alias
        type_name   = 'DD07T-DDTEXT'
    ).

    DATA(lr_table_counter) = NEW zcl_uitb_table_func_executor( ir_table = REF #( it_add_texts ) ).
    DATA(lv_textfield_count) = lr_table_counter->count_lines(
        VALUE #( ( fieldname = 'ID_TABLE'
                   selopt_itab = VALUE #( ( sign = 'I' option = 'EQ' low = ir_current_field->tabname ) ) )
                 ( fieldname = 'ID_FIELD'
                   selopt_itab = VALUE #( ( sign = 'I' option = 'EQ' low = ir_current_field->fieldname ) ) ) )
    ).

    " no text field exists
    IF lv_textfield_count = 0.
      RETURN.
    ELSEIF lv_textfield_count > 1.
      " create default line for domain text fields or multiple text fields for a single id field
      ct_comp_type_tab = VALUE #( BASE ct_comp_type_tab ( ls_ddtext_text_field_def ) ).
    ELSE.
      ASSIGN it_add_texts[ id_table = ir_current_field->tabname
                           id_field = ir_current_field->fieldname ] TO FIELD-SYMBOL(<ls_add_text>).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF <ls_add_text>-selection_type = zif_dbbr_c_text_selection_type=>domain_value.
        ct_comp_type_tab = VALUE #( BASE ct_comp_type_tab ( ls_ddtext_text_field_def ) ).
      ELSE.
        ct_comp_type_tab = VALUE #(
          BASE ct_comp_type_tab
          ( name        = ir_current_field->alv_fieldname
            simple_name = ir_current_field->fieldname
            table_alias = ir_current_field->alias
            type_name   = |{ <ls_add_text>-text_table }-{ <ls_add_text>-text_field }| )
        ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_dyn_comp_tab.
    FIELD-SYMBOLS: <lt_table>      TYPE STANDARD TABLE,
                   <lt_table_temp> TYPE STANDARD TABLE.


*...create fieldcatalog / `select`-part from output fields
*...1) update mode of tabfield list to `output`.
    ir_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).
    ir_tabfields->sort( ).

    ir_tabfields->initialize_iterator( ).

    WHILE ir_tabfields->has_more_lines( ).
      DATA(lr_current_field) = ir_tabfields->get_next_entry( ).

**...exclude parameters from output table
      CHECK lr_current_field->is_parameter = abap_false.

      IF lr_current_field->is_formula_field = abap_false.
        DATA(lv_type) = |{ lr_current_field->tabname }-{ lr_current_field->fieldname }|.
      ELSE.
        lv_type = lr_current_field->rollname.
      ENDIF.

      DATA(ls_comp) = VALUE zdbbr_abap_comp_type(
        name        = lr_current_field->alv_fieldname
        simple_name = lr_current_field->fieldname
        table_alias = lr_current_field->alias
        type_name   = lv_type
      ).

      IF lr_current_field->is_text_field = abap_false.

        IF is_tech_info-use_reduced_memory = abap_true.

          IF lr_current_field->output_active = abap_true OR
             lr_current_field->has_active_text_field = abap_true OR
             lr_current_field->is_calculation_field = abap_true OR
             lr_current_field->is_key = abap_true OR
             lr_current_field->needed_for_virtual_join = abap_true.

            rt_comp_tab = VALUE #( BASE rt_comp_tab ( ls_comp ) ).
          ENDIF.

        ELSE. " no memory reduction will result in all available fields in the output structure
          IF lr_current_field->is_formula_field = abap_true AND
             lr_current_field->output_active = abap_false.
            CONTINUE.
          ENDIF.

          rt_comp_tab = VALUE #( BASE rt_comp_tab ( ls_comp ) ).
        ENDIF.

      ELSE.

**...create entry for text field - if necessary
        IF lr_current_field->is_text_field = abap_true AND
           lr_current_field->output_active = abap_true.

          add_text_comp_to_dyntab( EXPORTING ir_current_field = lr_current_field
                                             it_add_texts     = it_add_texts
                                   CHANGING  ct_comp_type_tab = rt_comp_tab      ).
        ENDIF.
      ENDIF.
    ENDWHILE.

**...add additional column for line index -> needed in grouping mode
    rt_comp_tab = VALUE #(
      BASE rt_comp_tab
      ( name        = zif_dbbr_c_special_out_columns=>line_index
        simple_name = zif_dbbr_c_special_out_columns=>line_index
        type_name   = COND #( WHEN if_active_grouping = abap_false THEN
                                'SYST-TABIX'
                              ELSE
                                'SE16N_REF-SE16N_NR_LINES' ) )
      " cell color
      ( name        = zif_dbbr_c_special_out_columns=>cell_col_row_color
        simple_name = zif_dbbr_c_special_out_columns=>cell_col_row_color
        type_name   = zif_dbbr_c_special_out_columns=>alv_col_color_type )
        " for hiding certain rows
      ( name        = zif_dbbr_c_special_out_columns=>hide_flag
        simple_name = zif_dbbr_c_special_out_columns=>hide_flag
        type_name   = 'ZDBBR_ALV_SPECIAL_CELLS-HIDE_FLAG' )
    ).

*...create data descriptors for type names
    LOOP AT rt_comp_tab ASSIGNING FIELD-SYMBOL(<ls_component>).
      <ls_component>-type = CAST #( cl_abap_typedescr=>describe_by_name( <ls_component>-type_name ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD enhance_output_table_comps.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    CHECK ir_table IS BOUND.

    ASSIGN ir_table->* TO <lt_data>.

    zcl_uitb_rtti_util=>extend_table_by_components(
      EXPORTING
        it_data             = <lt_data>
        it_component_append = VALUE #(
          ( component = zif_dbbr_c_special_out_columns=>line_index
            type      = 'SYST-TABIX' )
          ( component = zif_dbbr_c_special_out_columns=>cell_col_row_color
            type      = zif_dbbr_c_special_out_columns=>alv_col_color_type )
          ( component = zif_dbbr_c_special_out_columns=>hide_flag
            type      = 'ZDBBR_ALV_SPECIAL_CELLS-HIDE_FLAG' )
        )
      IMPORTING
        er_line_type        = eo_line_type
        er_table_type       = eo_table_type
    ).

  ENDMETHOD.

ENDCLASS.
