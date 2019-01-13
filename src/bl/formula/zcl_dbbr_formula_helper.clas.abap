CLASS ZCL_DBBR_formula_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS generate_subroutine
      IMPORTING
        !it_lines TYPE string_table .
    CLASS-METHODS get_raw_row_field
      IMPORTING
        !iv_row_field TYPE string
      RETURNING
        VALUE(result) TYPE string .
    CLASS-METHODS update_tabflds_from_formula
      IMPORTING
        !ir_tabfields      TYPE REF TO ZCL_DBBR_tabfield_list
        !ir_formula        TYPE REF TO ZCL_DBBR_formula
        !it_form_selfields TYPE ZDBBR_tabfield_info_itab OPTIONAL
      RAISING
        ZCX_DBBR_formula_exception .

    CLASS-METHODS is_icon_field
      IMPORTING
        !ir_tabfield  TYPE REF TO ZDBBR_tabfield_info_ui
      RETURNING
        VALUE(result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS determine_calculation_fields
      IMPORTING
        ir_tabfields TYPE REF TO ZCL_DBBR_tabfield_list
        ir_formula   TYPE REF TO ZCL_DBBR_formula
      RAISING
        ZCX_DBBR_formula_exception.
    CLASS-METHODS add_form_field_to_tablist
      IMPORTING
        !is_form_field             TYPE ZIF_DBBR_fe_types=>ty_form_field
        !ir_tabfield_list          TYPE REF TO ZCL_DBBR_tabfield_list
        is_tabfield_control        TYPE ZDBBR_tabfield_control
        if_use_default_ctrl_values TYPE abap_bool .
ENDCLASS.



CLASS ZCL_DBBR_FORMULA_HELPER IMPLEMENTATION.


  METHOD add_form_field_to_tablist.
    DATA: ls_dfies TYPE dfies.

    DATA(lr_elem_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( is_form_field-type_name ) ).
    ls_dfies = lr_elem_descr->get_ddic_field( ).

    DATA(ls_tabfield) = CORRESPONDING zdbbr_tabfield_info_ui( ls_dfies ).
    ls_tabfield-tabname = ls_tabfield-tabname_alias
                        = zif_dbbr_global_consts=>gc_formula_dummy_table.
    ls_tabfield-fieldname = is_form_field-field.
    ls_tabfield-alias = zif_dbbr_global_consts=>c_formula_alias.
    ls_tabfield-is_formula_field = abap_true.
    ls_tabfield-field_ddtext = COND #( WHEN is_form_field-long_description IS NOT INITIAL THEN
                                        is_form_field-long_description
                                      ELSE
                                        is_form_field-short_description ).
    ls_tabfield-ddic_order = 9999.
    ls_tabfield-is_numeric = zcl_dbbr_dictionary_helper=>is_dtel_numeric( ls_dfies-rollname ).
    ls_tabfield-std_medium_text = is_form_field-short_description.
    ls_tabfield-std_long_text = is_form_field-long_description.
    ls_tabfield-output_active = is_tabfield_control-output_active.
    ls_tabfield-output_order = is_tabfield_control-output_order.

    ir_tabfield_list->add( ir_s_element = REF #( ls_tabfield ) ).
  ENDMETHOD.


  METHOD determine_calculation_fields.
    DATA: lr_field_ref TYPE REF TO ZDBBR_tabfield_info_ui.

    CHECK: ir_formula IS BOUND,
           ir_tabfields IS BOUND.

    ir_formula->get_statements( IMPORTING et_statements = DATA(lt_stmnt) ).

    LOOP AT lt_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>) WHERE type <> 'U'.
      LOOP AT <ls_stmnt>-tokens ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE is_row_field = abap_true.
        " remove ROW- from string
        DATA(lv_row_field_raw) = substring_after( val = <ls_token>-str sub = '-' ).

        TRY.
            " check if there is another hyphen because than it is a join field
            IF matches( val = lv_row_field_raw regex = |\\w-.*| ).
              DATA(lv_alias) = substring_before( val = lv_row_field_raw sub = '-' ).
              DATA(lv_field) = substring_after( val = lv_row_field_raw sub = '-' ).

              lr_field_ref = ir_tabfields->get_field_ref_by_alv_name( |{ lv_alias }_{ lv_field }| ).
            ELSE.
              lr_field_ref = ir_tabfields->get_field_ref( iv_fieldname = CONV #( lv_row_field_raw ) ).
            ENDIF.

            " field was found -> add it to the list of needed calculation fields for the formula
            lr_field_ref->is_calculation_field = abap_true.
          CATCH cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE ZCX_DBBR_formula_exception
              EXPORTING
                textid = ZCX_DBBR_formula_exception=>needed_calc_fld_not_in_list
                msgv1  = |{ lv_row_field_raw }|.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_subroutine.
    GENERATE SUBROUTINE POOL it_lines NAME DATA(lv_prog) MESSAGE DATA(lv_message).
  ENDMETHOD.


  METHOD get_raw_row_field.
    result = replace( val  = substring_after( val = iv_row_field sub = 'ROW-' )
                      sub  = '-'
                      with = '_' ).
  ENDMETHOD.


  METHOD is_icon_field.
    result = xsdbool( ir_tabfield->is_formula_field = abap_true AND
                      ( ir_tabfield->rollname = ZIF_DBBR_fe_constants=>c_icon_type OR
                        ir_tabfield->rollname = ZIF_DBBR_fe_constants=>c_icon_tt_type ) ).
  ENDMETHOD.


  METHOD update_tabflds_from_formula.
    DATA: ls_tabfield_control TYPE zdbbr_tabfield_control.

    CHECK: ir_tabfields IS BOUND,
           ir_formula IS BOUND.

    ir_tabfields->delete_formula_fields( ).
    ir_tabfields->clear_calculation_flag( ).
    ir_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).

    determine_calculation_fields(
      ir_formula   = ir_formula
      ir_tabfields = ir_tabfields
    ).

    ir_formula->get_formula_fields( IMPORTING et_fields = DATA(lt_form_fields) ).

    IF it_form_selfields IS INITIAL.
      DATA(lf_use_default_ctrl_values) = abap_true.
      ls_tabfield_control = VALUE #( output_active = abap_true ).
      IF ir_tabfields->custom_order_exists( ).
        DATA(lr_table_searcher) = NEW zcl_uitb_table_func_executor( ir_tabfields->get_fields_ref( ) ).
        DATA(lf_set_output_order) = abap_true.

        ls_tabfield_control-output_order  = lr_table_searcher->max_value( 'OUTPUT_ORDER' ).
      ENDIF.
    ENDIF.

    LOOP AT lt_form_fields ASSIGNING FIELD-SYMBOL(<ls_form_field>).

      IF lf_use_default_ctrl_values = abap_true.
        IF lf_set_output_order = abap_true.
          ADD 1 TO ls_tabfield_control-output_order.
        ENDIF.
      ELSE.
        ls_tabfield_control = CORRESPONDING #(
          VALUE #( it_form_selfields[
                        is_formula_field = abap_true
                        tabname          = zif_dbbr_global_consts=>gc_formula_dummy_table
                        fieldname        = <ls_form_field>-field ] OPTIONAL )
        ).
      ENDIF.

*... update formula fields
      zcl_dbbr_formula_helper=>add_form_field_to_tablist(
        is_form_field              = <ls_form_field>
        ir_tabfield_list           = ir_tabfields
        is_tabfield_control        = ls_tabfield_control
        if_use_default_ctrl_values = lf_use_default_ctrl_values
      ).
    ENDLOOP.

*.. at least one formula field exists so add the formula dummy table
    IF sy-subrc = 0.
      ir_tabfields->add_table(
        VALUE zdbbr_entity_info(
          active_selection     = abap_false
          tabname              = zif_dbbr_global_consts=>gc_formula_dummy_table
          tabname_alias        = zif_dbbr_global_consts=>gc_formula_dummy_table
          alias                = zif_dbbr_global_consts=>c_formula_alias
*        type                 =
          no_selection_allowed = abap_true
          description          = 'Formula Fields'
          fields_are_loaded    = abap_true
       )
      ).

    ENDIF.

    ir_tabfields->sort( ).
  ENDMETHOD.
ENDCLASS.
