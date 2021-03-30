CLASS zcl_dbbr_join_selection_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selection_util
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_entity_name
        REDEFINITION.

    METHODS init
        REDEFINITION.
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION.
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION .
  PROTECTED SECTION.

    METHODS create_from_clause
        REDEFINITION.
    METHODS before_selection
        REDEFINITION.
    METHODS after_selection
        REDEFINITION.
    "! <p class="shorttext synchronized" lang="en">Marks virtual join fields for later processing</p>
    METHODS mark_virt_join_selfields .
    "! <p class="shorttext synchronized" lang="en">Fill virtual join field in the output table</p>
    METHODS fill_virtual_join_fields
      RAISING
        zcx_dbbr_application_exc.
    "! <p class="shorttext synchronized" lang="en">Change CDS parameter values</p>
    METHODS change_parameters .
    "! <p class="shorttext synchronized" lang="en">Update parameters of entities with the given parameter</p>
    METHODS update_table_parameters
      IMPORTING
        is_param TYPE zsat_table_parameter.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Prefill cds view parameters</p>
    METHODS prefill_parameters
      IMPORTING
        io_tabfields TYPE REF TO zcl_dbbr_tabfield_list
      CHANGING
        cs_join      TYPE zif_sat_ty_global=>ty_s_join_def.
    "! <p class="shorttext synchronized" lang="en">Fill parameters for a certain entity in the join</p>
    METHODS fill_entity_params
      IMPORTING
        iv_entity       TYPE zsat_entity_id
        iv_entity_alias TYPE zsat_entity_alias
      CHANGING
        cs_join         TYPE zif_sat_ty_global=>ty_s_join_def.
ENDCLASS.



CLASS zcl_dbbr_join_selection_util IMPLEMENTATION.

  METHOD fill_virtual_join_fields.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    " only continue if virtual join helper is bound
    CHECK mo_post_join_helper IS BOUND.

    ASSIGN mr_t_data->* TO <lt_table>.

    mo_post_join_helper->fill_cache_tables( <lt_table> ).
    mo_post_join_helper->process_table( mr_t_data ).
  ENDMETHOD.

  METHOD mark_virt_join_selfields.
    CHECK line_exists( ms_join_def-tables[ is_virtual = abap_true ] ).

    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE is_virtual = abap_true.
      LOOP AT <ls_join_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_join_field>).

        " update field
        DATA(lr_s_field)    = mo_tabfields->get_field_ref(
           iv_tabname_alias = <ls_join_field>-ref_table
           iv_fieldname     = <ls_join_field>-ref_field
        ).

        lr_s_field->needed_for_virtual_join = abap_true.
      ENDLOOP.
    ENDLOOP.

    mt_virtual_join_table_range = VALUE #(
      LET sign = 'I' option = 'EQ' IN
      FOR <ls_join_tab> IN ms_join_def-tables
      WHERE ( is_virtual = abap_true )
      ( sign   = sign
        option = option
        low    = <ls_join_tab>-add_table )
    ).

    LOOP AT mt_selection_fields ASSIGNING FIELD-SYMBOL(<ls_selfield>) WHERE tabname IN mt_virtual_join_table_range.
      <ls_selfield>-virtual_join_field = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_entity_name.
    result = |{ 'Join Selection' }|.
  ENDMETHOD.

  METHOD init.
    ms_control_info-primary_table = mv_entity_id.

    IF mt_param_values IS NOT INITIAL.
      DATA(ls_join) = CORRESPONDING zif_sat_ty_global=>ty_s_join_def( DEEP ms_join_def ).
      prefill_parameters(
        EXPORTING io_tabfields = mo_tabfields
        CHANGING  cs_join      = ls_join
      ).
      ms_join_def = CORRESPONDING #( DEEP ls_join ).
    ENDIF.
  ENDMETHOD.

  METHOD create_from_clause.
    DATA: lv_handled_params TYPE i.

    LOOP AT mt_param_values ASSIGNING FIELD-SYMBOL(<ls_param_value>).
      update_table_parameters( is_param = CORRESPONDING #( <ls_param_value> MAPPING param_name = fieldname param_value = low ) ).
    ENDLOOP.

    IF ms_join_def-tables IS INITIAL.
      IF mv_entity_type = zif_sat_c_entity_type=>cds_view AND
         ( sy-saprl < 750 OR ms_technical_info-use_ddl_view_for_select = abap_true ).

        DATA(lv_ddl_ddic_view) = zcl_sat_cds_view_factory=>read_ddl_ddic_view_for_entity( mv_entity_id ).

        mt_from = VALUE #( ( |{ lv_ddl_ddic_view }| ) ).
      ELSE.
        mt_from = VALUE #( ( |{ ms_control_info-primary_table }| ) ).
      ENDIF.
    ELSE.
      mt_from = zcl_sat_join_helper=>build_from_clause_for_join_def(
        if_use_ddl_for_select = ms_technical_info-use_ddl_view_for_select
        is_join_def           = CORRESPONDING #( DEEP ms_join_def )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = VALUE #(
      ( LINES OF super->get_deactivated_functions( ) )
      ( zif_dbbr_c_selection_functions=>navigate_association )
    ).


    IF mt_param_values IS NOT INITIAL.
      DELETE result WHERE table_line = zif_dbbr_c_selection_functions=>change_cds_parameters.
    ENDIF.

    IF mf_aggregation = abap_true OR
       mf_group_by    = abap_true.

      result = VALUE #(
       BASE result
       ( zif_dbbr_c_selection_functions=>toggle_entity_info_header )
       ( zif_dbbr_c_selection_functions=>group_by_selected_columns )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_ui_function.
    super->handle_ui_function( CHANGING cv_function = cv_function ).

    CASE cv_function.

      WHEN zif_dbbr_c_selection_functions=>change_cds_parameters.
        change_parameters( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD change_parameters.
    DATA: lf_trigger_select TYPE abap_bool.

    DATA(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
        io_tabfields     = mo_tabfields_all
        iv_cds_view_name = mv_entity_id
        it_param_values  = VALUE #(
          FOR param IN mt_param_values WHERE ( is_parameter = abap_true ) ( name = param-fieldname value = param-low )
        )
    ).

    lo_param_popup->show( ).
    DATA(lt_new_param_values) = lo_param_popup->get_param_values( ).

    IF lt_new_param_values IS NOT INITIAL.
      LOOP AT lt_new_param_values ASSIGNING FIELD-SYMBOL(<ls_new_param>).
        ASSIGN mt_param_values[ fieldname = <ls_new_param>-name is_parameter = abap_true ] TO FIELD-SYMBOL(<ls_old_param>).
        IF sy-subrc = 0 AND
           <ls_old_param>-low <> <ls_new_param>-value.
          lf_trigger_select = abap_true.
          <ls_old_param>-low = <ls_new_param>-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lf_trigger_select = abap_true.
      CLEAR mt_from.
      create_from_clause( ).
      mo_select_program->update_from( mt_from ).
      refresh_selection( ).
    ENDIF.
  ENDMETHOD.

  METHOD update_table_parameters.
    FIELD-SYMBOLS: <ls_param> TYPE zsat_table_parameter.

    LOOP AT ms_join_def-parameters ASSIGNING <ls_param> WHERE param_name = is_param-param_name.
      <ls_param>-param_value = is_param-param_value.
    ENDLOOP.

    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      LOOP AT <ls_table>-parameters ASSIGNING <ls_param> WHERE param_name = is_param-param_name.
        <ls_param>-param_value = is_param-param_value.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD prefill_parameters.
    DATA: lo_cds_view TYPE REF TO zcl_sat_cds_view.

*.. Read parameters for primary and join entities
    DATA(lt_tables) = io_tabfields->get_table_list( ).

    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      CHECK <ls_table>-has_params = abap_true.

      fill_entity_params(
        EXPORTING iv_entity       = <ls_table>-tabname
                  iv_entity_alias = <ls_table>-tabname_alias
        CHANGING  cs_join         = cs_join
      ).

    ENDLOOP.
  ENDMETHOD.


  METHOD fill_entity_params.
    DATA: lr_params TYPE REF TO zsat_table_parameter_t.
*.. Get the correct entity in the join to fill the parameter names

    TRY.
        IF cs_join-primary_table_alias = iv_entity_alias.
          lr_params = REF #( cs_join-parameters ).
        ELSE.
          lr_params = REF #( cs_join-tables[ add_table_alias = iv_entity_alias ]-parameters ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF lr_params IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(lo_cds_view) = zcl_sat_cds_view_factory=>read_cds_view( iv_entity ).
        lr_params->* = VALUE #(
          FOR <cds_param> IN lo_cds_view->get_parameters( if_exclude_system_params = abap_true )
          ( param_name = <cds_param>-parametername )
        ).
      CATCH zcx_sat_data_read_error.
    ENDTRY.

  ENDMETHOD.


  METHOD before_selection.

    determine_group_by_state( ).
    determine_aggregation_state( ).
    mark_virt_join_selfields( ).

    read_entity_infos( ).

    build_full_fieldnames( ).

    IF mf_join_is_active = abap_true AND
       line_exists( ms_join_def-tables[ is_virtual = abap_true ] ).
      mo_post_join_helper = zcl_dbbr_virtual_join_helper=>create(
        is_join_def   = ms_join_def
        ir_fields     = mo_tabfields
        ir_fields_all = mo_tabfields_all
      ).
    ENDIF.

    mo_tabfields->update_text_field_status( ).

    handle_reduced_memory( ).

    zcl_dbbr_addtext_helper=>prepare_text_fields(
      EXPORTING ir_fields    = mo_tabfields
      CHANGING  ct_add_texts = mt_add_texts
    ).

    create_from_clause( ).

    create_where_clause( ).

    create_group_by_clause( ).

    create_order_by_clause( ).

    create_field_catalog( ).

    create_select_clause( ).

    create_dynamic_table( ).

    IF mo_formula IS BOUND.
      TRY.
          mo_formula_calculator = zcl_dbbr_formula_calculator=>create(
              ir_formula            = mo_formula
              ir_tabfields          = mo_tabfields
              it_tab_components     = mt_dyntab_components
          ).
        CATCH zcx_dbbr_exception INTO DATA(lr_exception).
          lr_exception->zif_sat_exception_message~print( ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD after_selection.

    fill_virtual_join_fields( ).
    super->after_selection( ).

  ENDMETHOD.

ENDCLASS.
