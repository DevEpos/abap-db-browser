CLASS zcl_dbbr_cds_variant_starter DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_variant_starter
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_variant_starter .

    METHODS constructor
      IMPORTING
        !iv_variant_id TYPE zdbbr_variant_id
        !iv_cds_view   TYPE zdbbr_cds_view_name .
  PROTECTED SECTION.

    METHODS fill_data_from_variant
        REDEFINITION.
    METHODS fill_primary_entity
        REDEFINITION.
  PRIVATE SECTION.

    DATA mo_cds_view TYPE REF TO zcl_dbbr_cds_view .
    DATA mv_cds_view TYPE zdbbr_cds_view_name .
    METHODS get_parameter_values_from_var
      IMPORTING
        it_vardata           TYPE zdbbr_vardata_itab
      RETURNING
        VALUE(rt_param_data) TYPE zif_dbbr_global_types=>tt_cds_param_value.
    METHODS get_params_from_variant
      RETURNING
        VALUE(rt_params) TYPE zif_dbbr_global_types=>tt_cds_param_value.
    METHODS create_automatic_variant
      IMPORTING
        it_selfields TYPE zdbbr_selfield_itab.
ENDCLASS.



CLASS zcl_dbbr_cds_variant_starter IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_variant_id ).
    mv_cds_view = iv_cds_view.
  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~execute_variant.
    show_start_progress_text( ).

    fill_data_from_variant( ).

    get_tabfields(
      IMPORTING
        er_tabfields     = DATA(lr_tabfields)
        er_tabfields_all = DATA(lr_tabfields_all)
    ).

    " create and start selection controller
    DATA(lr_controller) = zcl_dbbr_selection_controller=>create_controller(
       VALUE #(
         entity_id          = mv_cds_view
         entity_type        = zif_dbbr_c_selscreen_mode=>cds_view
         selection_fields   = mt_selfields
         multi_or           = mt_selfields_or
         technical_infos    = CORRESPONDING #( ms_global_data )
         selfields_multi    = mt_selfields_multi
         tabfields          = lr_tabfields
         tabfields_all      = lr_tabfields_all
         table_to_alias_map = mt_table_to_alias_map
         exclude_function   = VALUE #(
          ( zif_dbbr_c_selection_functions=>leave_screen_with_layout )
          ( zif_dbbr_c_selection_functions=>transfer_filter_values   )
         )
       )
    ).

    rf_no_data = lr_controller->execute_selection( ).
  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~initialize.
    ms_global_data-primary_table = mv_cds_view.

    fill_primary_entity( ).
    load_variant( ).

    IF mv_variant_id <> zif_dbbr_global_consts=>c_dummy_variant OR
       ms_global_data-called_from_adt = abap_true.

      DATA(ls_header) = mo_cds_view->get_header( ).

      IF ms_global_data-called_from_adt = abap_true.
        zcl_dbbr_usersettings_factory=>update_start_settings(
          iv_entity_id   = mv_cds_view
          iv_entity_type = zif_dbbr_c_entity_type=>cds_view
        ).
      ENDIF.

      NEW zcl_dbbr_favmenu_factory( )->refresh_most_used(
          iv_entry     = mv_cds_view
          iv_entry_raw = ls_header-entityname_raw
          iv_type      = zif_dbbr_c_favmenu_type=>cds_view
          iv_text      = ls_header-description
      ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_data_from_variant.
    DATA: lt_param_values TYPE zif_dbbr_global_types=>tt_cds_param_value.

    IF mv_variant_id = zif_dbbr_global_consts=>c_dummy_variant.
*.... Fill parameters if the cds view has any
      IF mo_cds_view->has_parameters( if_exclude_system_params = abap_true ).
*...... Read parameter values if automatic variant exists
        lt_param_values = get_params_from_variant( ).

        DATA(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
            io_tabfields     = mo_tabfield_list
            iv_cds_view_name = mv_cds_view
            it_param_values  = lt_param_values
        ).

        lo_param_popup->show( ).
        lt_param_values = lo_param_popup->get_param_values( ).

        IF lt_param_values IS INITIAL.
          RAISE EXCEPTION TYPE zcx_dbbr_variant_error
            EXPORTING
              textid = zcx_dbbr_variant_error=>missing_params
              msgv1  = |{ mo_cds_view->get_header( )-entityname_raw }|.
        ELSE.

          mt_selfields = VALUE #(
            FOR param IN lt_param_values
            ( is_parameter  = abap_true
              fieldname     = param-name
              tabname       = zif_dbbr_global_consts=>c_parameter_dummy_table
              tabname_alias = zif_dbbr_global_consts=>c_parameter_dummy_table
              sign          = zif_dbbr_c_options=>including
              option        = zif_dbbr_c_options=>equals
              low           = param-value )
          ).

*........ Create automatic variant if setting is active
          IF ms_global_data-auto_sel_filter_saving = abap_true.
            create_automatic_variant( mt_selfields ).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      super->fill_data_from_variant( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_primary_entity.

    mo_cds_view = zcl_dbbr_cds_view_factory=>read_cds_view( mv_cds_view ).

    create_cds_fields( VALUE #(
      tabname       = mv_cds_view
      tabname_alias = mv_cds_view
      is_primary    = abap_true
    ) ).

  ENDMETHOD.

  METHOD get_params_from_variant.
    DATA: ls_variant TYPE zdbbr_variant_data .

*.. Get parameters from default variant - if there is one
    IF ms_global_data-always_load_def_variant_first = abap_true.
      ls_variant = zcl_dbbr_variant_factory=>get_default_variant(
         iv_entity_id       = mv_cds_view
         iv_entity_type     = zif_dbbr_c_entity_type=>cds_view
         if_load_completely = abap_true
      ).
      IF ls_variant IS NOT INITIAL.
        rt_params = get_parameter_values_from_var( it_vardata = ls_variant-variant_data ).
        RETURN.
      ENDIF.
    ENDIF.

*.. Get parameters from automatic variant - if setting is active
    IF ms_global_data-auto_sel_filter_saving = abap_true.
      ls_variant = zcl_dbbr_variant_factory=>get_automatic_variant(
         iv_entity_id       = mv_cds_view
         iv_entity_type     = zif_dbbr_c_entity_type=>cds_view
         if_load_completely = abap_true
      ).
      rt_params = get_parameter_values_from_var( it_vardata = ls_variant-variant_data ).
    ENDIF.
  ENDMETHOD.

  METHOD get_parameter_values_from_var.

    LOOP AT it_vardata ASSIGNING FIELD-SYMBOL(<ls_vardata>) WHERE tabname = zif_dbbr_global_consts=>c_parameter_dummy_table.

*...... find the table field to get general information about selfield
      TRY.
          DATA(lr_field_ref) = mo_tabfield_list->get_field_ref(
             iv_tabname_alias = <ls_vardata>-tabname
             iv_fieldname     = <ls_vardata>-fieldname
          ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      DATA(ls_param) = VALUE zif_dbbr_global_types=>ty_cds_param_value(
        name = <ls_vardata>-fieldname
      ).

      IF <ls_vardata>-system_value_type IS NOT INITIAL.
        zcl_dbbr_system_helper=>get_system_value( EXPORTING iv_system_value_type = <ls_vardata>-system_value_type
                                                  IMPORTING ev_system_value      = ls_param-value ).
      ELSE.
        ls_param-value = <ls_vardata>-low_val.
      ENDIF.

      rt_param_data = VALUE #( BASE rt_param_data ( ls_param ) ).

    ENDLOOP.
  ENDMETHOD.

  METHOD create_automatic_variant.
    DATA(ls_variant) = zcl_dbbr_variant_creator=>create_variant(
        iv_entity_id    = mv_cds_view
        iv_entity_type  = zif_dbbr_c_entity_type=>cds_view
        it_selfields    = it_selfields
    ).

    CHECK ls_variant IS NOT INITIAL.

    zcl_dbbr_variant_factory=>save_auto_variant( ls_variant ).
  ENDMETHOD.

ENDCLASS.
