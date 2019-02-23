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
       iv_entity_id          = mv_cds_view
       iv_entity_type        = zif_dbbr_c_selscreen_mode=>cds_view
       it_selection_fields   = mt_selfields
       it_multi_or           = mt_selfields_or
       is_technical_infos    = CORRESPONDING #( ms_global_data )
       it_selfields_multi    = mt_selfields_multi
       ir_tabfields          = lr_tabfields
       ir_tabfields_all      = lr_tabfields_all
       it_table_to_alias_map = mt_table_to_alias_map
       it_exclude_function   = VALUE #(
         ( zif_dbbr_c_selection_functions=>leave_screen_with_layout )
         ( zif_dbbr_c_selection_functions=>transfer_filter_values   )
       )
    ).

    lr_controller->execute_selection( ).
  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~initialize.
    ms_global_data-primary_table = mv_cds_view.

    fill_primary_entity( ).
    load_variant( ).

    CHECK mv_variant_id <> zif_dbbr_global_consts=>c_dummy_variant.

    DATA(ls_header) = mo_cds_view->get_header( ).

    NEW zcl_dbbr_favmenu_factory( )->refresh_most_used(
        iv_entry     = mv_cds_view
        iv_entry_raw = ls_header-entityname_raw
        iv_type      = zif_dbbr_c_favmenu_type=>cds_view
        iv_text      = ls_header-description
    ).
  ENDMETHOD.

  METHOD fill_data_from_variant.
    IF mv_variant_id = zif_dbbr_global_consts=>c_dummy_variant.
*.... Fill parameters if the cds view has any
      IF mo_cds_view->has_parameters( ).
        DATA(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
            io_tabfields = mo_tabfield_list
        ).
        lo_param_popup->show( ).
        DATA(lt_param_values) = lo_param_popup->get_param_values( ).

        IF lt_param_values IS INITIAL.
          RAISE EXCEPTION TYPE zcx_dbbr_variant_error
            EXPORTING
              textid = zcx_dbbr_variant_error=>missing_params
              msgv1  = |{ mo_cds_view->get_header( )-entityname_raw }|.
        ELSE.
          mt_selfields = VALUE #(
            FOR param IN lt_param_values
            ( is_parameter = abap_true
              fieldname    = param-name
              low          = param-value )
          ).
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

ENDCLASS.
