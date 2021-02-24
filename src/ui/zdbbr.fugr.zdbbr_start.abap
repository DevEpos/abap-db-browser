FUNCTION ZDBBR_START.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IF_INITIALIZE) TYPE  BOOLEAN DEFAULT ABAP_TRUE
*"     VALUE(IF_PREVENT_SKIPPING) TYPE  BOOLEAN OPTIONAL
*"----------------------------------------------------------------------

* Initially the entity type is the Table mode
  gs_entity_info-entity_type = zif_sat_c_entity_type=>table.

  IF if_initialize = abap_true.
    " clear edit flags
    CLEAR: gs_data-edit,
           gs_data-delete_mode.
  ENDIF.

* initialize some global fields
  gs_data-settings = zcl_dbbr_usersettings_factory=>get_settings( ).

  zcl_sat_system_helper=>set_locale_language( ).

* Validate the last saved data
  IF gs_data-settings-last_entity_id IS NOT INITIAL AND
     gs_data-settings-last_entity_type = zif_sat_c_entity_type=>query.
    DATA(lr_query_f) = NEW zcl_dbbr_query_factory( ).
    IF NOT lr_query_f->query_exists( iv_query_name = gs_data-settings-last_entity_id ).
      gs_data-settings-last_entity_type = zif_sat_c_entity_type=>table.
      CLEAR gs_data-settings-last_entity_id.
    ENDIF.
  ENDIF.

  DATA(lv_entity_id) = gs_data-settings-last_entity_id.
  DATA(lv_entity_type) = gs_data-settings-last_entity_type.

* Check if the DB Browser was called from ADT
  DATA(ls_context) = cl_adt_gui_integration_context=>read_context( ).
  IF ls_context-parameters IS NOT INITIAL.

    DATA(lr_param_util) = NEW zcl_sat_adt_param_util( ls_context-parameters ).

    cl_adt_gui_integration_context=>initialize_instance( VALUE #( ) ).

    DATA(lt_params) = lr_param_util->get_parameters( ).
*.. Transaction was called via ADT
    IF lt_params IS NOT INITIAL AND
       lines( lt_params ) > 1 AND
       line_exists( lt_params[ param_id = zif_sat_c_adt_start_params=>adt_call ] ).

      DATA(lv_skip_selscreen_value) = VALUE #( lt_params[ param_id = zif_sat_c_adt_start_params=>skip_selscreen ]-param_value DEFAULT abap_false ).
      TRY.
          lv_entity_id = VALUE #( lt_params[ param_id = zif_sat_c_adt_start_params=>entity_id ]-param_value DEFAULT lv_entity_id ).
          lv_entity_type = VALUE #( lt_params[ param_id = zif_sat_c_adt_start_params=>entity_mode ]-param_value DEFAULT lv_entity_type ).

*........ determine the entity for the DDLS name
          IF lv_entity_type = zif_sat_c_entity_type=>cds_view.
            SELECT SINGLE entityid,
                          ddlname,
                          parentddlname
              FROM zsat_p_cdsviewbase
              WHERE entityid = @lv_entity_id
                 OR ddlname  = @lv_entity_id
            INTO @DATA(ls_cds_entity).
            IF sy-subrc <> 0.
              MESSAGE |The CDS View { lv_entity_id } does not exist| TYPE 'E'.
              RETURN.
            ELSEIF ls_cds_entity-parentddlname IS NOT INITIAL.
*............ If the view is an extension view, just use the parent instead
              SELECT SINGLE entityid
                FROM zsat_p_cdsviewbase
                WHERE ddlname = @ls_cds_entity-parentddlname
              INTO @lv_entity_id.
            ELSE.
              lv_entity_id = ls_cds_entity-entityid.
            ENDIF.
          ELSEIF lv_entity_type = zif_sat_c_entity_type=>view.
*.......... Views are considered just like tables, as the handling is not really any different
            lv_entity_type = zif_sat_c_entity_type=>table.
          ENDIF.

          DATA(lf_skip_selscreen) = COND #( WHEN lv_skip_selscreen_value  = 'false' THEN abap_false ELSE abap_true ).
          gs_data-called_from_adt = abap_true.

          IF lf_skip_selscreen = abap_true.
*....... Always activate the live filter when selection screen is skipped
            gs_data-settings-activate_alv_live_filter = abap_true.
            DATA(lr_variant_starter) = zcl_dbbr_variant_starter_fac=>create_variant_starter(
                iv_variant_id        = zif_dbbr_c_global=>c_dummy_variant
                iv_entity_type       = lv_entity_type
                iv_variant_entity_id = CONV #( lv_entity_id )
            ).

            lr_variant_starter->initialize( ).

            lr_variant_starter->execute_variant( ).
            RETURN.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          lv_entity_id = gs_data-settings-last_entity_id.
          lv_entity_type = gs_data-settings-last_entity_type.
        CATCH zcx_sat_data_read_error INTO DATA(lx_read_error).
          lx_read_error->show_message( ).
          RETURN.
        CATCH zcx_dbbr_variant_error INTO DATA(lx_variant_error).
          lx_variant_error->show_message( ).
          RETURN.
      ENDTRY.
    ENDIF.
  ELSE.
    cl_adt_gui_integration_context=>initialize_instance( VALUE #( ) ).
  ENDIF.

* call the selection screen
  CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN'
    EXPORTING
      iv_entity_id   = lv_entity_id
      iv_entity_type = lv_entity_type.

ENDFUNCTION.
