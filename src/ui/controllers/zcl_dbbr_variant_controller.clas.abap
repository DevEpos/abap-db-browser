"! <p class="shorttext synchronized" lang="en">Controlller zum Speichern/Lesen/Löschen von Varianten</p>
CLASS zcl_dbbr_variant_controller DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    CONSTANTS:
      BEGIN OF mc_modes,
        save   TYPE int1 VALUE 1,
        read   TYPE int1 VALUE 2,
        delete TYPE int1 VALUE 3,
      END OF mc_modes .

    METHODS constructor
      IMPORTING
        !iv_screen_mode    TYPE zdbbr_selscreen_mode
        !iv_mode           TYPE int1
        !is_query_info     TYPE zdbbr_query_info
        !ir_multi_or_itab  TYPE REF TO zdbbr_or_seltab_itab
        !ir_tabfields      TYPE REF TO zcl_dbbr_tabfield_list
        !ir_tabfields_grpd TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS call_variant_f4 .
    METHODS leave_screen .
  PROTECTED SECTION.
private section.

  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

  data:
    BEGIN OF ms_ui_refs,
      selfields_multi          TYPE REF TO zdbbr_selfield_itab,
      global_data              TYPE REF TO zdbbr_global_data,
      table_data               TYPE REF TO zdbbr_selfield_itab,
      variant_name             TYPE REF TO zdbbr_variant_name,
      variant_text             TYPE REF TO ddtext,
      xoutput_fields           TYPE REF TO boolean,
      xsort_fields             TYPE REF TO boolean,
      variant_table_name       TYPE REF TO tabname16,
      variant_query_name      TYPE REF TO zdbbr_query_name,
      variant_for_table_title  TYPE REF TO syst_title,
      variant_for_query_title TYPE REF TO syst_title,
      variant_for_cds_title    TYPE REF TO syst_title,
      variant_cds_name         TYPE REF TO zdbbr_cds_view_name,
    END OF ms_ui_refs .
  data MF_query_MODE type BOOLEAN .
  data MS_query_INFO type ZDBBR_query_INFO .
  data MR_TABFIELDS type ref to ZCL_DBBR_TABFIELD_LIST .
  data MR_TABFIELDS_GROUPED type ref to ZCL_DBBR_TABFIELD_LIST .
  data MV_MODE type INT1 .
  data MR_MULTI_OR_ITAB type ref to ZDBBR_OR_SELTAB_ITAB .
  "! <p class="shorttext synchronized" lang="en">Mode for Seleciton Screen of DB Browser</p>
  data MV_SELSCREEN_MODE type ZDBBR_SELSCREEN_MODE .

  "! <p class="shorttext synchronized" lang="en">Read variant </p>
  "!
  methods READ_VARIANT .
  "! <p class="shorttext synchronized" lang="en">Delete variant</p>
  "!
  methods DELETE_VARIANT .
  "! <p class="shorttext synchronized" lang="en">Create new variant</p>
  "!
  methods CREATE_VARIANT .


ENDCLASS.



CLASS ZCL_DBBR_VARIANT_CONTROLLER IMPLEMENTATION.


  METHOD call_variant_f4.
*&---------------------------------------------------------------------*
*& Description: Calls value help for getting existing variant
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF lty_variant_f4,
             variant_name      TYPE zdbbr_variant_name,
             description       TYPE ddtext,
             has_output_fields TYPE zdbbr_defined_outputfields,
             has_sort_fields   TYPE zdbbr_defined_sortfields,
             created_by        TYPE zdbbr_created_by,
           END OF lty_variant_f4.

    DATA: lt_variants TYPE TABLE OF lty_variant_f4.

    " get variants from db
    DATA(lr_variant_factory) = NEW zcl_dbbr_variant_factory( ).
    lr_variant_factory->find_variants(
      EXPORTING
        iv_variant_name = ms_ui_refs-variant_name->*
        iv_entity_id    = SWITCH #(
          mv_selscreen_mode
          WHEN zif_dbbr_c_selscreen_mode=>query THEN
             ms_query_info-query_id
          ELSE
             ms_ui_refs-global_data->primary_table
        )
        iv_entity_type  = mv_selscreen_mode
      IMPORTING
        et_variant_info = DATA(lt_variant_info)
    ).

    lt_variants = CORRESPONDING #( lt_variant_info ).

    DATA(lv_return) = zcl_dbbr_f4_helper=>call_int_table_f4(
        it_table_search      = lt_variants
        iv_return_field_name = 'VARIANT_NAME'
    ).

    IF lv_return IS NOT INITIAL.
      ms_ui_refs-variant_name->* = lv_return.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mv_mode = iv_mode.
    mv_selscreen_mode = iv_screen_mode.
    ms_query_info = is_query_info.
    mr_tabfields = ir_tabfields.
    mr_tabfields_grouped = ir_tabfields_grpd.
    mr_multi_or_itab = ir_multi_or_itab.

    " init some global data references
    DATA(lr_data_cache) = ZCL_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    ms_ui_refs-table_data = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields ) ).
    ms_ui_refs-global_data = CAST zdbbr_global_data( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).
    ms_ui_refs-selfields_multi = CAST zdbbr_selfield_itab( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_selection_fields_multi ) ).
    ms_ui_refs-variant_name = CAST zdbbr_variant_name( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_varnam ) ).
    ms_ui_refs-variant_text = CAST ddtext( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_vartxt ) ).
    ms_ui_refs-xsort_fields = CAST boolean( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_xsort ) ).
    ms_ui_refs-xoutput_fields = CAST boolean( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_xfield ) ).
    ms_ui_refs-variant_table_name = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_tab ) ).
    ms_ui_refs-variant_for_table_title = CAST syst_title( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_tvar_t ) ).
    ms_ui_refs-variant_for_query_title = CAST syst_title( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_svar_t ) ).
    ms_ui_refs-variant_for_cds_title = CAST syst_title( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>C_T_CDS_VIEW_VARIANT ) ).
    ms_ui_refs-variant_query_name = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_scrnam ) ).
    ms_ui_refs-variant_cds_name = cast #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_cds_view_name ) ).

  ENDMETHOD.


  METHOD create_variant.
*&---------------------------------------------------------------------*
*& Description: Creates the variant
*&---------------------------------------------------------------------*
    DATA: lv_tabname             TYPE tabname,
          lv_answer              TYPE char1,
          lv_existing_variant_id TYPE zdbbr_variant_id,
          ls_variant             TYPE zdbbr_variant_data,
          ls_vardata_entry       TYPE zdbbr_vardata,
          lv_entity_id           type zdbbr_entity_id.

    IF mf_query_mode = abap_true.
      lv_entity_id = ms_query_info-query_id.
    ELSE.
      lv_entity_id = ms_ui_refs-global_data->primary_table.
    ENDIF.

    DATA(lr_variant_factory) = NEW zcl_dbbr_variant_factory( ).

    " 1) does variant for this name already exist?
    IF lr_variant_factory->variant_exists( iv_variant_name = ms_ui_refs-variant_name->*
                                           iv_entity_id    = lv_entity_id
                                           iv_entity_type  = mv_selscreen_mode ).

      IF zcl_dbbr_appl_util=>popup_to_confirm(
             iv_title      = |{ text-ms2 }|
             iv_query      = |{ text-013 } '{ ms_ui_refs-variant_name->* }'. { text-014 } { text-016 }|
             iv_icon_type  = 'ICON_MESSAGE_WARNING' ) <> zif_dbbr_global_consts=>gc_popup_answers-yes.
        RETURN.
      ENDIF.

      lr_variant_factory->get_variant(
        EXPORTING
          iv_variant_name    = ms_ui_refs-variant_name->*
          iv_entity_id       = lv_entity_id
          iv_entity_type     = mv_selscreen_mode
          if_load_completely = abap_false
        IMPORTING
          es_variant         = DATA(ls_existing_variant)
      ).

      lv_existing_variant_id = ls_existing_variant-variant_id.
    ENDIF.

    ls_variant = zcl_dbbr_variant_creator=>create_variant(
        iv_variant_id          = ls_existing_variant-variant_id
        iv_variant_name        = ms_ui_refs-variant_name->*
        iv_entity_id           = lv_entity_id
        iv_entity_type         = mv_selscreen_mode
        iv_variant_description = ms_ui_refs-variant_text->*
        if_has_output_fields   = ms_ui_refs-xoutput_fields->*
        if_has_sort_fields     = ms_ui_refs-xsort_fields->*
        it_selfields           = ms_ui_refs-table_data->*
        it_multi_selfields     = ms_ui_refs-selfields_multi->*
        it_multi_or            = mr_multi_or_itab->*
    ).
***    " 2) create variant data
***    ls_variant = VALUE zdbbr_variant_data(
***        variant_id          = ls_existing_variant-variant_id
***        variant_name        = ms_ui_refs-variant_name->*
***        entity_id           = lv_entity_id
***        entity_type         = mv_selscreen_mode
***        created_by          = sy-uname
***        description         = ms_ui_refs-variant_text->*
***        has_output_fields   = ms_ui_refs-xoutput_fields->*
***        has_sort_fields     = ms_ui_refs-xsort_fields->*
***        has_multi_or_values = xsdbool( mr_multi_or_itab->* IS NOT INITIAL )
***    ).
***
***    ls_variant = CORRESPONDING #( BASE ( ls_variant ) ms_ui_refs-global_data->* ).
***
***    " save global data, like ALV-layout, technical setting, ...
***    " create fields for layout saving
***    LOOP AT ms_ui_refs-table_data->* ASSIGNING FIELD-SYMBOL(<ls_selection_field>)
***      WHERE is_table_header = abap_false.
***      DATA(lv_counter) = 0.
***
***      " fill default values
***      ls_vardata_entry = VALUE zdbbr_vardata(
***          tabname         = <ls_selection_field>-tabname
***          fieldname       = <ls_selection_field>-fieldname
***      ).
***
***      add_value_variant_data(
***        EXPORTING is_selfield     = <ls_selection_field>
***                  it_multi        = ms_ui_refs-selfields_multi->*
***        CHANGING  cs_layout_data  = ls_vardata_entry
***                  ct_layout_data  = ls_variant-variant_data
***                  cv_line_counter = lv_counter
***      ).
***
***      " group by?
***      IF <ls_selection_field>-group_by = abap_true.
***        add_special_variant_data(
***          EXPORTING iv_layout_data_type = zif_dbbr_global_consts=>gc_variant_datatypes-group_by
***          CHANGING  cs_layout_data      = ls_vardata_entry
***                    ct_layout_data      = ls_variant-variant_data
***                    cv_line_counter     = lv_counter              ).
***      ENDIF.
***
***      " aggregation ?
***      IF <ls_selection_field>-aggregation <> space.
***        add_special_variant_data(
***          EXPORTING iv_layout_data_type = zif_dbbr_global_consts=>gc_variant_datatypes-aggregation
***                    iv_layout_data_low  = <ls_selection_field>-aggregation
***          CHANGING  cs_layout_data      = ls_vardata_entry
***                    ct_layout_data      = ls_variant-variant_data
***                    cv_line_counter     = lv_counter
***        ).
***      ENDIF.
***
***    ENDLOOP.
***
***    """ add multi or data to variant
***    LOOP AT mr_multi_or_itab->* ASSIGNING FIELD-SYMBOL(<ls_multi_or_tuple>).
***
***      ls_vardata_entry = VALUE zdbbr_vardata(
***          tuple_id = <ls_multi_or_tuple>-pos
***      ).
***
***      " 1) process single tuple data
***      LOOP AT <ls_multi_or_tuple>-values ASSIGNING FIELD-SYMBOL(<ls_multi_or_tuple_data>).
***        ls_vardata_entry-tabname = <ls_multi_or_tuple_data>-tabname.
***        ls_vardata_entry-fieldname = <ls_multi_or_tuple_data>-fieldname.
***
***        add_multi_or_variant_data(
***          EXPORTING
***            is_selfield_info = <ls_multi_or_tuple_data>
***          CHANGING
***            cs_layout_data   = ls_vardata_entry
***            ct_layout_data   = ls_variant-variant_data
***        ).
***      ENDLOOP.
***    ENDLOOP.

    " create user/ddic sort order for output / sorting fields
    DATA(lr_tabfields) = mr_tabfields->copy( ).
    lr_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).
    lr_tabfields->sort( ).
    lr_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-sort ).
    lr_tabfields->sort( ).
    " clear flag for selected fields
    lr_tabfields->clear_active_flag( if_clear_selection = abap_true
                                     if_clear_output    = xsdbool( ms_ui_refs-xoutput_fields->* = abap_false )
                                     if_clear_sort      = xsdbool( ms_ui_refs-xsort_fields->*   = abap_false ) ).

    " save output / order by fields
    lr_tabfields->get_fields(
      EXPORTING
        if_include_only_checked = abap_true
        if_consider_output      = ms_ui_refs-xoutput_fields->*
        if_consider_sorted      = ms_ui_refs-xsort_fields->*
      IMPORTING
        et_fields               = DATA(lt_fields_ui)
    ).

    ls_variant-fields = CORRESPONDING #( lt_fields_ui ).

    " save the variant
    NEW zcl_dbbr_variant_factory( )->save_variant( ls_variant ).

    MESSAGE s028(zdbbr_info) WITH ms_ui_refs-variant_name->*.
    zcl_dbbr_screen_helper=>leave_screen( ).

  ENDMETHOD.


  METHOD delete_variant.
*&---------------------------------------------------------------------*
*& Description: Deletes specified variant
*&---------------------------------------------------------------------*
    DATA(lr_variant_factory) = NEW zcl_dbbr_variant_factory( ).

    DATA(lv_entity_id) = SWITCH zdbbr_entity_id(
      mv_selscreen_mode
      WHEN zif_dbbr_c_entity_type=>query THEN
        ms_query_info-query_id
      WHEN zif_dbbr_c_entity_type=>table OR
           zif_dbbr_c_entity_type=>cds_view THEN
        ms_ui_refs-global_data->primary_table
    ).

    lr_variant_factory->get_variant(
        EXPORTING
          iv_variant_name    = ms_ui_refs-variant_name->*
          iv_entity_id       = lv_entity_id
          iv_entity_type     = mv_selscreen_mode
          if_load_completely = abap_false
        IMPORTING
          es_variant         = DATA(ls_existing_variant)
    ).

    IF ls_existing_variant IS INITIAL.
      MESSAGE e030(zdbbr_info) WITH ms_ui_refs-variant_name->*.
      RETURN.
    ENDIF.

    lr_variant_factory->delete_variant( ls_existing_variant-variant_id ).

    MESSAGE s029(zdbbr_info) WITH ms_ui_refs-variant_name->*.
    zcl_dbbr_screen_helper=>leave_screen( ).

  ENDMETHOD.


  METHOD leave_screen.
*& Description: Leaves the screen
*&---------------------------------------------------------------------*
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD read_variant.
*& Description: Reads the specified variant
*&---------------------------------------------------------------------*
    DATA: ls_alv_variant TYPE disvariant.

    DATA(lr_variant_factory) = NEW zcl_dbbr_variant_factory( ).

    DATA(lr_variant_loader) = NEW zcl_dbbr_variant_loader(
        iv_variant_name      = ms_ui_refs-variant_name->*
        iv_entity_id         = cond #( when mv_selscreen_mode = zif_dbbr_c_selscreen_mode=>query then
                                          ms_query_info-query_id
                                       else
                                          ms_ui_refs-global_data->primary_table )
        iv_entity_type       = mv_selscreen_mode
        ir_t_multi_or        = mr_multi_or_itab
        ir_t_selfields       = ms_ui_refs-table_data
        ir_t_selfields_multi = ms_ui_refs-selfields_multi
        ir_tabfields         = mr_tabfields
        ir_s_global_data     = ms_ui_refs-global_data
        ir_tabfields_grouped = mr_tabfields_grouped
    ).

    TRY.
        DATA(ls_variant_info) = lr_variant_loader->load_variant( ).
        ms_ui_refs-xoutput_fields->* = ls_variant_info-has_output_fields.
        ms_ui_refs-xsort_fields->* = ls_variant_info-has_sort_fields.
        ms_ui_refs-variant_text->* = ls_variant_info-description.

        zcl_dbbr_screen_helper=>leave_screen( ).
      CATCH zcx_dbbr_validation_exception INTO DATA(lr_valid_error).
        lr_valid_error->show_message( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.

    IF mv_mode = mc_modes-save AND
       ms_ui_refs-variant_name->* IS INITIAL.

      CLEAR: ms_ui_refs-xoutput_fields->*,
             ms_ui_refs-xsort_fields->*.
    ENDIF.

    CASE mv_selscreen_mode.
      WHEN zif_dbbr_c_selscreen_mode=>table.
        ms_ui_refs-variant_table_name->* = ms_ui_refs-global_data->primary_table.

      WHEN zif_dbbr_c_selscreen_mode=>query.
        ms_ui_refs-variant_query_name->* = ms_query_info-query_name.

      WHEN zif_dbbr_c_selscreen_mode=>cds_view.
        ms_ui_refs-variant_cds_name->* = ms_ui_refs-global_data->primary_table.

    ENDCASE.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_variant_controller
            global_ref    = me )
        )
        iv_start_line    = 2
        iv_start_column  = 10
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.
    zcl_dbbr_screen_helper=>leave_screen( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = SWITCH #( mv_selscreen_mode
      WHEN zif_dbbr_c_selscreen_mode=>table    THEN zif_dbbr_screen_ids=>c_table_variant
      WHEN zif_dbbr_c_selscreen_mode=>query   THEN zif_dbbr_screen_ids=>c_query_variant
      WHEN zif_dbbr_c_selscreen_mode=>cds_view THEN zif_dbbr_screen_ids=>c_cds_view_variant
    ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    CASE cv_function_code.
      WHEN 'ENTER'.
        CASE mv_mode.
          WHEN mc_modes-read.
            read_variant( ).
          WHEN mc_modes-delete.
            delete_variant( ).
          WHEN mc_modes-save.
            create_variant( ).
        ENDCASE.

      WHEN 'CANCEL'.
        zcl_dbbr_screen_helper=>leave_screen( ).
    ENDCASE.

    CLEAR cv_function_code.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    zif_uitb_screen_controller~set_status( ).

    LOOP AT SCREEN INTO DATA(ls_screen).
      IF ls_screen-name = zif_dbbr_main_report_var_ids=>c_p_tab OR
         ls_screen-name = zif_dbbr_main_report_var_ids=>c_p_scrnam or
         ls_screen-name = zif_dbbr_main_report_var_ids=>c_p_cds_view_name.
        ls_screen-input = 0.
        MODIFY screen FROM ls_screen.
      ELSEIF ls_screen-group1 = 'GLO'.
        ls_screen-active = 0.
        MODIFY screen FROM ls_screen.
      ELSEIF ( ls_screen-name = zif_dbbr_main_report_var_ids=>c_p_vartxt OR
               ls_screen-group1 = 'TXT'  ) AND
             mv_mode <> mc_modes-save.
        ls_screen-active = 0.
        MODIFY screen FROM ls_screen.
      ELSEIF ls_screen-group1 = 'PAR' AND mv_mode <> mc_modes-save.
        ls_screen-active = 0.
        MODIFY screen FROM ls_screen.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    DATA: lt_excl TYPE TABLE OF sy-ucomm.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '0600'
      TABLES
        p_exclude = lt_excl.

    DATA(lv_title) = SWITCH string(
        mv_mode
        WHEN mc_modes-read THEN
            text-v01
        WHEN mc_modes-save THEN
            text-v02
        WHEN mc_modes-delete THEN
            text-v03
    ).

    CASE mv_selscreen_mode.

      WHEN zif_dbbr_c_selscreen_mode=>table.
        ms_ui_refs-variant_for_table_title->* = lv_title.

      WHEN zif_dbbr_c_selscreen_mode=>query.
        ms_ui_refs-variant_for_query_title->* = lv_title.

      WHEN zif_dbbr_c_selscreen_mode=>cds_view.
        ms_ui_refs-variant_for_cds_title->* = lv_title.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
  ENDMETHOD.
ENDCLASS.
