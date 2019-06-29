"! <p class="shorttext synchronized" lang="en">Controlller zum Speichern/Lesen/LÃ¶schen von Varianten</p>
CLASS zcl_dbbr_variant_controller DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    CONSTANTS:
      BEGIN OF c_modes,
        save   TYPE int1 VALUE 1,
        delete TYPE int1 VALUE 3,
      END OF c_modes .

    METHODS constructor
      IMPORTING
        iv_screen_mode          TYPE zdbbr_selscreen_mode
        if_default_variant_mode TYPE abap_bool OPTIONAL
        if_alv_mode             TYPE abap_bool OPTIONAL
        iv_entity_id            TYPE zdbbr_entity_id
        iv_entity_name          TYPE zdbbr_entity_id_raw
        iv_mode                 TYPE int1
        ir_s_global_data        TYPE REF TO zdbbr_global_data
        ir_t_selfields          TYPE REF TO zdbbr_selfield_itab OPTIONAL
        ir_t_selfields_multi    TYPE REF TO zdbbr_selfield_itab OPTIONAL
        ir_t_multi_or           TYPE REF TO zdbbr_or_seltab_itab OPTIONAL
        io_tabfields            TYPE REF TO zcl_dbbr_tabfield_list
        io_tabfields_grpd       TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS call_variant_f4 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    DATA:
      BEGIN OF ms_ui_refs,
        variant_name            TYPE REF TO zdbbr_variant_name,
        variant_text            TYPE REF TO ddtext,
        xfilter_fields          TYPE REF TO abap_bool,
        xoutput_fields          TYPE REF TO abap_bool,
        xsort_fields            TYPE REF TO abap_bool,
        xcolumn_widths          TYPE REF TO abap_bool,
        variant_table_name      TYPE REF TO tabname16,
        variant_query_name      TYPE REF TO zdbbr_query_name,
        variant_for_table_title TYPE REF TO syst_title,
        variant_for_query_title TYPE REF TO syst_title,
        variant_for_cds_title   TYPE REF TO syst_title,
        variant_cds_name        TYPE REF TO zdbbr_cds_view_name,
      END OF ms_ui_refs .
    DATA mr_t_selfields TYPE REF TO zdbbr_selfield_itab.
    DATA mr_t_selfields_multi TYPE REF TO zdbbr_selfield_itab.
    DATA mr_s_global_data TYPE REF TO zdbbr_global_data.
    DATA mo_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_tabfields_grouped TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mv_mode TYPE int1 .
    DATA mr_multi_or_itab TYPE REF TO zdbbr_or_seltab_itab .
    "! <p class="shorttext synchronized" lang="en">Mode for Seleciton Screen of DB Browser</p>
    DATA mv_selscreen_mode TYPE zdbbr_selscreen_mode .
    DATA mv_entity_id   TYPE zdbbr_entity_id.
    DATA mv_entity_name TYPE zdbbr_entity_id_raw.
    DATA mf_default_variant_mode TYPE abap_bool.
    DATA mf_alv_output TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Delete variant</p>
    METHODS delete_variant .

    "! <p class="shorttext synchronized" lang="en">Create new variant</p>
    METHODS create_variant .

    "! <p class="shorttext synchronized" lang="en">Creates/updates default variant for given entity</p>
    METHODS create_default_variant.

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if there are selection criteria</p>
    METHODS exists_criteria
      RETURNING
        VALUE(rf_exists) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Persists the variant from the current data</p>
    METHODS persist_variant
      IMPORTING
        iv_variant_id TYPE zdbbr_variant_id OPTIONAL.
ENDCLASS.



CLASS zcl_dbbr_variant_controller IMPLEMENTATION.


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
    zcl_dbbr_variant_factory=>find_variants(
      EXPORTING
        iv_variant_name = ms_ui_refs-variant_name->*
        iv_entity_id    = mv_entity_id
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
    mf_default_variant_mode = if_default_variant_mode.
    mf_alv_output = if_alv_mode.
    mv_selscreen_mode = iv_screen_mode.
    mv_entity_id = iv_entity_id.
    mv_entity_name = iv_entity_name.
    mo_tabfields = io_tabfields.
    mo_tabfields_grouped = io_tabfields_grpd.
    mr_multi_or_itab = ir_t_multi_or.
    mr_s_global_data = ir_s_global_data.
    mr_t_selfields = ir_t_selfields.
    mr_t_selfields_multi = ir_t_selfields_multi.

    " init some global data references
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    ms_ui_refs-variant_name = CAST zdbbr_variant_name( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_varnam ) ).
    ms_ui_refs-variant_text = CAST ddtext( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_vartxt ) ).
    ms_ui_refs-xsort_fields = CAST boolean( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_xsort ) ).
    ms_ui_refs-xcolumn_widths = CAST abap_bool( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_has_defined_column_widths ) ).
    ms_ui_refs-xfilter_fields = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_has_selection_criteria ) ).
    ms_ui_refs-xoutput_fields = CAST boolean( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_xfield ) ).
    ms_ui_refs-variant_table_name = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_tab ) ).
    ms_ui_refs-variant_for_table_title = CAST syst_title( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_tvar_t ) ).
    ms_ui_refs-variant_for_query_title = CAST syst_title( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_svar_t ) ).
    ms_ui_refs-variant_for_cds_title = CAST syst_title( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_t_cds_view_variant ) ).
    ms_ui_refs-variant_query_name = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_scrnam ) ).
    ms_ui_refs-variant_cds_name = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_p_cds_view_name ) ).

  ENDMETHOD.


  METHOD create_variant.

    IF zcl_dbbr_variant_factory=>variant_exists( iv_variant_name = ms_ui_refs-variant_name->*
                                                 iv_entity_id    = mv_entity_id
                                                 iv_entity_type  = mv_selscreen_mode ).

      IF zcl_dbbr_appl_util=>popup_to_confirm(
             iv_title      = |{ TEXT-ms2 }|
             iv_query      = |{ TEXT-013 } '{ ms_ui_refs-variant_name->* }'. { TEXT-014 } { TEXT-016 }|
             iv_icon_type  = 'ICON_MESSAGE_WARNING' ) <> zif_dbbr_global_consts=>gc_popup_answers-yes.
        RETURN.
      ENDIF.

      zcl_dbbr_variant_factory=>get_variant(
        EXPORTING
          iv_variant_name    = ms_ui_refs-variant_name->*
          iv_entity_id       = mv_entity_id
          iv_entity_type     = mv_selscreen_mode
          if_load_completely = abap_false
        IMPORTING
          es_variant         = DATA(ls_existing_variant)
      ).
    ENDIF.

    persist_variant( ls_existing_variant-variant_id ).

  ENDMETHOD.

  METHOD create_default_variant.
    persist_variant( ).
  ENDMETHOD.

  METHOD persist_variant.

    DATA(ls_variant) = zcl_dbbr_variant_creator=>create_variant(
         iv_variant_id          = iv_variant_id
         iv_variant_name        = ms_ui_refs-variant_name->*
         iv_entity_id           = mv_entity_id
         iv_entity_type         = mv_selscreen_mode
         iv_variant_description = ms_ui_refs-variant_text->*
         if_has_output_fields   = ms_ui_refs-xoutput_fields->*
         if_has_sort_fields     = ms_ui_refs-xsort_fields->*
         it_selfields           = COND #( WHEN ms_ui_refs-xfilter_fields->* = abap_true AND mr_t_selfields IS BOUND THEN mr_t_selfields->* )
         it_multi_selfields     = COND #( WHEN ms_ui_refs-xfilter_fields->* = abap_true AND mr_t_selfields_multi IS BOUND THEN mr_t_selfields_multi->* )
         it_multi_or            = COND #( WHEN ms_ui_refs-xfilter_fields->* = abap_true AND mr_multi_or_itab IS BOUND THEN mr_multi_or_itab->* )
     ).

*.. create user/ddic sort order for output / sorting fields
    IF  ms_ui_refs-xoutput_fields->* = abap_true OR
        ms_ui_refs-xsort_fields->* = abap_true.

      DATA(lr_tabfields) = mo_tabfields->copy( ).
      lr_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).
      lr_tabfields->sort( ).
      lr_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-sort ).
      lr_tabfields->sort( ).

*.... clear flag for selected fields
      lr_tabfields->clear_active_flag( if_clear_selection = abap_true
                                       if_clear_output    = xsdbool( ms_ui_refs-xoutput_fields->* = abap_false )
                                       if_clear_sort      = xsdbool( ms_ui_refs-xsort_fields->*   = abap_false ) ).

*.... save output / order by fields
      lr_tabfields->get_fields(
        EXPORTING if_include_only_checked = abap_true
                  if_consider_output      = ms_ui_refs-xoutput_fields->*
                  if_consider_sorted      = ms_ui_refs-xsort_fields->*
        IMPORTING et_fields               = DATA(lt_fields_ui)
      ).

      ls_variant-fields = CORRESPONDING #( lt_fields_ui ).
    ENDIF.

    " save the variant
    IF mf_default_variant_mode = abap_true.
      zcl_dbbr_variant_factory=>save_default_variant( ls_variant ).
      MESSAGE s095(zdbbr_info) WITH mv_entity_name.
    ELSE.
      zcl_dbbr_variant_factory=>save_variant( ls_variant ).
      MESSAGE s028(zdbbr_info) WITH ms_ui_refs-variant_name->*.
    ENDIF.

    zcl_dbbr_screen_helper=>leave_screen( ).

  ENDMETHOD.

  METHOD exists_criteria.
    CLEAR rf_exists.

    CHECK mr_t_selfields IS BOUND.

    LOOP AT mr_t_selfields->* ASSIGNING FIELD-SYMBOL(<ls_selfield>) WHERE low IS NOT INITIAL
                                                                       OR high IS NOT INITIAL
                                                                       OR option IS NOT INITIAL.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      rf_exists = abap_true.
    ELSE.
*.. Are there any or tuple values
      rf_exists = xsdbool( mr_multi_or_itab->* IS NOT INITIAL ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_variant.

    zcl_dbbr_variant_factory=>get_variant(
        EXPORTING
          iv_variant_name    = ms_ui_refs-variant_name->*
          iv_entity_id       = mv_entity_id
          iv_entity_type     = mv_selscreen_mode
          if_load_completely = abap_false
        IMPORTING
          es_variant         = DATA(ls_existing_variant)
    ).

    IF ls_existing_variant IS INITIAL.
      MESSAGE e030(zdbbr_info) WITH ms_ui_refs-variant_name->*.
      RETURN.
    ENDIF.

    zcl_dbbr_variant_factory=>delete_variant( ls_existing_variant-variant_id ).

    MESSAGE s029(zdbbr_info) WITH ms_ui_refs-variant_name->*.
    zcl_dbbr_screen_helper=>leave_screen( ).

  ENDMETHOD.

  METHOD zif_uitb_screen_controller~call_screen.

    IF mv_mode = c_modes-save AND
       ms_ui_refs-variant_name->* IS INITIAL.

      CLEAR: ms_ui_refs-xoutput_fields->*,
             ms_ui_refs-xsort_fields->*.
    ENDIF.

    ms_ui_refs-xfilter_fields->* = exists_criteria( ).
    CASE mv_selscreen_mode.
      WHEN zif_dbbr_c_selscreen_mode=>table.
        ms_ui_refs-variant_table_name->* = mv_entity_name.

      WHEN zif_dbbr_c_selscreen_mode=>query.
        ms_ui_refs-variant_query_name->* = mv_entity_name.

      WHEN zif_dbbr_c_selscreen_mode=>cds_view.
        ms_ui_refs-variant_cds_name->* = mv_entity_name.

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

          WHEN c_modes-delete.
            delete_variant( ).

          WHEN c_modes-save.
            IF mf_default_variant_mode = abap_true.
              create_default_variant( ).
            ELSE.
              create_variant( ).
            ENDIF.

        ENDCASE.

      WHEN 'CANCEL'.
        zcl_dbbr_screen_helper=>leave_screen( ).
    ENDCASE.

    CLEAR cv_function_code.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    zif_uitb_screen_controller~set_status( ).

    LOOP AT SCREEN.
      IF mf_default_variant_mode = abap_true.
        IF screen-group1 = 'TXT' OR screen-group1 = 'VRN'.
          screen-active = 0.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF screen-group1 = 'ALV' AND mf_alv_output = abap_false.
        screen-active = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      IF screen-name = zif_dbbr_main_report_var_ids=>c_p_tab OR
         screen-name = zif_dbbr_main_report_var_ids=>c_p_scrnam OR
         screen-name = zif_dbbr_main_report_var_ids=>c_p_cds_view_name.
        screen-input = 0.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'GLO'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF ( screen-name = zif_dbbr_main_report_var_ids=>c_p_vartxt OR
               screen-group1 = 'TXT'  ) AND
             mv_mode <> c_modes-save.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'PAR' AND mv_mode <> c_modes-save.
        screen-active = 0.
        MODIFY SCREEN FROM screen.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    zcl_uitb_screen_util=>set_selscreen_status(
        iv_status = '0600'
        iv_repid  = get_report_id( )
    ).

    DATA(lv_title) = SWITCH #(
      mf_default_variant_mode
      WHEN abap_false THEN SWITCH string(
          mv_mode
          WHEN c_modes-save THEN
              'Save variant'(v02)
          WHEN c_modes-delete THEN
              'Delete variant'(v03)
      )
      WHEN abap_true THEN SWITCH string(
          mv_mode
          WHEN c_modes-save THEN
              'Save Default Variant'(017)
      )
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
