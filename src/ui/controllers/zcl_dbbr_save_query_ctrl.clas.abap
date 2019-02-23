"! <p class="shorttext synchronized" lang="en">Controller for saving a query</p>
CLASS zcl_dbbr_save_query_ctrl DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    ALIASES show
      FOR zif_uitb_screen_controller~call_screen .
    ALIASES was_saved
      FOR zif_uitb_screen_controller~was_not_cancelled .

    METHODS constructor
      IMPORTING
        !ir_tabfield_list     TYPE REF TO zcl_dbbr_tabfield_list
        !is_query_info        TYPE zdbbr_query_info
        !is_join_def          TYPE zdbbr_join_def OPTIONAL
        !ir_t_multi_or        TYPE REF TO zdbbr_or_seltab_itab OPTIONAL
        !ir_t_selfields       TYPE REF TO zdbbr_selfield_itab OPTIONAL
        !ir_t_multi_selfields TYPE REF TO zdbbr_selfield_itab OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Returns the name of the saved query</p>
    METHODS get_query_name
      RETURNING
        VALUE(rv_query_name) TYPE zdbbr_query_name .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    DATA mr_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list .
    DATA ms_query_info TYPE zdbbr_query_info .
    DATA mr_ui_global_data TYPE REF TO zdbbr_global_data .
    DATA mr_ui_query_name TYPE REF TO zdbbr_query_name .
    DATA mr_ui_query_desc TYPE REF TO ddtext .
    DATA mr_ui_use_output_fields TYPE REF TO abap_bool .
    DATA mr_ui_use_sort_fields TYPE REF TO abap_bool .
    DATA mr_ui_use_selection_criteria TYPE REF TO abap_bool .
    DATA mr_ui_selection_fields TYPE REF TO zdbbr_selfield_itab .
    DATA mr_ui_selfields_multi TYPE REF TO zdbbr_selfield_itab .
    DATA mr_ui_is_global TYPE REF TO boolean .
    "! <p class="shorttext synchronized" lang="en">Definition of a Join</p>
    DATA ms_join_def TYPE zdbbr_join_def .
    DATA mf_saved TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">List of selection fields</p>
    DATA mr_t_selfields TYPE REF TO zdbbr_selfield_itab .
    "! <p class="shorttext synchronized" lang="en">List of selection fields</p>
    DATA mr_t_multi_selfields TYPE REF TO zdbbr_selfield_itab .
    "! <p class="shorttext synchronized" lang="en">List of OR selection tables</p>
    DATA mr_t_multi_or TYPE REF TO zdbbr_or_seltab_itab .
    DATA mr_query_f TYPE REF TO zcl_dbbr_query_factory.

    METHODS save_query .
ENDCLASS.



CLASS zcl_dbbr_save_query_ctrl IMPLEMENTATION.


  METHOD constructor.
    DEFINE read_parameter_reference.
      &1 = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>&2 ) ).
    END-OF-DEFINITION.

    mr_tabfield_list = ir_tabfield_list.
    mr_query_f = NEW #( ).
    ms_query_info = is_query_info.
    ms_join_def = is_join_def.
    mr_t_selfields = ir_t_selfields.
    mr_t_multi_or = ir_t_multi_or.
    mr_t_multi_selfields = ir_t_multi_selfields.

    " init some global data references from ui
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).
    read_parameter_reference:
      mr_ui_global_data            c_s_data,
      mr_ui_query_name            c_p_scrnam,
      mr_ui_query_desc            c_p_scrdec,
      mr_ui_use_output_fields      c_p_xfield,
      mr_ui_use_sort_fields        c_p_xsort,
      mr_ui_is_global              c_p_xglob,

      mr_ui_selection_fields       c_t_selection_fields,
      mr_ui_selfields_multi        c_t_selection_fields_multi,

      mr_ui_use_selection_criteria c_p_has_selection_criteria.
  ENDMETHOD.


  METHOD get_query_name.
    rv_query_name = mr_ui_query_name->*.
  ENDMETHOD.


  METHOD save_query.
    DATA: lv_primary_entity_type TYPE zdbbr_entity_type.

*.. validate query name
    TRY .
        zcl_dbbr_query_helper=>check_query_name( iv_query_name = mr_ui_query_name->*
                                                   if_global     = mr_ui_is_global->* ).
      CATCH zcx_dbbr_exception INTO DATA(lr_error).
        lr_error->show_message( iv_message_type = 'S' ).
        RETURN.
    ENDTRY.

    " 2) does this query already exists?
    DATA(lr_query_factory) = NEW zcl_dbbr_query_factory( ).
    DATA(ls_query) = lr_query_factory->get_query( mr_ui_query_name->* ).
    IF ls_query IS NOT INITIAL.

      """ check if overwrite is possible
      IF ls_query-is_global = abap_false AND ls_query-created_by <> sy-uname.
        MESSAGE e037(zdbbr_exception) WITH ls_query-query_name ls_query-created_by.
        RETURN.
      ENDIF.

      IF zcl_dbbr_appl_util=>popup_to_confirm(
            iv_title                 = 'Save?'
            iv_query                 = |{ 'There already is a query with the name'(011) } '{ mr_ui_query_name->* }'. | &&
                                       |{ 'Do you want to override the existing query?'(012) }|
            iv_icon_type             = 'ICON_MESSSAGE_WARNING' ) <> '1'.
        RETURN.
      ENDIF.

      " get information of existing query
      DATA(ls_query_existing) = lr_query_factory->get_query( iv_query_name = mr_ui_query_name->* ).
      lv_primary_entity_type = ls_query_existing-entity_type.
    ENDIF.

    IF lv_primary_entity_type IS INITIAL.
      SELECT SINGLE type
        FROM zdbbr_i_databaseentity( p_language = @sy-langu )
        WHERE entity = @ms_query_info-primary_table
      INTO @lv_primary_entity_type.
    ENDIF.

    " 3) save the query
    mr_tabfield_list->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-selection ).

    mr_tabfield_list->get_fields(
      EXPORTING if_include_only_checked = abap_true
                if_consider_selected    = abap_true
                if_consider_output      = mr_ui_use_output_fields->*
                if_consider_sorted      = mr_ui_use_sort_fields->*
      IMPORTING et_fields               = DATA(lt_fields_ui)
    ).

    DATA(lt_fields) = CORRESPONDING zdbbr_tabfield_info_itab( lt_fields_ui ).

    DATA(ls_query_data) = VALUE zdbbr_query_data(
        query_id         = ls_query_existing-query_id
        entity_type      = lv_primary_entity_type
        ref_join_id       = ls_query_existing-ref_join_id
        is_global         = mr_ui_is_global->*
        created_by        = COND #( WHEN ls_query_existing-created_by IS NOT INITIAL THEN ls_query_existing-created_by ELSE sy-uname )
        created_date      = COND #( WHEN ls_query_existing IS NOT INITIAL THEN ls_query_existing-created_date ELSE sy-datum )
        changed_date      = COND #( WHEN ls_query_existing IS NOT INITIAL THEN sy-datum )
        query_name       = mr_ui_query_name->*
        primary_table     = mr_ui_global_data->primary_table
        primary_table_alias = COND #( WHEN ms_join_def-tables IS NOT INITIAL THEN ms_join_def-primary_table_alias )
        description       = mr_ui_query_desc->*
        has_output_fields = mr_ui_use_output_fields->*
        has_sort_fields   = mr_ui_use_sort_fields->*
        has_filter_values = mr_ui_use_selection_criteria->*
*...... Retrieve stored jump destinations
        jump_fields       = COND #( WHEN ls_query_existing-query_id IS NOT INITIAL THEN
                                      CORRESPONDING #( DEEP
                                         NEW zcl_dbbr_jump_destination_f( )->get_jump_destinations( ls_query_existing-query_id ) )
                                      )
        join_def          = ms_join_def
        fields            = lt_fields
        tables            = CORRESPONDING #( mr_tabfield_list->get_table_list( ) )
        formula           = ms_query_info-formula
    ).

    DATA(lv_new_query_id) = mr_query_f->save_query( ls_query_data ).

    DATA(lr_variant_f) = NEW zcl_dbbr_variant_factory( ).

*.. if query has filter criteria, store / overwrite them es well
    IF mr_ui_use_selection_criteria->* = abap_true.
*.... Check if there already is a default variant for the query
      DATA(ls_variant) = zcl_dbbr_variant_creator=>create_variant(
          iv_entity_id           = CONV #( lv_new_query_id )
          iv_variant_id          = lr_variant_f->find_default_query_variant( iv_query_id = lv_new_query_id )
          iv_entity_type         = zif_dbbr_c_entity_type=>query
          iv_variant_description = 'Default'(013)
          it_selfields           = mr_t_selfields->*
          it_multi_selfields     = mr_t_multi_selfields->*
          it_multi_or            = mr_t_multi_or->*
      ).
      IF ls_variant-variant_data IS NOT INITIAL.
        lr_variant_f->save_variant( is_var_data = ls_variant ).
      ENDIF.
    ELSE.
*.... Delete existing default variant
      DATA(lv_default_variant_id) = lr_variant_f->find_default_query_variant( iv_query_id = lv_new_query_id ).
      IF lv_default_variant_id IS NOT INITIAL.
        lr_variant_f->delete_variant( lv_default_variant_id ).
      ENDIF.
    ENDIF.

    MESSAGE s022(zdbbr_info) WITH mr_ui_query_name->*.
    mf_saved = abap_true.

    zcl_dbbr_screen_helper=>leave_screen( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    mr_ui_query_name->* = ms_query_info-query_name.
    mr_ui_query_desc->* = ms_query_info-description.
    mr_ui_is_global->* = ms_query_info-is_global.
    mr_ui_use_output_fields->* = abap_true.
    mr_ui_use_sort_fields->* = abap_true.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_save_query_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
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
    result = zif_dbbr_screen_ids=>c_save_query.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    CASE cv_function_code.
      WHEN 'SAVE' OR 'SAVE_LOAD'.
        save_query( ).

    ENDCASE.

    CLEAR cv_function_code.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    zif_uitb_screen_controller~set_status( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    zcl_uitb_screen_util=>set_selscreen_status(
        iv_status              = '0710'
        iv_repid               = zif_dbbr_c_report_id=>main
        it_excluding_functions = VALUE #( ( 'SAVE_LOAD' ) )
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_saved.
  ENDMETHOD.
ENDCLASS.
