CLASS zcl_dbbr_cds_selscreen_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selscreen_util
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ir_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data .


    METHODS get_entity_information
        REDEFINITION .
    METHODS get_title
        REDEFINITION .
    METHODS load_entity
        REDEFINITION .
    METHODS set_custom_functions
        REDEFINITION .
    METHODS update_description_texts
        REDEFINITION .
    METHODS zif_dbbr_screen_table_util~handle_pbo
        REDEFINITION .
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION .
    METHODS zif_dbbr_screen_util~handle_pbo
        REDEFINITION .
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION .
    METHODS clear
        REDEFINITION .
  PROTECTED SECTION.

    METHODS create_table_header
        REDEFINITION .
    METHODS fill_selection_mask
        REDEFINITION .
    METHODS fill_primary_entity
        REDEFINITION .


  PRIVATE SECTION.

    DATA mo_cds_view TYPE REF TO zcl_dbbr_cds_view .
    DATA mv_cds_view TYPE zdbbr_cds_view_name .
    DATA mv_cds_view_description TYPE ddtext .
    DATA mv_cds_view_name_raw TYPE zdbbr_cds_view_name .
    DATA mf_associations_loaded TYPE abap_bool .


    METHODS choose_cds_sub_entity
      IMPORTING
        !if_return_chosen_directly   TYPE abap_bool OPTIONAL
        !if_only_associations        TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_chosen_association) TYPE zdbbr_cds_association .
ENDCLASS.



CLASS zcl_dbbr_cds_selscreen_util IMPLEMENTATION.

  METHOD get_entity_information.
    super->get_entity_information(
      IMPORTING
        ev_description = ev_description
    ).
    ev_type = zif_dbbr_c_favmenu_type=>cds_view.
    ev_entity = mv_cds_view.
    ev_entity_raw = mv_cds_view_name_raw.
  ENDMETHOD.


  METHOD get_title.
    IF has_content( ).
      DATA(ls_header) = mo_cds_view->get_header( ).
      result = |{ 'CDS View'(001) } - { ls_header-entityname_raw }|.
    ELSE.
      result = |CDS Mode|.
    ENDIF.
  ENDMETHOD.


  METHOD load_entity.
    CHECK mv_cds_view IS NOT INITIAL.

    TRY.
        mo_cds_view = zcl_dbbr_cds_view_factory=>read_cds_view( iv_cds_view = mv_cds_view ).
      CATCH zcx_dbbr_data_read_error INTO DATA(lx_read_error).
        MESSAGE lx_read_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    IF mo_cds_view IS INITIAL.
*.... Set the type to CDS View anyway
      mo_data->mr_s_entity_info->* = VALUE #(
        entity_id   = space
        entity_type = mv_entity_type
      ).
      update_entity_type_sh( ).
      RETURN.
    ENDIF.

    mo_data->set_join_active( abap_false ).

    DATA(ls_cds_header) = mo_cds_view->get_header( ).
    mv_cds_view_description = mo_cds_view->get_description( ).
    mv_cds_view_name_raw = ls_cds_header-entityname_raw.

    zcl_dbbr_screen_helper=>show_progress( iv_text = `Selection Mask for ` && get_title( ) && ` is loading...`
                                           iv_progress = 1 ).

    clear_edit_flags( ).

*... disable edit mode
    mo_data->mr_s_settings->disable_edit = abap_true.

    ASSIGN mo_data->mr_s_global_data->* TO FIELD-SYMBOL(<ls_global_data>).

    " 1) load query from database
    DATA(lr_query_f) = NEW zcl_dbbr_query_factory( ).

*... fill name of query on screen
    mo_data->mr_v_selmask_entity_text->* = mv_cds_view_description.

*... clear tab field list
    mo_data->mo_tabfield_aggr_list->clear( ).
    mo_data->mo_tabfield_list->clear( ).

    fill_primary_entity( ).

*... update selection mask
    fill_selection_mask( ).

    mv_entity_id = mv_cds_view.
    mo_data->mr_s_entity_info->entity_id = mv_cds_view.

    mo_data->mr_s_entity_info->* = VALUE #(
      entity_id   = mv_cds_view
      entity_type = mv_entity_type
    ).

    update_entity_type_sh( ).

    finish_loading( ).

    rf_entity_loaded = abap_true.
  ENDMETHOD.


  METHOD set_custom_functions.
    mo_data->clear_custom_functions( ).

    mo_data->mr_s_top_custom_menu->text = 'CDS View'(001).

*... set custom functions
    mo_data->mr_s_entity_function1->text = 'Show DDL Source'(002).
    mo_data->mr_s_entity_function2->text = 'Navigate to Sub Entity'(003).
    mo_data->mr_s_entity_function3->text = 'Open with ADT'(005).
    mo_data->mr_s_entity_function4->text = 'Go to DDIC View'(006).

  ENDMETHOD.


  METHOD update_description_texts.
*.. reload the cds view with the new language
    TRY.
        mo_cds_view = zcl_dbbr_cds_view_factory=>read_cds_view( mv_cds_view ).
      CATCH zcx_dbbr_data_read_error INTO DATA(lx_read_error).
        MESSAGE lx_read_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

*.. update enitity description
    mo_data->mr_v_selmask_entity_text->* = mo_cds_view->get_header( )-description.

    DATA(lr_t_fields) = mo_data->mo_tabfield_list->get_fields_ref( ).

    LOOP AT lr_t_fields->* ASSIGNING FIELD-SYMBOL(<ls_field>)
      GROUP BY ( tabname = <ls_field>-tabname )
      ASSIGNING FIELD-SYMBOL(<ls_grouped_fields>).

      CHECK <ls_grouped_fields>-tabname = mv_cds_view.

      LOOP AT GROUP <ls_grouped_fields> ASSIGNING FIELD-SYMBOL(<ls_group_entry>).
        DATA(ls_data_element) = zcl_dbbr_dictionary_helper=>get_data_element( <ls_group_entry>-rollname ).
        <ls_group_entry>-std_short_text  = ls_data_element-scrtext_s.
        <ls_group_entry>-std_medium_text = ls_data_element-scrtext_m.
        <ls_group_entry>-std_long_text   = ls_data_element-scrtext_l.
        <ls_group_entry>-header_text     = ls_data_element-reptext.
        <ls_group_entry>-field_ddtext    = ls_data_element-ddtext.

        " update selfield column description
        DATA(lr_s_selfield) = REF #( mo_data->mr_t_table_data->*[
            tabname         = <ls_group_entry>-tabname
            fieldname       = <ls_group_entry>-fieldname
            is_table_header = abap_false ] OPTIONAL ).

        IF lr_s_selfield IS NOT INITIAL.
          lr_s_selfield->texts = CORRESPONDING #( ls_data_element ).
          lr_s_selfield->description = COND #( WHEN ls_data_element-scrtext_l IS NOT INITIAL THEN
                                                  ls_data_element-scrtext_l
                                               WHEN ls_data_element-scrtext_m IS NOT INITIAL THEN
                                                  ls_data_element-scrtext_m
                                               WHEN ls_data_element-ddtext IS NOT INITIAL THEN
                                                  ls_data_element-ddtext
                                               ELSE
                                                  <ls_group_entry>-fieldname_raw ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_table_header.
    " check if table is special parameter table
    IF iv_tabname = zif_dbbr_global_consts=>c_parameter_dummy_table.
      rs_table_header = VALUE zdbbr_selfield(
          tabname              = iv_tabname
          is_table_header      = abap_true
          description          = 'Parameters'(004)
          fieldname_raw        = iv_tabname
          ddic_order           = 0
      ).
    ELSE.
      rs_table_header = super->create_table_header(
          iv_tabname       = iv_tabname
          iv_tabname_alias = iv_tabname_alias
          iv_typename      = iv_typename
      ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_selection_mask.
    IF mo_cds_view->has_parameters( ) OR
       mo_data->mr_s_join_def->tables IS NOT INITIAL.
      update_multi_selection_mask( ).
    ELSE.
      update_selection_mask( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_primary_entity.
    DATA: ls_table_info TYPE dd02v.

    " set cursor to first line
    mo_data->mo_selection_table->zif_uitb_page_scroller~scroll_page_top( ).
    mo_data->mo_custom_f4_map->clear( ).

    check_primary_entity( ).

    check_edit_mode( ).

*... create parameters
    zcl_dbbr_cds_tabfield_util=>add_parameters(
        ir_tabfield_list = mo_data->mo_tabfield_list
        it_parameters    = mo_cds_view->get_parameters( )
    ).
*... create table fields for cds view
    DATA(ls_header) = mo_cds_view->get_header( ).

    mo_data->mo_custom_f4_map->read_custom_f4_definitions( mv_cds_view ).

    zcl_dbbr_cds_tabfield_util=>add_view_colums(
        io_custom_f4_map = mo_data->mo_custom_f4_map
        ir_tabfield_list = mo_data->mo_tabfield_list
        it_columns       = mo_cds_view->get_columns( )
        iv_name          = mv_cds_view
        if_has_params    = mo_cds_view->has_parameters( )
        iv_raw_name      = ls_header-entityname_raw
        iv_description   = mv_cds_view_description
        if_is_primary    = abap_true
    ).

    mo_data->mo_tabfield_list->update_alias_names( ).
  ENDMETHOD.

  METHOD choose_cds_sub_entity.
    DATA(lr_choose_sub_entity_view) = NEW zcl_dbbr_cds_sub_entity_sel(
        ir_cds_view          = mo_cds_view
        if_only_associations = if_only_associations
    ).

    lr_choose_sub_entity_view->zif_uitb_view~show( ).

    IF if_return_chosen_directly = abap_true.
      rs_chosen_association = lr_choose_sub_entity_view->get_chosen_association( ).
    ELSE.
      lr_choose_sub_entity_view->get_chosen_sub_entity(
        IMPORTING
          ev_enttiy_id   = DATA(lv_entity_id)
          ev_entity_type = DATA(lv_entity_type)
      ).

      CHECK: lv_entity_id IS NOT INITIAL,
             lv_entity_type IS NOT INITIAL.

      RAISE EVENT request_new_entity
        EXPORTING
          ev_id   = lv_entity_id
          ev_type = lv_entity_type.
    ENDIF.
  ENDMETHOD.


  METHOD clear.
    super->clear( ).

    CLEAR: mf_associations_loaded,
           mo_cds_view,
           mv_cds_view,
           mv_cds_view_description,
           mv_cds_view_name_raw.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
      ir_selscreen_data = ir_selscreen_data
      iv_entity_type    = zif_dbbr_c_entity_type=>cds_view
    ).

    mo_custom_menu = NEW cl_ctmenu( ).
    mo_custom_menu->add_function(
      fcode = zif_dbbr_c_selscreen_functions=>open_cds_view_with_adt
      text  = |{ 'Open with ADT'(005) }|
    ).
    mo_custom_menu->add_function(
      fcode = zif_dbbr_c_selscreen_functions=>go_to_ddic_view_of_cds
      text  = |{ 'Go to DDIC View'(006) }|
    ).
    mo_custom_menu->add_function(
      fcode = zif_dbbr_c_selscreen_functions=>choose_cds_sub_entity
      text  = |{ 'Navigate to Sub Entity'(003) }|
    ).
    mo_custom_menu->add_separator( ).
    mo_custom_menu->add_function(
      fcode = zif_dbbr_c_selscreen_functions=>show_ddls_source
      text  = |{ 'Show DDL Source'(002) }|
    ).
    mo_custom_menu->add_function(
      fcode = zif_dbbr_c_selscreen_functions=>show_cds_dependency_tree
      text  = |{ 'Show Dependency Tree'(011) }|
    ).

    DATA(lo_toolbar) = fill_toolbar( if_create_extended_search = abap_true ).

    cl_gui_cfw=>flush( ).

    mv_cds_view = mo_data->mr_s_global_data->primary_table.
    mo_data->mr_v_selmask_entity_type->* = 'CDS View'(001).
  ENDMETHOD.


  METHOD zif_dbbr_screen_table_util~handle_pbo.
*... handle expanded mode
    DATA(lv_visibility) = COND i(
      WHEN NOT has_content( )                     THEN 1
      WHEN NOT mo_cds_view->has_parameters( ) AND
           NOT mo_data->is_join_active( )         THEN 1
      ELSE                                             0 ).

    LOOP AT mo_data->mr_s_tableview->cols ASSIGNING FIELD-SYMBOL(<ls_column>).
      IF <ls_column>-screen-group1 = 'EXP'.
        <ls_column>-invisible = lv_visibility.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = VALUE #(
      ( LINES OF super->get_deactivated_functions( ) )
      ( zif_dbbr_c_selscreen_functions=>select_additional_texts )
    ).

    IF mo_cds_view IS BOUND.
      DATA(lv_source_type) = mo_cds_view->get_header( )-source_type.
      IF lv_source_type = zif_dbbr_c_cds_view_type=>abstract_entity OR
         lv_source_type = zif_dbbr_c_cds_view_type=>extend.
        result = VALUE #(
          BASE result
          ( zif_dbbr_c_selscreen_functions=>execute_selection )
          ( zif_dbbr_c_selscreen_functions=>exec_selection_without_grp )
          ( zif_dbbr_c_selscreen_functions=>count_lines )
        ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_pbo.
    update_multiple_table_buttons(
      COND #(
       WHEN NOT has_content( )                THEN abap_false
       WHEN mo_cds_view->has_parameters( ) OR
            mo_data->is_join_active( )        THEN abap_true
       ELSE                                        abap_false
      )
    ).
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_ui_function.
    CASE cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>show_ddls_source.
        TRY.
            DATA(lv_source) = zcl_dbbr_cds_view_factory=>read_ddls_source( mv_cds_view ).
            zcl_uitb_abap_code_viewer=>show_code(
                iv_title = |DDL Source { mv_cds_view_name_raw }|
                iv_code  = lv_source
                iv_theme = mo_data->mr_s_global_data->settings-code_viewer_theme
            ).
          CATCH zcx_dbbr_application_exc INTO DATA(lx_app_error).
            lx_app_error->zif_dbbr_exception_message~print( ).
        ENDTRY.
        CLEAR: cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>show_cds_dependency_tree.
        NEW zcl_dbbr_cds_dependency_tree( iv_cds_view = mv_cds_view_name_raw )->show( ).
        CLEAR: cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>choose_cds_sub_entity.
        choose_cds_sub_entity( ).
        CLEAR: cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>open_cds_view_with_adt.
        TRY .
            zcl_dbbr_adt_util=>jump_adt(
                iv_obj_name        = |{ mo_cds_view->get_header( )-ddlname }|
                iv_obj_type        = 'DDLS'
            ).
          CATCH zcx_dbbr_adt_error INTO DATA(lx_adt_error).
            RAISE EXCEPTION TYPE zcx_dbbr_validation_exception
              EXPORTING
                previous = lx_adt_error.
        ENDTRY.

      WHEN zif_dbbr_c_selscreen_functions=>go_to_ddic_view_of_cds.
        zcl_dbbr_dictionary_helper=>navigate_to_table( iv_tabname = mo_cds_view->get_header( )-ddlview ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
