"! <p class="shorttext synchronized">Utitlity methods for Tables for Selection Controller</p>
CLASS zcl_dbbr_table_selection_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selection_util
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS execute_function                               REDEFINITION.
    METHODS get_entity_name                                REDEFINITION.
    METHODS init                                           REDEFINITION.
    METHODS zif_dbbr_screen_util~get_deactivated_functions REDEFINITION.
    METHODS zif_dbbr_screen_util~handle_ui_function        REDEFINITION.

  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Maintain data for the selection criteria</p>
    METHODS edit_data
      IMPORTING
        if_called_from_output TYPE abap_bool OPTIONAL.

    "! <p class="shorttext synchronized">Delete data for the given selection criteria</p>
    METHODS delete_data.
    METHODS read_entity_infos REDEFINITION.

  PRIVATE SECTION.
    METHODS export_data.
ENDCLASS.


CLASS zcl_dbbr_table_selection_util IMPLEMENTATION.
  METHOD delete_data.
    build_full_fieldnames( ).
    create_where_clause( ).
    create_from_clause( ).

    TRY.
        select_data( if_count_lines = abap_true ).
      CATCH zcx_dbbr_application_exc INTO DATA(lx_appl_exc).
        lx_appl_exc->show_message( iv_message_type = 'I' ).
        RETURN.
    ENDTRY.

    " if no selection occurred, prevent screen visibility
    IF mv_selected_lines <= 0.
      MESSAGE i060(zdbbr_info).
    ELSE.
      DATA(lv_number_of_lines) = |{ mv_selected_lines NUMBER = USER }|.

      DATA(lv_result) = zcl_dbbr_appl_util=>popup_to_confirm(
          iv_title                 = 'Delete Rows?'
          iv_query                 = |Are you sure you want to delete { lv_number_of_lines } from the Table | &&
                                     | { ms_control_info-primary_table }?|
          if_display_cancel_button = abap_true
          iv_icon_type             = 'ICON_WARNING' ).

      IF lv_result = '2' OR lv_result = 'A'.
        MESSAGE i008(zdbbr_exception).
      ELSE.

        " delete lines
        DELETE FROM (mt_from)
          WHERE (mt_where).

        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          MESSAGE i061(zdbbr_info) WITH |{ lv_number_of_lines }| ms_control_info-primary_table.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD edit_data.
    DATA lr_iterator TYPE REF TO zif_uitb_data_ref_iterator.
    DATA lr_field TYPE REF TO zdbbr_tabfield_info_ui.
    DATA lr_s_output_field TYPE REF TO data.

    FIELD-SYMBOLS <lt_or_selfields> TYPE table.
    FIELD-SYMBOLS <lt_output_selfields> TYPE table.

    DATA(lo_output_fields_table) = zcl_uitb_data_list=>create_for_table_name( 'SE16N_OUTPUT' ).
    DATA(lo_or_selfields_table) = zcl_uitb_data_list=>create_for_table_name( 'SE16N_OR_SELTAB' ).

    IF if_called_from_output = abap_true.
      mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat2) ).
      LOOP AT lt_fieldcat2 ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WHERE     no_out = abap_false
                                                                       AND tech   = abap_false.
        lr_s_output_field = lo_output_fields_table->create_new_line( ).
        lo_output_fields_table->fill_component( ir_s_element = lr_s_output_field
                                                iv_comp_name = 'FIELD'
                                                iv_value     = <ls_fieldcat>-fieldname ).
      ENDLOOP.
    ELSE.
      mo_tabfields->switch_mode( zif_dbbr_c_global=>c_field_chooser_modes-output ).
      lr_iterator = mo_tabfields->get_iterator( if_for_active = abap_true ).

      WHILE lr_iterator->has_next( ).
        lr_field = CAST zdbbr_tabfield_info_ui( lr_iterator->get_next( ) ).
        lr_s_output_field = lo_output_fields_table->create_new_line( ).
        lo_output_fields_table->fill_component( ir_s_element = lr_s_output_field
                                                iv_comp_name = 'FIELD'
                                                iv_value     = lr_field->fieldname ).
      ENDWHILE.
    ENDIF.

    IF mt_or IS INITIAL.
      create_where_clause( ).
    ENDIF.

    LOOP AT mt_or ASSIGNING FIELD-SYMBOL(<ls_or>).
      DATA(lr_s_or_tuple) = lo_or_selfields_table->create_new_line( ).
      lo_or_selfields_table->fill_component( ir_s_element = lr_s_or_tuple
                                             iv_comp_name = 'POS'
                                             iv_value     = <ls_or>-pos ).
      lo_or_selfields_table->fill_component( ir_s_element         = lr_s_or_tuple
                                             iv_comp_name         = 'SELTAB'
                                             iv_value             = <ls_or>-values
                                             if_use_corresponding = abap_true ).
    ENDLOOP.

    DATA(lr_t_or_selfields) = lo_or_selfields_table->get_all( ).
    DATA(lr_t_output_selfields) = lo_output_fields_table->get_all( ).

    ASSIGN lr_t_or_selfields->* TO <lt_or_selfields>.
    ASSIGN lr_t_output_selfields->* TO <lt_output_selfields>.

    CALL FUNCTION 'SE16N_INTERFACE'
      EXPORTING  i_tab            = ms_control_info-primary_table
                 i_edit           = abap_true
                 i_sapedit        = abap_true
                 i_max_lines      = ms_technical_info-max_lines
                 i_clnt_dep       = ms_control_info-client_dependent
                 i_tech_names     = ms_technical_info-tech_names
                 i_uname          = sy-uname
      TABLES     it_or_selfields  = <lt_or_selfields>
                 it_output_fields = <lt_output_selfields>
      EXCEPTIONS no_values        = 1
                 OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD get_entity_name.
    result = mv_entity_id.
  ENDMETHOD.

  METHOD init.
  ENDMETHOD.

  METHOD read_entity_infos.
    DATA(ls_table_info) = zcl_sat_ddic_repo_access=>get_table_info( ms_control_info-primary_table ).

    ms_control_info-primary_table_name     = ls_table_info-ddtext.
    ms_control_info-client_dependent       = ls_table_info-clidep.
    ms_control_info-primary_table_tabclass = ls_table_info-tabclass.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = VALUE #( ( LINES OF super->get_deactivated_functions( ) )
                      ( zif_dbbr_c_selection_functions=>navigate_association ) ).

    IF     mf_group_by = abap_false
       AND mf_aggregation = abap_false
       AND ms_control_info-primary_table_tabclass = 'TRANSP'
       AND ms_technical_info-advanced_mode        = abap_true
       AND zcl_dbbr_dep_feature_util=>is_se16n_available( ).
      DELETE result WHERE table_line = zif_dbbr_c_selection_functions=>edit_data.
    ENDIF.

    IF ms_technical_info-experimental_mode = abap_false OR is_join_active( ).
      result = VALUE #( BASE result ( zif_dbbr_c_selection_functions=>export_json_data ) ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_ui_function.
    CASE cv_function.

      WHEN zif_dbbr_c_selection_functions=>edit_data.
        CLEAR cv_function.
        edit_data( if_called_from_output = abap_true ).

      WHEN zif_dbbr_c_selection_functions=>export_json_data.
*        CLEAR cv_function.
        export_data( ).

    ENDCASE.

    IF cv_function IS NOT INITIAL.
      super->handle_ui_function( CHANGING cv_function = cv_function ).
    ENDIF.
  ENDMETHOD.

  METHOD execute_function.
    CASE iv_function.

      WHEN zif_dbbr_c_selection_functions=>edit_data.
        edit_data( ).

      WHEN zif_dbbr_c_selection_functions=>delete_data.
        delete_data( ).

      WHEN zif_dbbr_c_selection_functions=>export_json_data.
        export_data( ).

    ENDCASE.
  ENDMETHOD.

  METHOD export_data.
    TYPES x1024 TYPE x LENGTH 1024.
    DATA lt_xjson_data TYPE STANDARD TABLE OF x1024.
    DATA lr_t_export TYPE REF TO data.

    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_export> TYPE STANDARD TABLE.

    mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    DATA(lt_dyn_comp) = mt_dyntab_components.

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WHERE no_out = abap_true OR tech = abap_true.
      DELETE lt_dyn_comp WHERE name = <ls_fieldcat>-fieldname.
    ENDLOOP.

    DELETE lt_dyn_comp WHERE type_name NP |{ mv_entity_id }-*|.

    TRY.
        DATA(lo_tab_descr) = cl_abap_tabledescr=>create(
            p_line_type = cl_abap_structdescr=>create( p_components = CORRESPONDING #( lt_dyn_comp )
                                                       p_strict     = abap_false ) ).
      CATCH cx_sy_table_creation cx_sy_struct_creation INTO DATA(lx_exc). " TODO: variable is assigned but never used (ABAP cleaner)
        RETURN.
    ENDTRY.

    DATA(lo_xml_writer) = cl_sxml_table_writer=>create( type = if_sxml=>co_xt_json ).
    lo_xml_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_no_empty ).

    CREATE DATA lr_t_export TYPE HANDLE lo_tab_descr.
    ASSIGN lr_t_export->* TO <lt_export>.
    ASSIGN mr_t_data->* TO <lt_data>.

    <lt_export> = CORRESPONDING #( <lt_data> ).

    CALL TRANSFORMATION id
         SOURCE data = <lt_export>
         RESULT XML lo_xml_writer
         OPTIONS initial_components = 'suppress'.

    lo_xml_writer->get_output( IMPORTING output = lt_xjson_data ).

    DATA lv_fullpath TYPE string.
    DATA lv_useraction TYPE i.
    DATA lv_filename TYPE string.
    DATA lv_path TYPE string.

    cl_gui_frontend_services=>file_save_dialog( EXPORTING default_extension   = 'json'
                                                          default_file_name   = |{ mv_entity_id }_data.json|
                                                          prompt_on_overwrite = abap_true
                                                CHANGING  filename            = lv_filename
                                                          path                = lv_path
                                                          fullpath            = lv_fullpath
                                                          user_action         = lv_useraction ).

    IF lv_useraction = cl_gui_frontend_services=>action_ok AND lv_fullpath IS NOT INITIAL.
      cl_gui_frontend_services=>gui_download( EXPORTING  filename = lv_fullpath
                                                         filetype = 'BIN'
                                              CHANGING   data_tab = lt_xjson_data
                                              EXCEPTIONS OTHERS   = 1 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
