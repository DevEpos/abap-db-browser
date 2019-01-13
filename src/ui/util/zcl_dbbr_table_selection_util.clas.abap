"! <p class="shorttext synchronized" lang="en">Utitlity methods for Tables for Selection Controller</p>
CLASS zcl_dbbr_table_selection_util DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_selection_util
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS execute_selection
        REDEFINITION .
    METHODS get_entity_name
        REDEFINITION .
    METHODS handle_alv_ctx_menu_request
        REDEFINITION .
    METHODS init
        REDEFINITION .
    METHODS refresh_selection
        REDEFINITION .
    METHODS zif_dbbr_screen_util~get_deactivated_functions
        REDEFINITION.
    METHODS zif_dbbr_screen_util~handle_ui_function
        REDEFINITION .
  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">Marks virtual join fields for later processing</p>
    METHODS mark_virt_join_selfields .
    "! <p class="shorttext synchronized" lang="en">Fill virtual join field in the output table</p>
    METHODS fill_virtual_join_fields
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Maintain data for the selection criteria</p>
    METHODS edit_data
      IMPORTING
        if_called_from_output TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Delete data for the given selection criteria</p>
    METHODS delete_data .

    METHODS read_entity_infos
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_table_selection_util IMPLEMENTATION.


  METHOD delete_data.
    create_where_clause( ).

    create_from_clause( ).

    CHECK select_data( if_count_lines = abap_true ).

    " if no selection occurred, prevent screen visibility
    IF ms_control_info-number <= 0.
      MESSAGE i060(zdbbr_info).
    ELSE.
      DATA(lv_number_of_lines) = |{ ms_control_info-number NUMBER = USER }|.

      DATA(lv_result) = zcl_dbbr_appl_util=>popup_to_confirm(
          iv_title                 = 'Delete Rows?'
          iv_query                 = |Are you sure you want to delete { lv_number_of_lines } from the Table | &&
                                     | { ms_control_info-primary_table }?|
          if_display_cancel_button = abap_true
          iv_icon_type             = 'ICON_WARNING'
      ).

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
    DATA: lt_output_fields TYPE STANDARD TABLE OF se16n_output,
          lt_or_selfields  TYPE STANDARD TABLE OF se16n_or_seltab.

    IF if_called_from_output = abap_true.
      mr_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).
      lt_output_fields = VALUE #(
        FOR output_field IN lt_fieldcat
        WHERE ( no_out = abap_false AND
                tech   = abap_false )
        ( field = output_field-fieldname )
      ).
    ELSE.
      mr_tabfields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).
      DATA(lr_iterator) = mr_tabfields->get_iterator( if_for_active = abap_true ).

      WHILE lr_iterator->has_next( ).
        DATA(lr_field) = CAST zdbbr_tabfield_info_ui( lr_iterator->get_next( ) ).
        lt_output_fields = VALUE #( BASE lt_output_fields ( field = lr_field->fieldname ) ).
      ENDWHILE.
    ENDIF.

    IF mt_or IS INITIAL.
      create_where_clause( ).
    ENDIF.

    lt_or_selfields = CORRESPONDING #( mt_or MAPPING seltab = values ).

    CALL FUNCTION 'SE16N_INTERFACE'
      EXPORTING
        i_tab            = ms_control_info-primary_table
        i_edit           = abap_true
        i_sapedit        = abap_true
        i_max_lines      = ms_technical_info-max_lines
        i_clnt_dep       = ms_control_info-client_dependent
        i_variant        = ms_technical_info-alv_variant
        i_tech_names     = ms_technical_info-tech_names
        i_uname          = sy-uname
      TABLES
        it_or_selfields  = lt_or_selfields
        it_output_fields = lt_output_fields
      EXCEPTIONS
        no_values        = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD execute_selection.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    determine_group_by_state( ).
    determine_aggregation_state( ).
    mark_virt_join_selfields( ).

    """ only count lines for current selection and display result
    IF mf_count_lines = abap_true.
      count_lines( ).
      RETURN.
    ENDIF.

    """ go into edit mode for given table - SE16N is used for this
    IF ms_control_info-edit = abap_true.
      edit_data( ).
      RETURN.
    ENDIF.

    """ delete entries from database
    IF ms_control_info-delete_mode = abap_true.
      delete_data( ).
      RETURN.
    ENDIF.

    read_entity_infos( ).

    build_full_fieldnames( ).

    IF mf_join_is_active = abap_true AND
       line_exists( ms_join_def-tables[ is_virtual = abap_true ] ).
      mr_post_join_helper = zcl_dbbr_virtual_join_helper=>create(
        is_join_def   = ms_join_def
        ir_fields     = mr_tabfields
        ir_fields_all = mr_tabfields_all
      ).
    ENDIF.

    mr_tabfields->update_text_field_status( ).

    zcl_dbbr_addtext_helper=>prepare_text_fields(
      EXPORTING ir_fields    = mr_tabfields
      CHANGING  ct_add_texts = mt_add_texts
    ).

    create_from_clause( ).

    create_where_clause( ).

    create_group_by_clause( ).

    create_order_by_clause( ).

    create_field_catalog( ).

    create_select_clause( ).

    create_dynamic_table( ).

    IF mr_formula IS BOUND.
      TRY.
          mr_formula_calculator = zcl_dbbr_formula_calculator=>create(
              ir_formula            = mr_formula
              ir_tabfields          = mr_tabfields
              it_tab_components     = mt_dyntab_components
          ).
        CATCH zcx_dbbr_exception INTO DATA(lr_exception).
          lr_exception->zif_dbbr_exception_message~print( ).
      ENDTRY.
    ENDIF.

    CHECK select_data( ).

    " if no selection occurred, prevent screen visibility
    IF ms_control_info-number <= 0.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    CHECK fill_virtual_join_fields( ).

    execute_formula_for_lines( ).

    set_miscinfo_for_selected_data( ).

    RAISE EVENT selection_finished
      EXPORTING
         ef_first_select = abap_true.
  ENDMETHOD.


  METHOD fill_virtual_join_fields.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    result = abap_true.

*.. only continue if virtual join helper is bound
    CHECK mr_post_join_helper IS BOUND.

    ASSIGN mr_t_data->* TO <lt_table>.

    TRY.
        mr_post_join_helper->fill_cache_tables( <lt_table> ).
        result = mr_post_join_helper->process_table( mr_t_data ).
      CATCH zcx_dbbr_selection_common INTO DATA(lx_sql_error).
        CLEAR result.
        MESSAGE lx_sql_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD get_entity_name.
    result = mv_entity_id.
  ENDMETHOD.


  METHOD handle_alv_ctx_menu_request.
  ENDMETHOD.


  METHOD init.
  ENDMETHOD.


  METHOD mark_virt_join_selfields.
    CHECK mf_join_is_active = abap_true.
    CHECK line_exists( ms_join_def-tables[ is_virtual = abap_true ] ).


    LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE is_virtual = abap_true.
      LOOP AT <ls_join_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_join_field>).

        " update field
        DATA(lr_s_field) = mr_tabfields->get_field_ref(
           iv_tabname_alias   = <ls_join_field>-ref_table
           iv_fieldname = <ls_join_field>-ref_field
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


  METHOD read_entity_infos.
    DATA(lv_special_group_count) = 1.

    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( ms_control_info-primary_table ).

    ms_control_info-primary_table_name = ls_table_info-ddtext.
    ms_control_info-client_dependent = ls_table_info-clidep.
    ms_control_info-primary_table_tabclass = ls_table_info-tabclass.

    DATA(lv_sp_group) = CONV lvc_spgrp( c_col_group_prefix && lv_special_group_count ).
    mt_column_groups = VALUE #(
      ( sp_group = lv_sp_group
        text     = `Table - ` && ms_control_info-primary_table )
    ).

    mt_group_tab_map = VALUE #( ( sp_group = lv_sp_group tabname = ms_control_info-primary_table ) ).

    ADD 1 TO lv_special_group_count.

    " update join table names
    IF ms_join_def-tables IS NOT INITIAL.
      LOOP AT ms_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE table_name IS INITIAL.
        <ls_join_table>-table_name = zcl_dbbr_dictionary_helper=>get_table_info( <ls_join_table>-add_table )-ddtext.
        lv_sp_group = c_col_group_prefix && lv_special_group_count.
        mt_column_groups = VALUE #(
          BASE mt_column_groups
          ( sp_group = lv_sp_group
            text     = `Table - ` && <ls_join_table>-add_table )
        ).
        mt_group_tab_map = VALUE #(
          BASE mt_group_tab_map
          ( sp_group = lv_sp_group
            tabname  = <ls_join_table>-add_table )
        ).
        ADD 1 TO lv_special_group_count.
      ENDLOOP.
    ELSE.
      CLEAR mt_column_groups.
    ENDIF.
  ENDMETHOD.


  METHOD refresh_selection.
    CHECK select_data( if_refresh_only = abap_true ).

    IF ms_control_info-number = 0.
      RAISE EVENT no_data.
      RETURN.
    ENDIF.

    CHECK fill_virtual_join_fields( ).

    execute_formula_for_lines( ).

    set_miscinfo_for_selected_data( ).

    RAISE EVENT selection_finished.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    result = VALUE #(
      ( LINES OF super->get_deactivated_functions( ) )
      ( zif_dbbr_c_selection_functions=>navigate_association )
    ).

    IF mf_join_is_active = abap_false AND
       mf_group_by = abap_false AND
       mf_aggregation = abap_false AND
       ms_control_info-primary_table_tabclass = 'TRANSP' and
       ms_technical_info-advanced_mode = abap_true.
      DELETE result WHERE table_line = zif_dbbr_c_selection_functions=>edit_data.
    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_ui_function.
    CASE cv_function.

      WHEN zif_dbbr_c_selection_functions=>edit_data.
        CLEAR cv_function.
        edit_data( ).

    ENDCASE.

    IF cv_function IS NOT INITIAL.
      super->handle_ui_function( CHANGING cv_function = cv_function ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
