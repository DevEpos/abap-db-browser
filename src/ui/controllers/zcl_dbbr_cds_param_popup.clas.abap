"! <p class="shorttext synchronized" lang="en">Popup for selecting CDS view parameters</p>
CLASS zcl_dbbr_cds_param_popup DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_modal_dialog
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        it_param_values  TYPE ZIF_SAT_TY_GLOBAL=>ty_t_cds_param_value OPTIONAL
        iv_cds_view_name TYPE ZSAT_CDS_VIEW_NAME
        !io_tabfields    TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Get entered param values</p>
    "!
    METHODS get_param_values
      RETURNING
        VALUE(rt_values) TYPE ZIF_SAT_TY_GLOBAL=>ty_t_cds_param_value .
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
    METHODS zif_uitb_gui_screen~show
        REDEFINITION.
  PROTECTED SECTION.

    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
    METHODS handle_exit_request
        REDEFINITION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_functions,
        update_params TYPE ui_func VALUE 'UPDATE',
      END OF c_functions.

    TYPES:
      BEGIN OF ty_param.
    TYPES: paramname_raw TYPE ddtext.
        INCLUDE TYPE ZIF_SAT_TY_GLOBAL=>ty_s_cds_param_value.
    TYPES: has_custom_f4 TYPE abap_bool.
    TYPES: t_style       TYPE lvc_t_styl.
    TYPES: END OF ty_param .

    DATA mo_alv TYPE REF TO zcl_uitb_alv .
    DATA mt_parameters TYPE STANDARD TABLE OF ty_param .
    DATA mo_custom_f4_map TYPE REF TO zcl_dbbr_custom_f4_map.
    DATA mo_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mv_cds_view TYPE ZSAT_CDS_VIEW_NAME.

    "! <p class="shorttext synchronized" lang="en">Convert parameter to display format</p>
    "!
    METHODS convert_param_to_display
      IMPORTING
        ir_s_param  TYPE REF TO ty_param
        is_tabfield TYPE zdbbr_tabfield_info_ui.
    "! <p class="shorttext synchronized" lang="en">Convert parameters to internal format</p>
    "!
    METHODS convert_params_to_internal .
    "! <p class="shorttext synchronized" lang="en">Create ALV for input</p>
    "!
    METHODS create_alv
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Handler for data changed event</p>
    "!
    METHODS on_data_changed
          FOR EVENT data_changed OF zcl_uitb_alv_events
      IMPORTING
          !ef_onf4
          !ef_onf4_after
          !ef_onf4_before
          !er_change_protocol
          !ev_function .
    "! <p class="shorttext synchronized" lang="en">Handler for F4 event</p>
    "!
    METHODS on_f4
          FOR EVENT f4 OF zcl_uitb_alv_events
      IMPORTING
          !ef_display
          !er_event_data
          !es_row_no
          !et_bad_cells
          !ev_fieldname
          !ev_fieldvalue .
ENDCLASS.



CLASS zcl_dbbr_cds_param_popup IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_title = |{ 'Enter Parameters'(001) }| ).

    mo_tabfields = io_tabfields.
    mv_cds_view = iv_cds_view_name.
    mo_custom_f4_map = NEW zcl_dbbr_custom_f4_map( ).

    DATA(lr_iterator) = io_tabfields->zif_uitb_data_ref_list~get_iterator(
        iv_where = 'IS_PARAMETER = abap_true'
    ).
    WHILE lr_iterator->has_next( ).
      DATA(lr_s_next_param) = CAST zdbbr_tabfield_info_ui( lr_iterator->get_next( ) ).
      DATA(ls_param) = VALUE ty_param(
        paramname_raw = lr_s_next_param->fieldname_raw
        name          = lr_s_next_param->fieldname
        t_style       = VALUE #( ( maxlen = lr_s_next_param->outputlen ) )
        value         = lr_s_next_param->default_low
        has_custom_f4 = mo_custom_f4_map->entry_exists(
          iv_tabname       = lr_s_next_param->tabname
          iv_fieldname     = lr_s_next_param->fieldname
          iv_rollname      = lr_s_next_param->rollname
          is_built_in_type = VALUE #(
            datatype = lr_s_next_param->datatype
            inttype  = lr_s_next_param->inttype
            leng     = lr_s_next_param->length
          )
        )
      ).
      IF it_param_values IS NOT INITIAL.
*...... Retrieve passed parameter value
        ls_param-value = VALUE #( it_param_values[ name = ls_param-name ]-value OPTIONAL ).
      ENDIF.

*.... convert filled parameter value to display format
      IF ls_param-value IS NOT INITIAL.
        convert_param_to_display(
            ir_s_param  = REF #( ls_param )
            is_tabfield = lr_s_next_param->*
        ).
      ENDIF.
      mt_parameters = VALUE #( BASE mt_parameters
        ( ls_param )
      ).
    ENDWHILE.
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    CASE io_command->mv_function.

      WHEN c_functions-update_params.
*.... Run check for parameters
        CHECK mo_alv->get_data_changes( )->check_changed( ).

        LOOP AT mt_parameters ASSIGNING FIELD-SYMBOL(<ls_param>) WHERE value IS INITIAL.
        ENDLOOP.
        IF sy-subrc = 0.
          MESSAGE |{ 'Enter missing parameter values'(003) }| TYPE 'E'.
          RETURN.
        ENDIF.

        convert_params_to_internal( ).
        leave_screen( ).

    ENDCASE.
  ENDMETHOD.

  METHOD get_param_values.
    rt_values = CORRESPONDING #( mt_parameters ).
  ENDMETHOD.



  METHOD zif_uitb_gui_screen~show.
    super->show(
        iv_top    = 10
        iv_left   = 20
        iv_width  = 60
        iv_height = 8
    ).
  ENDMETHOD.


  METHOD convert_params_to_internal.
    LOOP AT mt_parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
      DATA(lr_s_field) = mo_tabfields->get_field_ref(
           iv_tabname_alias                = zif_dbbr_c_global=>c_parameter_dummy_table
           iv_fieldname              = <ls_param>-name
      ).

      ZCL_SAT_DATA_CONVERTER=>convert_values_to_int_format(
        EXPORTING iv_rollname            = lr_s_field->rollname
                  iv_type                = lr_s_field->inttype
                  iv_length              = CONV #( lr_s_field->length )
                  iv_decimals            = CONV #( lr_s_field->decimals )
        CHANGING  cv_value1              = <ls_param>-value
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_param_to_display.
    ZCL_SAT_DATA_CONVERTER=>convert_values_to_disp_format(
      EXPORTING iv_rollname            = is_tabfield-rollname
                iv_type                = is_tabfield-inttype
                iv_length              = CONV #( is_tabfield-length )
                iv_decimals            = CONV #( is_tabfield-decimals )
      CHANGING  cv_value1              = ir_s_param->value
    ).
  ENDMETHOD.


  METHOD create_alv.
    DATA: lr_col TYPE REF TO zcl_uitb_alv_column.

    CHECK mo_alv IS INITIAL.

    mo_alv = zcl_uitb_alv=>create_alv(
       ir_data       = REF #( mt_parameters )
       ir_container  = io_container
       if_editable   = abap_true
    ).

    DATA(lo_cols) = mo_alv->get_columns( ).

    DATA(lo_col_iterator) = lo_cols->zif_uitb_list~get_iterator( ).
    lo_cols->set_style_column( 'T_STYLE' ).

    WHILE lo_col_iterator->has_next( ).
      DATA(lo_col) = CAST zcl_uitb_alv_column( lo_col_iterator->get_next( ) ).

      CASE lo_col->get_name( ).

        WHEN 'PARAMNAME_RAW'.
          lo_col->set_descriptions( iv_long = 'Name' ).
          lo_col->set_output_length( 20 ).

        WHEN 'VALUE'.
          lo_col->set_descriptions( iv_long = 'Value' ).
          lo_col->set_editable( ).
          lo_col->set_output_length( 20 ).
          lo_col->set_f4( ).
          lo_col->set_custom_f4( ).

        WHEN OTHERS.
          lo_col->set_technical( ).
      ENDCASE.

    ENDWHILE.

    DATA(lr_functions) = mo_alv->get_functions( ).
    lr_functions->set_all( abap_false ).

    DATA(lr_display_settings) = mo_alv->get_display_settings( ).
    lr_display_settings->set_row_insertions( abap_false ).
    lr_display_settings->set_row_marks( abap_false ).
    lr_display_settings->set_row_move_allowed( abap_false ).

    DATA(lr_events) = mo_alv->get_events( ).
    SET HANDLER:
        on_data_changed FOR lr_events,
        on_f4 FOR lr_events.

    mo_alv->get_selections( )->set_current_cell( value #( row = 1 column = 'VALUE' ) ).
    mo_alv->display( ).
    mo_alv->zif_uitb_gui_control~focus( ).
  ENDMETHOD.

  METHOD handle_exit_request.
    CLEAR mt_parameters.
  ENDMETHOD.

  METHOD create_content.
    create_control_toolbar(
      EXPORTING
        io_parent    = io_container
        it_button    = VALUE #(
          ( function  = c_functions-update_params
            icon      = icon_execute_object
            quickinfo = |{ 'Update parameter values (F8)'(004) }| )
        )
      IMPORTING
        eo_client    = DATA(lo_container)
    ).
    create_alv( lo_container ).
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->map_fkey_function( iv_fkey            = zif_uitb_c_gui_screen=>c_functions-f8
                                    iv_mapped_function = c_functions-update_params
                                    iv_text            = |{ 'Update parameter values'(004) }| ).

    IF NOT io_callback->is_first_screen_call( ).
      mo_alv->zif_uitb_gui_control~focus( ).
    ENDIF.

    io_callback->deactivate_function( zif_uitb_c_gui_screen=>c_functions-save ).
  ENDMETHOD.


  METHOD on_data_changed.
    DATA: lv_value TYPE ZSAT_VALUE.

    CHECK: ef_onf4 = abap_false,
           ef_onf4_after = abap_false,
           ef_onf4_before = abap_false.

    LOOP AT er_change_protocol->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_mod_cell>).
      CHECK <ls_mod_cell>-value IS NOT INITIAL.

      DATA(lr_s_param_value) = REF #( mt_parameters[ <ls_mod_cell>-row_id ] ).

      DATA(ls_param_field) = mo_tabfields->get_field(
        iv_tabname   = zif_dbbr_c_global=>c_parameter_dummy_table
        iv_fieldname = mt_parameters[ <ls_mod_cell>-row_id ]-name
      ).

*.... perform conversion to internal value
      lv_value = <ls_mod_cell>-value.

*.... Consider sy-datum
      IF ls_param_field-inttype = cl_abap_typedescr=>typekind_date.
        DATA(lv_system_date_value) = to_upper( lv_value ).
        IF lv_system_date_value = 'SY-DATUM'.
          lv_value = |{ sy-datum DATE = USER }|.
        ENDIF.
      ENDIF.

      TRY.
          ZCL_SAT_DATA_CONVERTER=>convert_values_to_int_format(
            EXPORTING iv_rollname            = ls_param_field-rollname
                      iv_decimals            = CONV #( ls_param_field-decimals )
                      iv_length              = CONV #( ls_param_field-length )
                      iv_type                = ls_param_field-inttype
                      if_print_error_message = abap_false
            CHANGING cv_value1               = lv_value
          ).
          lr_s_param_value->value = lv_value.
*........ Convert value again to display the output format to the user
          ZCL_SAT_DATA_CONVERTER=>convert_values_to_disp_format(
            EXPORTING iv_rollname            = ls_param_field-rollname
                      iv_decimals            = CONV #( ls_param_field-decimals )
                      iv_length              = CONV #( ls_param_field-length )
                      iv_type                = ls_param_field-inttype
            CHANGING cv_value1               = lv_value
          ).
          er_change_protocol->modify_cell(
              i_row_id    = <ls_mod_cell>-row_id
              i_fieldname = 'VALUE'
              i_value     = lv_value
          ).
        CATCH ZCX_SAT_CONVERSION_EXC INTO DATA(lx_conv_error).
          er_change_protocol->add_protocol_entry(
              i_msgid     = lx_conv_error->if_t100_message~t100key-msgid
              i_msgty     = 'E'
              i_msgno     = lx_conv_error->if_t100_message~t100key-msgno
              i_msgv1     = lx_conv_error->msgv1
              i_msgv2     = lx_conv_error->msgv2
              i_msgv3     = lx_conv_error->msgv3
              i_msgv4     = lx_conv_error->msgv4
              i_fieldname = <ls_mod_cell>-fieldname
              i_row_id    = <ls_mod_cell>-row_id
          ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD on_f4.
    FIELD-SYMBOLS: <lt_modified> TYPE lvc_t_modi.

*.. Retrieve param information for current field
    DATA(lr_s_param) = REF #( mt_parameters[ es_row_no-row_id ] ).
    DATA(ls_param_field) = mo_tabfields->get_field(
        iv_tabname = zif_dbbr_c_global=>c_parameter_dummy_table
        iv_fieldname = lr_s_param->name
    ).
    IF lr_s_param->has_custom_f4 = abap_true AND mo_custom_f4_map IS BOUND.
      DATA(ls_selfield) = CORRESPONDING zdbbr_selfield(
        ls_param_field
        MAPPING lowercase        = is_lowercase
                key              = is_key
                option           = default_option
                sign             = default_sign
                low              = default_low
                has_cust_f4_help = has_custom_f4

      ).
      ls_selfield-has_cust_f4_help = abap_true.
      zcl_dbbr_f4_helper=>call_f4(
        EXPORTING
          io_custom_f4_map = mo_custom_f4_map
        CHANGING
          cs_selfield      = ls_selfield
      ).
      IF ls_selfield-low IS NOT INITIAL.
        ev_fieldvalue = ls_selfield-low.
        er_event_data->m_event_handled = abap_true.
        ASSIGN er_event_data->m_data->* TO <lt_modified>.

        <lt_modified> = VALUE #(
          ( fieldname = 'VALUE' row_id = es_row_no-row_id value = ls_selfield-low )
        ).
      ENDIF.
    ELSEIF ls_param_field-rollname IS NOT INITIAL.
      DATA: lv_f4_result TYPE ZSAT_VALUE.

      zcl_dbbr_f4_helper=>call_built_in_f4(
        EXPORTING iv_tablename = ls_param_field-rollname
        CHANGING  cv_value     = lv_f4_result
      ).

      IF lv_f4_result IS NOT INITIAL.
        ev_fieldvalue = lv_f4_result.
        er_event_data->m_event_handled = abap_true.
        ASSIGN er_event_data->m_data->* TO <lt_modified>.

        <lt_modified> = VALUE #(
          ( fieldname = 'VALUE' row_id = es_row_no-row_id value = lv_f4_result )
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
