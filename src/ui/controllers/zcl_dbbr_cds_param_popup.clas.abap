"! <p class="shorttext synchronized" lang="en">Popup for selecting CDS view parameters</p>
CLASS zcl_dbbr_cds_param_popup DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "! @parameter IR_CDS_VIEW | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_param_values | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter IR_TABFIELDS | <p class="shorttext synchronized" lang="en"></p>
    METHODS constructor
      IMPORTING
        !ir_cds_view    TYPE REF TO zcl_dbbr_cds_view
        it_param_values TYPE zif_dbbr_global_types=>tt_cds_param_value OPTIONAL
        !ir_tabfields   TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Show popup for entering parameter values</p>
    "! @parameter RT_VALUES | <p class="shorttext synchronized" lang="en"></p>
    METHODS show
      RETURNING
        VALUE(rt_values) TYPE zif_dbbr_global_types=>tt_cds_param_value .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_param.
    TYPES: paramname_raw TYPE ddtext.
        INCLUDE TYPE zif_dbbr_global_types=>ty_cds_param_value.
    TYPES: t_style       TYPE lvc_t_styl.
    TYPES: END OF ty_param .

    DATA mr_alv TYPE REF TO zcl_uitb_alv .
    DATA mr_view TYPE REF TO zif_uitb_template_prog .
    DATA:
      mt_parameters TYPE STANDARD TABLE OF ty_param .
    DATA mr_cds_view TYPE REF TO zcl_dbbr_cds_view .
    DATA mr_tabfields TYPE REF TO zcl_dbbr_tabfield_list .

    "! <p class="shorttext synchronized" lang="en">Convert parameter to display format</p>
    "! @parameter ir_s_param | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter is_tabfield | <p class="shorttext synchronized" lang="en"></p>
    METHODS convert_param_to_display
      IMPORTING
        ir_s_param  TYPE REF TO ty_param
        is_tabfield TYPE zdbbr_tabfield_info_ui.
    "! <p class="shorttext synchronized" lang="en">Convert parameters to internal format</p>
    METHODS convert_params_to_internal .
    "! <p class="shorttext synchronized" lang="en">Create ALV for input</p>
    METHODS create_alv .
    "! <p class="shorttext synchronized" lang="en">Handler for exit event</p>
    "! @parameter ER_CALLBACK | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_cancel
          FOR EVENT exit OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
    "! <p class="shorttext synchronized" lang="en">Handler for data changed event</p>
    "! @parameter EF_ONF4 | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter EF_ONF4_AFTER | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter EF_ONF4_BEFORE | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ER_CHANGE_PROTOCOL | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter EV_FUNCTION | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_data_changed
          FOR EVENT data_changed OF zcl_uitb_alv_events
      IMPORTING
          !ef_onf4
          !ef_onf4_after
          !ef_onf4_before
          !er_change_protocol
          !ev_function .
    "! <p class="shorttext synchronized" lang="en">Handler for F4 event</p>
    "! @parameter EF_DISPLAY | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ER_EVENT_DATA | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ES_ROW_NO | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ET_BAD_CELLS | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter EV_FIELDNAME | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter EV_FIELDVALUE | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_f4
          FOR EVENT f4 OF zcl_uitb_alv_events
      IMPORTING
          !ef_display
          !er_event_data
          !es_row_no
          !et_bad_cells
          !ev_fieldname
          !ev_fieldvalue .
    "! <p class="shorttext synchronized" lang="en">Handler for PAI event</p>
    "! @parameter ER_CALLBACK | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter EV_FUNCTION_ID | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_pai
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          !er_callback
          !ev_function_id .
    "! <p class="shorttext synchronized" lang="en">Handler for PBO event</p>
    "! @parameter ER_CALLBACK | <p class="shorttext synchronized" lang="en"></p>
    METHODS on_pbo
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
ENDCLASS.



CLASS zcl_dbbr_cds_param_popup IMPLEMENTATION.


  METHOD constructor.
    mr_cds_view = ir_cds_view.
    mr_tabfields = ir_tabfields.
    mr_view = zcl_uitb_templt_prog_callback=>create_template_program( iv_title = |{ 'Enter Parameters'(001) }| ).

    mr_view->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_f8
        iv_text        = |{ 'Execute Selection'(002) }|
        iv_icon        = icon_execute_object
    ).

    SET HANDLER:
      on_pbo FOR mr_view,
      on_cancel FOR mr_view,
      on_pai FOR mr_view.

    DATA(lr_iterator) = ir_tabfields->zif_uitb_data_ref_list~get_iterator(
        iv_where = 'IS_PARAMETER = abap_true'
    ).
    WHILE lr_iterator->has_next( ).
      DATA(lr_s_next_param) = CAST zdbbr_tabfield_info_ui( lr_iterator->get_next( ) ).
      DATA(ls_param) = VALUE ty_param(
        paramname_raw = lr_s_next_param->fieldname_raw
        name          = lr_s_next_param->fieldname
        t_style       = VALUE #( ( maxlen = lr_s_next_param->outputlen ) )
        value         = lr_s_next_param->default_low
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


  METHOD convert_params_to_internal.
    LOOP AT mt_parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
      DATA(lr_s_field) = mr_tabfields->get_field_ref(
           iv_tabname_alias                = zif_dbbr_global_consts=>c_parameter_dummy_table
           iv_fieldname              = <ls_param>-name
      ).
      zcl_dbbr_data_converter=>convert_values_to_int_format(
        EXPORTING iv_rollname            = lr_s_field->rollname
                  iv_type                = lr_s_field->inttype
                  iv_length              = CONV #( lr_s_field->length )
                  iv_decimals            = CONV #( lr_s_field->decimals )
                  if_print_error_message = abap_false
        CHANGING  cv_value1              = <ls_param>-value
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_param_to_display.
    zcl_dbbr_data_converter=>convert_values_to_disp_format(
      EXPORTING iv_rollname            = is_tabfield-rollname
                iv_type                = is_tabfield-inttype
                iv_length              = CONV #( is_tabfield-length )
                iv_decimals            = CONV #( is_tabfield-decimals )
      CHANGING  cv_value1              = ir_s_param->value
    ).
  ENDMETHOD.


  METHOD create_alv.
    DATA: lr_col TYPE REF TO zcl_uitb_alv_column.

    CHECK mr_alv IS INITIAL.

    mr_alv = zcl_uitb_alv=>create_alv(
       ir_data                 = REF #( mt_parameters )
       ir_container            = mr_view->get_container( )
       if_editable             = abap_true
    ).

    DATA(lr_cols) = mr_alv->get_columns( ).
    lr_cols->set_style_column( 'T_STYLE' ).

    lr_cols->get_column( 'NAME' )->set_technical( ).

    lr_col = lr_cols->get_column( 'PARAMNAME_RAW' ).
    lr_col->set_descriptions( iv_long = 'Name' ).
    lr_col->set_output_length( 20 ).

    lr_col = lr_cols->get_column( 'VALUE' ).
    lr_col->set_descriptions( iv_long = 'Value' ).
    lr_col->set_editable( ).
    lr_col->set_output_length( 20 ).
    lr_col->set_f4( ).
    lr_col->set_custom_f4( ).


    DATA(lr_functions) = mr_alv->get_functions( ).
    lr_functions->set_all( abap_false ).

    DATA(lr_display_settings) = mr_alv->get_display_settings( ).
    lr_display_settings->set_row_insertions( abap_false ).
    lr_display_settings->set_row_marks( abap_false ).
    lr_display_settings->set_row_move_allowed( abap_false ).

    DATA(lr_events) = mr_alv->get_events( ).
    SET HANDLER:
        on_data_changed FOR lr_events,
        on_f4 FOR lr_events.

    mr_alv->display( ).
    mr_alv->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD on_cancel.
    CLEAR mt_parameters.
  ENDMETHOD.


  METHOD on_data_changed.
    DATA: lv_value TYPE zdbbr_value.

    CHECK: ef_onf4 = abap_false,
           ef_onf4_after = abap_false,
           ef_onf4_before = abap_false.

    LOOP AT er_change_protocol->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_mod_cell>).
      CHECK <ls_mod_cell>-value IS NOT INITIAL.

      DATA(lr_s_param_value) = REF #( mt_parameters[ <ls_mod_cell>-row_id ] ).

      DATA(ls_param_field) = mr_tabfields->get_field(
        iv_tabname   = zif_dbbr_global_consts=>c_parameter_dummy_table
        iv_fieldname = mt_parameters[ <ls_mod_cell>-row_id ]-name
      ).

*.... perform conversion to internal value
      lv_value = <ls_mod_cell>-value.
      TRY.
          zcl_dbbr_data_converter=>convert_values_to_int_format(
            EXPORTING iv_rollname            = ls_param_field-rollname
                      iv_decimals            = CONV #( ls_param_field-decimals )
                      iv_length              = CONV #( ls_param_field-length )
                      iv_type                = ls_param_field-inttype
                      if_print_error_message = abap_false
            CHANGING cv_value1               = lv_value
          ).
          lr_s_param_value->value = lv_value.
*........ Convert value again to display the output format to the user
          zcl_dbbr_data_converter=>convert_values_to_disp_format(
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
        CATCH zcx_dbbr_conversion_exc INTO DATA(lx_conv_error).
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
    DATA: lv_f4_result TYPE zdbbr_value.
    FIELD-SYMBOLS: <lt_modified> TYPE lvc_t_modi.

*.. Retrieve param information for current field
    DATA(lr_s_param) = REF #( mt_parameters[ es_row_no-row_id ] ).
    DATA(ls_param_field) = mr_tabfields->get_field(
        iv_tabname = zif_dbbr_global_consts=>c_parameter_dummy_table
        iv_fieldname = lr_s_param->name
    ).
    IF ls_param_field-rollname IS NOT INITIAL.

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


  METHOD on_pai.
    CASE ev_function_id.

      WHEN zif_uitb_template_prog=>c_func_f8.
*.... Run check for parameters
        CHECK mr_alv->get_data_changes( )->check_changed( ).

        LOOP AT mt_parameters ASSIGNING FIELD-SYMBOL(<ls_param>) WHERE value IS INITIAL.
        ENDLOOP.
        IF sy-subrc = 0.
          MESSAGE |{ 'Enter missing parameter values'(003) }| TYPE 'E'.
          RETURN.
        ENDIF.

        convert_params_to_internal( ).
        mr_view->leave_program( ).

    ENDCASE.
  ENDMETHOD.


  METHOD on_pbo.
    IF er_callback->is_first_screen_call( ).
      create_alv( ).
    ELSE.
      mr_alv->zif_uitb_gui_control~focus( ).
    ENDIF.

    er_callback->deactivate_function( iv_function = zif_uitb_template_prog=>c_func_save ).
    er_callback->deactivate_function( iv_function = zif_uitb_template_prog=>c_func_ok ).
  ENDMETHOD.


  METHOD show.
    mr_view->zif_uitb_view~show(
        iv_start_line   = 10
        iv_start_column = 20
        iv_end_line     = 18
        iv_end_column   = 80
    ).

    rt_values = CORRESPONDING #( mt_parameters ).
  ENDMETHOD.
ENDCLASS.
