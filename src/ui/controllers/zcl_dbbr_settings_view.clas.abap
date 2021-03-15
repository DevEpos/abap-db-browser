class ZCL_DBBR_SETTINGS_VIEW definition
  public
  create public .

public section.

  interfaces ZIF_UITB_VIEW .

  aliases SHOW
    for ZIF_UITB_VIEW~SHOW .

  methods CONSTRUCTOR
    importing
      !IV_TITLE type C
      !IT_SETTINGS type ZDBBR_SETTING_T .
  methods GET_SETTINGS
    returning
      value(RESULT) type ZDBBR_SETTING_T .
  methods HAS_BEEN_UPDATED
    returning
      value(RESULT) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  types:
    "ZIF_DBBR_C_INPUT_TYPE
    BEGIN OF ty_data.
            INCLUDE TYPE zdbbr_setting.
    TYPES: t_cell        TYPE lvc_t_styl.
    TYPES: END OF ty_data .
  types:
    tt_data TYPE STANDARD TABLE OF ty_data .

  data MF_TAKE_VALUES type ABAP_BOOL .
  data MR_ALV type ref to ZCL_UITB_ALV .
  data MR_VIEW type ref to ZIF_UITB_TEMPLATE_PROG .
  data MT_DATA type TT_DATA .

  methods CREATE_ALV .
  methods ON_PBO
    for event BEFORE_OUTPUT of ZIF_UITB_VIEW_CALLBACK
    importing
      !ER_CALLBACK .
  methods ON_PAI
    for event USER_COMMAND of ZIF_UITB_VIEW_CALLBACK
    importing
      !ER_CALLBACK
      !EV_FUNCTION_ID .
ENDCLASS.



CLASS ZCL_DBBR_SETTINGS_VIEW IMPLEMENTATION.


  METHOD constructor.
    mt_data = VALUE #(
      FOR setting IN it_settings
      ( setting_id = setting-setting_id
        setting_name = setting-setting_name
        setting_value = setting-setting_value
        t_cell = VALUE #(
          ( fieldname = 'SETTING_VALUE'
            style = COND #(
              WHEN setting-input_type = zif_dbbr_c_input_type=>checkbox AND
                   setting-setting_value = abap_false THEN zif_uitb_c_alv_cell_style=>checkbox_not_checked
              WHEN setting-input_type = zif_dbbr_c_input_type=>checkbox AND
                   setting-setting_value = abap_true THEN zif_uitb_c_alv_cell_style=>checkbox_checked
            )
            maxlen = cond #( when setting-max_length is not INITIAL then setting-max_length )
          )
          ( fieldname = 'SETTING_NAME'
            style = zif_uitb_c_alv_cell_style=>font_bold
          )
        )
      )
    ).

    mr_view = zcl_uitb_templt_prog_callback=>create_template_program( iv_title = iv_title ).

    SET HANDLER:
      on_pbo FOR mr_view,
      on_pai FOR mr_view.

  ENDMETHOD.


  METHOD create_alv.

    DATA: lr_col TYPE REF TO zcl_uitb_alv_column.

    mr_alv = zcl_uitb_alv=>create_alv(
        ir_data       = REF #( mt_data )
        ir_container  = mr_view->get_container( )
        if_editable   = abap_true
    ).

    mr_alv->get_functions( )->set_all( abap_false ).

    mr_alv->get_display_settings( )->set_row_marks( abap_false ).
    mr_alv->get_display_settings( )->set_row_insertions( abap_false ).
    mr_alv->get_display_settings( )->set_row_move_allowed( abap_false ).

    DATA(lr_cols) = mr_alv->get_columns( ).
    lr_cols->set_column_headers_visible( abap_false ).
    lr_cols->set_style_column( 'T_CELL' ).

    lr_cols->get_column( 'SETTING_ID' )->set_technical( ).
    lr_cols->get_column( 'INPUT_TYPE' )->set_technical( ).
    lr_cols->get_column( 'MAX_LENGTH' )->set_technical( ).
    lr_cols->get_column( 'LOWERCASE' )->set_technical( ).

    lr_col = lr_cols->get_column( 'SETTING_NAME' ).
    lr_col->set_key( ).
    lr_col->set_editable( abap_false ).
    lr_col->set_output_length( 20 ).

    lr_col = lr_cols->get_column( 'SETTING_VALUE' ).
    lr_col->set_editable( ).
    lr_col->set_output_length( 20 ).

*    mr_alv->get_data_changes( )->set_change_event( cl_gui_alv_grid=>mc_evt_modified ).

    mr_alv->display( ).

  ENDMETHOD.


  METHOD get_settings.
    result = CORRESPONDING #( mt_data ).
  ENDMETHOD.


  METHOD has_been_updated.
    result = mf_take_values.
  ENDMETHOD.


  METHOD on_pai.
    CASE ev_function_id.

      WHEN zif_uitb_template_prog=>c_func_ok.
        mf_take_values = abap_true.
        er_callback->exit_screen( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD on_pbo.

    er_callback->deactivate_function( zif_uitb_template_prog=>c_save ).

    IF er_callback->is_first_screen_call( ).
      create_alv( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_view~show.
    mr_view->show(
      iv_start_column = 10
      iv_start_line   = 2
      iv_end_column   = 60
      iv_end_line     = 12
    ).
  ENDMETHOD.
ENDCLASS.
