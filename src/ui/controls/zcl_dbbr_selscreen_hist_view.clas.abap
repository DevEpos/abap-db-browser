class ZCL_DBBR_SELSCREEN_HIST_VIEW definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_VIEW .
  interfaces ZIF_UITB_DISPOSABLE .

  aliases DISPOSE
    for ZIF_UITB_DISPOSABLE~DISPOSE .
  aliases IS_VISIBLE
    for ZIF_UITB_VIEW~IS_VISIBLE .
  aliases SHOW
    for ZIF_UITB_VIEW~SHOW .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
protected section.
private section.

  aliases MF_VISIBLE
    for ZIF_UITB_VIEW~MF_VISIBLE .

  types:
    BEGIN OF ty_history.
  TYPES: type_icon TYPE iconname.
          INCLUDE TYPE zdbbr_selscreen_history.
  TYPES: color TYPE lvc_t_scol.
  TYPES: END OF ty_history .

  data MR_TMPLT_PROG type ref to ZIF_UITB_TEMPLATE_PROG .
  data MR_DOCK type ref to CL_GUI_DOCKING_CONTAINER .
  data MR_ALV type ref to ZCL_UITB_ALV .
  data:
    mt_history TYPE STANDARD TABLE OF ty_history WITH EMPTY KEY .
  class-data ST_YELLOW type LVC_T_SCOL .
  data MR_PARENT type ref to CL_GUI_CONTAINER .
  constants C_NAVBACK_FUNCTION type UI_FUNC value 'NAVBACK' ##NO_TEXT.
  constants C_NAVFORWARD_FUNCTION type UI_FUNC value 'NAVFORWARD' ##NO_TEXT.

  methods UPDATE_FUNCTION_STATE .
  methods INITIALIZE .
  methods FILL_HISTORY_TABLE .
  methods ON_HISTORY_MODIFIED
    for event HISTORY_MODIFIED of ZCL_DBBR_SELSCREEN_HISTORY .
  methods ON_HISTORY_NAVIGATION
    for event NAVIGATED of ZCL_DBBR_SELSCREEN_HISTORY
    importing
      !ES_HISTORY_ENTRY
      !EV_CURRENT_INDEX .
  methods ON_USER_COMMAND
    for event FUNCTION_CHOSEN of ZCL_UITB_ALV_EVENTS
    importing
      !EV_FUNCTION
      !EV_TAG .
  methods ON_LINK_CLICK
    for event LINK_CLICK of ZCL_UITB_ALV_EVENTS
    importing
      !EV_ROW
      !EV_COLUMN .
ENDCLASS.



CLASS ZCL_DBBR_SELSCREEN_HIST_VIEW IMPLEMENTATION.


  METHOD class_constructor.
    st_yellow = value #(
      ( color = zcl_uitb_alv_color_mapper=>string_to_struc( zif_uitb_c_alv_colors=>yellow ) )
    ).
  ENDMETHOD.


  METHOD constructor.

  ENDMETHOD.


  METHOD fill_history_table.
*... Load and fill history
    mt_history = CORRESPONDING #( zcl_dbbr_selscreen_history=>get_history( ) ).
    DATA(lv_current_history_index) = zcl_dbbr_selscreen_history=>get_current_index( ).

    LOOP AT mt_history ASSIGNING FIELD-SYMBOL(<ls_history>).

*... color current history entry
      IF sy-tabix = lv_current_history_index.
        <ls_history>-color = st_yellow.
      ENDIF.

*... fill type icon
      <ls_history>-type_icon = SWITCH #(
        <ls_history>-entity_type
        WHEN zif_dbbr_c_entity_type=>table THEN
          zif_dbbr_c_icon=>database_table
        WHEN zif_dbbr_c_entity_type=>query THEN
          zif_dbbr_c_icon=>query
        when zif_dbbr_c_entity_type=>view then
          zif_dbbr_c_icon=>database_view
        WHEN zif_dbbr_c_entity_type=>cds_view THEN
          zif_dbbr_c_icon=>cds_view
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD initialize.
    DATA: lr_col TYPE REF TO zcl_uitb_alv_column.

    CHECK mr_dock IS INITIAL.

    mr_dock = NEW cl_gui_docking_container(
        side      = cl_gui_docking_container=>dock_at_bottom
        extension = 120
    ).

    fill_history_table( ).

    mr_alv = zcl_uitb_alv=>create_alv(
        ir_data                 = REF #( mt_history )
        iv_description_language = zcl_dbbr_appl_util=>get_description_language( )
        ir_container            = mr_dock
        if_editable             = abap_false
    ).
*... adjust columns
    DATA(lr_columns) = mr_alv->get_columns( ).

    lr_columns->get_column( 'VARIANT_ID' )->set_technical( ).
    lr_columns->get_column( 'VARIANT_NAME' )->set_technical( ).
    lr_columns->get_column( 'ENTITY_TYPE' )->set_technical( ).

    lr_col = lr_columns->get_column( 'ENTITY_ID' ).
    lr_col->set_cell_type( zif_uitb_c_alv_cell_types=>hotspot ).
    lr_col->set_output_length( 30 ).

    lr_col = lr_columns->get_column( 'TYPE_ICON' ).
    lr_col->set_icon( ).
    lr_col->set_descriptions( iv_long = 'Type' ).
    lr_col->set_output_length( 5 ).

    lr_columns->set_color_column( 'COLOR' ).

    lr_col = lr_columns->get_column( 'DESCRIPTION' ).
    lr_col->set_output_length( 60 ).

    data(lr_disp_settings) = mr_alv->get_display_settings( ).
    lr_disp_settings->set_small_title( ).
    lr_disp_settings->set_title( 'Navigation Stack' ).
    lr_disp_settings->set_row_marks( abap_false ).

    mr_alv->get_selections( )->set_mode( value = zif_uitb_c_alv_selection=>row_column ).

    DATA(lr_functions) = mr_alv->get_functions( ).
    lr_functions->set_all( abap_false ).
    lr_functions->add_function(
        iv_name                  = c_navback_function
        iv_icon                  = |{ icon_arrow_left }|
        iv_tooltip               = CONV #( 'Previous Object'(001) )
        if_start_of_toolbar      = abap_true
    ).
    lr_functions->add_function(
        iv_name                  = c_navforward_function
        iv_icon                  = |{ icon_arrow_right }|
        iv_tooltip               = CONV #( 'Next Object'(002) )
        if_start_of_toolbar      = abap_true
    ).
    lr_functions->add_function(
        iv_name                  = 'CLOSE'
        iv_icon                  = |{ icon_close }|
        iv_tooltip               = CONV #( 'Close History'(003) )
        if_start_of_toolbar      = abap_true
    ).

    update_function_state( ).

    SET HANDLER:
      on_user_command FOR mr_alv->get_events( ),
      on_link_click   FOR mr_alv->get_events( ).

  ENDMETHOD.


  METHOD on_history_modified.
*... update history table and set selected index back to 1
    fill_history_table( ).

    update_function_state( ).

    mr_alv->refresh( if_soft = abap_true ). "if_keep_scroll_position = abap_true ).
  ENDMETHOD.


  METHOD on_history_navigation.
    LOOP AT mt_history ASSIGNING FIELD-SYMBOL(<ls_history>).
      CLEAR: <ls_history>-color.

      IF sy-tabix = ev_current_index.
        <ls_history>-color = st_yellow.
      ENDIF.
    ENDLOOP.

    update_function_state( ).

    mr_alv->get_selections( )->set_current_cell(
      value #( row    = ev_current_index
               column = 'ENTITY_ID' )
    ).
    mr_alv->refresh( if_soft = abap_true ).
  ENDMETHOD.


  METHOD on_link_click.
    zcl_dbbr_selscreen_history=>navigate_to( ev_row ).
  ENDMETHOD.


  METHOD on_user_command.
    CASE ev_function.

      WHEN 'CLOSE'.
        dispose( ).

      WHEN 'NAVBACK'.
        zcl_dbbr_selscreen_history=>navigate_back( ).

      WHEN 'NAVFORWARD'.
        zcl_dbbr_selscreen_history=>navigate_forward( ).
    ENDCASE.

  ENDMETHOD.


  METHOD update_function_state.
    DATA(lr_functions) = mr_alv->get_functions( ).
    lr_functions->set_function(
        iv_name    = c_navback_function
        if_enable  = zcl_dbbr_selscreen_history=>has_previous( )
    ).
    lr_functions->set_function(
        iv_name    = c_navforward_function
        if_enable  = zcl_dbbr_selscreen_history=>has_next( )
    ).
  ENDMETHOD.


  METHOD zif_uitb_disposable~dispose.
    CHECK mr_dock IS NOT INITIAL.

    mr_dock->free( ).
    CLEAR: mr_dock,
           mr_alv.

*... unregister event handlers as reference to control may still exist
    SET HANDLER:
      on_history_modified ACTIVATION abap_false,
      on_history_navigation ACTIVATION abap_false.

    CLEAR mf_visible.
  ENDMETHOD.


  METHOD zif_uitb_view~is_visible.
    result = mf_visible.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    initialize( ).

*... show alv
    mr_alv->display( ).

*... register event handlers for history
    SET HANDLER:
       on_history_modified,
       on_history_navigation.

*... set view to visible
    mf_visible = abap_true.
  ENDMETHOD.
ENDCLASS.
