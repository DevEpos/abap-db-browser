"! <p class="shorttext synchronized" lang="en">History View for Selection Screen</p>
CLASS zcl_dbbr_selscreen_hist_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view .
    INTERFACES zif_uitb_disposable .

    ALIASES dispose
      FOR zif_uitb_disposable~dispose .
    ALIASES is_visible
      FOR zif_uitb_view~is_visible .
    ALIASES show
      FOR zif_uitb_view~show .

    "! <p class="shorttext synchronized" lang="en">CLASS_CONSTRUCTOR</p>
    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mf_visible
      FOR zif_uitb_view~mf_visible .

    TYPES:
      BEGIN OF ty_history.
    TYPES: type_icon TYPE iconname.
        INCLUDE TYPE zdbbr_selscreen_history.
    TYPES: color TYPE lvc_t_scol.
    TYPES: END OF ty_history .

    DATA mr_tmplt_prog TYPE REF TO zif_uitb_template_prog .
    DATA mr_dock TYPE REF TO cl_gui_docking_container .
    DATA mr_alv TYPE REF TO zcl_uitb_alv .
    DATA:
      mt_history TYPE STANDARD TABLE OF ty_history WITH EMPTY KEY .
    CLASS-DATA st_yellow TYPE lvc_t_scol .
    DATA mr_parent TYPE REF TO cl_gui_container .

    CONSTANTS c_navback_function TYPE ui_func VALUE 'NAVBACK' ##NO_TEXT.
    CONSTANTS c_navforward_function TYPE ui_func VALUE 'NAVFORWARD' ##NO_TEXT.
    CONSTANTS c_clear_function TYPE ui_func VALUE 'CLEAR' ##NO_TEXT.
    CONSTANTS c_close_function TYPE ui_func VALUE 'CLOSE' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">Update state of navigation functions in toolbar</p>
    METHODS update_function_state .
    "! <p class="shorttext synchronized" lang="en">Initilization</p>
    METHODS initialize .
    "! <p class="shorttext synchronized" lang="en">Fills the history table from the current navigation history</p>
    METHODS fill_history_table .
    "! <p class="shorttext synchronized" lang="en">Event handler for when history was modified</p>
    METHODS on_history_modified
        FOR EVENT history_modified OF zcl_dbbr_selscreen_history .
    "! <p class="shorttext synchronized" lang="en">Event handler for history navigation</p>
    METHODS on_history_navigation
          FOR EVENT navigated OF zcl_dbbr_selscreen_history
      IMPORTING
          !es_history_entry
          !ev_current_index .
    "! <p class="shorttext synchronized" lang="en">Event handler for alv toolbar command</p>
    METHODS on_user_command
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          !ev_function
          !ev_tag .
    "! <p class="shorttext synchronized" lang="en">Event Handler for link click on alv cell</p>
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          !ev_row
          !ev_column .
ENDCLASS.



CLASS zcl_dbbr_selscreen_hist_view IMPLEMENTATION.


  METHOD class_constructor.
    st_yellow = VALUE #(
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
        WHEN zif_dbbr_c_entity_type=>view THEN
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
        iv_description_language = zcl_dbbr_system_helper=>get_system_language( )
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

    DATA(lr_disp_settings) = mr_alv->get_display_settings( ).
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
    lr_functions->add_function( iv_type = zcl_uitb_alv_functions=>separator ).
    lr_functions->add_function(
        iv_name                  = c_clear_function
        iv_icon                  = |{ icon_delete }|
        iv_tooltip               = |{ 'Clear History'(004) }|
        if_start_of_toolbar      = abap_true
    ).
    lr_functions->add_function( iv_type = zcl_uitb_alv_functions=>separator ).
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
      VALUE #( row    = ev_current_index
               column = 'ENTITY_ID' )
    ).
    mr_alv->refresh( if_soft = abap_true ).
  ENDMETHOD.


  METHOD on_link_click.
    zcl_dbbr_selscreen_history=>navigate_to( ev_row ).
  ENDMETHOD.


  METHOD on_user_command.
    CASE ev_function.

      WHEN c_close_function.
        dispose( ).

      WHEN c_navback_function.
        zcl_dbbr_selscreen_history=>navigate_back( ).

      WHEN c_clear_function.
        dispose( ).
        zcl_dbbr_selscreen_history=>clear_history( ).

      WHEN c_navforward_function.
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
