class ZCL_DBBR_FIELD_SORTER_CTRL definition
  public
  create public .

public section.

  interfaces ZIF_UITB_SCREEN_CONTROLLER .

  methods CONSTRUCTOR
    importing
      !IR_FIELDS type ref to ZCL_DBBR_TABFIELD_LIST .
  methods WAS_UPDATED
    returning
      value(RF_UPDATED) type BOOLEAN .
  methods GET_UPDATED_TABFIELDS
    returning
      value(RESULT) type ref to ZCL_DBBR_TABFIELD_LIST .
  PROTECTED SECTION.
private section.

  aliases GET_REPORT_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_REPORT_ID .
  aliases GET_SCREEN_ID
    for ZIF_UITB_SCREEN_CONTROLLER~GET_SCREEN_ID .

  types:
    BEGIN OF mty_sorted,
        fullfieldname   TYPE fieldname,
        fieldname       TYPE fieldname,
        tabname         TYPE tabname,
        sort_direction  TYPE ZDBBR_sort_direction,
        sort_order      TYPE numc3,
        scrtext_l       TYPE scrtext_l,
        sort_ascending  TYPE tv_image,
        sort_descending TYPE tv_image,
        drop_handled    TYPE boolean,
      END OF mty_sorted .
  types:
    mtt_sorted TYPE STANDARD TABLE OF mty_sorted .
  types:
    BEGIN OF mty_available,
        fullfieldname TYPE fieldname,
        fieldname     TYPE fieldname,
        tabname       TYPE tabname,
        alias         TYPE ZDBBR_table_alias,
        ddic_order    TYPE tabfdpos,
        scrtext_l     TYPE scrtext_l,
      END OF mty_available .
  types:
    mtt_available TYPE STANDARD TABLE OF mty_available .

  constants MC_SORTED_CONTAINER type DYNFNAM value 'SORTED' ##NO_TEXT.
  constants MC_AVAILABLE_CONTAINER type DYNFNAM value 'AVAILABLE' ##NO_TEXT.
  constants MC_TOOLBAR type DYNFNAM value 'TOOLBAR' ##NO_TEXT.
  data MR_FIELDS type ref to ZCL_DBBR_TABFIELD_LIST .
  data MR_SORTED_ALV type ref to CL_ALV_DD_LISTBOX .
  data MR_SORTED_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data MR_AVAILABLE_FIELDS_ALV type ref to CL_ALV_DD_LISTBOX .
  data MR_AVAILABLE_FIELDS_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data MR_DND_DOUBLE_LISTBOX type ref to CL_ALV_DD_DOUBLE_LISTBOX .
  data MT_SORTED_FIELDS type MTT_SORTED .
  data MT_AVAILABLE_FIELDS type MTT_AVAILABLE .
  data MF_UPDATED type BOOLEAN .

  methods CREATE_CONTROLS .
  methods CREATE_SORTED_ALV .
  methods CREATE_AVAILABLE_FIELDS_ALV .
  methods UPDATE_SORTED_FIELDS .
  methods SEARCH .
  methods ON_AVAILABLE_ALV_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_INTERACTIVE
      !E_OBJECT .
  methods ON_AVAILABLE_DROP_COMPLETE
    for event DROP_COMPLETE of CL_ALV_DD_LISTBOX .
  methods ON_SORTED_HOTSPOT
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO
      !E_COLUMN_ID
      !E_ROW_ID .
  methods ON_SORTED_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO
      !E_COLUMN
      !E_ROW .
  methods ON_AVAILABLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO
      !E_COLUMN
      !E_ROW .
ENDCLASS.



CLASS ZCL_DBBR_FIELD_SORTER_CTRL IMPLEMENTATION.


  METHOD constructor.

    mr_fields = ir_fields->copy( ).
    mr_fields->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-output ).

    mr_fields->sort( ).
    mr_fields->initialize_iterator( if_for_active = abap_true ).

    WHILE mr_fields->has_more_lines( ).
      DATA(lr_current_entry) = mr_fields->get_next_entry( ).

      CHECK lr_current_entry->is_text_field = abap_false.
      CHECK lr_current_entry->is_formula_field = abap_false.

      DATA(lv_fullfieldname)   = COND #( WHEN lr_current_entry->alias IS NOT INITIAL THEN
                                           lr_current_entry->alias && '~' && lr_current_entry->fieldname
                                         ELSE
                                           lr_current_entry->fieldname_raw ).
      IF lr_current_entry->sort_active = abap_true.
        APPEND VALUE mty_sorted(
            fullfieldname   = lv_fullfieldname
            fieldname       = lr_current_entry->fieldname
            tabname         = lr_current_entry->tabname
            scrtext_l       = lr_current_entry->field_ddtext
            sort_direction  = lr_current_entry->sort_direction
            sort_order      = lr_current_entry->sort_order
            sort_ascending  = COND #( WHEN lr_current_entry->sort_direction = zif_dbbr_global_consts=>gc_sort_direction-ascending THEN
                                        zif_dbbr_c_icon=>radio_button
                                      ELSE
                                        zif_dbbr_c_icon=>radio_button_empty
                                    )
            sort_descending = COND #( WHEN lr_current_entry->sort_direction = zif_dbbr_global_consts=>gc_sort_direction-descending THEN
                                        zif_dbbr_c_icon=>radio_button
                                      ELSE
                                        zif_dbbr_c_icon=>radio_button_empty
                                    )
            drop_handled    = abap_true
        ) TO mt_sorted_fields.
      ELSE.
        APPEND VALUE mty_available(
            fullfieldname = lv_fullfieldname
            fieldname     = lr_current_entry->fieldname_raw
            alias         = lr_current_entry->alias
            ddic_order    = lr_current_entry->ddic_order
            tabname       = lr_current_entry->tabname
            scrtext_l     = lr_current_entry->field_ddtext
        ) TO mt_available_fields.
      ENDIF.
    ENDWHILE.

    """ sort internal alv tables
    SORT mt_sorted_fields BY sort_order.

  ENDMETHOD.


  METHOD create_available_fields_alv.

    DATA: lt_fieldcat TYPE lvc_t_fcat.

    CHECK mr_available_fields_container IS INITIAL.

    DATA(ls_layout) = VALUE lvc_s_layo(
        grid_title   = 'Available Columns'
        sel_mode     = 'C'
        sgl_clk_hd   = abap_true
    ).

    lt_fieldcat = VALUE lvc_t_fcat(
      ( fieldname      = 'FULLFIELDNAME'
        outputlen      = 25
        rollname       = 'FIELDNAME'
        reptext        = 'Column Name'
        coltext        = 'Column Name' )
      ( fieldname      = 'SCRTEXT_L'
        outputlen      = 25
        rollname       = 'SCRTEXT_L'
        reptext        = 'Description'
        coltext        = 'Description' )
    ).

    mr_available_fields_container = NEW #( container_name = mc_available_container ).
    mr_available_fields_alv = NEW #( i_parent = mr_available_fields_container ).

    SET HANDLER:
        on_available_alv_toolbar FOR mr_available_fields_alv,
        on_available_drop_complete FOR mr_available_fields_alv,
        on_available_double_click FOR mr_available_fields_alv.
*        on_user_command FOR mr_available_fields_alv.

    mr_available_fields_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = mt_available_fields
        it_fieldcatalog               = lt_fieldcat
    ).

  ENDMETHOD.


  METHOD create_controls.

    create_sorted_alv( ).
    create_available_fields_alv( ).

    IF mr_dnd_double_listbox IS INITIAL.
      mr_dnd_double_listbox = NEW cl_alv_dd_double_listbox(
          i_grid2 = mr_available_fields_alv
          i_grid1 = mr_sorted_alv
      ).

      cl_gui_control=>set_focus( mr_available_fields_alv ).
    ENDIF.

  ENDMETHOD.


  METHOD create_sorted_alv.

    DATA: lt_fieldcat TYPE lvc_t_fcat.

    CHECK mr_sorted_container IS INITIAL.

    mr_sorted_container = NEW #( container_name = mc_sorted_container ).
    mr_sorted_alv = NEW #( i_parent = mr_sorted_container ).

    SET HANDLER: on_sorted_hotspot FOR mr_sorted_alv,
                 on_sorted_double_click FOR mr_sorted_alv.

    DATA(ls_layout) = VALUE lvc_s_layo(
        sel_mode     = 'C'
    ).

    lt_fieldcat = VALUE lvc_t_fcat(
      ( fieldname      = 'FULLFIELDNAME'
        outputlen      = 20
        rollname       = 'FIELDNAME'
        reptext        = 'Column Name'
        coltext        = 'Column Name'
        scrtext_l      = 'Column Name'
        scrtext_m      = 'Column Name'
        scrtext_s      = 'Column Name' )
      ( fieldname      = 'SORT_ASCENDING'
        outputlen      = 5
        icon           = abap_true
        hotspot        = abap_true
        rollname       = 'TV_IMAGE'
        reptext        = 'Sort Up'
        tooltip        = 'Sort Up'
        coltext        = zif_dbbr_c_icon=>sort_up )
      ( fieldname      = 'SORT_DESCENDING'
        outputlen      = 5
        icon           = abap_true
        hotspot        = abap_true
        rollname       = 'TV_IMAGE'
        reptext        = 'Sort Down'
        tooltip        = 'Sort Down'
        coltext        = zif_dbbr_c_icon=>sort_down )
    ).

    mr_sorted_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = mt_sorted_fields
        it_fieldcatalog               = lt_fieldcat
    ).

  ENDMETHOD.


  METHOD get_updated_tabfields.
    result = mr_fields.
  ENDMETHOD.


  METHOD on_available_alv_toolbar.

    DELETE e_object->mt_toolbar WHERE function <> cl_gui_alv_grid=>mc_fc_find.

*    e_object->mt_toolbar = VALUE #(
*      BASE e_object->mt_toolbar
*      ( butn_type = cntb_btype_menu
*        function  = '$FILT_MENU'
*        icon      = icon_filter
*        text      = 'Alles' )
*    ).

  ENDMETHOD.


  METHOD on_available_double_click.

    mr_dnd_double_listbox->movetogrid1(
        i_ok_code = space
        is_stable = VALUE #( row = abap_true col = abap_true )
    ).

  ENDMETHOD.


  METHOD on_available_drop_complete.

    """ get selected lines of `availabe fields` alv
    LOOP AT mt_sorted_fields ASSIGNING FIELD-SYMBOL(<ls_sorted_fields>) WHERE drop_handled = abap_false.
      <ls_sorted_fields>-sort_descending = zif_dbbr_c_icon=>radio_button_empty.
      <ls_sorted_fields>-sort_direction = ZIF_DBBR_global_consts=>gc_sort_direction-ascending.
      <ls_sorted_fields>-sort_ascending = zif_dbbr_c_icon=>radio_button.
      <ls_sorted_fields>-drop_handled = abap_true.
      DATA(lf_updated) = abap_true.
    ENDLOOP.

    IF lf_updated = abap_true.
      mr_sorted_alv->refresh_table_display( is_stable  = VALUE #( row = abap_true ) ).
    ENDIF.

  ENDMETHOD.


  METHOD on_sorted_double_click.

    mr_dnd_double_listbox->movetogrid2(
        i_ok_code = space
        is_stable = VALUE #( row = abap_true col = abap_true )
    ).

  ENDMETHOD.


  METHOD on_sorted_hotspot.

    " get current line
    DATA(lr_sort_field) = REF #( mt_sorted_fields[ e_row_id-index ] ).

    CASE e_column_id.
      WHEN 'SORT_ASCENDING'.
        IF lr_sort_field->sort_direction = ZIF_DBBR_global_consts=>gc_sort_direction-ascending.
          RETURN.
        ENDIF.

        lr_sort_field->sort_direction = ZIF_DBBR_global_consts=>gc_sort_direction-ascending.
        lr_sort_field->sort_ascending = zif_dbbr_c_icon=>radio_button.
        lr_sort_field->sort_descending = zif_dbbr_c_icon=>radio_button_empty.

      WHEN 'SORT_DESCENDING'.
        IF lr_sort_field->sort_direction = ZIF_DBBR_global_consts=>gc_sort_direction-descending.
          RETURN.
        ENDIF.
        lr_sort_field->sort_direction = ZIF_DBBR_global_consts=>gc_sort_direction-descending.
        lr_sort_field->sort_descending = zif_dbbr_c_icon=>radio_button.
        lr_sort_field->sort_ascending = zif_dbbr_c_icon=>radio_button_empty.
    ENDCASE.

    mr_sorted_alv->refresh_table_display( is_stable  = VALUE #( row = abap_true ) ).

  ENDMETHOD.


  METHOD search.
    DATA: lr_listbox TYPE REF TO cl_alv_dd_listbox.

    cl_gui_control=>get_focus(
      IMPORTING
         control = DATA(lr_focused_control)
      EXCEPTIONS
         cntl_error = 1
         cntl_system_error = 2
         OTHERS = 3
    ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        lr_listbox = CAST #( lr_focused_control ).
        " manually set the focus
        cl_gui_control=>set_focus( lr_focused_control ).
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    DATA(lv_ucomm) = CONV sy-ucomm( '%SC' ).
    lr_listbox->set_function_code( CHANGING c_ucomm = lv_ucomm ).

  ENDMETHOD.


  METHOD update_sorted_fields.


    """ create order from current list of sorted fields
    LOOP AT mt_sorted_fields ASSIGNING FIELD-SYMBOL(<ls_sorted_field>).
      <ls_sorted_field>-sort_order = sy-tabix.
    ENDLOOP.

    mr_fields->initialize_iterator( ).

    WHILE mr_fields->has_more_lines( ).
      DATA(lr_current_entry) = mr_fields->get_next_entry( ).

      CHECK lr_current_entry->is_text_field = abap_false.
      CHECK lr_current_entry->is_formula_field = abap_false.

      IF line_exists( mt_sorted_fields[ tabname   = lr_current_entry->tabname
                                        fieldname = lr_current_entry->fieldname ] ).
        DATA(lr_sorted_field) = REF #( mt_sorted_fields[ tabname   = lr_current_entry->tabname
                                                         fieldname = lr_current_entry->fieldname ] ).
        lr_current_entry->sort_active = abap_true.
        lr_current_entry->sort_direction = lr_sorted_field->sort_direction.
        lr_current_entry->sort_order = lr_sorted_field->sort_order.
      ELSE.
        CLEAR: lr_current_entry->sort_active,
               lr_current_entry->sort_direction,
               lr_current_entry->sort_order.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD was_updated.

    rf_updated = mf_updated.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_sort_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~cancel.

    CASE iv_function_code.
      WHEN ZIF_DBBR_global_consts=>gc_function_codes-leave_screen OR
           ZIF_DBBR_global_consts=>gc_function_codes-quit_program OR
           ZIF_DBBR_global_consts=>gc_function_codes-cancel_screen OR
           ZIF_DBBR_global_consts=>gc_function_codes-cancel.

        ZCL_DBBR_screen_helper=>leave_screen( ).

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~free_screen_resources.

    mr_sorted_container->free( ).
    mr_available_fields_container->free( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_define_field_sorting.
  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~handle_user_command.

    cl_gui_cfw=>dispatch( IMPORTING return_code = DATA(lv_function) ).
    IF lv_function <> cl_gui_cfw=>rc_noevent.
      " a control event occured => exit PAI
      RETURN.
    ENDIF.

    CASE cv_function_code.

      WHEN 'OK'.
        mf_updated = abap_true.
        update_sorted_fields( ).
        ZCL_DBBR_screen_helper=>leave_screen( ).

      WHEN 'MOVE_LEFT'.
        mr_dnd_double_listbox->movetogrid1(
            i_ok_code = cv_function_code
        ).

      WHEN 'MOVE_RIGHT'.
        mr_dnd_double_listbox->movetogrid2(
            i_ok_code = cv_function_code
        ).

      WHEN 'SEARCH'.
        search( ).

    ENDCASE.

    CLEAR cv_function_code.

  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~pbo.

    ZIF_UITB_SCREEN_CONTROLLER~set_status( ).
    create_controls( ).

  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~set_status.

    SET PF-STATUS '0820' OF PROGRAM zif_dbbr_c_report_id=>main.
    SET TITLEBAR 'SORT_FIELDS_TITLE' OF PROGRAM zif_dbbr_c_report_id=>main.

  ENDMETHOD.


  METHOD ZIF_UITB_SCREEN_CONTROLLER~was_not_cancelled.
  ENDMETHOD.
ENDCLASS.
