"! <p class="shorttext synchronized" lang="en">Controller for defining sorting fields</p>
CLASS zcl_dbbr_field_sorter_ctrl DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    METHODS constructor
      IMPORTING
        !ir_fields TYPE REF TO zcl_dbbr_tabfield_list .
    METHODS was_updated
      RETURNING
        VALUE(rf_updated) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Retrieves the updated table fields</p>
    METHODS get_updated_tabfields
      RETURNING
        VALUE(result) TYPE REF TO zcl_dbbr_tabfield_list .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    TYPES:
      BEGIN OF mty_sorted,
        fullfieldname   TYPE fieldname,
        fieldname       TYPE fieldname,
        tabname_alias   TYPE tabname,
        sort_direction  TYPE zdbbr_sort_direction,
        sort_order      TYPE numc3,
        scrtext_l       TYPE scrtext_l,
        sort_ascending  TYPE tv_image,
        sort_descending TYPE tv_image,
        drop_handled    TYPE boolean,
      END OF mty_sorted .
    TYPES:
      mtt_sorted TYPE STANDARD TABLE OF mty_sorted .
    TYPES:
      BEGIN OF mty_available,
        fullfieldname TYPE fieldname,
        fieldname     TYPE fieldname,
        tabname_alias TYPE tabname,
        alias         TYPE zdbbr_table_alias,
        ddic_order    TYPE tabfdpos,
        scrtext_l     TYPE scrtext_l,
      END OF mty_available .
    TYPES:
      mtt_available TYPE STANDARD TABLE OF mty_available .

    CONSTANTS mc_sorted_container TYPE dynfnam VALUE 'SORTED' ##NO_TEXT.
    CONSTANTS mc_available_container TYPE dynfnam VALUE 'AVAILABLE' ##NO_TEXT.
    CONSTANTS mc_toolbar TYPE dynfnam VALUE 'TOOLBAR' ##NO_TEXT.
    DATA mr_fields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mr_sorted_alv TYPE REF TO cl_alv_dd_listbox .
    DATA mr_sorted_container TYPE REF TO cl_gui_custom_container .
    DATA mr_available_fields_alv TYPE REF TO cl_alv_dd_listbox .
    DATA mr_available_fields_container TYPE REF TO cl_gui_custom_container .
    DATA mr_dnd_double_listbox TYPE REF TO cl_alv_dd_double_listbox .
    DATA mt_sorted_fields TYPE mtt_sorted .
    DATA mt_available_fields TYPE mtt_available .
    DATA mf_updated TYPE boolean .

    METHODS create_controls .
    METHODS create_sorted_alv .
    METHODS create_available_fields_alv .
    METHODS update_sorted_fields .
    METHODS search .
    METHODS on_available_alv_toolbar
          FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
          !e_interactive
          !e_object .
    METHODS on_available_drop_complete
        FOR EVENT drop_complete OF cl_alv_dd_listbox .
    METHODS on_sorted_hotspot
          FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
          !es_row_no
          !e_column_id
          !e_row_id .
    METHODS on_sorted_double_click
          FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
          !es_row_no
          !e_column
          !e_row .
    METHODS on_available_double_click
          FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
          !es_row_no
          !e_column
          !e_row .
ENDCLASS.



CLASS zcl_dbbr_field_sorter_ctrl IMPLEMENTATION.


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
            tabname_alias   = lr_current_entry->tabname_alias
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
            fieldname     = lr_current_entry->fieldname
            tabname_alias = lr_current_entry->tabname
            scrtext_l     = lr_current_entry->field_ddtext
            alias         = lr_current_entry->alias
            ddic_order    = lr_current_entry->ddic_order
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
      <ls_sorted_fields>-sort_direction = zif_dbbr_global_consts=>gc_sort_direction-ascending.
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
        IF lr_sort_field->sort_direction = zif_dbbr_global_consts=>gc_sort_direction-ascending.
          RETURN.
        ENDIF.

        lr_sort_field->sort_direction = zif_dbbr_global_consts=>gc_sort_direction-ascending.
        lr_sort_field->sort_ascending = zif_dbbr_c_icon=>radio_button.
        lr_sort_field->sort_descending = zif_dbbr_c_icon=>radio_button_empty.

      WHEN 'SORT_DESCENDING'.
        IF lr_sort_field->sort_direction = zif_dbbr_global_consts=>gc_sort_direction-descending.
          RETURN.
        ENDIF.
        lr_sort_field->sort_direction = zif_dbbr_global_consts=>gc_sort_direction-descending.
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

      IF line_exists( mt_sorted_fields[ tabname_alias   = lr_current_entry->tabname_alias
                                        fieldname       = lr_current_entry->fieldname ] ).
        DATA(lr_sorted_field) = REF #( mt_sorted_fields[ tabname_alias   = lr_current_entry->tabname
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


  METHOD zif_uitb_screen_controller~cancel.

    CASE iv_function_code.
      WHEN zif_dbbr_global_consts=>gc_function_codes-leave_screen OR
           zif_dbbr_global_consts=>gc_function_codes-quit_program OR
           zif_dbbr_global_consts=>gc_function_codes-cancel_screen OR
           zif_dbbr_global_consts=>gc_function_codes-cancel.

        zcl_dbbr_screen_helper=>leave_screen( ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.

    mr_sorted_container->free( ).
    mr_available_fields_container->free( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_define_field_sorting.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    cl_gui_cfw=>dispatch( IMPORTING return_code = DATA(lv_function) ).
    IF lv_function <> cl_gui_cfw=>rc_noevent.
      " a control event occured => exit PAI
      RETURN.
    ENDIF.

    CASE cv_function_code.

      WHEN 'OK'.
        mf_updated = abap_true.
        update_sorted_fields( ).
        zcl_dbbr_screen_helper=>leave_screen( ).

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


  METHOD zif_uitb_screen_controller~pbo.

    zif_uitb_screen_controller~set_status( ).
    create_controls( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    SET PF-STATUS '0820' OF PROGRAM zif_dbbr_c_report_id=>main.
    SET TITLEBAR 'SORT_FIELDS_TITLE' OF PROGRAM zif_dbbr_c_report_id=>main.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
  ENDMETHOD.
ENDCLASS.
