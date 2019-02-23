"! <p class="shorttext synchronized" lang="en">Navigation Control in Selection Screen</p>
CLASS zcl_dbbr_object_navigator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view.
    INTERFACES zif_uitb_gui_composite_view .
    INTERFACES zif_uitb_gui_control .
    INTERFACES zif_uitb_content_searcher .

    ALIASES focus
      FOR zif_uitb_gui_control~focus .
    ALIASES free
      FOR zif_uitb_view~free .
    ALIASES has_focus
      FOR zif_uitb_gui_control~has_focus .
    ALIASES hide
      FOR zif_uitb_view~hide .
    ALIASES is_visible
      FOR zif_uitb_view~is_visible .
    ALIASES search
      FOR zif_uitb_content_searcher~search .
    ALIASES search_next
      FOR zif_uitb_content_searcher~search_next .
    ALIASES show
      FOR zif_uitb_view~show .

    CONSTANTS:
      BEGIN OF c_view,
        favorites      TYPE ui_func VALUE 'FAV' ##NO_TEXT,
        history        TYPE ui_func VALUE 'HISTORY' ##no_text,
        object_browser TYPE ui_func VALUE 'REPOSITORY' ##NO_TEXT,
      END OF c_view.

    CONSTANTS:
      BEGIN OF c_command_id,
        show_object_list TYPE ui_func VALUE 'SHOW_OBJ_LIST',
      END OF c_command_id.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor .
    "! <p class="shorttext synchronized" lang="en">Updates the content view of the navigator</p>
    METHODS update_view
      IMPORTING
        iv_function TYPE ui_func .

  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mf_visible
      FOR zif_uitb_view~mf_visible .

    TYPES:
      BEGIN OF ty_view_caption,
        icon TYPE icon_d,
        text TYPE text40,
      END OF ty_view_caption .
    TYPES:
      BEGIN OF ty_view_info,
        cell_id   TYPE i,
        function  TYPE ui_func,
        caption   TYPE ty_view_caption,
        is_active TYPE abap_bool,
      END OF ty_view_info .

    "! <p class="shorttext synchronized" lang="en">Maximum number of views</p>
    CONSTANTS c_max_views TYPE i VALUE 3 ##NO_TEXT.
    CONSTANTS c_first_view_id TYPE i VALUE 2.
    "! <p class="shorttext synchronized" lang="en">Docking Control Container</p>
    DATA mo_dock TYPE REF TO cl_gui_docking_container .
    DATA mo_favorites_tree TYPE REF TO zcl_dbbr_favorites_tree .
    "! <p class="shorttext synchronized" lang="en">Toolbar Control</p>
    DATA mo_mode_toolbar TYPE REF TO cl_gui_toolbar .
    DATA mo_object_browser TYPE REF TO zcl_dbbr_object_browser_tree .
    "! <p class="shorttext synchronized" lang="en">Splitter Control</p>
    DATA mo_splitter TYPE REF TO cl_gui_splitter_container .
    DATA mo_history TYPE REF TO zcl_dbbr_object_history_tree .
    DATA:
      mt_view_info      TYPE STANDARD TABLE OF ty_view_info .
    DATA mv_dock_extension TYPE i .
    DATA mv_last_view_id TYPE i.

    "! <p class="shorttext synchronized" lang="en">Adapt view and toolbar after toolbar click</p>
    METHODS adapt_view_and_toolbar
      IMPORTING
        !iv_new_view_id    TYPE i
        !iv_active_view_id TYPE i OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Creates docking container for controls</p>
    METHODS create_dock .
    "! <p class="shorttext synchronized" lang="en">Creates Splitter for toolbar and content</p>
    METHODS create_splitter .
    "! <p class="shorttext synchronized" lang="en">Creates toolbar for view switching</p>
    METHODS create_toolbar .
    METHODS get_active_content_searcher
      RETURNING
        VALUE(rr_control) TYPE REF TO zif_uitb_content_searcher .
    "! <p class="shorttext synchronized" lang="en">Retrieve the currently active view in the navigator</p>
    METHODS get_active_control
      RETURNING
        VALUE(rr_control) TYPE REF TO zif_uitb_gui_control .
    "! <p class="shorttext synchronized" lang="en">Handler for Toolbar Button event</p>
    METHODS on_toolbar_button
          FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
          !fcode .
    "! <p class="shorttext synchronized" lang="en">Handler for goto next object nav. view</p>
    "!
    METHODS on_goto_next_objnav_view
        FOR EVENT goto_next_view_in_objnav OF zcl_dbbr_selscr_nav_events.
    "! <p class="shorttext synchronized" lang="en">Show Favorites view in navigator</p>
    METHODS show_favorites
      IMPORTING
        !ir_container TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Show history view</p>
    "!
    "! @parameter ir_container | <p class="shorttext synchronized" lang="en"></p>
    METHODS show_history
      IMPORTING
        !ir_container TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Show Repository Browser (DB,CDS View) in navigator</p>
    METHODS show_repo_browser
      IMPORTING
        !ir_container TYPE REF TO cl_gui_container .
    METHODS get_next_view_function
      RETURNING
        VALUE(rv_next_view_func) TYPE ui_func.
ENDCLASS.



CLASS zcl_dbbr_object_navigator IMPLEMENTATION.


  METHOD adapt_view_and_toolbar.

    IF iv_active_view_id <> -1.
      DATA(lo_s_old_active_cell) = REF #( mt_view_info[ cell_id = iv_active_view_id ] OPTIONAL ).
      lo_s_old_active_cell->is_active = abap_false.
      mo_mode_toolbar->set_button_state( EXPORTING checked = abap_false
                                                   fcode   = lo_s_old_active_cell->function
                                         EXCEPTIONS OTHERS = 1 ).
      mo_splitter->set_row_height( EXPORTING id      = iv_active_view_id
                                             height  = 0
                                   EXCEPTIONS OTHERS = 1 ).
    ENDIF.

    DATA(lo_s_new_active_cell) = REF #( mt_view_info[ cell_id = iv_new_view_id ] ).
    lo_s_new_active_cell->is_active = abap_true.
    mo_mode_toolbar->set_button_state( EXPORTING  checked = abap_true
                                                  fcode   = lo_s_new_active_cell->function
                                       EXCEPTIONS OTHERS  = 1 ).
    mo_splitter->set_row_height( EXPORTING  id     = iv_new_view_id
                                            height = -1
                                 EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.


  METHOD constructor.
*.. Create view information
    mt_view_info = VALUE #(
      ( cell_id  = c_first_view_id
        function = c_view-history
        caption  = VALUE #(
          text = 'History'(003)
          icon = zif_dbbr_c_icon=>history
        )
      )
      ( cell_id  = 3
        function = c_view-favorites
        caption  = VALUE #(
          text = 'Favorites'(001)
          icon = icon_system_favorites
        )
      )
      ( cell_id  = 4
        function = c_view-object_browser
        caption = VALUE #(
          text = 'Object Browser'(002)
          icon = icon_tree
        )
      )
    ).

    mv_last_view_id = c_first_view_id + c_max_views - 1.

    create_dock( ).
  ENDMETHOD.


  METHOD create_dock.
    CHECK mo_dock IS INITIAL.

    mo_dock = NEW cl_gui_docking_container(
        side      = cl_gui_docking_container=>dock_at_left
        dynnr     = zif_dbbr_screen_ids=>c_selection_screen
        repid     = zif_dbbr_c_report_id=>main
        extension = 460
    ).

    mo_dock->set_visible( abap_false ).
  ENDMETHOD.


  METHOD create_splitter.
    CHECK: mo_splitter IS INITIAL,
           mo_dock IS BOUND.

    DATA(lv_max_views) = c_max_views.

    mo_splitter = NEW cl_gui_splitter_container(
        parent                  = mo_dock
        rows                    = lv_max_views + 1
        columns                 = 1
    ).

    mo_splitter->set_row_mode( cl_gui_splitter_container=>mode_absolute ).
    mo_splitter->set_border( EXPORTING border = abap_false EXCEPTIONS OTHERS = 0 ).

    DO lv_max_views + 1 TIMES.
      mo_splitter->set_row_sash(
          id     = sy-index
          type   = cl_gui_splitter_container=>type_movable
          value  = cl_gui_splitter_container=>false
      ).

      mo_splitter->set_row_sash(
          id     = sy-index
          type   = cl_gui_splitter_container=>type_sashvisible
          value  = cl_gui_splitter_container=>false
      ).

      IF sy-index > 1.
        mo_splitter->set_row_height(
            id     = sy-index
            height = 0
        ).
      ENDIF.
    ENDDO.

*.. Set height of toolbar panel
    mo_splitter->set_row_height(
        id     = 1
        height = cl_gui_cfw=>compute_metric_from_dynp(
           metric = cl_gui_control=>metric_pixel
           x_or_y = 'Y'
           in = lv_max_views
        ) + 10 "10px margin
    ).

  ENDMETHOD.


  METHOD create_toolbar.
    DATA: lt_events TYPE cntl_simple_events.

    CHECK mo_mode_toolbar IS INITIAL.

    DATA(lo_container) = mo_splitter->get_container( row = 1 column = 1 ).
    mo_mode_toolbar = NEW #(
      parent       = lo_container
      display_mode = cl_gui_toolbar=>m_mode_vertical
    ).

    lt_events = VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
                         ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  appl_event = abap_true ) ).

    mo_mode_toolbar->set_registered_events( events = lt_events ).

    SET HANDLER:
      on_toolbar_button FOR mo_mode_toolbar.

    LOOP AT mt_view_info ASSIGNING FIELD-SYMBOL(<ls_view_info>).
      mo_mode_toolbar->add_button(
        fcode      = <ls_view_info>-function
        icon       = <ls_view_info>-caption-icon
        butn_type  = cntb_btype_check
        text       = <ls_view_info>-caption-text
        is_checked = xsdbool( <ls_view_info>-is_active = abap_true )
      ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_active_content_searcher.
    DATA(lr_view_info) = REF #( mt_view_info[ is_active = abap_true ] OPTIONAL ).
    CHECK lr_view_info IS NOT INITIAL.

    CASE lr_view_info->function.

      WHEN c_view-favorites.
        rr_control = mo_favorites_tree.

      WHEN c_view-object_browser.
        rr_control = mo_object_browser.

      WHEN c_view-history.
        rr_control = mo_history.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.



  METHOD get_active_control.
    DATA(lr_view_info) = REF #( mt_view_info[ is_active = abap_true ] OPTIONAL ).
    CHECK lr_view_info IS NOT INITIAL.

    CASE lr_view_info->function.

      WHEN c_view-favorites.
        rr_control = mo_favorites_tree.

      WHEN c_view-object_browser.
        rr_control = mo_object_browser.

      WHEN c_view-history.
        rr_control = mo_history.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD get_next_view_function.
    DATA: lv_active_view_id TYPE i VALUE -1.

    DATA(lv_default_func) = mt_view_info[ cell_id = c_first_view_id ]-function.

*.. Retrieve active cell
    DATA(lo_s_active_view) = REF #( mt_view_info[ is_active = abap_true ] OPTIONAL ).
    IF lo_s_active_view IS NOT INITIAL.
      rv_next_view_func = VALUE #( mt_view_info[ cell_id = lo_s_active_view->cell_id + 1 ]-function DEFAULT lv_default_func ).
    ELSE.
      rv_next_view_func = lv_default_func.
    ENDIF.
  ENDMETHOD.

  METHOD on_goto_next_objnav_view.
    update_view( get_next_view_function( ) ).
  ENDMETHOD.


  METHOD on_toolbar_button.
    update_view( iv_function = fcode ).
  ENDMETHOD.


  METHOD show_favorites.

    CHECK mo_favorites_tree IS INITIAL.

    DATA(lo_s_global_data) = CAST zdbbr_global_data( zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main )->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).
    mo_favorites_tree = NEW #(
        io_parent      = ir_container
        io_parent_view = me
    ).
    TRY.
        mo_favorites_tree->show(
            if_global = xsdbool( lo_s_global_data->fav_user_mode = zif_dbbr_global_consts=>gc_fav_user_modes-global )
        ).
      CATCH zcx_uitb_tree_error INTO DATA(lx_tree_error).
        MESSAGE s055(zdbbr_exception) DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD show_history.
    CHECK mo_history IS INITIAL.

    mo_history = NEW zcl_dbbr_object_history_tree(
      io_parent_container = ir_container
      io_parent_view      = me
    ).
  ENDMETHOD.


  METHOD show_repo_browser.
    CHECK mo_object_browser IS INITIAL.

    mo_object_browser = NEW zcl_dbbr_object_browser_tree(
        io_parent      = ir_container
        io_parent_view = me
    ).
  ENDMETHOD.


  METHOD update_view.
    DATA: lv_active_view_id TYPE i VALUE -1.

*.. Retrieve active cell
    DATA(lr_active_view) = REF #( mt_view_info[ is_active = abap_true ] OPTIONAL ).
    IF lr_active_view IS NOT INITIAL.
      IF lr_active_view->function = iv_function.
        RETURN.
      ENDIF.
      lv_active_view_id = lr_active_view->cell_id.
    ENDIF.

    DATA(lr_new_view) = REF #( mt_view_info[ function = iv_function ] ).

    DATA(lo_container) = mo_splitter->get_container( row = lr_new_view->cell_id column = 1 ).

    CASE iv_function.
      WHEN c_view-favorites.
        show_favorites( lo_container ).

      WHEN c_view-history.
        show_history( lo_container ).

      WHEN c_view-object_browser.
        show_repo_browser( lo_container ).

      WHEN OTHERS.
    ENDCASE.

    adapt_view_and_toolbar(
        iv_active_view_id = lv_active_view_id
        iv_new_view_id    = lr_new_view->cell_id
    ).
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.
    DATA(lo_active_control) = get_active_content_searcher( ).

    IF lo_active_control IS INITIAL.
      RETURN.
    ENDIF.

    lo_active_control->search( ).

  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    DATA(lo_active_control) = get_active_content_searcher( ).

    IF lo_active_control IS INITIAL.
      RETURN.
    ENDIF.

    lo_active_control->search_next( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    DATA(lo_active_control) = get_active_control( ).

    IF lo_active_control IS INITIAL.
      RETURN.
    ENDIF.

    lo_active_control->focus( ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    DATA(lo_active_control) = get_active_control( ).

    IF lo_active_control IS INITIAL.
      RETURN.
    ENDIF.

    rf_has_focus = lo_active_control->has_focus( ).
  ENDMETHOD.


  METHOD zif_uitb_view~free.
    IF mo_dock IS BOUND.
      mo_dock->free( ).
    ENDIF.

    CLEAR: mf_visible.
  ENDMETHOD.


  METHOD zif_uitb_view~hide.
    CHECK: mo_dock IS BOUND.

    mo_dock->set_visible( abap_false ).
    cl_gui_cfw=>flush( ).
    CLEAR mf_visible.

    SET HANDLER: on_goto_next_objnav_view ACTIVATION abap_false.
  ENDMETHOD.


  METHOD zif_uitb_view~is_visible.
    result = mf_visible.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    IF mo_splitter IS INITIAL.
      create_splitter( ).
      create_toolbar( ).
*.... Get global DATA for setting some initial values
      DATA(lr_global_data) = CAST zdbbr_global_data( zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main )->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).
      update_view( iv_function = SWITCH #( lr_global_data->initial_obj_nav_mode
                                           WHEN zif_dbbr_c_obj_navigator_mode=>favorites      THEN c_view-favorites
                                           WHEN zif_dbbr_c_obj_navigator_mode=>object_browser THEN c_view-object_browser
                                           WHEN zif_dbbr_c_obj_navigator_mode=>history        THEN c_view-history
                                           ELSE c_view-history
                                         ) ).
      mo_dock->set_visible( abap_true ).
    ELSE.
      mo_dock->set_visible( abap_true ).
      cl_gui_cfw=>flush( ).
    ENDIF.

    mf_visible = abap_true.

    SET HANDLER: on_goto_next_objnav_view.
  ENDMETHOD.


  METHOD zif_uitb_gui_composite_view~set_child_visibility.
*.. Currently only the object browser control requests visibility
    DATA(lr_view_info) = REF #( mt_view_info[ is_active = abap_true ] OPTIONAL ).
    IF lr_view_info->function <> zif_dbbr_c_obj_navigator_mode=>object_browser.
      update_view( c_view-object_browser ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_gui_composite_view~execute_command.
    CHECK io_command IS BOUND.

    CASE io_command->mv_function.

      WHEN c_command_id-show_object_list.
        CHECK io_command->mr_params IS BOUND.

        TRY.
            DATA(lr_entity_info) = CAST zdbbr_entity( io_command->mr_params ).
          CATCH cx_sy_move_cast_error.
            RETURN.
        ENDTRY.

        update_view( c_view-object_browser ).
        zcl_dbbr_selscr_nav_events=>raise_display_object_list(
            iv_entity_id   = lr_entity_info->entity_id
            iv_entity_type = lr_entity_info->entity_type
        ).
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
