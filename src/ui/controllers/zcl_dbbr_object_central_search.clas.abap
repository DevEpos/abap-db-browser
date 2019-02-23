CLASS zcl_dbbr_object_central_search DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_uitb_gui_screen
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !if_new_transaction_mode TYPE abap_bool OPTIONAL
        !if_specific_search      TYPE abap_bool OPTIONAL
        !iv_entity_type          TYPE zdbbr_entity_type OPTIONAL
        !iv_initial_search_value TYPE string OPTIONAL .
    METHODS get_chosen_entity
      EXPORTING
        ev_entity_id   TYPE zdbbr_entity_id
        ev_entity_type TYPE zdbbr_entity_type .
    METHODS: zif_uitb_gui_command_handler~execute_command REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
  PRIVATE SECTION.

    DATA mf_specific_search TYPE abap_bool .
    TYPES: BEGIN OF ty_s_result.
    TYPES: type_icon TYPE char40.
        INCLUDE TYPE zdbbr_entity.
    TYPES: END OF ty_s_result.
    CONSTANTS: BEGIN OF c_functions,
                 adt                         TYPE ui_func VALUE 'INADT',
                 show_cds_source             TYPE ui_func VALUE 'SHOWSOURCE',
                 db_browser                  TYPE ui_func VALUE 'INDBBROWSER',
                 db_browser_content          TYPE ui_func VALUE 'CONTENT',
                 db_browser_content_new_task TYPE ui_func VALUE 'CONTENT_NEW_TASK',
                 db_browser_new_task         TYPE ui_func VALUE 'INDBBROWSERNEW',
                 settings                    TYPE ui_func VALUE 'SETTINGS',
                 show_help                   TYPE ui_func VALUE 'HELP',
               END OF c_functions.
    DATA mt_result TYPE STANDARD TABLE OF ty_s_result WITH EMPTY KEY.
    DATA mv_search_entity_type TYPE zdbbr_entity_type .
    DATA mv_chosen_entity_id TYPE zdbbr_entity_id .
    DATA mv_chosen_entity_type TYPE zdbbr_entity_type .
    DATA mv_nav_tree_visible TYPE char1 .
    DATA mf_start_transaction_mode TYPE abap_bool .
    DATA mo_input_dd TYPE REF TO cl_dd_document.
    DATA mv_current_search_type TYPE zdbbr_obj_browser_mode .
    DATA ms_settings TYPE zdbbr_entbrwsus.
    DATA: mo_search_input       TYPE REF TO cl_dd_input_element,
          mo_search_type_select TYPE REF TO cl_dd_select_element,
          mo_search_query       TYPE REF TO zcl_dbbr_object_search_query,
          mo_alv                TYPE REF TO zcl_uitb_alv,
          mr_current_selected   TYPE REF TO zcl_dbbr_object_central_search=>ty_s_result.

    METHODS show_settings .
    "! <p class="shorttext synchronized" lang="en">Create dynamic document for search input</p>
    "!
    METHODS create_dd_input
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Create ALV List output</p>
    "!
    METHODS create_alv_output
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Trigger new entity search</p>
    "!
    METHODS trigger_new_search.
    "! <p class="shorttext synchronized" lang="en">Fill alv toolbar</p>
    "!
    METHODS fill_toolbar.
    "! <p class="shorttext synchronized" lang="en">Retrieve currently selected row in ALV</p>
    "!
    METHODS get_selected_index
      RETURNING
        VALUE(rv_selected_row) TYPE int4.

    METHODS navigate_on_chosen_result
      IMPORTING
        iv_entity_id   TYPE zdbbr_entity_id
        iv_entity_type TYPE zdbbr_entity_type
        iv_action      TYPE zdbbr_entity_browser_link_mode.
    METHODS enrich_result.
    "! <p class="shorttext synchronized" lang="en">Open selected result in various ways specified by action</p>
    "!
    METHODS open_in
      IMPORTING
        iv_action TYPE zdbbr_entity_browser_link_mode OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Show content of currently seleted result entry</p>
    "!
    METHODS show_content
      IMPORTING
        if_new_window TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Handler for performing the search</p>
    "!
    METHODS on_perform_search
        FOR EVENT clicked OF cl_dd_button_element .
    "! <p class="shorttext synchronized" lang="en">Enter Handler for serach input</p>
    METHODS on_search_input_enter
        FOR EVENT entered OF cl_dd_input_element .
    "! <p class="shorttext synchronized" lang="en">Dropdown Selection changed handler</p>
    METHODS on_search_type_selected
        FOR EVENT selected OF cl_dd_select_element .
    "! <p class="shorttext synchronized" lang="en">User command handerl for ALV action</p>
    "!
    METHODS on_user_command
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          !ev_function
          !ev_tag .
    "! <p class="shorttext synchronized" lang="en">Context menu handler for ALV</p>
    "!
    METHODS on_alv_context_menu
          FOR EVENT context_menu OF zcl_uitb_alv_events
      IMPORTING
          er_menu.
    "! <p class="shorttext synchronized" lang="en">Link Click handler for ALV</p>
    "!
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          !ev_row
          !ev_column .
ENDCLASS.



CLASS zcl_dbbr_object_central_search IMPLEMENTATION.


  METHOD constructor.
    super->constructor( |{ 'DB Browser - Central Search'(001) }| ).

    mf_start_transaction_mode = if_new_transaction_mode.
    mf_specific_search = if_specific_search.
    mv_search_entity_type = iv_entity_type.
    ms_settings = zcl_dbbr_usersettings_factory=>get_entity_browser_settings( ).
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.

    CASE io_command->mv_function.

      WHEN c_functions-db_browser_content.
        IF mt_result IS NOT INITIAL.
          DATA(lv_selected_index) = get_selected_index( ).
          IF lv_selected_index IS INITIAL.
            MESSAGE |Select exactly one entity| TYPE 'S'.
          ELSE.
            mr_current_selected = REF #( mt_result[ lv_selected_index ] ).
          ENDIF.
          on_user_command( ev_function = io_command->mv_function ).
        ENDIF.

      WHEN OTHERS.
        on_user_command( ev_function = io_command->mv_function ).

    ENDCASE.
  ENDMETHOD.

  METHOD create_content.
    DATA(mo_splitter) = NEW zcl_uitb_gui_splitter_cont(
      iv_elements = 2
      iv_size     = '55:*'
      io_parent   = io_container
    ).

    mo_splitter->set_sash_properties( iv_index = 1 if_movable = abap_false if_visible = abap_false ).

    create_dd_input( EXPORTING io_container = mo_splitter->get_container( 1 ) ).
    create_alv_output( EXPORTING io_container = mo_splitter->get_container( 2 ) ).
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->deactivate_function( zif_uitb_c_gui_screen=>c_functions-save ).

    io_callback->map_fkey_functions( VALUE #(
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f7        mapped_function = c_functions-db_browser_content text = |{ 'Show Content'(012) }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f12 mapped_function = c_functions-show_help          text = |{ 'Show Help'(015) }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f9        mapped_function = c_functions-settings           text = |{ 'Customize Search Settings'(014) }| )
    ) ).
  ENDMETHOD.


  METHOD get_chosen_entity.
    ev_entity_id = mv_chosen_entity_id.
    ev_entity_type = mv_chosen_entity_type.
  ENDMETHOD.

  METHOD show_content.
    IF if_new_window = abap_true.
      CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_OBJ_BRS_EXEC_JUMP'
        EXPORTING
          iv_entity_id       = mr_current_selected->entity_id
          iv_entity_type     = mr_current_selected->entity_type
          if_skip_selscreen  = abap_true
          if_load_parameters = abap_true.

    ELSE.
      DATA(lo_variant_starter) = zcl_dbbr_variant_starter_fac=>create_variant_starter(
          iv_variant_id        = zif_dbbr_global_consts=>c_dummy_variant
          iv_entity_type       = mr_current_selected->entity_type
          iv_variant_entity_id = |{ mr_current_selected->entity_id }|
      ).

      lo_variant_starter->initialize( ).
      TRY.
          lo_variant_starter->execute_variant( ).
        CATCH zcx_dbbr_variant_error INTO DATA(lx_variant_error).
          lx_variant_error->show_message( ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD navigate_on_chosen_result.
    DATA(lv_mode) = COND zdbbr_entity_browser_link_mode( WHEN iv_action IS NOT INITIAL THEN iv_action ELSE ms_settings-link_mode ).

    IF iv_entity_type = zif_dbbr_c_entity_type=>query AND
       lv_mode = zif_dbbr_c_eb_link_mode=>open_with_adt.
      lv_mode = zif_dbbr_c_eb_link_mode=>open_in_db_browser.
    ENDIF.

    IF mf_start_transaction_mode = abap_true OR lv_mode = zif_dbbr_c_eb_link_mode=>open_with_adt.

      CASE lv_mode.

        WHEN zif_dbbr_c_eb_link_mode=>open_in_db_browser.
*........ Start Z2 Transaction with chosen entity
          CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN'
            EXPORTING
              iv_entity_id           = iv_entity_id
              iv_entity_type         = iv_entity_type
              if_from_central_search = abap_true
              if_load_parameters     = abap_true.

        WHEN zif_dbbr_c_eb_link_mode=>open_in_db_browser_new_task.
*........ Start Z2 Transaction with chosen entity in new task
          CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_SEARCH'
            EXPORTING
              iv_entity_id       = iv_entity_id
              iv_entity_type     = iv_entity_type
              if_load_parameters = abap_true.

        WHEN zif_dbbr_c_eb_link_mode=>open_with_adt.
*........ Open the chosen entity with ADT Tools
          TRY .
              zcl_dbbr_adt_util=>jump_adt(
                  iv_obj_name        = CONV #( to_upper( iv_entity_id ) )
                  iv_obj_type        = SWITCH #( iv_entity_type
                    WHEN zif_dbbr_c_entity_type=>table THEN 'TABL'
                    when zif_dbbr_c_entity_type=>view then 'VIEW'
                    WHEN zif_dbbr_c_entity_type=>cds_view THEN 'DDLS'
                  )
              ).
            CATCH zcx_dbbr_adt_error INTO DATA(lx_adt_error).
              lx_adt_error->zif_dbbr_exception_message~print( ).
          ENDTRY.
      ENDCASE.

    ELSE.
      mv_chosen_entity_id = iv_entity_id.
      mv_chosen_entity_type = iv_entity_type.

      leave_screen( ).
    ENDIF.
  ENDMETHOD.

  METHOD show_settings.
    DATA(lr_settings_view) = NEW zcl_dbbr_eb_settings_view( ).
    lr_settings_view->show( ).

    IF lr_settings_view->was_saved( ).
      ms_settings = zcl_dbbr_usersettings_factory=>get_entity_browser_settings( ).
    ENDIF.
  ENDMETHOD.


  METHOD create_dd_input.
    mo_input_dd = NEW cl_dd_document( ).

    mo_input_dd->initialize_document(  no_margins = abap_true ).

*.. Fill the document
    mo_input_dd->add_form( IMPORTING formarea = DATA(lo_form) ).
    lo_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lo_form->add_select_element(
      EXPORTING
        tooltip        = |{ 'Object Category'(002) }|
        options        = VALUE #(
          ( value = zif_dbbr_c_object_browser_mode=>cds_view            text = 'CDS View'(003) )
          ( value = zif_dbbr_c_object_browser_mode=>database_table_view text = 'Database Table/View'(004) )
          ( value = zif_dbbr_c_object_browser_mode=>query               text = 'Query'(005) )
        )
      IMPORTING
        select_element = mo_search_type_select
    ).

    CASE mv_search_entity_type.

      WHEN zif_dbbr_c_entity_type=>cds_view.
        mo_search_type_select->set_value( |{ zif_dbbr_c_object_browser_mode=>cds_view }| ).

      WHEN zif_dbbr_c_entity_type=>table.
        mo_search_type_select->set_value( |{ zif_dbbr_c_object_browser_mode=>database_table_view }| ).

      WHEN zif_dbbr_c_entity_type=>query.
        mo_search_type_select->set_value( |{ zif_dbbr_c_object_browser_mode=>query }| ).

      WHEN OTHERS.
        mo_search_type_select->set_value( |{ ms_settings-search_function }| ).
    ENDCASE.

    mv_current_search_type = mo_search_type_select->value.

    lo_form->line_with_layout( end = abap_true ).
    lo_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lo_form->add_input_element(
      EXPORTING
        size          = 100
        tooltip       = |{ 'Enter name of Entity'(006) }|
        maxlength     = 200
      IMPORTING
        input_element = mo_search_input
    ).

    lo_form->add_button(
      EXPORTING
        sap_icon = |ICON_DISPLAY|
        tooltip  = |{ 'Perform search'(007) }|
      IMPORTING
        button   = DATA(lo_search_button)
    ).

    lo_form->line_with_layout( end = abap_true ).

*.. Register event handlers for form elements
    SET HANDLER:
      on_search_input_enter FOR mo_search_input,
      on_perform_search FOR lo_search_button,
      on_search_type_selected FOR mo_search_type_select.

    mo_input_dd->merge_document( ).

    mo_input_dd->display_document( parent        = io_container
                                   reuse_control = abap_true ).

  ENDMETHOD.


  METHOD create_alv_output.
    DATA: lo_col TYPE REF TO zcl_uitb_alv_column.

    mo_alv = zcl_uitb_alv=>create_alv(
      ir_data      = REF #( mt_result )
      ir_container = io_container
      if_editable  = abap_false
    ).

    mo_alv->get_selections(  )->set_mode( value = zif_uitb_c_alv_selection=>cell ).

    SET HANDLER:
      on_user_command FOR mo_alv->get_events( ),
      on_link_click FOR mo_alv->get_events( ),
      on_alv_context_menu FOR mo_alv->get_events( ).

    TRY.
        fill_toolbar( ).

        lo_col = mo_alv->get_columns( )->get_column( 'TYPE_ICON' ).
        lo_col->set_icon( ).
        lo_col->set_descriptions( iv_long = 'Type' ).
        lo_col->set_output_length( 5 ).

        mo_alv->get_columns( )->get_column( 'ENTITY_ID' )->set_technical( ).
        mo_alv->get_columns( )->get_column( 'ENTITY_ALIAS' )->set_technical( ).
        mo_alv->get_columns( )->get_column( 'ENTITY_ALIAS_ALV' )->set_technical( ).
        mo_alv->get_columns( )->get_column( 'ENTITY_TYPE' )->set_technical( ).

        mo_alv->get_columns( )->get_column( 'ENTITY_ID_RAW' )->set_hotspot( ).

        DATA(lo_sorts) = mo_alv->get_sorting( ).
        lo_sorts->add_sort(
            iv_column_name        = 'ENTITY_ID_RAW'
            iv_sequence           = zif_uitb_c_alv_sorting=>descending
        ).

        mo_alv->display( ).
      CATCH zcx_uitb_alv_error.
    ENDTRY.
  ENDMETHOD.

  METHOD fill_toolbar.
    DATA(lo_functions) = mo_alv->get_functions( ).

    lo_functions->set_default( ).

    lo_functions->set_function( zif_uitb_c_alv_functions=>filter_menu ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>filter ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>filter_delete ).
    lo_functions->set_quickfilter( ).

    lo_functions->add_function(
        iv_name             = c_functions-settings
        iv_icon             = |{ icon_personal_settings }|
        iv_tooltip          = |{ TEXT-014 }|
    ).
    lo_functions->add_function(
        iv_name             = c_functions-show_help
        iv_icon             = |{ icon_information }|
        iv_tooltip          = |{ 'Show Help'(015) }|
    ).
  ENDMETHOD.

  METHOD on_perform_search.
    CHECK mo_search_input->value IS NOT INITIAL.

    TRY.
        mo_search_query = zcl_dbbr_object_search_query=>parse_query_string(
           iv_query       = |{ mo_search_input->value }|
           iv_search_type = mv_current_search_type
        ).
      CATCH zcx_dbbr_application_exc INTO DATA(lx_parse_error).
        MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    trigger_new_search( ).
  ENDMETHOD.


  METHOD on_search_input_enter.
    CHECK mo_search_input->value IS NOT INITIAL.

    TRY.
        mo_search_query = zcl_dbbr_object_search_query=>parse_query_string(
           iv_query       = |{ mo_search_input->value }|
           iv_search_type = mv_current_search_type
        ).
      CATCH zcx_dbbr_application_exc INTO DATA(lx_parse_error).
        MESSAGE lx_parse_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    trigger_new_search( ).
  ENDMETHOD.


  METHOD on_search_type_selected.

    mv_current_search_type = mo_search_type_select->value.
    mo_search_type_select->set_value( |{ mv_current_search_type }| ).

  ENDMETHOD.

  METHOD trigger_new_search.
    DATA: lt_search_result TYPE zdbbr_entity_t,
          lv_found_lines   TYPE sy-tabix.

    zcl_dbbr_screen_helper=>show_progress( iv_progress = 1 iv_text     = 'Searching...' ).

    DATA(ls_max_option) = mo_search_query->get_option( zif_dbbr_c_object_browser=>c_search_option-max_rows ).
*.. Automatically fill max rows option from search settings
    IF ls_max_option IS INITIAL.
      mo_search_query->set_option( VALUE #( option = zif_dbbr_c_object_browser=>c_search_option-max_rows
                                            value_range = VALUE #( ( sign = 'I' option = 'EQ' low = |{ ms_settings-max_hits }| ) )
                                          ) ).
    ENDIF.

    TRY.
        CASE mv_current_search_type.

*........ New CDS View Search
          WHEN zif_dbbr_c_object_browser_mode=>cds_view.
            lt_search_result = NEW zcl_dbbr_ob_cds_searcher( ir_query = mo_search_query )->zif_dbbr_object_searcher~search( ).

*........ New Package search
          WHEN zif_dbbr_c_object_browser_mode=>package.

*........ New Database Table/View search
          WHEN zif_dbbr_c_object_browser_mode=>database_table_view.
            lt_search_result = NEW zcl_dbbr_ob_dbtab_searcher( ir_query = mo_search_query )->zif_dbbr_object_searcher~search( ).

*........ New Query search
          WHEN zif_dbbr_c_object_browser_mode=>query.
            lt_search_result = NEW zcl_dbbr_ob_query_searcher( ir_query = mo_search_query )->zif_dbbr_object_searcher~search( ).

        ENDCASE.

        mt_result = CORRESPONDING #( lt_search_result ).
        enrich_result( ).
        lv_found_lines = lines( lt_search_result ).
        IF lv_found_lines = 0.
          MESSAGE s086(zdbbr_info).
        ELSEIF lv_found_lines = 1.
          MESSAGE s087(zdbbr_info) WITH 1.
        ELSEIF lv_found_lines > 1.
          IF lv_found_lines >= mo_search_query->mv_max_rows.
            MESSAGE s084(zdbbr_info) WITH mo_search_query->mv_max_rows.
          ELSE.
            MESSAGE s087(zdbbr_info) WITH lv_found_lines.
          ENDIF.
        ENDIF.

        mo_alv->refresh( ).
      CATCH zcx_dbbr_application_exc INTO DATA(lx_appl_error).
        lx_appl_error->zif_dbbr_exception_message~print( iv_display_type = 'E' ).
    ENDTRY.
  ENDMETHOD.

  METHOD on_alv_context_menu.
    DATA(lv_selected_index) = get_selected_index( ).
    CHECK lv_selected_index IS NOT INITIAL.

    mr_current_selected = REF #( mt_result[ lv_selected_index ] ).

    er_menu->add_separator( ).
    IF mr_current_selected->entity_type = zif_dbbr_c_entity_type=>cds_view.
      er_menu->add_function(
          fcode = zcl_dbbr_object_central_search=>c_functions-show_cds_source
          text  = |{ 'Show CDS Source'(008) }|
      ).
      er_menu->add_separator( ).
    ENDIF.

    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_functions-db_browser_content
        text  = |{ 'Show Content'(012) }|
    ).
    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_functions-db_browser_content_new_task
        text  = |{ 'Show Content (New Window)'(013) }|
    ).
    er_menu->add_separator( ).
    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_functions-adt
        text  = |{ 'Open with ADT Tools'(009) }|
    ).
    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_functions-db_browser
        text  = |{ 'Open with DB Browser'(010) }|
    ).
    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_functions-db_browser_new_task
        text  = |{ 'Open with DB Browser (New Window)'(011) }|
    ).
  ENDMETHOD.

  METHOD get_selected_index.
    DATA(lt_selected_rows) = mo_alv->get_selections( )->get_selected_rows( ).

    IF lt_selected_rows IS INITIAL.
      DATA(lt_selected_cells) = mo_alv->get_selections( )->get_selected_cells( ).

      lt_selected_rows = VALUE #(
        FOR cell IN lt_selected_cells
        ( cell-row )
      ).
      SORT lt_selected_rows.
      DELETE ADJACENT DUPLICATES FROM lt_selected_rows.
    ENDIF.

    CHECK lines( lt_selected_rows ) = 1.

    rv_selected_row = lt_selected_rows[ 1 ].
  ENDMETHOD.

  METHOD open_in.
*.. Retrieve the selected row
    DATA(lv_selected_row) = get_selected_index( ).
    CHECK lv_selected_row IS NOT INITIAL.

    DATA(lr_selected) = REF #( mt_result[ lv_selected_row ] ).

    navigate_on_chosen_result(
        iv_entity_id   = lr_selected->entity_id
        iv_entity_type = lr_selected->entity_type
        iv_action      = iv_action
    ).
  ENDMETHOD.


  METHOD on_link_click.
    open_in( ).
  ENDMETHOD.

  METHOD on_user_command.

    CASE ev_function.

      WHEN c_functions-adt.
        open_in( zif_dbbr_c_eb_link_mode=>open_with_adt ).

      WHEN c_functions-db_browser.
        open_in( zif_dbbr_c_eb_link_mode=>open_in_db_browser ).

      WHEN c_functions-db_browser_new_task.
        open_in( zif_dbbr_c_eb_link_mode=>open_in_db_browser_new_task ).

      WHEN c_functions-settings.
        show_settings( ).

      WHEN c_functions-show_help.
        zcl_dbbr_help_repository=>show_help( zcl_dbbr_help_repository=>c_help_id-object_search ).

      WHEN c_functions-show_cds_source.
        TRY.
            zcl_uitb_abap_code_viewer=>show_code(
                iv_title  = |CDS Source Code of { mr_current_selected->entity_id_raw }|
                iv_code   = zcl_dbbr_cds_view_factory=>read_ddls_source( mr_current_selected->entity_id )
            ).
          CATCH zcx_dbbr_application_exc.
            "handle exception
        ENDTRY.

      WHEN c_functions-db_browser_content.
        show_content( ).

      WHEN c_functions-db_browser_content_new_task.
        show_content( if_new_window = abap_true ).
    ENDCASE.
  ENDMETHOD.


  METHOD enrich_result.
    LOOP AT mt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
      <ls_result>-type_icon = SWITCH #(
        <ls_result>-entity_type
        WHEN zif_dbbr_c_entity_type=>cds_view THEN zif_dbbr_c_icon=>cds_view
        WHEN zif_dbbr_c_entity_type=>query THEN zif_dbbr_c_icon=>query
        WHEN zif_dbbr_c_entity_type=>table THEN zif_dbbr_c_icon=>database_table
        WHEN zif_dbbr_c_entity_type=>view THEN zif_dbbr_c_icon=>database_view
      ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
