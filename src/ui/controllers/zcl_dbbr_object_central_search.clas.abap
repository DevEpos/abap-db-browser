CLASS zcl_dbbr_object_central_search DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_open_with_func,
        adt                 TYPE ui_func VALUE 'INADT',
        db_browser          TYPE ui_func VALUE 'INDBBROWSER',
        db_browser_new_task TYPE ui_func VALUE 'INDBBROWSERNEW',
      END OF c_open_with_func.

    INTERFACES zif_uitb_view .

    METHODS constructor
      IMPORTING
        !if_new_transaction_mode TYPE abap_bool OPTIONAL
        !if_specific_search      TYPE abap_bool OPTIONAL
        !iv_entity_type          TYPE zdbbr_entity_type OPTIONAL
        !iv_initial_search_value TYPE string OPTIONAL .
    METHODS get_chosen_entity
      EXPORTING
        !ev_entity_id   TYPE zdbbr_entity_id
        !ev_entity_type TYPE zdbbr_entity_type .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mf_specific_search TYPE abap_bool .
    DATA mv_search_entity_type TYPE zdbbr_entity_type .
    DATA mv_chosen_entity_id TYPE zdbbr_entity_id .
    DATA mv_chosen_entity_type TYPE zdbbr_entity_type .
    CONSTANTS:
      BEGIN OF c_functions,
        toggle_nav_tree TYPE sy-ucomm VALUE zif_uitb_template_prog=>c_func_f7,
        show_settings   TYPE sy-ucomm VALUE zif_uitb_template_prog=>c_func_f9,
      END OF c_functions .
    CONSTANTS c_action_column TYPE tv_itmname VALUE 'ACTION' ##NO_TEXT.
    CONSTANTS c_top_node TYPE tm_nodekey VALUE 'TOP' ##NO_TEXT.
    DATA mr_view TYPE REF TO zif_uitb_template_prog .
    DATA mr_navigation_dock TYPE REF TO cl_gui_docking_container .
    DATA mr_navigation_tree TYPE REF TO zcl_uitb_column_tree_model .
    DATA mr_search_control TYPE REF TO zif_dbbr_search_control .
    DATA mv_nav_tree_visible TYPE char1 .
    CONSTANTS c_cds_search_node TYPE tm_nodekey VALUE 'CDS' ##NO_TEXT.
    CONSTANTS c_table_search_node TYPE tm_nodekey VALUE 'TABLE' ##NO_TEXT.
    CONSTANTS c_query_search_node TYPE tm_nodekey VALUE 'query' ##NO_TEXT.
    DATA mv_current_search_node TYPE tm_nodekey .
    DATA mf_start_transaction_mode TYPE abap_bool .
    DATA mv_initial_search_value TYPE string .
    DATA ms_settings TYPE zdbbr_entbrwsus.

    METHODS show_settings .
    METHODS create_search_control
      IMPORTING
        !iv_node_key TYPE tm_nodekey DEFAULT c_table_search_node .
    METHODS create_tree .
    METHODS do_on_first_call .
    METHODS free .
    METHODS on_exit
          FOR EVENT exit OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
    METHODS on_item_link_click
          FOR EVENT link_click OF zcl_uitb_ctm_events
      IMPORTING
          !ev_item_name
          !ev_node_key .
    METHODS on_pai
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          !ev_function_id
          !er_callback .
    METHODS on_pbo
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
    METHODS on_search_result_chosen
          FOR EVENT entry_chosen OF zif_dbbr_search_control
      IMPORTING
          !ev_entity_id
          !ev_entity_type
          ev_action.
    METHODS on_toolbar_function
          FOR EVENT function_selected OF zif_uitb_toolbar_events
      IMPORTING
          !ev_fcode .
    METHODS toggle_nav_tree .
ENDCLASS.



CLASS ZCL_DBBR_OBJECT_CENTRAL_SEARCH IMPLEMENTATION.


  METHOD constructor.
    mr_view = zcl_uitb_templt_prog_callback=>create_template_program( iv_title = 'DB Browser - Central Search' ).
    mf_start_transaction_mode = if_new_transaction_mode.
    mf_specific_search = if_specific_search.
    mv_search_entity_type = iv_entity_type.
    mv_initial_search_value = iv_initial_search_value.
    ms_settings = zcl_dbbr_usersettings_factory=>get_entity_browser_settings( ).

    SET HANDLER:
      on_exit FOR mr_view,
      on_pai FOR mr_view,
      on_pbo FOR mr_view.

    mr_view->add_function(
        iv_function_id = c_functions-toggle_nav_tree
        iv_text        = 'Toggle Function Tree'(004)
        iv_icon        = icon_toggle_function
    ).
    mr_view->add_function(
        iv_function_id = c_functions-show_settings
        iv_text        = 'Show Settings'(005)
        iv_icon        = icon_personal_settings
        iv_icon_text   = 'Settings'(006)
    ).
  ENDMETHOD.


  METHOD create_search_control.
    CHECK iv_node_key <> mv_current_search_node.

    DATA(lr_nodes) = mr_navigation_tree->get_nodes( ).

    IF mf_specific_search = abap_false.
      IF iv_node_key = c_table_search_node.
        lr_nodes->get_node( c_table_search_node )->set_style( zif_uitb_c_ctm_style=>light_yellow ).
        lr_nodes->get_node( c_cds_search_node )->set_style( zif_uitb_c_ctm_style=>default ).
      ELSEIF iv_node_key = c_cds_search_node.
        lr_nodes->get_node( c_cds_search_node )->set_style( zif_uitb_c_ctm_style=>light_yellow ).
        lr_nodes->get_node( c_table_search_node )->set_style( zif_uitb_c_ctm_style=>default ).
      ENDIF.
    ENDIF.

    mv_current_search_node = iv_node_key.

    DATA(lr_container) = mr_view->get_container( ).

    IF mr_search_control IS BOUND.
      mr_search_control->dispose( ).
    ENDIF.

    mr_search_control = SWITCH #(
      mv_current_search_node
      WHEN c_table_search_node THEN zcl_dbbr_db_search_control=>create(
                                      ir_parent        = lr_container
                                      iv_initial_value = mv_initial_search_value
                                    )
      WHEN c_cds_search_node THEN zcl_dbbr_cds_view_search=>create(
                                    ir_parent        = lr_container
                                    iv_initial_value = mv_initial_search_value
                                  )
    ).

*.. Register listener for chosen entity
    SET HANDLER:
      on_search_result_chosen FOR mr_search_control.

*.. set number of max hits for the search control
    mr_search_control->set_max_hits( ms_settings-max_hits ).

  ENDMETHOD.


  METHOD create_tree.
    CHECK mr_navigation_dock IS INITIAL.

    mr_navigation_dock = NEW #(
      extension = 350
      lifetime  = cl_gui_container=>lifetime_dynpro
    ).

    IF mf_specific_search = abap_true.
      mr_navigation_dock->set_visible( abap_false ).
    ENDIF.

    mr_view->add_control_to_lifecycle( mr_navigation_dock ).

*.. create the tree
    mr_navigation_tree = NEW zcl_uitb_column_tree_model(
        ir_parent           = mr_navigation_dock
        is_hierarchy_header = VALUE #(
          width  = 40
          heading = 'Functions'
        )
        if_item_selection   = abap_true
        if_with_toolbar     = abap_true
    ).

    DATA(lr_toolbar) = mr_navigation_tree->get_toolbar( ).
    lr_toolbar->add_expander_buttons( ).

    SET HANDLER:
      on_item_link_click FOR mr_navigation_tree->get_events( ),
      on_toolbar_function FOR lr_toolbar.

    TRY.
*...... Create additional columns
        mr_navigation_tree->get_columns( )->add_column(
            iv_colname          = c_action_column
            iv_width            = 10
            iv_header_text      = 'Action'
            iv_alignment        = cl_item_tree_model=>align_center
        ).
        DATA(lr_nodes) = mr_navigation_tree->get_nodes( ).

        IF mf_specific_search = abap_false OR
           ( mf_specific_search = abap_true AND mv_search_entity_type = zif_dbbr_c_entity_type=>table ).

          lr_nodes->add_node(
              iv_node_key          = |{ c_table_search_node }-TOP|
              if_folder            = abap_true
              it_item_table        = VALUE #(
                ( item_name   = mr_navigation_tree->c_hierarchy_column
                  text        = 'Find Tables'(002)
                  class       = cl_item_tree_model=>item_class_text )
              )
          ).
          lr_nodes->add_node(
              iv_node_key          = c_table_search_node
              iv_relative_node_key = |{ c_table_search_node }-TOP|
              iv_image             = |{ icon_space }|
              it_item_table        = VALUE #(
                ( item_name   = mr_navigation_tree->c_hierarchy_column
                  text        = 'Execute Function'(001)
                  class       = cl_item_tree_model=>item_class_text )
                ( item_name   = c_action_column
                  class       = cl_item_tree_model=>item_class_link
                  t_image     = icon_execute_object )
              )
          ).
        ENDIF.

        IF mf_specific_search = abap_false OR
           ( mf_specific_search = abap_true AND mv_search_entity_type = zif_dbbr_c_entity_type=>cds_view ).
          lr_nodes->add_node(
              iv_node_key          = |{ c_cds_search_node }-TOP|
              if_folder            = abap_true
              it_item_table        = VALUE #(
                ( item_name   = mr_navigation_tree->c_hierarchy_column
                  text        = 'Find CDS Views'(003)
                  class       = cl_item_tree_model=>item_class_text )
              )
          ).
          lr_nodes->add_node(
              iv_node_key          = c_cds_search_node
              iv_relative_node_key = |{ c_cds_search_node }-TOP|
              iv_image             = |{ icon_space }|
              it_item_table        = VALUE #(
                ( item_name   = mr_navigation_tree->c_hierarchy_column
                  text        = 'Execute Function'(001)
                  class       = cl_item_tree_model=>item_class_text )
                ( item_name   = c_action_column
                  class       = cl_item_tree_model=>item_class_link
                  t_image     = icon_execute_object )
              )
          ).
        ENDIF.
        mr_navigation_tree->get_nodes( )->expand_root_nodes( ).
        mr_navigation_tree->create_tree_control( ).
      CATCH zcx_uitb_tree_error.
    ENDTRY.
  ENDMETHOD.


  METHOD do_on_first_call.
    DATA: lv_function_node_key TYPE tm_nodekey VALUE c_table_search_node.

    create_tree( ).

    IF mf_specific_search = abap_true.
      CASE mv_search_entity_type.

        WHEN zif_dbbr_c_entity_type=>table.
          lv_function_node_key = c_table_search_node.

        WHEN zif_dbbr_c_entity_type=>cds_view.
          lv_function_node_key = c_cds_search_node.

        WHEN OTHERS.
      ENDCASE.
    ELSE.
      lv_function_node_key = SWITCH #( ms_settings-entry_search_function
        WHEN zif_dbbr_c_search_function=>find_tables THEN c_table_search_node
        WHEN zif_dbbr_c_search_function=>find_cds_view THEN c_cds_search_node
      ).
    ENDIF.

    create_search_control( lv_function_node_key ).
  ENDMETHOD.


  METHOD free.
  ENDMETHOD.


  METHOD get_chosen_entity.
    ev_entity_id = mv_chosen_entity_id.
    ev_entity_type = mv_chosen_entity_type.
  ENDMETHOD.


  METHOD on_exit.
  ENDMETHOD.


  METHOD on_item_link_click.
    create_search_control( ev_node_key ).
  ENDMETHOD.


  METHOD on_pai.
    CASE ev_function_id.

      WHEN c_functions-toggle_nav_tree.
        toggle_nav_tree( ).

      WHEN c_functions-show_settings.
        show_settings( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD on_pbo.
    IF er_callback->is_first_screen_call( ).
      do_on_first_call( ).
    ENDIF.
  ENDMETHOD.


  METHOD on_search_result_chosen.
    DATA(lv_mode) = COND zdbbr_entity_browser_link_mode( WHEN ev_action IS NOT INITIAL THEN ev_action ELSE ms_settings-link_mode ).

    IF mf_start_transaction_mode = abap_true or lv_mode = zif_dbbr_c_eb_link_mode=>open_with_adt.
      CASE lv_mode.

        WHEN zif_dbbr_c_eb_link_mode=>open_in_db_browser.
*........ Start Z2 Transaction with chosen entity
          CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN'
            EXPORTING
              iv_entity_id           = ev_entity_id
              iv_entity_type         = ev_entity_type
              if_from_central_search = abap_true
              if_load_parameters     = abap_true.

        WHEN zif_dbbr_c_eb_link_mode=>open_in_db_browser_new_task.
*........ Start Z2 Transaction with chosen entity in new task
          CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'ZDBBR_SEARCH'
            EXPORTING
              iv_entity_id       = ev_entity_id
              iv_entity_type     = ev_entity_type
              if_load_parameters = abap_true.

        WHEN zif_dbbr_c_eb_link_mode=>open_with_adt.
*........ Open the chosen entity with ADT Tools
          TRY .
              zcl_dbbr_adt_util=>jump_adt(
                  iv_obj_name        = CONV #( to_upper( ev_entity_id ) )
                  iv_obj_type        = SWITCH #( ev_entity_type
                    WHEN zif_dbbr_c_entity_type=>table THEN 'TABL'
                    WHEN zif_dbbr_c_entity_type=>cds_view THEN 'DDLS'
                  )
              ).
            CATCH zcx_dbbr_adt_error INTO DATA(lx_adt_error).
              lx_adt_error->zif_dbbr_exception_message~print( ).
          ENDTRY.
      ENDCASE.
    ELSE.
      mv_chosen_entity_id = ev_entity_id.
      mv_chosen_entity_type = ev_entity_type.

      mr_view->leave_program( ).
    ENDIF.
  ENDMETHOD.


  METHOD on_toolbar_function.
    CASE ev_fcode.

      WHEN zif_uitb_c_toolbar_functions=>collapse_all.
        mr_navigation_tree->get_nodes( )->collapse_all_nodes( ).

      WHEN zif_uitb_c_toolbar_functions=>expand_all.
        mr_navigation_tree->get_nodes( )->expand_root_nodes( ).
    ENDCASE.
  ENDMETHOD.


  METHOD show_settings.
    DATA(lr_settings_view) = NEW zcl_dbbr_eb_settings_view( ).
    lr_settings_view->show( ).

    IF lr_settings_view->was_saved( ).
      ms_settings = zcl_dbbr_usersettings_factory=>get_entity_browser_settings( ).
      IF mr_search_control IS BOUND.
        mr_search_control->set_max_hits( ms_settings-max_hits ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD toggle_nav_tree.
    mr_navigation_dock->get_visible( IMPORTING visible = mv_nav_tree_visible ).
    cl_gui_cfw=>flush( ).

    IF mv_nav_tree_visible = cl_gui_control=>visible_true.
      mv_nav_tree_visible = cl_gui_control=>visible_false.
    ELSE.
      mv_nav_tree_visible = cl_gui_control=>visible_true.
    ENDIF.

    mr_navigation_dock->set_visible( mv_nav_tree_visible ).
    cl_gui_cfw=>flush( ).

  ENDMETHOD.


  METHOD zif_uitb_view~is_visible.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    mr_view->show( ).
  ENDMETHOD.
ENDCLASS.
