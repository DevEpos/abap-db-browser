"! <p class="shorttext synchronized" lang="en">Mangaer for Object Browser search favorites</p>
CLASS zcl_dbbr_ob_fav_manager DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_modal_dialog
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        edit_name  TYPE ui_func VALUE 'EDIT_NAME',
        move_up    TYPE ui_func VALUE 'MOVE_UP',
        move_down  TYPE ui_func VALUE 'MOVE_DOWN',
        delete     TYPE ui_func VALUE 'DELETE',
        focus_tree TYPE ui_func VALUE 'FOCUS_ON_TREE',
      END OF c_functions.

    CONSTANTS:
      BEGIN OF c_cols,
        description TYPE tv_itmname VALUE 'DESCR',
      END OF c_cols.

    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model.

    "! <p class="shorttext synchronized" lang="en">Creates favorite tree</p>
    METHODS create_tree
      IMPORTING
        io_parent TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Change name of favorite</p>
    METHODS edit_favorite_name.
    "! <p class="shorttext synchronized" lang="en">Delete favorite</p>
    METHODS delete_favorite.

    "! <p class="shorttext synchronized" lang="en">Event handler for node double click</p>
    METHODS on_node_double_click
          FOR EVENT node_double_click OF zcl_uitb_ctm_events
      IMPORTING
          ev_node_key.
ENDCLASS.



CLASS zcl_dbbr_ob_fav_manager IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_title = |{ 'Object Browser - Favorites' }| ).
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.

    CASE io_command->mv_function.

      WHEN c_functions-delete.
        delete_favorite( ).

      WHEN c_functions-focus_tree.
        mo_tree->zif_uitb_gui_control~focus( ).

      WHEN c_functions-move_up.

      WHEN c_functions-move_down.

      WHEN c_functions-edit_name.
        edit_favorite_name( ).
    ENDCASE.
  ENDMETHOD.

  METHOD create_content.
    create_control_toolbar(
      EXPORTING
        io_parent    = io_container
        it_button    = VALUE #(
          ( function = c_functions-edit_name
            icon     = icon_change
            text     = |{ 'Change'(007) }| )
          ( function = c_functions-delete
            icon     = icon_delete
            quickinfo = |{ text-006 }| )
        )
      IMPORTING
        eo_toolbar   = DATA(lo_toolbar)
        eo_client    = DATA(lo_container)
    ).

    create_tree( EXPORTING io_parent = lo_container ).
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->map_fkey_functions( VALUE #(
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f1 mapped_function = c_functions-focus_tree text = |{ 'Set focus to Tree'(005) }| )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f2 mapped_function = c_functions-delete text = |{ 'Delete Favorite'(006) }| )
    ) ).
  ENDMETHOD.

  METHOD create_tree.
    TYPES: BEGIN OF lty_s_fav_groups,
             name TYPE string,
             type TYPE zdbbr_obj_browser_mode,
           END OF lty_s_fav_groups.
    DATA: lt_fav_groups TYPE STANDARD TABLE OF lty_s_fav_groups.

    mo_tree = NEW zcl_uitb_column_tree_model(
      ir_parent           = io_parent
      is_hierarchy_header = VALUE #(
         heading = |{ 'Object Type/Query'(003) }|
         width   = 100
      )
      if_auto_node_key    = abap_true
    ).

    mo_tree->get_columns( )->add_column(
        iv_colname        = c_cols-description
        iv_width          = 50
        iv_header_text    = |{ 'Description'(004) }|
    ).

    set handler:
       on_node_double_click for mo_tree->get_events( ).

    mo_tree->create_tree_control( ).

*.. Fill tree with favorites
    DATA(lt_favorites) = zcl_dbbr_ob_fav_factory=>get_favorites( ).

    lt_fav_groups = VALUE #(
      ( type = zif_dbbr_c_object_browser_mode=>cds_view            name = 'CDS view' )
      ( type = zif_dbbr_c_object_browser_mode=>database_table_view name = 'Database Table/View' )
      ( type = zif_dbbr_c_object_browser_mode=>query               name = 'Query' )
      ( type = zif_dbbr_c_object_browser_mode=>package             name = 'Package' )
    ).

    DATA(lo_nodes) = mo_tree->get_nodes( ).

    LOOP AT lt_fav_groups ASSIGNING FIELD-SYMBOL(<ls_fav_group>).
      DATA(lo_type_node) = lo_nodes->add_node(
          if_folder            = abap_true
          if_use_prop_font     = abap_true
          it_item_table        = VALUE #(
            ( item_name = mo_tree->c_hierarchy_column
              text      = <ls_fav_group>-name )
          )
      ).

      LOOP AT lt_favorites ASSIGNING FIELD-SYMBOL(<ls_fav>) WHERE entity_type = <ls_fav_group>-type.
        lo_nodes->add_node(
            iv_relative_node_key = lo_type_node->mv_node_key
            iv_image             = zif_dbbr_c_icon=>no_icon
            if_use_prop_font     = abap_true
            ir_user_data         = NEW zdbbr_objbrsfav( <ls_fav> )
            it_item_table        = VALUE #(
              ( item_name = mo_tree->c_hierarchy_column
                text      = <ls_fav>-search_string )
              ( item_name = c_cols-description
                text      = <ls_fav>-favorite_name )
            )
        ).
      ENDLOOP.
    ENDLOOP.

    lo_nodes->expand_root_nodes( ).

  ENDMETHOD.

  METHOD edit_favorite_name.
    DATA: lf_cancelled TYPE abap_bool.

    DATA(lo_selected_node) = mo_tree->get_selections( )->get_selected_node( ).
    CHECK lo_selected_node IS BOUND.

    DATA(lr_user_data) = CAST zdbbr_objbrsfav( lo_selected_node->get_user_data( ) ).
    CHECK lr_user_data IS BOUND.

    DATA(lv_result) = zcl_dbbr_appl_util=>popup_get_value(
      EXPORTING
        is_field = VALUE #( tabname = 'ZDBBR_OBJBRSFAV' fieldname = 'FAVORITE_NAME' field_obl = abap_true value = lr_user_data->favorite_name )
        iv_title = |{ 'Enter new name for the favorite'(001) }|
      IMPORTING
        ef_cancelled = lf_cancelled
    ).

    CHECK: lv_result <> space,
           lf_cancelled = abap_false,
           lv_result <> lr_user_data->favorite_name.

    CHECK zcl_dbbr_ob_fav_factory=>change_favorite_name(
      iv_id   = lr_user_data->id
      iv_name = CONV #( lv_result )
    ).

    lo_selected_node->get_item( c_cols-description )->set_text( |{ lv_result }| ).
    lr_user_data->favorite_name = lv_result.

    MESSAGE s091(zdbbr_info).
  ENDMETHOD.


  METHOD delete_favorite.
    DATA(lo_selected_node) = mo_tree->get_selections( )->get_selected_node( ).
    CHECK lo_selected_node IS BOUND.

    DATA(lr_user_data) = CAST zdbbr_objbrsfav( lo_selected_node->get_user_data( ) ).
    CHECK lr_user_data IS BOUND.

    IF zcl_dbbr_appl_util=>popup_to_confirm(
            iv_title  = 'Delete?'
            iv_query  = |{ 'Are you sure you want to delete the selected Favorite'(002) } | &&
                        |'{ lr_user_data->favorite_name }'|
            iv_icon_type             = 'ICON_MESSAGE_WARNING' ) = '1'.
      CHECK zcl_dbbr_ob_fav_factory=>delete_favorite( lr_user_data->id ).
      MESSAGE s092(zdbbr_info) WITH lr_user_data->favorite_name.

      mo_tree->get_nodes( )->delete_node( lo_selected_node->mv_node_key ).
    ENDIF.
  ENDMETHOD.

  METHOD on_node_double_click.
    check mo_tree->get_nodes( )->node_has_user_data( ev_node_key ).

    edit_favorite_name( ).
  ENDMETHOD.

ENDCLASS.
