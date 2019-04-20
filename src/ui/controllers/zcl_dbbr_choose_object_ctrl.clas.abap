"! <p class="shorttext synchronized" lang="en">Controller for choosing a table/query</p>
CLASS zcl_dbbr_choose_object_ctrl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .
    ALIASES was_not_cancelled
      FOR zif_uitb_screen_controller~was_not_cancelled.

    METHODS constructor
      IMPORTING
        if_global_fav_mode TYPE abap_bool OPTIONAL
        if_for_fav_menu    TYPE abap_bool OPTIONAL.
    METHODS get_chosen_entry
      EXPORTING
        !ev_entry TYPE tabname
        !ev_type  TYPE zdbbr_favmenu_type .
    METHODS validate_user_input
      IMPORTING
        !iv_function_code TYPE sy-ucomm .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    CONSTANTS c_tab_name_field TYPE dynfnam VALUE 'GS_ENTITY-TAB_NAME' ##NO_TEXT.
    CONSTANTS c_cds_name_field TYPE dynfnam VALUE 'GS_ENTITY-CDS_NAME' ##NO_TEXT.
    CONSTANTS c_query_name_field TYPE dynfnam VALUE 'GS_ENTITY-QUERY_NAME' ##NO_TEXT.

    DATA mf_take_data TYPE abap_bool .
    DATA mf_for_fav_menu TYPE abap_bool .
    DATA mf_global_fav_mode TYPE abap_bool .
    " ui data definition
    DATA mr_s_entity TYPE REF TO zdbbr_browser_mode_data .
ENDCLASS.



CLASS zcl_dbbr_choose_object_ctrl IMPLEMENTATION.


  METHOD constructor.
    mf_global_fav_mode = if_global_fav_mode.
    mf_for_fav_menu = if_for_fav_menu.
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).

    mr_s_entity = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_entity ) ).
  ENDMETHOD.


  METHOD get_chosen_entry.
    IF mr_s_entity->query_mode = abap_true.
      ev_type = zif_dbbr_c_favmenu_type=>query.
      ev_entry = mr_s_entity->query_name.
    ELSEIF mr_s_entity->table_mode = abap_true.
      ev_type = zif_dbbr_c_favmenu_type=>table.
      ev_entry = mr_s_entity->tab_name.
    ELSEIF mr_s_entity->cds_mode = abap_true.
      ev_type = zif_dbbr_c_favmenu_type=>cds_view.
      ev_entry = mr_s_entity->cds_name.
    ENDIF.
  ENDMETHOD.


  METHOD validate_user_input.
    " don't validate during cancel action
    IF iv_function_code = zif_dbbr_global_consts=>gc_function_codes-cancel.
      RETURN.
    ENDIF.

    IF mr_s_entity->table_mode = abap_true.
      IF mr_s_entity->tab_name = space.
        zcx_dbbr_validation_exception=>raise_with_text( 'Enter a table' ).
      ENDIF.

      zcl_dbbr_dictionary_helper=>validate_table_name(
        iv_table_name               = CONV #( mr_s_entity->tab_name )
        if_customizing_view_allowed = abap_true
      ).
    ELSEIF mr_s_entity->query_mode = abap_true. " query mode is active
      IF mr_s_entity->query_name = space.
        zcx_dbbr_validation_exception=>raise_with_text( 'Enter a query' ).
      ENDIF.

      " Validate the query name
      DATA(lr_query_f) = NEW zcl_dbbr_query_factory( ).
      DATA(ls_query) = lr_query_f->get_query(
          iv_query_name     = mr_s_entity->query_name
          if_load_completely = abap_false
      ).

      IF ls_query IS INITIAL.
        MESSAGE e035(zdbbr_info) WITH mr_s_entity->query_name INTO DATA(lv_msg).
        zcx_dbbr_validation_exception=>raise_from_sy( ).
      ELSEIF ls_query-is_global = abap_false AND
             mf_global_fav_mode = abap_true AND
             mf_for_fav_menu = abap_true.
        zcx_dbbr_validation_exception=>raise_with_text( |The Global Favorite menu allows only Global Favorites| ).
      ENDIF.
    ELSE.
      " cds view mode
      IF mr_s_entity->cds_name = space.
        zcx_dbbr_validation_exception=>raise_with_text( 'Enter a CDS View Name' ).
      ENDIF.

      IF NOT zcl_dbbr_cds_view_factory=>exists( mr_s_entity->cds_name ).
        MESSAGE e072(zdbbr_info) WITH mr_s_entity->cds_name INTO lv_msg.
        zcx_dbbr_validation_exception=>raise_from_sy( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    IF mr_s_entity->table_mode = abap_true.
      zcl_uitb_cursor=>get_cursor( )->set_field( c_tab_name_field ).
    ELSEIF mr_s_entity->query_mode = abap_true.
      zcl_uitb_cursor=>get_cursor( )->set_field( c_query_name_field ).
    ELSEIF mr_s_entity->cds_mode = abap_true.
      zcl_uitb_cursor=>get_cursor( )->set_field( c_cds_name_field ).
    ELSE.
      zcl_uitb_cursor=>get_cursor( )->set_field( c_tab_name_field ).
      mr_s_entity->table_mode = abap_true.
    ENDIF.

    DATA(lv_start_column) = 15.
    DATA(lv_start_line) = 2.

    IF if_as_dialog IS SUPPLIED AND if_as_dialog = abap_false.
      CLEAR: lv_start_column,
             lv_start_line.
    ENDIF.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_choose_entity_controller
            global_ref    = me )
        )
        iv_start_column = lv_start_column
        iv_start_line   = lv_start_line
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_choose_table_query.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.
    CONSTANTS c_s_entity_var TYPE dynfnam VALUE zif_dbbr_main_report_var_ids=>c_s_entity.
    DATA(lv_function) = cv_function_code.

    CLEAR cv_function_code.

    zcl_uitb_cursor=>get_cursor( ).

    CASE lv_function.
      WHEN zif_dbbr_global_consts=>gc_function_codes-cancel.
        zcl_dbbr_screen_helper=>leave_screen( ).

      WHEN 'MODE_CHANGE'.
        IF mr_s_entity->query_mode = abap_true.
          DATA(lv_cursor_field) = CONV dynfnam( |{ c_s_entity_var }-QUERY_NAME| ).
        ELSEIF mr_s_entity->table_mode = abap_true.
          lv_cursor_field = |{ c_s_entity_var }-TAB_NAME|.
        ELSE.
          lv_cursor_field = |{ c_s_entity_var }-CDS_NAME|.
        ENDIF.
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_field( lv_cursor_field ).

      WHEN 'TAB_MODE'.
        mr_s_entity->query_mode = abap_false.
        mr_s_entity->cds_mode = abap_false.
        mr_s_entity->table_mode = abap_true.
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_field( |{ c_s_entity_var }-TAB_NAME| ).

      WHEN 'QUERY_MOD'.
        mr_s_entity->table_mode = abap_false.
        mr_s_entity->cds_mode = abap_false.
        mr_s_entity->query_mode = abap_true.
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_field( |{ c_s_entity_var }-QUERY_NAME| ).

      WHEN 'CDS_MODE'.
        mr_s_entity->table_mode = abap_false.
        mr_s_entity->query_mode = abap_false.
        mr_s_entity->cds_mode = abap_true.
        zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_field( |{ c_s_entity_var }-CDS_NAME| ).

      WHEN 'OK'.
        TRY .
            validate_user_input( lv_function ).
            mf_take_data = abap_true.
            zcl_dbbr_screen_helper=>leave_screen( ).
          CATCH zcx_dbbr_validation_exception INTO DATA(lr_validation_exc).
            MESSAGE lr_validation_exc->zif_dbbr_exception_message~get_message( ) TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

    ENDCASE.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.
    DATA: lv_cds_name_input   TYPE screen-input,
          lv_tab_name_input   TYPE screen-input,
          lv_query_name_input TYPE screen-input.

    zif_uitb_screen_controller~set_status( ).

    IF mr_s_entity->cds_mode = abap_true.
      lv_cds_name_input = 1.
      lv_query_name_input = 0.
      lv_tab_name_input = 0.
    ELSEIF mr_s_entity->table_mode = abap_true.
      lv_cds_name_input = 0.
      lv_query_name_input = 0.
      lv_tab_name_input = 1.
    ELSEIF mr_s_entity->query_mode = abap_true.
      lv_cds_name_input = 0.
      lv_query_name_input = 1.
      lv_tab_name_input = 0.
    ENDIF.

    LOOP AT SCREEN INTO DATA(ls_screen).
      IF ls_screen-name = c_cds_name_field.
        ls_screen-input = lv_cds_name_input.
        MODIFY SCREEN FROM ls_screen.
      ELSEIF ls_screen-name = c_tab_name_field.
        ls_screen-input = lv_tab_name_input.
        MODIFY SCREEN FROM ls_screen.
      ELSEIF ls_screen-name = c_query_name_field.
        ls_screen-input = lv_query_name_input.
        MODIFY SCREEN FROM ls_screen.
      ENDIF.
    ENDLOOP.

    zcl_uitb_cursor=>refresh_cursor( ).
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    SET PF-STATUS 'CHOOSE_ENTITY' OF PROGRAM zif_dbbr_c_report_id=>main.
    SET TITLEBAR 'PROGTITLE' OF PROGRAM zif_dbbr_c_report_id=>main WITH TEXT-001.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_take_data.
  ENDMETHOD.
ENDCLASS.
