CLASS zcl_dbbr_query_manager DEFINITION
  INHERITING FROM zcl_uitb_fullscreen_alv_table
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_dnd_flavor_query_row TYPE cndd_flavor VALUE 'query'.
    METHODS constructor
      IMPORTING
        it_created_by_so  TYPE zif_dbbr_global_types=>ty_created_by_range OPTIONAL
        it_query_name_so TYPE zif_dbbr_global_types=>ty_query_name_range OPTIONAL
        it_description_so TYPE zif_dbbr_global_types=>ty_ddtext_range OPTIONAL
        it_first_table_so TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_any_tables_so  TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_all_tables_so  TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_only_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL
        it_none_tables_so TYPE zif_dbbr_global_types=>ty_tabname16_range OPTIONAL.

  PROTECTED SECTION.
    METHODS: select_data REDEFINITION,
      get_report_id REDEFINITION,
      get_status REDEFINITION,
      get_title REDEFINITION,
      on_user_command REDEFINITION,
      enrich_data REDEFINITION,
      get_table_reference REDEFINITION,
      get_selection_mode REDEFINITION,
      do_before_alv_creation REDEFINITION,
      on_double_click REDEFINITION,
      adjust_columns REDEFINITION.
private section.

  data MR_ALV_GRID type ref to CL_GUI_ALV_GRID .
  data MT_QUERY_INFO type ZDBBR_QUERY_INFO_UI_ITAB .
  data MV_PRIMARY_TABLE type TABNAME .
  data MT_QUERY_NAME_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_query_name_range .
  data MT_CREATED_BY_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_created_by_range .
  data MT_FIRST_TABLE_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_tabname16_range .
  data MT_ANY_TABLES_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_tabname16_range .
  data MT_ALL_TABLES_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_tabname16_range .
  data MT_ONLY_TABLES_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_tabname16_range .
  data MT_NONE_TABLES_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_tabname16_range .
  data MT_DESCRIPTION_SO type ZIF_DBBR_GLOBAL_TYPES=>ty_ddtext_range .
  data MF_FAVORITE_TREE_VISIBLE type SAP_BOOL .
  constants:
    BEGIN OF mc_function_codes,
        start_query            TYPE sy-ucomm VALUE 'EXECSCRIPT',
        edit_jump_definitions   TYPE sy-ucomm VALUE 'EDITJUMPS',
        export_queries          TYPE sy-ucomm VALUE 'EXP_QUERY',
        delete_query           TYPE sy-ucomm VALUE 'DEL_QUERY',
        copy_query             TYPE sy-ucomm VALUE 'COPY',
        refresh_queries         TYPE sy-ucomm VALUE 'REFRESH',
      END OF mc_function_codes .
  constants C_GUI_STATUS type SY-PFKEY value 'QUERY_MANAGER' ##NO_TEXT.

  methods DELETE_QUERIES .
  methods EXPORT_SELECTED_QUERIES .
  methods ADJUST_FLAGS
    changing
      !CS_QUERY_INFO type ZDBBR_QUERY_INFO_UI .
  methods ADJUST_ROW_COLOR
    changing
      !CS_QUERY_INFO type ZDBBR_QUERY_INFO_UI .
  methods EDIT_JUMP_DEFINITIONS .
  methods COPY_QUERY
    importing
      !IS_QUERY type ZDBBR_QUERY_INFO_UI .
  methods START_QUERY .
  methods REFRESH_QUERIES .
ENDCLASS.



CLASS ZCL_DBBR_QUERY_MANAGER IMPLEMENTATION.


  METHOD adjust_columns.
    DATA: lr_column TYPE REF TO cl_salv_column_table.

    ir_columns->set_color_column( 'COLOR' ).

    ir_columns->set_optimize( ).
    ir_columns->get_column( 'QUERY_ID' )->set_technical( ).
    ir_columns->get_column( 'REF_JOIN_ID' )->set_technical( ).
    ir_columns->get_column( 'FORMULA' )->set_technical( ).

    " set custom column texts
    lr_column ?= ir_columns->get_column( 'PRIMARY_TABLE' ).
    lr_column->set_short_text( '1. Table' ).
    lr_column->set_medium_text( '1. Table in query' ).
    lr_column->set_long_text( lr_column->get_medium_text( ) ).

    lr_column ?= ir_columns->get_column( 'HAS_SORT_FIELDS' ).
    lr_column->set_symbol( ).
    lr_column->set_f4( abap_false ).
    lr_column->set_alignment( if_salv_c_alignment=>centered ).
    lr_column->set_long_text( 'Defined Sorting Fields' ).

    lr_column ?= ir_columns->get_column( 'HAS_JUMP_FIELDS' ).
    lr_column->set_symbol( ).
    lr_column->set_f4( abap_false ).
    lr_column->set_alignment( if_salv_c_alignment=>centered ).

    lr_column ?= ir_columns->get_column( 'HAS_OUTPUT_FIELDS' ).
    lr_column->set_symbol( ).
    lr_column->set_f4( abap_false ).
    lr_column->set_alignment( if_salv_c_alignment=>centered ).
    lr_column->set_long_text( 'Defined Output Fields' ).

    lr_column ?= ir_columns->get_column( 'HAS_FILTER_VALUES' ).
    lr_column->set_symbol( ).
    lr_column->set_f4( abap_false ).
    lr_column->set_alignment( if_salv_c_alignment=>centered ).
    lr_column->set_long_text( 'Defined Selection Criteria' ).

    lr_column ?= ir_columns->get_column( 'IS_GLOBAL' ).
    lr_column->set_symbol( ).
    lr_column->set_f4( abap_false ).
    lr_column->set_alignment( if_salv_c_alignment=>centered ).

    lr_column ?= ir_columns->get_column( 'QUERY_NAME' ).
  ENDMETHOD.


  METHOD adjust_flags.
    IF cs_query_info-has_output_fields = abap_true.
      cs_query_info-has_output_fields = sym_filled_circle.
    ELSE.
      CLEAR: cs_query_info-has_output_fields.
    ENDIF.

    IF cs_query_info-has_jump_fields = abap_true.
      cs_query_info-has_jump_fields = sym_filled_circle.
    ELSE.
      CLEAR: cs_query_info-has_jump_fields.
    ENDIF.

    IF cs_query_info-has_sort_fields = abap_true.
      cs_query_info-has_sort_fields = sym_filled_circle.
    ELSE.
      CLEAR: cs_query_info-has_sort_fields.
    ENDIF.

    IF cs_query_info-has_filter_values = abap_true.
      cs_query_info-has_filter_values = sym_filled_circle.
    ELSE.
      CLEAR: cs_query_info-has_sort_fields.
    ENDIF.

    IF cs_query_info-is_global = abap_true.
      cs_query_info-is_global = sym_filled_circle.
    ELSE.
      CLEAR cs_query_info-is_global.
    ENDIF.
  ENDMETHOD.


  METHOD adjust_row_color.
    IF matches( val = cs_query_info-query_name regex = '\$.*' ) .
      cs_query_info-color = VALUE lvc_t_scol(
        ( color = VALUE #( col = 3 int = 1 ) )
      ).
    ELSEIF cs_query_info-created_by <> sy-uname.
      cs_query_info-color = VALUE lvc_t_scol(
        ( color = VALUE #( col = 7 int = 1 ) )
      ).
    ELSE.
      cs_query_info-color = VALUE lvc_t_scol(
        ( color = VALUE #( col = 1 int = 1 ) )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mt_query_name_so = it_query_name_so.
    mt_created_by_so  = it_created_by_so.
    mt_first_table_so = it_first_table_so.
    mt_any_tables_so  = it_any_tables_so.
    mt_all_tables_so  = it_all_tables_so.
    mt_only_tables_so = it_only_tables_so.
    mt_none_tables_so = it_none_tables_so.
    mt_description_so = it_description_so.
  ENDMETHOD.


  METHOD copy_query.
    DATA: ls_new_query TYPE zdbbr_query_info_ui.

    DATA(ls_query) = is_query.

    ls_query-is_global = COND #( WHEN is_query-is_global = sym_filled_circle THEN abap_true ).

    DATA(lr_copy_query_controller) = NEW zcl_dbbr_copy_query_ctrl( ls_query ).

    lr_copy_query_controller->zif_uitb_screen_controller~call_screen( ).

    IF lr_copy_query_controller->zif_uitb_screen_controller~was_not_cancelled( ).
      ls_new_query = lr_copy_query_controller->get_new_query( ).
    ENDIF.

    IF ls_new_query IS INITIAL.
      RETURN.
    ENDIF.

    adjust_flags( CHANGING cs_query_info = ls_new_query ).
    adjust_row_color( CHANGING cs_query_info = ls_new_query ).

    " add new query to alv list
    DATA(lv_index) = line_index( mt_query_info[ table_line = is_query ] ) + 1.

    IF lv_index > lines( mt_query_info ).
      APPEND ls_new_query TO mt_query_info.
    ELSE.
      INSERT ls_new_query INTO mt_query_info INDEX lv_index.
    ENDIF.

    refresh( ).
  ENDMETHOD.


  METHOD delete_queries.
    DATA: lt_query_ids        TYPE RANGE OF zdbbr_query_id,
          lt_query_name_range TYPE RANGE OF zdbbr_query_name.

    DATA: lv_answer TYPE char1.

    FIELD-SYMBOLS: <ls_selected_line> TYPE zdbbr_query_info_ui.

    DATA(lt_selected_rows) = get_selected_rows( ).

    " collect query information
    LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<lv_row_index>).
      DATA(lr_query_info) = REF #( mt_query_info[ <lv_row_index> ] ).

      " check if query can be deleted
      IF lr_query_info->is_global = abap_false AND
         lr_query_info->created_by <> sy-uname.
        MESSAGE s011(zdbbr_exception) WITH lr_query_info->query_name DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      lt_query_ids = VALUE #( BASE lt_query_ids ( sign = 'I' option = 'EQ' low = lr_query_info->query_id ) ).
      lt_query_name_range = VALUE #( BASE lt_query_name_range ( sign = 'I' option = 'EQ' low = lr_query_info->query_name ) ).
    ENDLOOP.

    CHECK zcl_dbbr_appl_util=>popup_to_confirm(
        iv_title     = 'Delete?'
        iv_query     = |Do you really want to delete { lines( lt_selected_rows ) } querys?|
        iv_icon_type =  'ICON_MESSAGE_INFO'
    ) = 1.

    LOOP AT lt_query_ids ASSIGNING FIELD-SYMBOL(<ls_query>).
      NEW zcl_dbbr_query_factory( )->delete_query_by_id( <ls_query>-low ).
    ENDLOOP.

*... delete favorites for deleted querys
    NEW zcl_dbbr_favmenu_factory( )->delete_favs_for_query_names( CORRESPONDING #( lt_query_name_range ) ).

    DELETE mt_query_info WHERE query_id IN lt_query_ids.

    refresh( if_update_title = abap_true ).
    MESSAGE s056(zdbbr_info) WITH |{ lines( lt_query_ids ) }|.

  ENDMETHOD.


  METHOD do_before_alv_creation.
    register_double_click( ).
  ENDMETHOD.


  METHOD edit_jump_definitions.
    DATA: lf_jump_fields_defined TYPE boolean.

    DATA(lv_selected_query_index) = get_selected_row( ).

    IF lv_selected_query_index < 1.
      RETURN.
    ENDIF.

    ASSIGN mt_query_info[ lv_selected_query_index ] TO FIELD-SYMBOL(<ls_query_info>).


    DATA(lr_jumplist_table) = NEW zcl_dbbr_jumplist_table( ir_tabfield_list = NEW #( ) ).
    DATA(lr_jumplist_controller) = NEW zcl_dbbr_jumplist_controller(
      iv_query_id = <ls_query_info>-query_id
      ir_table     = lr_jumplist_table
    ).

    lr_jumplist_controller->zif_uitb_screen_controller~call_screen( ).

    " update jump field definition flag in query
    IF lr_jumplist_controller->has_jump_destinations( ) <> <ls_query_info>-has_jump_fields.
      DATA(lr_query_f) = NEW zcl_dbbr_query_factory( ).
      lr_query_f->update_query_flags(
        iv_query_id         = <ls_query_info>-query_id
        if_has_jump_fields   = lf_jump_fields_defined
      ).
    ENDIF.

    <ls_query_info>-has_jump_fields = COND #( WHEN lf_jump_fields_defined = abap_true THEN sym_filled_circle ELSE '' ).

    refresh( ).
  ENDMETHOD.


  METHOD enrich_data.
    LOOP AT mt_query_info ASSIGNING FIELD-SYMBOL(<ls_query_info>).
      adjust_flags( CHANGING cs_query_info = <ls_query_info> ).

      adjust_row_color( CHANGING cs_query_info = <ls_query_info> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD export_selected_queries.
    TRY .
        DATA(lt_selected_rows) = get_selected_rows( ).
      CATCH zcx_dbbr_exception INTO DATA(lr_exc).
        lr_exc->show_message( iv_message_type = 'S' ).
        RETURN.
    ENDTRY.

    " get value of selected line
    DATA(lt_selected_queries) = VALUE zdbbr_query_info_ui_itab(
      FOR row IN lt_selected_rows
      ( mt_query_info[ row ] )
    ).

    " export the selected querys to file
    NEW zcl_dbbr_query_exporter( lt_selected_queries )->export_data( ).
  ENDMETHOD.


  METHOD get_report_id.
    rv_repid = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD get_selection_mode.
    rv_sel_mode = if_salv_c_selection_mode=>row_column.
  ENDMETHOD.


  METHOD get_status.
    rv_status = c_gui_status.
  ENDMETHOD.


  METHOD get_table_reference.
    rr_table_ref = REF #( mt_query_info ).
  ENDMETHOD.


  METHOD get_title.
    rv_title = |DB Browser - query Catalog - { lines( mt_query_info ) } querys selected|.
  ENDMETHOD.


  METHOD on_double_click.
    start_query( ).
  ENDMETHOD.


  METHOD on_user_command.

    IF e_salv_function = mc_function_codes-start_query OR
       e_salv_function = mc_function_codes-edit_jump_definitions OR
       e_salv_function = mc_function_codes-copy_query.
      " get selected line
      DATA(lv_selected_line) = get_selected_row( ).
      IF lv_selected_line = 0.
        RETURN.
      ENDIF.

      ASSIGN mt_query_info[ lv_selected_line ] TO FIELD-SYMBOL(<ls_selected_line>).

      IF e_salv_function <> mc_function_codes-start_query AND
         e_salv_function <> mc_function_codes-copy_query.
        IF <ls_selected_line>-created_by <> sy-uname.
          MESSAGE s011(zdbbr_exception) WITH <ls_selected_line>-query_name DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    CASE e_salv_function.

      WHEN mc_function_codes-refresh_queries.
        refresh_queries( ).
        refresh( ).

      WHEN mc_function_codes-start_query.
        start_query( ).

      WHEN mc_function_codes-edit_jump_definitions.
        edit_jump_definitions( ).

      WHEN mc_function_codes-export_queries.
        export_selected_queries( ).

      WHEN mc_function_codes-copy_query.
        copy_query( is_query = <ls_selected_line> ).

      WHEN mc_function_codes-delete_query.
        delete_queries( ).

    ENDCASE.
  ENDMETHOD.


  METHOD refresh_queries.
    refresh( if_refresh_from_db = abap_true ).

    IF mt_query_info IS INITIAL.
      MESSAGE s057(zdbbr_info).
    ENDIF.

  ENDMETHOD.


  METHOD select_data.
    FIELD-SYMBOLS: <ls_table_selopt> LIKE LINE OF mt_all_tables_so.

    SELECT * FROM zdbbr_queryh INTO CORRESPONDING FIELDS OF TABLE mt_query_info
      WHERE query_name IN mt_query_name_so
        AND created_by IN mt_created_by_so
        AND primary_table IN mt_first_table_so
        AND description IN mt_description_so.

    SELECT * FROM zdbbr_queryt INTO TABLE @DATA(lt_query_tables)
    FOR ALL ENTRIES IN @mt_query_info
      WHERE ref_query_id = @mt_query_info-query_id.

    LOOP AT lt_query_tables ASSIGNING FIELD-SYMBOL(<ls_query_table>)
      GROUP BY ( query_id = <ls_query_table>-ref_query_id )
      ASSIGNING FIELD-SYMBOL(<ls_query_table_group>).


      DATA(lf_one) = abap_false.
      DATA(lf_all) = abap_true.
      DATA(lf_only) = abap_false.
      DATA(lf_none) = abap_true.
      DATA(lf_query_ok) = abap_true.

      LOOP AT GROUP <ls_query_table_group> ASSIGNING FIELD-SYMBOL(<ls_table>).
        IF mt_any_tables_so IS NOT INITIAL AND <ls_table>-tabname IN mt_any_tables_so.
          lf_one = abap_true.
          EXIT.
        ENDIF.

        IF mt_all_tables_so IS NOT INITIAL AND <ls_table>-tabname NOT IN mt_all_tables_so.
          lf_all = abap_false.
          EXIT.
        ENDIF.

        IF mt_only_tables_so IS NOT INITIAL AND <ls_table>-tabname NOT IN mt_only_tables_so.
          lf_only = abap_false.
          EXIT.
        ENDIF.

        IF mt_none_tables_so IS NOT INITIAL AND <ls_table>-tabname IN mt_none_tables_so.
          lf_none = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF mt_any_tables_so IS NOT INITIAL AND lf_one = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF mt_all_tables_so IS NOT INITIAL AND lf_all = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF mt_only_tables_so IS NOT INITIAL AND lf_only = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF mt_none_tables_so IS NOT INITIAL AND lf_none = abap_false.
        lf_query_ok = abap_false.
      ENDIF.

      IF lf_query_ok = abap_false.
        DELETE mt_query_info WHERE query_id = <ls_query_table_group>-query_id.
      ENDIF.

    ENDLOOP.

    IF mt_query_info IS INITIAL.
      RAISE EXCEPTION TYPE zcx_uitb_alv_no_data.
    ENDIF.

    SORT mt_query_info BY primary_table created_by.
  ENDMETHOD.


  METHOD start_query.
    DATA(lv_selected_query_index) = get_selected_row( ).

    IF lv_selected_query_index < 1.
      RETURN.
    ENDIF.

    ASSIGN mt_query_info[ lv_selected_query_index ] TO FIELD-SYMBOL(<ls_selected_query>).

    CALL FUNCTION 'ZDBBR_SHOW_SELSCREEN' STARTING NEW TASK 'query_CATALOG'
      EXPORTING
        iv_entity_id       = <ls_selected_query>-query_name
        iv_entity_type     = zif_dbbr_c_entity_type=>query
        if_load_parameters = abap_true.
  ENDMETHOD.
ENDCLASS.
