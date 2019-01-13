CLASS zcl_dbbr_db_search_control DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_search_control .
    INTERFACES zif_uitb_disposable .

    CLASS-METHODS create
      IMPORTING
        !ir_parent        TYPE REF TO cl_gui_container
        !iv_initial_value TYPE string OPTIONAL
      RETURNING
        VALUE(result)     TYPE REF TO zcl_dbbr_db_search_control .
  PROTECTED SECTION.

    METHODS constructor
      IMPORTING
        !ir_parent TYPE REF TO cl_gui_container .
  PRIVATE SECTION.

    ALIASES table_selected
      FOR zif_dbbr_search_control~entry_chosen .

    DATA mt_table_alias_map TYPE zdbbr_table_to_alias_map_itab .
    CONSTANTS c_all_lang_option TYPE ui_func VALUE 'ALL_LANG' ##NO_TEXT.
    CONSTANTS c_exact_search_option TYPE ui_func VALUE 'EXACT_SEARCH' ##NO_TEXT.
    CONSTANTS c_search_left_aligned_option TYPE ui_func VALUE 'SEARCH_LEFT_ALIGNED' ##NO_TEXT.
    CONSTANTS c_search_text_option TYPE ui_func VALUE 'SEARCH_TEXT_AREA' ##NO_TEXT.
    CONSTANTS c_search_fld_name_option TYPE ui_func VALUE 'SEARCH_FLD_NAME' ##NO_TEXT.
    CONSTANTS c_show_where_condition TYPE ui_func VALUE 'WHERE' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_aliases,
        base   TYPE zdbbr_table_alias VALUE 'A',
        fields TYPE zdbbr_table_alias VALUE 'B',
        texts  TYPE zdbbr_table_alias VALUE 'C',
      END OF c_aliases .
    CONSTANTS:
      BEGIN OF c_tables,
        base   TYPE tabname VALUE 'ZDBBRDBSEARCH_V',
        fields TYPE tabname VALUE 'DDFTX',
        texts  TYPE tabname VALUE 'DD02T',
      END OF c_tables .
    CONSTANTS:
      BEGIN OF c_base_fields,
        tabname       TYPE fieldname VALUE 'TABNAME',
        devclass      TYPE fieldname VALUE 'DEVCLASS',
        app_component TYPE fieldname VALUE 'APP_COMPONENT',
      END OF c_base_fields .
    CONSTANTS:
      BEGIN OF c_fields_fields,
        tabname    TYPE fieldname VALUE 'TABNAME',
        fieldname  TYPE fieldname VALUE 'FIELDNAME',
        ddlanguage TYPE fieldname VALUE 'DDLANGUAGE',
        position   TYPE fieldname VALUE 'POSITION',
        domname    TYPE fieldname VALUE 'DOMNAME',
        rollname   TYPE fieldname VALUE 'ROLLNAME',
        scrlen1    TYPE fieldname VALUE 'SCRLEN1',
        scrlen2    TYPE fieldname VALUE 'SCRLEN2',
        scrlen3    TYPE fieldname VALUE 'SCRLEN3',
        headlen    TYPE fieldname VALUE 'HEADLEN',
        scrtext_s  TYPE fieldname VALUE 'SCRTEXT_S',
        scrtext_m  TYPE fieldname VALUE 'SCRTEXT_M',
        scrtext_l  TYPE fieldname VALUE 'SCRTEXT_L',
        reptext    TYPE fieldname VALUE 'REPTEXT',
        fieldtext  TYPE fieldname VALUE 'FIELDTEXT',
      END OF c_fields_fields .
    CONSTANTS:
      BEGIN OF c_text_fields,
        tabname  TYPE fieldname VALUE 'TABNAME',
        language TYPE fieldname VALUE 'DDLANGUAGE',
        text     TYPE fieldname VALUE 'DDTEXT',
      END OF c_text_fields .
    DATA mr_container TYPE REF TO cl_gui_container .
    DATA mr_splitter TYPE REF TO cl_gui_splitter_container .
    DATA mr_search_input TYPE REF TO cl_dd_input_element .
    DATA mr_input_dd TYPE REF TO cl_dd_document .
    DATA mr_alv TYPE REF TO zcl_uitb_alv .
    DATA mt_result TYPE zdbbr_table_search_t .
    DATA:
      BEGIN OF ms_search_options,
        all_lang            TYPE abap_bool,
        exact_search        TYPE abap_bool,
        search_left_aligned TYPE abap_bool,
        search_text_area    TYPE abap_bool VALUE abap_true,
        search_fld_name     TYPE abap_bool,
      END OF ms_search_options .
    DATA mv_search TYPE string .
    DATA ms_join_def TYPE zdbbr_join_def .
    DATA mt_from TYPE zdbbr_string_t .
    DATA mt_select TYPE zdbbr_string_t .
    DATA mt_where TYPE zdbbr_string_t .
    DATA mt_order_by TYPE zdbbr_string_t .
    DATA mv_max_hits TYPE int2 VALUE 500.

    METHODS create_alv .
    METHODS create_input
      IMPORTING
        !iv_initial_value TYPE string OPTIONAL .
    METHODS create_splitter .
    METHODS fill_table_input
      IMPORTING
        !iv_input           TYPE tabname
      CHANGING
        !cv_input_fieldtext TYPE as4text
        !cv_input_fieldname TYPE fieldname
        !cv_input_tabname   TYPE tabname
        !cv_input_domname   TYPE domname
        !cv_input_rollname  TYPE rollname
        !cv_input_scrtext_s TYPE scrtext_s
        !cv_input_scrtext_m TYPE scrtext_m
        !cv_input_scrtext_l TYPE scrtext_l
        !cv_input_reptext   TYPE reptext .
    METHODS fill_table_seltab
      IMPORTING
        !iv_field     TYPE fieldname
        !iv_sign      TYPE ddsign DEFAULT 'I'
        !iv_option    TYPE ddoption DEFAULT 'CP'
        !iv_input     TYPE clike
      CHANGING
        !ct_or_seltab TYPE zdbbr_or_seltab_sql_t .
    METHODS fill_toolbar .
    METHODS fill_where_clause .
    METHODS fill_select_clause .
    METHODS fill_from_clause .
    METHODS on_search_input_enter
         FOR EVENT entered OF cl_dd_input_element .
    METHODS on_search_press
         FOR EVENT clicked OF cl_dd_button_element .
    METHODS on_user_command
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          !ev_function
          !ev_tag .
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          !ev_row
          !ev_column .
    METHODS on_alv_context_menu
          FOR EVENT context_menu OF zcl_uitb_alv_events
      IMPORTING
          er_menu.
    METHODS search
      IMPORTING
        !iv_search TYPE string OPTIONAL .
    METHODS search_db_tables .
    METHODS show .
    METHODS open_table_in
      IMPORTING
        iv_open_mode TYPE zdbbr_entity_browser_link_mode.
ENDCLASS.



CLASS ZCL_DBBR_DB_SEARCH_CONTROL IMPLEMENTATION.


  METHOD constructor.
    mr_container = ir_parent.
    mt_table_alias_map = VALUE #(
     ( alias = c_aliases-base   tabname = c_tables-base   )
     ( alias = c_aliases-fields tabname = c_tables-fields )
     ( alias = c_aliases-texts  tabname = c_tables-texts )
    ).
    fill_select_clause( ).
    mt_order_by = VALUE #(
      ( |{ c_aliases-base }~{ c_base_fields-tabname } ASCENDING| )
    ).
  ENDMETHOD.


  METHOD create.
    DATA(lr_control) = NEW zcl_dbbr_db_search_control( ir_parent ).
    lr_control->create_splitter( ).
    lr_control->create_input( iv_initial_value ).
    lr_control->create_alv( ).
    lr_control->show( ).

    result = lr_control.
  ENDMETHOD.


  METHOD create_alv.
    mr_alv = zcl_uitb_alv=>create_alv(
         ir_data      = REF #( mt_result )
         ir_container = mr_splitter->get_container( column = 1 row = 2 )
         if_editable  = abap_false
    ).

    SET HANDLER:
      on_user_command FOR mr_alv->get_events( ),
      on_link_click FOR mr_alv->get_events( ),
      on_alv_context_menu for mr_alv->get_events( ).

    mr_alv->get_columns( )->get_column( 'TABNAME' )->set_hotspot( ).

    fill_toolbar( ).

    DATA(lr_sorts) = mr_alv->get_sorting( ).
    lr_sorts->add_sort(
        iv_column_name        = 'TABNAME'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
    lr_sorts->add_sort(
        iv_column_name        = 'DEVCLASS'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
    lr_sorts->add_sort(
        iv_column_name        = 'APPLICATION_COMPONENT'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
    lr_sorts->add_sort(
        iv_column_name        = 'DDLANGUAGE'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
  ENDMETHOD.


  METHOD create_input.
    mr_input_dd = NEW cl_dd_document( ).

    mr_input_dd->initialize_document( background_color = cl_dd_area=>col_background_level1 ).

*.. Fill the document
    mr_input_dd->add_form( IMPORTING formarea = DATA(lr_form) ).
    lr_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lr_form->add_input_element(
      EXPORTING
        size          = 60    " Length of Input Field
        tooltip       = |{ 'Enter your Search here'(012) }|
        maxlength     = 100    " Maximum Permitted Text Entry Length
      IMPORTING
        input_element = mr_search_input
    ).

    mr_search_input->set_value( |{ iv_initial_value }| ).

    lr_form->add_button(
      EXPORTING
        sap_icon = |ICON_SEARCH|
        tooltip  = |{ 'Execute Search'(013) }|
      IMPORTING
        button   = DATA(lr_search_button)
    ).

    lr_form->line_with_layout( end = abap_true ).

*.. Register event handlers for form elements
    SET HANDLER:
      on_search_input_enter FOR mr_search_input,
      on_search_press FOR lr_search_button.

    mr_input_dd->merge_document( ).

    DATA(lr_top_container) = mr_splitter->get_container( row = 1 column = 1 ).
    mr_input_dd->display_document( parent = lr_top_container reuse_control = abap_true ).

  ENDMETHOD.


  METHOD create_splitter.
    mr_splitter = NEW cl_gui_splitter_container(
        parent      = mr_container
        rows        = 2
        columns     = 1
    ).
    mr_splitter->set_row_sash(
      id    = 1
      type  = cl_gui_splitter_container=>type_movable
      value = cl_gui_splitter_container=>false
    ).
    mr_splitter->set_row_sash(
      id    = 1
      type  = cl_gui_splitter_container=>type_sashvisible
      value = cl_gui_splitter_container=>false
    ).

    mr_splitter->set_row_mode( cl_gui_splitter_container=>mode_absolute ).
    mr_splitter->set_row_height( id = 1 height = 40 ).
  ENDMETHOD.


  METHOD fill_from_clause.
    DATA: lt_tables            TYPE zdbbr_join_table_ui_itab,
          lt_text_filter_cond  TYPE zdbbr_join_filter_cond_t,
          lt_field_filter_cond TYPE zdbbr_join_filter_cond_t.

    IF ms_search_options-all_lang = abap_false.
      lt_text_filter_cond = VALUE #(
        ( fieldname        = c_text_fields-language
          tabname          = c_tables-texts
          operator         = zif_dbbr_c_operator=>equals
          value            = sy-langu
        )
      ).
      lt_field_filter_cond = VALUE #(
        ( fieldname        = c_fields_fields-ddlanguage
          tabname          = c_tables-fields
          operator         = zif_dbbr_c_operator=>equals
          value            = sy-langu
        )
      ).
    ENDIF.

    ms_join_def = VALUE #(
      primary_table = c_tables-base
      tables = VALUE #(
        ( add_table        = c_tables-texts
          join_type        = zif_dbbr_c_join_types=>left_outer_join
          field_conditions = VALUE #(
            ( field     = c_text_fields-tabname
              ref_field = c_base_fields-tabname
              ref_table = c_tables-base
              operator  = zif_dbbr_c_operator=>equals
            )
          )
          filter_conditions = lt_text_filter_cond
        )
      )
    ).

    IF ms_search_options-search_fld_name = abap_true AND
       ms_search_options-exact_search = abap_false.
      ms_join_def-tables = VALUE #( BASE ms_join_def-tables
        ( add_table        = c_tables-fields
          join_type        = zif_dbbr_c_join_types=>left_outer_join
          field_conditions = VALUE #(
            ( field       = c_fields_fields-tabname
              ref_field   = c_base_fields-tabname
              ref_table   = c_tables-base
              operator  = zif_dbbr_c_operator=>equals )
          )
          filter_conditions = lt_field_filter_cond
        )
      ).
    ENDIF.

    CHECK ms_join_def IS NOT INITIAL.

    mt_from = zcl_dbbr_join_helper=>build_from_clause_for_join_def(
       is_join_def        = ms_join_def
       it_table_alias_map = mt_table_alias_map
    ).
  ENDMETHOD.


  METHOD fill_select_clause.
    mt_select = VALUE #(
     ( |{ c_aliases-base }~{ c_base_fields-tabname }, | )
     ( |{ c_aliases-texts }~{ c_text_fields-language }, | )
     ( |{ c_aliases-texts }~{ c_text_fields-text }, | )
     ( |{ c_aliases-base }~{ c_base_fields-devclass }, | )
     ( |{ c_aliases-base }~{ c_base_fields-app_component } AS APPLICATION_COMPONENT| )
    ).
  ENDMETHOD.


  METHOD fill_table_input.
    DATA: lv_input TYPE se16n_value.

    cv_input_fieldname = |*{ to_upper( iv_input ) }*|.

    IF ms_search_options-exact_search = abap_true.
      cv_input_tabname = to_upper( iv_input ).
    ELSE.
      IF ms_search_options-search_left_aligned = abap_true.
        cv_input_tabname = |{ to_upper( iv_input ) }*|.
      ELSE.
        cv_input_tabname = |*{ to_upper( iv_input ) }*|.
      ENDIF.
    ENDIF.

    cv_input_domname = |*{ to_upper( iv_input ) }*|.

    cv_input_rollname = |*{ to_upper( iv_input ) }*|.

    " handle text fields
    lv_input = |*{ iv_input }*|.
    cv_input_scrtext_s =
    cv_input_scrtext_m =
    cv_input_scrtext_l =
    cv_input_reptext =
    cv_input_fieldtext = lv_input.
  ENDMETHOD.


  METHOD fill_table_seltab.
    ct_or_seltab = VALUE #(
      BASE ct_or_seltab
      ( values = VALUE #(
          ( field  = iv_field
            sign   = iv_sign
            option = iv_option
            low    = iv_input )
        )
      )
    ).
  ENDMETHOD.


  METHOD fill_toolbar.
    DATA(lr_functions) = mr_alv->get_functions( ).

    lr_functions->set_default( ).
    lr_functions->set_function( iv_name = zif_uitb_c_alv_functions=>filter ).
    lr_functions->set_function( iv_name = zif_uitb_c_alv_functions=>filter_delete ).
    lr_functions->set_function( iv_name = zif_uitb_c_alv_functions=>filter_menu ).

    lr_functions->add_function(
        iv_name    = c_show_where_condition
        iv_icon    = |{ icon_tools }|
        iv_tooltip = 'Show Where condition'
    ).
    lr_functions->set_quickfilter( ).
    lr_functions->add_function( iv_type = zif_uitb_c_alv_function_type=>separator ).
    lr_functions->add_function( iv_name = c_all_lang_option            iv_tag = 'SEARCH' iv_text = CONV #( text-001 ) ).
    lr_functions->add_function( iv_name = c_exact_search_option        iv_tag = 'SEARCH' iv_text = CONV #( text-002 ) ).
    lr_functions->add_function( iv_name = c_search_left_aligned_option iv_tag = 'SEARCH' iv_text = CONV #( text-003 ) ).
    lr_functions->add_function( iv_name = c_search_text_option         iv_tag = 'SEARCH' iv_text = CONV #( text-004 ) ).
    lr_functions->add_function( iv_name = c_search_fld_name_option     iv_tag = 'SEARCH' iv_text = CONV #( text-005 ) ).

    lr_functions->toggle_checked( iv_user_function = c_search_text_option ).
  ENDMETHOD.


  METHOD fill_where_clause.
    TYPES: BEGIN OF ty_input,
             input TYPE ttext_stct,
           END OF ty_input.

    DEFINE translate_first_letter.
      zcl_dbbr_appl_util=>translate_first_letter(
        EXPORTING iv_input = lv_input_&1
        IMPORTING ev_output = lv_input_&1_l
      ).
    END-OF-DEFINITION.

    DATA: lv_input_fieldname   TYPE fieldname,
          lv_input_tabname     TYPE tabname,
          lv_input_domname     TYPE domname,
          lv_input_rollname    TYPE rollname,
          lv_input_scrtext_s   TYPE scrtext_s,
          lv_input_scrtext_s_l TYPE scrtext_s,
          lv_input_scrtext_m   TYPE scrtext_m,
          lv_input_scrtext_m_l TYPE scrtext_m,
          lv_input_scrtext_l   TYPE scrtext_l,
          lv_input_scrtext_l_l TYPE scrtext_l,
          lv_input_reptext     TYPE reptext,
          lv_input_reptext_l   TYPE reptext,
          lv_input_fieldtext   TYPE as4text,
          lv_input_fieldtext_l TYPE as4text,

          lv_input             TYPE tabname,

          ls_input             TYPE ty_input,
          lt_input             TYPE STANDARD TABLE OF ty_input,
          lt_or_seltab         TYPE zdbbr_or_seltab_sql_t,
          lt_and_seltab        TYPE zdbbr_and_seltab_t.



*... split input into each word fragment
    SPLIT mv_search AT space INTO TABLE lt_input.

    LOOP AT lt_input INTO ls_input.
      lv_input = ls_input-input.
      CLEAR: lt_or_seltab.
*... convert input due to different length of text fields
      fill_table_input(
        EXPORTING
          iv_input           = lv_input
        CHANGING
          cv_input_fieldname = lv_input_fieldname
          cv_input_fieldtext = lv_input_fieldtext
          cv_input_tabname   = lv_input_tabname
          cv_input_domname   = lv_input_domname
          cv_input_rollname  = lv_input_rollname
          cv_input_scrtext_s = lv_input_scrtext_s
          cv_input_scrtext_m = lv_input_scrtext_m
          cv_input_scrtext_l = lv_input_scrtext_l
          cv_input_reptext   = lv_input_reptext
      ).
*... switch first character to also get lower/upper case
      translate_first_letter:
         scrtext_s,
         scrtext_m,
         scrtext_l,
         reptext,
         fieldtext.

*... fill where clause
**... table name
      fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-base }~{ c_base_fields-tabname }|
                                   iv_input     = lv_input_tabname
                         CHANGING  ct_or_seltab = lt_or_seltab           ).
**... Table text
      IF ms_search_options-search_text_area = abap_true AND
         ms_search_options-exact_search = abap_false.

        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-texts }~{ c_text_fields-text }|
                                     iv_input     = lv_input_scrtext_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
**... capital letter
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-texts }~{ c_text_fields-text }|
                                     iv_input     = lv_input_scrtext_l_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
      ENDIF.
      IF ms_search_options-search_fld_name = abap_true AND
         ms_search_options-exact_search = abap_false.
**... fieldnames
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-domname }|
                                     iv_input     = lv_input_domname
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-rollname }|
                                     iv_input     = lv_input_rollname
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-fieldname }|
                                     iv_input     = lv_input_fieldname
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-scrtext_s }|
                                     iv_input     = lv_input_scrtext_s
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-scrtext_s }|
                                     iv_input     = lv_input_scrtext_s_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-scrtext_m }|
                                     iv_input     = lv_input_scrtext_m
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-scrtext_m }|
                                     iv_input     = lv_input_scrtext_m_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-scrtext_l }|
                                     iv_input     = lv_input_scrtext_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-scrtext_l }|
                                     iv_input     = lv_input_scrtext_l_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-reptext }|
                                     iv_input     = lv_input_reptext
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-reptext }|
                                     iv_input     = lv_input_reptext_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-fieldtext }|
                                     iv_input     = lv_input_fieldtext
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
        fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-fields }~{ c_fields_fields-fieldtext }|
                                     iv_input     = lv_input_fieldtext_l
                           CHANGING  ct_or_seltab = lt_or_seltab           ).
      ENDIF.
**... Each of these OR's needs to be combined with AND
      IF NOT lt_or_seltab IS INITIAL.
        lt_and_seltab = VALUE #( BASE lt_and_seltab ( lt_or_seltab ) ).
      ENDIF.
    ENDLOOP.

*...fill where condition table
    mt_where = zcl_dbbr_where_clause_builder=>create_and_condition(
        it_and_seltab = lt_and_seltab
    ).
  ENDMETHOD.


  METHOD on_alv_context_menu.
    DATA(lt_selected_rows) = mr_alv->get_selections( )->get_selected_rows( ).
    CHECK lt_selected_rows IS NOT INITIAL.

    er_menu->add_separator( ).
    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_open_with_func-adt
        text  = 'Open with ADT Tools'
    ).
    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_open_with_func-db_browser
        text  = 'Open with DB Browser'
    ).
    er_menu->add_function(
        fcode = zcl_dbbr_object_central_search=>c_open_with_func-db_browser_new_task
        text  = 'Open with DB Browser (New Window)'
    ).
  ENDMETHOD.


  METHOD on_link_click.
    RAISE EVENT table_selected
      EXPORTING
        ev_entity_id   = mt_result[ ev_row ]-tabname
        ev_entity_type = zif_dbbr_c_entity_type=>table.
  ENDMETHOD.


  METHOD on_search_input_enter.
    search( |{ mr_search_input->value }| ).
  ENDMETHOD.


  METHOD on_search_press.
    search( |{ mr_search_input->value }| ).
  ENDMETHOD.


  METHOD on_user_command.
    DATA(lr_functions) = mr_alv->get_functions( ).

    IF ev_tag = 'SEARCH'.
      DATA(lf_search_function) = abap_true.
      lr_functions->toggle_checked( ev_function ).
      ASSIGN COMPONENT ev_function OF STRUCTURE ms_search_options TO FIELD-SYMBOL(<lf_option>).
      IF sy-subrc = 0.
        zcl_uitb_appl_util=>toggle( CHANGING value = <lf_option> ).
      ENDIF.
    ENDIF.

    CASE ev_function.
      WHEN c_exact_search_option.
        DATA(lf_disabled) = lr_functions->is_checked( ev_function ).
        lr_functions->toggle_disabled( iv_user_function = c_search_left_aligned_option if_disabled = lf_disabled ).
        lr_functions->toggle_disabled( iv_user_function = c_search_text_option         if_disabled = lf_disabled ).
        lr_functions->toggle_disabled( iv_user_function = c_search_fld_name_option     if_disabled = lf_disabled ).

      WHEN c_show_where_condition.
        DATA: lv_where TYPE string.
        CONCATENATE LINES OF mt_where INTO lv_where SEPARATED BY cl_abap_char_utilities=>newline.
        NEW zcl_uitb_popup_editor(
          iv_text         = lv_where
          iv_editor_title = 'WHERE Condition for Search'
        )->zif_uitb_view~show(
          iv_end_line = 30
        ).
        RETURN.

      WHEN zcl_dbbr_object_central_search=>c_open_with_func-adt.
        open_table_in( zif_dbbr_c_eb_link_mode=>open_with_adt ).

      WHEN zcl_dbbr_object_central_search=>c_open_with_func-db_browser.
        open_table_in( zif_dbbr_c_eb_link_mode=>open_in_db_browser ).

      WHEN zcl_dbbr_object_central_search=>c_open_with_func-db_browser_new_task.
        open_table_in( zif_dbbr_c_eb_link_mode=>open_in_db_browser_new_task ).

    ENDCASE.

    IF lf_search_function = abap_true.
      mr_alv->refresh( if_soft = abap_true ).
      search( |{ mr_search_input->value }| ).
    ENDIF.
  ENDMETHOD.


  METHOD open_table_in.
    DATA(lt_selected_rows) = mr_alv->get_selections( )->get_selected_rows( ).
    CHECK lt_selected_rows IS NOT INITIAL.

    DATA(lr_s_table) = REF #( mt_result[ lt_selected_rows[ 1 ] ] ).

    RAISE EVENT table_selected
      EXPORTING
        ev_entity_id   = |{ to_upper( lr_s_table->tabname ) }|
        ev_entity_type = zif_dbbr_c_entity_type=>table
        ev_action      = iv_open_mode.
  ENDMETHOD.


  METHOD search.
    CHECK iv_search IS NOT INITIAL.

    mv_search = iv_search.

    " search for db tables and refresh alv afterwards
    DATA(lr_functions) = mr_alv->get_functions( ).

    zcl_dbbr_screen_helper=>show_progress( iv_progress = 1 iv_text = CONV #( 'Searching for Tables...'(008) ) ).

    search_db_tables( ).

    DATA(lv_hit_count_text) = COND #( WHEN lines( mt_result ) >= mv_max_hits THEN |> | ).
    mr_alv->get_display_settings( )->set_title( |Hit count { lv_hit_count_text }{ lines( mt_result ) NUMBER = USER }| ).
    mr_alv->get_columns( )->set_optimized( ).

    mr_alv->refresh( ).
  ENDMETHOD.


  METHOD search_db_tables.
    fill_where_clause( ).
    fill_select_clause( ).
    fill_from_clause( ).

    TRY.
        SELECT DISTINCT (mt_select)
          FROM (mt_from)
          WHERE (mt_where)
          ORDER BY (mt_order_by)
        INTO CORRESPONDING FIELDS of TABLE @mt_result
          UP TO @mv_max_hits ROWS.

      CATCH cx_root INTO DATA(lx_root).
    ENDTRY.

  ENDMETHOD.


  METHOD show.
    mr_alv->display( ).

*.. perform initial search if value was supplied
    IF mr_search_input->value IS NOT INITIAL.
      search( |{ mr_search_input->value }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_search_control~set_max_hits.
    mv_max_hits = iv_max_hits.
  ENDMETHOD.


  METHOD zif_uitb_disposable~dispose.
    CHECK mr_splitter IS BOUND.

    mr_splitter->free( ).

    CLEAR mr_splitter.
  ENDMETHOD.
ENDCLASS.
