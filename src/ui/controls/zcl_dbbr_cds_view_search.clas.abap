CLASS zcl_dbbr_cds_view_search DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_uitb_disposable .
    INTERFACES zif_dbbr_search_control .

    ALIASES view_selected
      FOR zif_dbbr_search_control~entry_chosen .

    CLASS-METHODS create
      IMPORTING
        !ir_parent        TYPE REF TO cl_gui_container
        !iv_initial_value TYPE string OPTIONAL
      RETURNING
        VALUE(result)     TYPE REF TO zcl_dbbr_cds_view_search .

  PROTECTED SECTION.

    METHODS constructor
      IMPORTING
        !ir_parent TYPE REF TO cl_gui_container .
  PRIVATE SECTION.
    CONSTANTS c_cds_action_tag TYPE string VALUE 'CDS_ACTION' ##NO_TEXT.
    CONSTANTS c_all_lang_option TYPE ui_func VALUE 'ALL_LANG' ##NO_TEXT.
    CONSTANTS c_exact_search_option TYPE ui_func VALUE 'EXACT_SEARCH' ##NO_TEXT.
    CONSTANTS c_search_left_aligned_option TYPE ui_func VALUE 'SEARCH_LEFT_ALIGNED' ##NO_TEXT.
    CONSTANTS c_search_text_option TYPE ui_func VALUE 'SEARCH_TEXT' ##NO_TEXT.
    CONSTANTS c_search_fld_name_option TYPE ui_func VALUE 'SEARCH_FLD_NAME' ##NO_TEXT.
    CONSTANTS c_show_where_condition TYPE ui_func VALUE 'WHERE' ##NO_TEXT.
    CONSTANTS c_jump_adt_func TYPE ui_func VALUE 'ADT_JUMP' ##NO_TEXT.
    CONSTANTS c_show_source_func TYPE ui_func VALUE 'SHOW_SOURCE' ##NO_TEXT.
    CONSTANTS c_open_ddic_view_func TYPE ui_func VALUE 'OPEN DDIC VIEW' ##NO_TEXT.
    DATA mt_api_state_texts TYPE ddfixvalues.
    DATA mr_input_dd TYPE REF TO cl_dd_document .
    CONSTANTS:
      BEGIN OF c_search_type,
        by_table         TYPE char1 VALUE '0',
        by_name_descr    TYPE char1 VALUE '1',
        by_annotation    TYPE char1 VALUE '2',
        by_used_cds_view TYPE char1 VALUE '3',
      END OF c_search_type .
    CONSTANTS:
      BEGIN OF c_aliases,
        ddl_sources       TYPE zdbbr_table_alias VALUE 'A',
        ddl_search_view   TYPE zdbbr_table_alias VALUE 'B',
        ddl_api_states    TYPE zdbbr_table_alias VALUE 'C',
        ddl_text          TYPE zdbbr_table_alias VALUE 'D',
        ddl_fields        TYPE zdbbr_table_alias VALUE 'E',
        ddl_head_anno     TYPE zdbbr_table_alias VALUE 'F',
        ddl_field_anno    TYPE zdbbr_table_alias VALUE 'G',
        ddl_dependency    TYPE zdbbr_table_alias VALUE 'H',
        ddic_tables       TYPE zdbbr_table_alias VALUE 'I',
        ddl_base_tables   TYPE zdbbr_table_alias VALUE 'J',
        ddl_used_cds_view TYPE zdbbr_table_alias VALUE 'K',
      END OF c_aliases .
    CONSTANTS:
      BEGIN OF c_tables,
        ddl_sources        TYPE tabname VALUE 'DDDDLSRC',
        ddl_fields         TYPE tabname VALUE 'DD03ND',
        ddl_search_view    TYPE tabname VALUE 'ZDBBRDDLS_V',
        ddl_text_740       TYPE tabname VALUE 'DD02BT',
        ddl_text_750       TYPE tabname VALUE 'DDDDLSRCT',
        ddl_field_anno     TYPE tabname VALUE 'DDFIELDANNO',
        ddl_head_anno      TYPE tabname VALUE 'DDHEADANNO',
        ddl_api_states_750 TYPE tabname VALUE 'ZDBBR_APISTATES',
        ddl_dependency     TYPE tabname VALUE 'DDLDEPENDENCY',
        ddic_tables        TYPE tabname VALUE 'ZDBBRDBTAB_V',
        ddl_base_tables    TYPE tabname VALUE 'DD26S',
        " DDL cannot be used togeter with Table/View in Open SQL join, therefore DDL SQL view will be used
*        ddl_used_cds_view  TYPE tabname VALUE 'ZDBBR_DDLUSAGEINDDL',
        ddl_used_cds_view  TYPE tabname VALUE 'ZDBBRIDDLUSGD',
      END OF c_tables .
    CONSTANTS:
      BEGIN OF c_ddl_used_cds_view_fields,
        ddl        TYPE fieldname VALUE 'DDLNAME',
        target_ddl TYPE fieldname VALUE 'USEDDDLNAME',
      END OF c_ddl_used_cds_view_fields.
    CONSTANTS:
      BEGIN OF c_ddl_base_tables_fields,
        viewname TYPE fieldname VALUE 'VIEWNAME',
        tabname  TYPE fieldname VALUE 'TABNAME',
      END OF c_ddl_base_tables_fields .
    CONSTANTS:
      BEGIN OF c_ddl_dependency_fields,
        ddl    TYPE fieldname VALUE 'DDLNAME',
        object TYPE fieldname VALUE 'OBJECTNAME',
        type   TYPE fieldname VALUE 'OBJECTTYPE',
      END OF c_ddl_dependency_fields .
    CONSTANTS:
      BEGIN OF c_ddic_tables_fields,
        tabname TYPE fieldname VALUE 'TABNAME',
      END OF c_ddic_tables_fields .
    CONSTANTS:
      BEGIN OF c_ddl_fields_fields,
        ddl       TYPE fieldname VALUE 'STRUCOBJN',
        fieldname TYPE fieldname VALUE 'FIELDNAME',
        rollname  TYPE fieldname VALUE 'ROLLNAME',
        domname   TYPE fieldname VALUE 'DOMNAME',
        as4local  TYPE fieldname VALUE 'AS4LOCAL',
      END OF c_ddl_fields_fields .
    CONSTANTS:
      BEGIN OF c_ddl_search_view_fields,
        ddlname       TYPE fieldname VALUE 'DDLNAME',
        ddlname_int   TYPE fieldname VALUE 'DDLNAME_INT',
        ddlname_raw   TYPE fieldname VALUE 'DDLNAME_RAW',
        parentname    TYPE fieldname VALUE 'PARENTDDL',
        devclass      TYPE fieldname VALUE 'DEVCLASS',
        app_component TYPE fieldname VALUE 'APP_COMPONENT',
      END OF c_ddl_search_view_fields .
    CONSTANTS:
      BEGIN OF c_ddl_api_states_fields,
        object_type TYPE fieldname VALUE 'OBJECTTYPE',
        ddl         TYPE fieldname VALUE 'OBJECTNAME',
        filtervalue TYPE fieldname VALUE 'FILTERVALUE',
      END OF c_ddl_api_states_fields .
    CONSTANTS:
      BEGIN OF c_ddl_field_anno_fields,
        ddl   TYPE fieldname VALUE 'STRUCOBJN',
        name  TYPE fieldname VALUE 'NAME',
        value TYPE fieldname VALUE 'VALUE',
      END OF c_ddl_field_anno_fields .
    CONSTANTS:
      BEGIN OF c_ddl_head_anno_fields,
        ddl   TYPE fieldname VALUE 'STRUCOBJN',
        field TYPE fieldname VALUE 'LFIELDNAME',
        name  TYPE fieldname VALUE 'NAME',
        value TYPE fieldname VALUE 'VALUE',
      END OF c_ddl_head_anno_fields .
    CONSTANTS:
      BEGIN OF c_ddl_sources_fields,
        ddl         TYPE fieldname VALUE 'DDLNAME',
        parentname  TYPE fieldname VALUE 'PARENTNAME',
        source_type TYPE fieldname VALUE 'SOURCE_TYPE',
      END OF c_ddl_sources_fields .
    CONSTANTS:
      BEGIN OF c_ddl_text_fields,
        ddl_750  TYPE fieldname VALUE 'DDLNAME',
        ddl_740  TYPE fieldname VALUE 'STRUCOBJN',
        language TYPE fieldname VALUE 'DDLANGUAGE',
        text     TYPE fieldname VALUE 'DDTEXT',
      END OF c_ddl_text_fields .
    DATA mv_text_table_fieldname TYPE fieldname .
    DATA mv_text_table TYPE tabname .
    DATA mr_container TYPE REF TO cl_gui_container .
    DATA mr_splitter TYPE REF TO cl_gui_splitter_container .
    DATA mr_alv TYPE REF TO zcl_uitb_alv .
    DATA mt_result TYPE zdbbr_cds_view_search_t .
    DATA mr_search_input TYPE REF TO cl_dd_input_element .
    DATA mr_search_type_select TYPE REF TO cl_dd_select_element .
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
    DATA mt_where TYPE zdbbr_string_t .
    DATA mt_select TYPE zdbbr_string_t .
    DATA mt_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab .
    DATA mv_selected_search_type TYPE char1 .
    DATA mv_current_search_type TYPE char1 .
    DATA mv_max_hits TYPE int2 VALUE 500.


    METHODS create_alv .
    METHODS create_input_dd
      IMPORTING
        !iv_initial_value TYPE string OPTIONAL .
    METHODS create_splitter .
    METHODS fill_from_clause
      RETURNING
        VALUE(result) TYPE zdbbr_string_t .
    METHODS fill_select_clause .
    METHODS fill_table_input
      IMPORTING
        !iv_input             TYPE tabname
      EXPORTING
        !ev_input_fieldtext   TYPE as4text
        !ev_input_fieldname   TYPE fieldname
        !ev_input_tabname     TYPE tabname
        ev_input_tabname_like TYPE tabname
        !ev_input_domname     TYPE domname
        !ev_input_rollname    TYPE rollname.
    METHODS fill_table_seltab
      IMPORTING
        !iv_field     TYPE fieldname
        !iv_sign      TYPE ddsign DEFAULT 'I'
        !iv_option    TYPE ddoption DEFAULT 'CP'
        !iv_input     TYPE clike
      CHANGING
        !ct_or_seltab TYPE zdbbr_or_seltab_sql_t .
    METHODS fill_toolbar .
    METHODS fill_where_clause
      RETURNING
        VALUE(result) TYPE zdbbr_string_t .
    METHODS handle_cds_view_function
      IMPORTING
        !iv_function TYPE ui_func .
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          !ev_row
          !ev_column .
    METHODS on_search_input_enter
        FOR EVENT entered OF cl_dd_input_element .
    METHODS on_search_press
        FOR EVENT clicked OF cl_dd_button_element .
    METHODS on_search_type_selected
        FOR EVENT selected OF cl_dd_select_element .
    METHODS on_user_command
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          !ev_function
          !ev_tag .
    METHODS on_alv_context_menu
          FOR EVENT context_menu OF zcl_uitb_alv_events
      IMPORTING
          er_menu.
    METHODS search
      IMPORTING
        !iv_search                   TYPE string OPTIONAL
        !if_search_parameter_changed TYPE abap_bool OPTIONAL .
    METHODS search_cds_views .
    METHODS set_search_functions .
    METHODS show .
    METHODS open_cds_view_in
      IMPORTING
        iv_open_mode TYPE zdbbr_entity_browser_link_mode.
    METHODS get_selected_view_index
      RETURNING
        VALUE(rv_selected_row) TYPE int4.
ENDCLASS.



CLASS zcl_dbbr_cds_view_search IMPLEMENTATION.


  METHOD constructor.
    mr_container = ir_parent.
    mv_selected_search_type = c_search_type-by_name_descr.

    mt_table_to_alias_map = VALUE #(
      ( alias = c_aliases-ddl_head_anno      tabname = c_tables-ddl_head_anno )
      ( alias = c_aliases-ddl_field_anno     tabname = c_tables-ddl_field_anno )
      ( alias = c_aliases-ddl_sources        tabname = c_tables-ddl_sources )
      ( alias = c_aliases-ddl_search_view    tabname = c_tables-ddl_search_view )
      ( alias = c_aliases-ddl_fields         tabname = c_tables-ddl_fields )
      ( alias = c_aliases-ddl_dependency     tabname = c_tables-ddl_dependency )
      ( alias = c_aliases-ddl_base_tables    tabname = c_tables-ddl_base_tables )
      ( alias = c_aliases-ddic_tables        tabname = c_tables-ddic_tables )
      ( alias = c_aliases-ddl_used_cds_view  tabname = c_tables-ddl_used_cds_view )
    ).

    IF sy-saprl >= 750.
      mt_table_to_alias_map = VALUE #( BASE mt_table_to_alias_map
        ( alias = c_aliases-ddl_text         tabname = c_tables-ddl_text_750 )
      ).
      mv_text_table_fieldname = c_ddl_text_fields-ddl_750.
      mv_text_table = c_tables-ddl_text_750.
    ELSE.
      mt_table_to_alias_map = VALUE #( BASE mt_table_to_alias_map
        ( alias = c_aliases-ddl_text         tabname = c_tables-ddl_text_740 )
      ).
      mv_text_table_fieldname = c_ddl_text_fields-ddl_740.
      mv_text_table = c_tables-ddl_text_740.
    ENDIF.

  ENDMETHOD.


  METHOD create.
    DATA(lr_control) = NEW zcl_dbbr_cds_view_search( ir_parent ).
    lr_control->create_splitter( ).
    lr_control->create_input_dd( iv_initial_value ).
    lr_control->create_alv( ).
    lr_control->show( ).

    result = lr_control.
  ENDMETHOD.


  METHOD create_alv.
    DATA: lr_col TYPE REF TO zcl_uitb_alv_column.

    mr_alv = zcl_uitb_alv=>create_alv(
      ir_data      = REF #( mt_result )
      ir_container = mr_splitter->get_container( column = 1 row = 2 )
      if_editable  = abap_false
    ).

    mr_alv->get_selections(  )->set_mode( value = zif_uitb_c_alv_selection=>cell ).

    SET HANDLER:
      on_user_command FOR mr_alv->get_events( ),
      on_link_click FOR mr_alv->get_events( ),
      on_alv_context_menu FOR mr_alv->get_events( ).

    DEFINE set_column_status.
      lr_col = mr_alv->get_columns( )->get_column( '&1' ).
      IF sy-saprl < 750.
        lr_col->set_technical( ).
      ELSE.
        lr_col->set_descriptions(
            iv_long    = &2
            iv_tooltip = &3
        ).
      ENDIF.
    END-OF-DEFINITION.

    set_column_status:
      release_state           'API State'          'API State of CDS View',
      key_user_apps_released  'Key Apps'           'Use in Key User Apps',
      cloud_released          'Use in Cloud Apps'  'Use in Cloud Applications',
      remote_api_released     'Use in Remote API'  'Use as Remote API',
      custom_fields_released  'Add Custom Fields'  'Add Custom Fields'
      .
    mr_alv->get_columns( )->get_column( 'KEY_USER_APPS_RELEASED' )->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
    mr_alv->get_columns( )->get_column( 'CLOUD_RELEASED' )->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
    mr_alv->get_columns( )->get_column( 'REMOTE_API_RELEASED' )->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).
    mr_alv->get_columns( )->get_column( 'CUSTOM_FIELDS_RELEASED' )->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).

    fill_toolbar( ).

    mr_alv->get_columns( )->get_column( 'DDLNAME_INT' )->set_technical( ).
    mr_alv->get_columns( )->get_column( 'DDLNAME' )->set_hotspot( ).

    DATA(lr_sorts) = mr_alv->get_sorting( ).
    lr_sorts->add_sort(
        iv_column_name        = 'DDLNAME'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
    lr_sorts->add_sort(
        iv_column_name        = 'DEVCLASS'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
    lr_sorts->add_sort(
        iv_column_name        = 'APP_COMPONENT'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
    lr_sorts->add_sort(
        iv_column_name        = 'DDLANGUAGE'
        iv_sequence           = zif_uitb_c_alv_sorting=>descending
    ).
  ENDMETHOD.


  METHOD create_input_dd.
    mr_input_dd = NEW cl_dd_document( ).

    mr_input_dd->initialize_document( background_color = cl_dd_area=>col_background_level1 ).

*.. Fill the document
    mr_input_dd->add_form( IMPORTING formarea = DATA(lr_form) ).
    lr_form->line_with_layout( start = abap_true no_leading_break = abap_true ).
    lr_form->add_select_element(
      EXPORTING
        value          = CONV #( mv_selected_search_type )
        options         = VALUE #(
          ( value = c_search_type-by_name_descr    text = 'By Name/Field/Description'(009) )
          ( value = c_search_type-by_table         text = 'By Used Table'(010) )
          ( value = c_search_type-by_used_cds_view text = 'By Used CDS View'(018) )
          ( value = c_search_type-by_annotation    text = 'By Annotation'(011) )
        )
      IMPORTING
        select_element = mr_search_type_select
    ).

    lr_form->line_with_layout( end = abap_true ).
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
      on_search_press FOR lr_search_button,
      on_search_type_selected FOR mr_search_type_select.

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

    mr_splitter->set_row_height( id = 1 height = 65 ).
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

  ENDMETHOD.


  METHOD fill_from_clause.
    DATA: lt_text_filter_cond TYPE zdbbr_join_filter_cond_t.

    CLEAR: ms_join_def.


    IF ms_search_options-all_lang = abap_false.
      lt_text_filter_cond = VALUE #(
        ( fieldname        = c_ddl_text_fields-language
          tabname          = mv_text_table
          operator         = zif_dbbr_c_operator=>equals
          value            = sy-langu
        )
      ).
    ENDIF.


    IF mv_selected_search_type = c_search_type-by_used_cds_view.
      ms_join_def = VALUE zdbbr_join_def(
        primary_table = c_tables-ddl_used_cds_view
        tables        = VALUE #(
          ( add_table        = c_tables-ddl_sources
            field_conditions = VALUE #(
              ( field        = c_ddl_sources_fields-ddl
                ref_field    = c_ddl_used_cds_view_fields-ddl
                ref_table    = c_tables-ddl_used_cds_view
                operator     = zif_dbbr_c_operator=>equals
              )
            )
          )
        )
      ).
    ELSE.
      ms_join_def = VALUE zdbbr_join_def(
        primary_table = c_tables-ddl_sources
      ).
    ENDIF.

    ms_join_def-tables = VALUE #( BASE ms_join_def-tables
       ( add_table = c_tables-ddl_search_view
         join_type = zif_dbbr_c_join_types=>inner_join
         field_conditions = VALUE #(
           ( field     = c_ddl_search_view_fields-ddlname
             ref_field = c_ddl_sources_fields-ddl
             ref_table = c_tables-ddl_sources
             operator  = zif_dbbr_c_operator=>equals )
         )
         filter_conditions = VALUE #(
           ( fieldname = c_ddl_sources_fields-parentname
             tabname   = c_tables-ddl_sources
             value     = space
             operator  = zif_dbbr_c_operator=>equals )
         )
       )
       ( add_table = mv_text_table
         join_type = zif_dbbr_c_join_types=>left_outer_join
         field_conditions = VALUE #(
          ( field     = mv_text_table_fieldname
            ref_field = c_ddl_sources_fields-ddl
            ref_table = c_tables-ddl_sources
            operator  = zif_dbbr_c_operator=>equals )
         )
         filter_conditions = lt_text_filter_cond
       )
    ).

    CASE mv_selected_search_type.

*.... Search for CDS Views by Annotation name
      WHEN c_search_type-by_annotation.
        ms_join_def-tables = VALUE #( BASE ms_join_def-tables
          ( add_table        = c_tables-ddl_head_anno
            join_type        = zif_dbbr_c_join_types=>left_outer_join
            field_conditions = VALUE #(
              ( field     = c_ddl_head_anno_fields-ddl
                ref_field = c_ddl_search_view_fields-ddlname
                ref_table = c_tables-ddl_search_view
              )
            )
          )
          ( add_table        = c_tables-ddl_field_anno
            join_type        = zif_dbbr_c_join_types=>left_outer_join
            field_conditions = VALUE #(
              ( field     = c_ddl_field_anno_fields-ddl
                ref_field = c_ddl_search_view_fields-ddlname
                ref_table = c_tables-ddl_search_view
              )
            )
          )
        ).

*.... Search for CDS Views by field / name / description
      WHEN c_search_type-by_name_descr.

        IF ms_search_options-search_fld_name = abap_true AND
           ms_search_options-exact_search = abap_false.

          ms_join_def-tables = VALUE #( BASE ms_join_def-tables
            ( add_table = c_tables-ddl_fields
              join_type = zif_dbbr_c_join_types=>left_outer_join
              field_conditions = VALUE #(
                ( field     = c_ddl_fields_fields-ddl
                  ref_field = c_ddl_sources_fields-ddl
                  ref_table = c_tables-ddl_sources
                  operator  = zif_dbbr_c_operator=>equals )
              )
              filter_conditions = VALUE #(
                ( fieldname = c_ddl_fields_fields-as4local
                  tabname   = c_tables-ddl_fields
                  operator  = zif_dbbr_c_operator=>equals
                  value     = 'A' )
              )
            )
          ).
        ENDIF.

*.... Search for CDS Views where a certain table is used
      WHEN c_search_type-by_table.
        ms_join_def-tables = VALUE #( BASE ms_join_def-tables
          ( add_table         = c_tables-ddl_dependency
            join_type         = zif_dbbr_c_join_types=>inner_join
            field_conditions  = VALUE #(
              ( field     = c_ddl_dependency_fields-ddl
                ref_field = c_ddl_sources_fields-ddl
                ref_table = c_tables-ddl_sources
                operator  = zif_dbbr_c_operator=>equals )
            )
          )
          ( add_table         = c_tables-ddl_base_tables
            join_type         = zif_dbbr_c_join_types=>inner_join
            field_conditions  = VALUE #(
              ( field     = c_ddl_base_tables_fields-viewname
                ref_field = c_ddl_dependency_fields-object
                ref_table = c_tables-ddl_dependency
                operator  = zif_dbbr_c_operator=>equals )
            )
            filter_conditions = VALUE #(
              ( fieldname = c_ddl_dependency_fields-type
                tabname   = c_tables-ddl_dependency
                value     = 'VIEW'
                operator  = zif_dbbr_c_operator=>equals )
            )
          )
          ( add_table         = c_tables-ddic_tables
            join_type         = zif_dbbr_c_join_types=>inner_join
            field_conditions  = VALUE #(
              ( field       = c_ddic_tables_fields-tabname
                ref_field   = c_ddl_base_tables_fields-tabname
                ref_table   = c_tables-ddl_base_tables
                operator    = zif_dbbr_c_operator=>equals )
            )
          )
        ).

    ENDCASE.

    CHECK ms_join_def IS NOT INITIAL.

    mt_from = zcl_dbbr_join_helper=>build_from_clause_for_join_def(
        is_join_def        = ms_join_def
        it_table_alias_map = mt_table_to_alias_map
    ).

  ENDMETHOD.


  METHOD fill_select_clause.
    IF sy-saprl >= 750.
      mt_select = VALUE #(
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-ddlname } as { c_ddl_search_view_fields-ddlname_int }, | )
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-ddlname_raw } as { c_ddl_search_view_fields-ddlname }, | )
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-devclass }, | )
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-app_component }, | )
        ( |{ c_aliases-ddl_text }~{ c_ddl_text_fields-text }, | )
        ( |CASE { c_aliases-ddl_sources }~{ c_ddl_sources_fields-source_type } | &&
          | WHEN 'V' THEN 'View' | &&
          | WHEN 'T' THEN 'Table Entity' | &&
          | WHEN 'F' THEN 'Table Function' | &&
          |END AS { c_ddl_sources_fields-source_type }, | )
        ( |{ c_aliases-ddl_text }~{ c_ddl_text_fields-language } | )
      ).

    ELSE.
      mt_select = VALUE #(
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-ddlname } as { c_ddl_search_view_fields-ddlname_int }, | )
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-ddlname_raw } as { c_ddl_search_view_fields-ddlname }, | )
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-devclass }, | )
        ( |{ c_aliases-ddl_search_view }~{ c_ddl_search_view_fields-app_component }, | )
        ( |{ c_aliases-ddl_text }~{ c_ddl_text_fields-text }, | )
        ( |{ c_aliases-ddl_text }~{ c_ddl_text_fields-language }| )
      ).

    ENDIF.
  ENDMETHOD.


  METHOD fill_table_input.

    IF ms_search_options-exact_search = abap_true.
      ev_input_tabname = to_upper( iv_input ).
    ELSE.
      IF ms_search_options-search_left_aligned = abap_true.
        ev_input_tabname = |{ to_upper( iv_input ) }*|.
      ELSE.
        ev_input_tabname = |*{ to_upper( iv_input ) }*|.
      ENDIF.
    ENDIF.

    ev_input_tabname_like = |*{ to_upper( iv_input ) }*|.
    ev_input_fieldname = |*{ to_upper( iv_input ) }*|.
    ev_input_rollname = |*{ to_upper( iv_input ) }*|.
    ev_input_domname = |*{ to_upper( iv_input ) }*|.

    IF ms_search_options-search_left_aligned = abap_true AND
       mv_selected_search_type = c_search_type-by_annotation.
      ev_input_fieldname = |{ to_upper( iv_input ) }*|.
    ENDIF.

    ev_input_fieldtext = |*{ iv_input }*|.

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
        iv_tooltip = |{ 'Show Where condition'(014) }|
    ).
    lr_functions->add_function( iv_type = zif_uitb_c_alv_function_type=>separator ).

    DATA(lr_environment_func_menu) = NEW cl_ctmenu( ).
    lr_environment_func_menu->add_function(
        fcode = c_jump_adt_func
        text  = 'Open with ADT Tools'(015)
    ).
    lr_environment_func_menu->add_function(
        fcode = c_show_source_func
        text  = 'Show DDL Source'(016)
    ).
    lr_environment_func_menu->add_function(
        fcode = c_open_ddic_view_func
        text  = 'Open DDIC View'(017)
    ).

    lr_functions->add_function(
*        iv_name  = 'ENVIRONMENT'
        iv_icon  = |{ icon_reference_list }|
        iv_type  = zif_uitb_c_alv_function_type=>menu_button
        iv_tag   = c_cds_action_tag
        ir_menu  = lr_environment_func_menu
    ).

    lr_functions->add_function( iv_type = zif_uitb_c_alv_function_type=>separator ).

    set_search_functions( ).
  ENDMETHOD.


  METHOD fill_where_clause.
    TYPES: BEGIN OF ty_input,
             input TYPE ttext_stct,
           END OF ty_input.

    DATA: lv_input_fieldtext_l TYPE as4text,
          lv_input             TYPE tabname,
          ls_input             TYPE ty_input,
          lt_input             TYPE STANDARD TABLE OF ty_input,
          lt_or_seltab         TYPE zdbbr_or_seltab_sql_t,
          lt_and_seltab        TYPE zdbbr_and_seltab_t.


*... split input into each word fragment
    SPLIT mv_search AT space INTO TABLE lt_input.

    LOOP AT lt_input INTO ls_input.
      DATA(lv_input_word_number) = sy-tabix.
      lv_input = ls_input-input.
      CLEAR: lt_or_seltab.

*.... convert input due to different length of text fields
      fill_table_input(
        EXPORTING
          iv_input           = lv_input
        IMPORTING
          ev_input_fieldname    = DATA(lv_input_fieldname)
          ev_input_fieldtext    = DATA(lv_input_fieldtext)
          ev_input_rollname     = DATA(lv_input_rollname)
          ev_input_domname      = DATA(lv_input_domname)
          ev_input_tabname      = DATA(lv_input_tabname)
          ev_input_tabname_like = DATA(lv_input_tabname_like)
      ).

      CASE mv_selected_search_type.

        WHEN c_search_type-by_annotation.
          IF lv_input_word_number = 1.
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_head_anno }~{ c_ddl_head_anno_fields-name }|
                                         iv_input     = lv_input_fieldname
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_field_anno }~{ c_ddl_field_anno_fields-name }|
                                         iv_input     = lv_input_fieldname
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
          ELSE.
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_sources }~{ c_ddl_sources_fields-ddl }|
                                         iv_input     = lv_input_tabname_like
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
          ENDIF.

        WHEN c_search_type-by_name_descr.

*........ switch first character to also get lower/upper case
          zcl_dbbr_appl_util=>translate_first_letter(
            EXPORTING iv_input = lv_input_fieldtext
            IMPORTING ev_output = lv_input_fieldtext_l
          ).

*........ fill where clause
**....... table name
          fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_sources }~{ c_ddl_sources_fields-ddl  }|
                                       iv_input     = lv_input_tabname
                             CHANGING  ct_or_seltab = lt_or_seltab           ).
**....... Table text
          IF ms_search_options-search_text_area = abap_true AND
             ms_search_options-exact_search = abap_false.

            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_text }~{ c_ddl_text_fields-text }|
                                         iv_input     = lv_input_fieldtext
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
**......... capital letter
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_text }~{ c_ddl_text_fields-text }|
                                         iv_input     = lv_input_fieldtext_l
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
          ENDIF.
          IF ms_search_options-search_fld_name = abap_true AND
             ms_search_options-exact_search = abap_false.
**......... fieldnames
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_fields }~{ c_ddl_fields_fields-fieldname }|
                                         iv_input     = lv_input_fieldname
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_fields }~{ c_ddl_fields_fields-domname }|
                                         iv_input     = lv_input_domname
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_fields }~{ c_ddl_fields_fields-rollname }|
                                         iv_input     = lv_input_rollname
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
          ENDIF.

        WHEN c_search_type-by_table.
          fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_base_tables }~{ c_ddl_base_tables_fields-tabname }|
                                       iv_input     = lv_input_tabname
                             CHANGING  ct_or_seltab = lt_or_seltab           ).

        WHEN c_search_type-by_used_cds_view.
          IF lv_input_word_number = 1.
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_used_cds_view }~{ c_ddl_used_cds_view_fields-target_ddl }|
                                         iv_input     = lv_input_tabname
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
          ELSE.
            fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_used_cds_view }~{ c_ddl_used_cds_view_fields-ddl }|
                                         iv_input     = lv_input_tabname_like
                               CHANGING  ct_or_seltab = lt_or_seltab           ).
          ENDIF.

      ENDCASE.

**... Each of these OR's needs to be combined with AND
      IF NOT lt_or_seltab IS INITIAL.
        lt_and_seltab = VALUE #( BASE lt_and_seltab ( lt_or_seltab ) ).
      ENDIF.
    ENDLOOP.

    CLEAR lt_or_seltab.

*.. Fill conditions for source type in NW >= 7.50
    IF sy-saprl >= 750.
      fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_sources }~{ c_ddl_sources_fields-source_type }|
                                   iv_input     = 'V' " Views
                         CHANGING  ct_or_seltab = lt_or_seltab           ).
      fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_sources }~{ c_ddl_sources_fields-source_type }|
                                   iv_input     = 'F' " Table Function
                         CHANGING  ct_or_seltab = lt_or_seltab           ).
      fill_table_seltab( EXPORTING iv_field     = |{ c_aliases-ddl_sources }~{ c_ddl_sources_fields-source_type }|
                                   iv_input     = 'T' " Table Entity
                         CHANGING  ct_or_seltab = lt_or_seltab           ).
      lt_and_seltab = VALUE #( BASE lt_and_seltab ( lt_or_seltab ) ).
    ENDIF.

*...return result as string table
    mt_where = zcl_dbbr_where_clause_builder=>create_and_condition(
        it_and_seltab = lt_and_seltab
    ).
  ENDMETHOD.


  METHOD get_selected_view_index.
    DATA(lt_selected_rows) = mr_alv->get_selections( )->get_selected_rows( ).
    IF lt_selected_rows IS INITIAL.
      DATA(lt_selected_cells) = mr_alv->get_selections( )->get_selected_cells( ).
      lt_selected_rows = VALUE #(
        FOR cell IN lt_selected_cells
        WHERE ( column = 'DDLNAME' )
        ( cell-row )
      ).
      SORT lt_selected_rows.
      DELETE ADJACENT DUPLICATES FROM lt_selected_rows.
    ENDIF.

    CHECK lines( lt_selected_rows ) = 1.

    rv_selected_row = lt_selected_rows[ 1 ].
  ENDMETHOD.


  METHOD handle_cds_view_function.
*.. Get the current selected row
    DATA(lt_selected_rows) = mr_alv->get_selections( )->get_selected_rows( ).
    IF lt_selected_rows IS INITIAL.
      DATA(lt_selected_cells) = mr_alv->get_selections( )->get_selected_cells( ).
      lt_selected_rows = VALUE #(
        FOR cell IN lt_selected_cells
        ( cell-row )
      ).
      SORT lt_selected_rows.
      DELETE ADJACENT DUPLICATES FROM lt_selected_rows.
    ENDIF.

    CHECK lines( lt_selected_rows ) = 1.

    DATA(lr_s_cds_view) = REF #( mt_result[ lt_selected_rows[ 1 ] ] ).
    DATA(lv_cds_view) = to_upper( lr_s_cds_view->ddlname ).

    TRY.

        CASE iv_function.

          WHEN c_show_source_func.
            DATA(lv_source) = zcl_dbbr_cds_view_factory=>read_ddls_source( |{ lv_cds_view }|  ).
            NEW zcl_uitb_popup_editor(
                iv_text         = lv_source
                iv_editor_title = 'CDS View Source Code'
            )->zif_uitb_view~show( iv_end_column = 150 iv_end_line = 30 ).

          WHEN c_open_ddic_view_func.
            zcl_dbbr_dictionary_helper=>navigate_to_table(
              zcl_dbbr_cds_view_factory=>read_ddl_ddic_view( |{ lv_cds_view }| )
            ).

          WHEN c_jump_adt_func.
            zcl_dbbr_adt_util=>jump_adt(
                iv_obj_name        = |{ lv_cds_view }|
                iv_obj_type        = 'DDLS'
            ).
        ENDCASE.
      CATCH zcx_dbbr_application_exc INTO DATA(lx_appl_error).
        lx_appl_error->zif_dbbr_exception_message~print( ).
    ENDTRY.
  ENDMETHOD.


  METHOD on_alv_context_menu.
    DATA(lv_selected_index) = get_selected_view_index( ).
    CHECK lv_selected_index IS NOT INITIAL.

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
    RAISE EVENT view_selected
      EXPORTING
        ev_entity_id   = |{ to_upper( mt_result[ ev_row ]-ddlname ) }|
        ev_entity_type = zif_dbbr_c_entity_type=>cds_view.
  ENDMETHOD.


  METHOD on_search_input_enter.
    search( iv_search = |{ mr_search_input->value }| ).
  ENDMETHOD.


  METHOD on_search_press.
    search( iv_search = |{ mr_search_input->value }| ).
  ENDMETHOD.


  METHOD on_search_type_selected.
    DATA: lf_search_function_enabled TYPE abap_bool VALUE abap_true.

    mv_selected_search_type = mr_search_type_select->value.

    set_search_functions( ).

    CLEAR: mt_result,
           mv_search,
           mv_current_search_type.

    search( iv_search = |{ mr_search_input->value }| ).
  ENDMETHOD.


  METHOD on_user_command.
    DATA(lr_functions) = mr_alv->get_functions( ).

    IF ev_tag = 'SEARCH'.
      DATA(lf_search) = abap_true.
      lr_functions->toggle_checked( ev_function ).
      ASSIGN COMPONENT ev_function OF STRUCTURE ms_search_options TO FIELD-SYMBOL(<lf_option>).
      IF sy-subrc = 0.
        zcl_uitb_appl_util=>toggle( CHANGING value = <lf_option> ).
      ENDIF.
    ELSEIF ev_tag = 'CDS_ACTION'.
      handle_cds_view_function( ev_function ).
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

      WHEN zcl_dbbr_object_central_search=>c_open_with_func-adt.
        open_cds_view_in( zif_dbbr_c_eb_link_mode=>open_with_adt ).

      WHEN zcl_dbbr_object_central_search=>c_open_with_func-db_browser.
        open_cds_view_in( zif_dbbr_c_eb_link_mode=>open_in_db_browser ).

      WHEN zcl_dbbr_object_central_search=>c_open_with_func-db_browser_new_task.
        open_cds_view_in( zif_dbbr_c_eb_link_mode=>open_in_db_browser_new_task ).

    ENDCASE.

    IF lf_search = abap_true.
      mr_alv->refresh( if_soft = abap_true ).
      search(
        iv_search                   = |{ mr_search_input->value }|
        if_search_parameter_changed = abap_true
      ).
    ENDIF.
  ENDMETHOD.


  METHOD open_cds_view_in.
    DATA(lv_selected_index) = get_selected_view_index( ).
    CHECK lv_selected_index IS NOT INITIAL.

    DATA(lr_s_cds_view) = REF #( mt_result[ lv_selected_index ] ).
    DATA(lv_cds_view) = to_upper( lr_s_cds_view->ddlname ).

    RAISE EVENT view_selected
      EXPORTING
        ev_entity_id   = |{ lv_cds_view }|
        ev_entity_type = zif_dbbr_c_entity_type=>cds_view
        ev_action      = iv_open_mode.
  ENDMETHOD.


  METHOD search.
    CHECK iv_search IS NOT INITIAL.

    IF if_search_parameter_changed = abap_false AND
       mv_search = iv_search AND
       mv_current_search_type = mv_selected_search_type.
      RETURN.
    ENDIF.

    mv_search = iv_search.
    mv_current_search_type = mv_selected_search_type.

*.. search for cds view and refresh alv afterwards
    DATA(lr_functions) = mr_alv->get_functions( ).

*.. collect chosen functions from toolbar
    CLEAR ms_search_options.

    ms_search_options-all_lang = lr_functions->is_checked( c_all_lang_option ).

    ms_search_options-exact_search = lr_functions->is_checked( c_exact_search_option ).
    IF mv_selected_search_type = c_search_type-by_name_descr.
      ms_search_options-search_fld_name = lr_functions->is_checked( c_search_fld_name_option ).
    ENDIF.
    ms_search_options-search_text_area = lr_functions->is_checked( c_search_text_option ).
    ms_search_options-search_left_aligned = lr_functions->is_checked( c_search_left_aligned_option ).

    zcl_dbbr_screen_helper=>show_progress( iv_progress = 1 iv_text = CONV #( 'Searching for CDS Views...'(008) ) ).

    search_cds_views( ).

    DATA(lv_hit_count_text) = COND #( WHEN lines( mt_result ) >= mv_max_hits THEN |> | ).

    mr_alv->get_display_settings( )->set_title( |Hit count { lv_hit_count_text }{ lines( mt_result ) NUMBER = USER }| ).
    mr_alv->get_columns( )->set_optimized( ).

    mr_alv->refresh( ).
  ENDMETHOD.


  METHOD search_cds_views.
    fill_select_clause( ).
    fill_from_clause( ).
    fill_where_clause( ).

    TRY.
        SELECT DISTINCT (mt_select)
          FROM (mt_from)
          WHERE (mt_where)
        INTO CORRESPONDING FIELDS OF TABLE @mt_result
          UP TO @mv_max_hits ROWS.

*...... Select API Status for 750 Releases
        IF sy-saprl < 750 OR mt_result IS  INITIAL.
          RETURN.
        ENDIF.

        SELECT *
         FROM zdbbr_i_apistates INTO TABLE @DATA(lt_api_states)
         FOR ALL ENTRIES IN @mt_result
         WHERE objectname = @mt_result-ddlname_int
           AND objecttype = 'DDLS'.

        LOOP AT mt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
          LOOP AT lt_api_states ASSIGNING FIELD-SYMBOL(<ls_api_state>) WHERE objectname = <ls_result>-ddlname_int.

            CASE <ls_api_state>-filtervalue.

              WHEN zif_dbbr_c_cds_api_state=>add_custom_fields.
                <ls_result>-custom_fields_released = abap_true.

              WHEN zif_dbbr_c_cds_api_state=>use_as_remote_api.
                <ls_result>-remote_api_released = abap_true.

              WHEN zif_dbbr_c_cds_api_state=>use_in_key_user_apps.
                <ls_result>-key_user_apps_released = abap_true.

              WHEN zif_dbbr_c_cds_api_state=>use_in_sap_cloud_platform.
                <ls_result>-cloud_released = abap_true.

            ENDCASE.

          ENDLOOP.
          IF sy-subrc <> 0.
            <ls_result>-release_state = 'NOT_RELEASED'.
          ELSE.
            <ls_result>-release_state = <ls_api_state>-apistate.
          ENDIF.
        ENDLOOP.
      CATCH cx_root INTO DATA(lx_root).
    ENDTRY.

  ENDMETHOD.


  METHOD set_search_functions.
    DATA(lr_functions) = mr_alv->get_functions( ).

    lr_functions->remove_function( iv_tag = 'SEARCH' ).

    lr_functions->add_function( iv_name = c_all_lang_option
                                iv_tag  = 'SEARCH'
                                iv_text = CONV #( TEXT-001 ) ).

    CASE mv_selected_search_type.

      WHEN c_search_type-by_name_descr.

        lr_functions->add_function( iv_name = c_exact_search_option
                                    iv_tag  = 'SEARCH'
                                    iv_text = CONV #( TEXT-002 ) ).
        lr_functions->add_function( iv_name = c_search_left_aligned_option
                                    iv_tag  = 'SEARCH'
                                    iv_text = CONV #( TEXT-003 ) ).
        lr_functions->add_function( iv_name = c_search_text_option
                                    iv_tag  = 'SEARCH'
                                    iv_text = CONV #( TEXT-004 ) ).

        IF mv_selected_search_type = c_search_type-by_name_descr.
          lr_functions->add_function( iv_name = c_search_fld_name_option
                                      iv_tag  = 'SEARCH'
                                      iv_text = CONV #( TEXT-005 ) ).
        ENDIF.
        lr_functions->toggle_checked( iv_user_function = c_search_text_option ).

      WHEN c_search_type-by_annotation.
        lr_functions->add_function( iv_name = c_search_left_aligned_option
                                    iv_tag  = 'SEARCH'
                                    iv_text = CONV #( TEXT-003 ) ).

      WHEN c_search_type-by_table OR
           c_search_type-by_used_cds_view.
        lr_functions->add_function( iv_name = c_exact_search_option
                                    iv_tag  = 'SEARCH'
                                    iv_text = CONV #( TEXT-002 ) ).

    ENDCASE.

    mr_alv->refresh( if_soft = abap_true ).
  ENDMETHOD.


  METHOD show.
    mr_alv->display( ).

*.. perform initial search if value was supplied
    IF mr_search_input->value IS NOT INITIAL.
      search( iv_search = |{ mr_search_input->value }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_search_control~set_max_hits.
    IF mv_max_hits <> iv_max_hits.
      CLEAR: mv_current_search_type.
    ENDIF.
    mv_max_hits = iv_max_hits.
  ENDMETHOD.


  METHOD zif_uitb_disposable~dispose.
    CHECK mr_splitter IS BOUND.

    mr_splitter->free( ).
    CLEAR mr_splitter.
  ENDMETHOD.

ENDCLASS.
