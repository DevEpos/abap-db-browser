CLASS zcl_dbbr_fe_dnd_tree_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_content_searcher .

    METHODS constructor
      IMPORTING
        ir_parent        TYPE REF TO cl_gui_container
        ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list
        it_join_tables   TYPE zdbbr_join_table_ui_itab.
    METHODS free .
    METHODS show.
    METHODS refresh_saved_formulas.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_top_node TYPE tm_nodekey VALUE '00001' ##NO_TEXT.

    TYPES:
      BEGIN OF mty_node_data.
        INCLUDE TYPE treemsnod.
    TYPES: items TYPE treemcitab.
    TYPES: END OF mty_node_data .

    DATA mv_current_node_index TYPE num5 VALUE 00000 ##NO_TEXT.
    DATA mv_top_node TYPE tm_nodekey .
    DATA mv_icon_top_node TYPE tm_nodekey .
    DATA mv_color_top_node TYPE tm_nodekey .
    DATA mv_examples_top_node TYPE tm_nodekey .
    DATA mv_special_func_top_node TYPE tm_nodekey .
    DATA mv_availbl_stmnts_top_node TYPE tm_nodekey .
    DATA mv_formula_def_top_node TYPE tm_nodekey .
    DATA mv_saved_formulas_top_node TYPE tm_nodekey .
    DATA mv_other_saved_forms_top_node TYPE tm_nodekey.
    DATA mt_join_tables TYPE zdbbr_join_table_ui_itab .
    DATA mr_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list .

    TYPES:
      BEGIN OF ty_userobject_map,
        node_key    TYPE tm_nodekey,
        user_object TYPE REF TO zcl_dbbr_fe_dnd_object,
      END OF ty_userobject_map .

    DATA:
      mt_userobject_map TYPE HASHED TABLE OF ty_userobject_map WITH UNIQUE KEY node_key .

    CONSTANTS:
      BEGIN OF c_column_names,
        favorite_id          TYPE tv_itmname VALUE '1',
        placeholder          TYPE tv_itmname VALUE '2',
        favorite_description TYPE tv_itmname VALUE '3',
      END OF c_column_names .
    DATA mr_tree_model TYPE REF TO cl_list_tree_model .
    DATA mr_parent TYPE REF TO cl_gui_container .
    DATA mr_tree_control TYPE REF TO cl_gui_control .
    DATA mr_tree_wrapper TYPE REF TO zcl_dbbr_list_tree_wrapper .
    DATA mr_example_dnd_behaviour TYPE REF TO cl_dragdrop .
    DATA mr_element_dnd_behaviour TYPE REF TO cl_dragdrop .
    DATA mt_formulas TYPE zif_dbbr_fe_types=>tt_formula_defs .

    METHODS create_tree .
    METHODS fill_tree.

    METHODS create_top_node .
    METHODS create_color_nodes .
    METHODS create_icon_nodes .
    METHODS create_row_nodes .
    METHODS create_special_functions_nodes .
    METHODS create_available_stmtns_nodes .
    METHODS create_formula_def_templates .
    METHODS create_saved_formula_nodes
      IMPORTING
        if_create_top TYPE abap_bool
        iv_user_name  TYPE sy-uname OPTIONAL
      CHANGING
        cv_top_node   TYPE tm_nodekey.
    METHODS create_example_nodes .
    METHODS create_saved_formula_node
      IMPORTING
        !is_formula    TYPE zdbbr_ffdef
        !iv_top_node   TYPE tm_nodekey
        !iv_dnd_handle TYPE i.
    METHODS get_next_node_key
      RETURNING
        VALUE(rv_nodekey) TYPE tm_nodekey .
    METHODS create_simple_folder_node
      IMPORTING
        !iv_parent_node        TYPE tm_nodekey OPTIONAL
        !iv_description        TYPE string
      RETURNING
        VALUE(rv_new_node_key) TYPE tm_nodekey .
    METHODS create_simple_node
      IMPORTING
        !iv_parent_node TYPE tm_nodekey
        !iv_description TYPE string
        !iv_dnd_handle  TYPE i OPTIONAL
        !ir_user_object TYPE REF TO zcl_dbbr_fe_dnd_object OPTIONAL .
    METHODS create_color_node
      IMPORTING
        !iv_style          TYPE i
        !iv_color_code     TYPE string
      RETURNING
        VALUE(rv_node_key) TYPE tm_nodekey .
    METHODS create_text_node
      IMPORTING
        iv_parent TYPE tm_nodekey
        !iv_text  TYPE string
        !iv_icon  TYPE icon_d DEFAULT icon_space .
    METHODS register_user_object_for_node
      IMPORTING
        !iv_node_key    TYPE tm_nodekey
        !ir_user_object TYPE REF TO zcl_dbbr_fe_dnd_object .
    METHODS create_form_command_node
      IMPORTING
        iv_parent_node  TYPE tm_nodekey
        !iv_keyword     TYPE string
        !iv_description TYPE string
        !iv_handle      TYPE i
        !ir_user_object TYPE REF TO zcl_dbbr_fe_dnd_object .
    METHODS expand_top_nodes.
    METHODS delete_formulas
      IMPORTING
        if_all      TYPE abap_bool
        iv_node_key TYPE tm_nodekey.


    METHODS on_tree_drag
          FOR EVENT drag OF cl_list_tree_model
      IMPORTING
          !drag_drop_object
          !node_key .
    METHODS on_node_context_menu_request
          FOR EVENT node_context_menu_request OF cl_list_tree_model
      IMPORTING
          !menu
          !node_key .
    METHODS on_node_context_menu_select
          FOR EVENT node_context_menu_select OF cl_list_tree_model
      IMPORTING
          !fcode
          !node_key .
    METHODS on_node_double_click
          FOR EVENT node_double_click OF cl_list_tree_model
      IMPORTING
          !node_key .

    METHODS on_node_enter_key
          FOR EVENT node_keypress OF cl_list_tree_model
      IMPORTING
          key
          node_key.
    METHODS on_toolbar_button
          FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
          sender
          fcode.

ENDCLASS.



CLASS zcl_dbbr_fe_dnd_tree_model IMPLEMENTATION.


  METHOD constructor.
    mr_parent = ir_parent.
    mr_tabfield_list = ir_tabfield_list.
    mt_join_tables = it_join_tables.
  ENDMETHOD.


  METHOD create_available_stmtns_nodes.
    mv_availbl_stmnts_top_node = create_simple_folder_node( iv_parent_node = mv_top_node
                                                            iv_description = 'Available Commands'  ).
    LOOP AT zcl_dbbr_fe_templates=>st_valid_keywords_range ASSIGNING FIELD-SYMBOL(<ls_valid_keyword>).
      create_text_node( iv_parent = mv_availbl_stmnts_top_node iv_text = CONV #( <ls_valid_keyword>-low ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD create_color_node.
    DATA(lv_node_key) = get_next_node_key( ).

    mr_element_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

    mr_tree_model->add_node(
      node_key                = lv_node_key
      relative_node_key       = mv_color_top_node
      isfolder                = abap_false
      image                   = |{ icon_space }|
      drag_drop_id            = lv_dnd_handle
      user_object             = NEW zcl_dbbr_fe_dnd_object( iv_text = |'{ iv_color_code }'| )
      item_table              = VALUE treemlitab(
        ( item_name  = 1
          class      = cl_gui_list_tree=>item_class_text
          length     = 5
          text       = iv_color_code )
        ( item_name  = 2
          class      = cl_gui_list_tree=>item_class_text
          length     = 20
          style      = iv_style  )
      )
    ).
  ENDMETHOD.


  METHOD create_color_nodes.
    mv_color_top_node = create_simple_folder_node( iv_parent_node = mv_top_node
                                                   iv_description = 'Useful Colors'  ).
    create_color_node( iv_color_code = 'C300'  iv_style = cl_list_tree_model=>style_emphasized ).
    create_color_node( iv_color_code = 'C500'  iv_style = cl_list_tree_model=>style_emphasized_positive ).
    create_color_node( iv_color_code = 'C100'  iv_style = cl_list_tree_model=>style_emphasized_a ).
    create_color_node( iv_color_code = 'C400'  iv_style = cl_list_tree_model=>style_emphasized_b ).
    create_color_node( iv_color_code = 'C700'  iv_style = cl_list_tree_model=>style_emphasized_c ).
    create_color_node( iv_color_code = 'C600'  iv_style = cl_list_tree_model=>style_emphasized_negative ).
  ENDMETHOD.


  METHOD create_example_nodes.
    mv_examples_top_node = create_simple_folder_node( iv_parent_node = mv_top_node
                                                      iv_description = 'Examples'  ).

    mr_example_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_handle) ).

    create_simple_node(
        iv_parent_node = mv_examples_top_node
        iv_description = 'Short Manual'
        iv_dnd_handle  = lv_handle
        ir_user_object = NEW zcl_dbbr_fe_dnd_object(
           iv_text         = zcl_dbbr_fe_templates=>sv_introduction_template
           if_is_long_text = abap_true )
    ).

    create_simple_node(
        iv_parent_node = mv_examples_top_node
        iv_description = 'Calculate Difference'
        iv_dnd_handle  = lv_handle
        ir_user_object = NEW zcl_dbbr_fe_dnd_object(
           iv_text         = zcl_dbbr_fe_templates=>sv_calc_difference_exmple
           if_is_long_text = abap_true )

    ).

    create_simple_node(
        iv_parent_node = mv_examples_top_node
        iv_description = 'Add Icon to the List'
        iv_dnd_handle  = lv_handle
        ir_user_object = NEW zcl_dbbr_fe_dnd_object(
           iv_text         = zcl_dbbr_fe_templates=>sv_icon_example
           if_is_long_text = abap_true )
    ).

    create_simple_node(
        iv_parent_node = mv_examples_top_node
        iv_description = 'Add Icon with Quickinfo to the List'
        iv_dnd_handle  = lv_handle
        ir_user_object = NEW zcl_dbbr_fe_dnd_object(
           iv_text         = zcl_dbbr_fe_templates=>sv_icon_quicktip_example
           if_is_long_text = abap_true )
    ).
  ENDMETHOD.


  METHOD create_formula_def_templates.
    mv_formula_def_top_node = create_simple_folder_node( iv_parent_node = mv_top_node
                                                             iv_description = 'Definitions of Formulas'  ).

    mr_element_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_handle) ).

    create_form_command_node(
        iv_parent_node  = mv_formula_def_top_node
        iv_keyword      = zif_dbbr_c_fe_keywords=>define_field
        iv_description  = 'Define Formula Field'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_form_field_tmplt ) ).
    create_form_command_node(
        iv_parent_node  = mv_formula_def_top_node
        iv_keyword      = space
        iv_description  = 'Define Formula Field with Data Element Type'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_form_field_rllnam_tmplt ) ).
    create_form_command_node(
        iv_parent_node  = mv_formula_def_top_node
        iv_keyword      = zif_dbbr_c_fe_keywords=>define_description
        iv_description  = 'Define Heading for Formula Field'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_text_for_field_tmplt ) ).
    create_form_command_node(
        iv_parent_node  = mv_formula_def_top_node
        iv_keyword      = zif_dbbr_c_fe_keywords=>define_icon
        iv_description  = 'Define Icon Formula Field'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_icon_field_tmplt ) ).
    create_form_command_node(
        iv_parent_node  = mv_formula_def_top_node
        iv_keyword      = zif_dbbr_c_fe_keywords=>define_icon_quick
        iv_description  = 'Define Icon Formula Field with Quickinfo'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_icon_tt_field_tmplt ) ).

  ENDMETHOD.


  METHOD create_form_command_node.
    DATA(lv_node_key) = get_next_node_key( ).

    mr_tree_model->add_node(
      node_key                = lv_node_key
      relative_node_key       = iv_parent_node
      relationship            = cl_item_tree_model=>relat_last_child
      isfolder                = abap_false
      drag_drop_id            = iv_handle
      image                   = |{ icon_space }|
      user_object             = ir_user_object
      item_table              = VALUE treemlitab(
       ( item_name  = 1
         class      = cl_gui_list_tree=>item_class_text
         font       = cl_gui_list_tree=>item_font_fixed
         length     = 20
         text       = iv_keyword )
       ( item_name  = 2
         class      = cl_gui_list_tree=>item_class_text
         font       = cl_gui_list_tree=>item_font_prop
         length     = 60
         text       = iv_description )
      )
    ).
  ENDMETHOD.


  METHOD create_icon_nodes.
    mv_icon_top_node = create_simple_folder_node( iv_parent_node = mv_top_node
                                                  iv_description = 'Useful Icons'  ).

    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_okay             iv_text = 'ICON_OKAY' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_cancel           iv_text = 'ICON_CANCEL' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_sum              iv_text = 'ICON_SUM' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_sum_red          iv_text = 'ICON_SUM_RED' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_arrow_left       iv_text = 'ICON_ARROW_LEFT' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_arrow_right      iv_text = 'ICON_ARROW_RIGHT' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_location         iv_text = 'ICON_LOCATION' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_release          iv_text = 'ICON_RELEASE' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_defect           iv_text = 'ICON_DEFECT' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_alert            iv_text = 'ICON_ALERT' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_alarm            iv_text = 'ICON_ALARM' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_warning          iv_text = 'ICON_WARNING' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_hint             iv_text = 'ICON_HINT' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_breakpoint       iv_text = 'ICON_BREAKPOINT' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_system_help      iv_text = 'ICON_SYSTEM_HELP' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_led_green        iv_text = 'ICON_LED_GREEN' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_led_red          iv_text = 'ICON_LED_RED' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_led_yellow       iv_text = 'ICON_LED_YELLOW' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_date             iv_text = 'ICON_DATE' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_transport        iv_text = 'ICON_TRANSPORT' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_trend_up         iv_text = 'ICON_TREND_UP' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_trend_rising     iv_text = 'ICON_TREND_RISING' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_trend_unchanged  iv_text = 'ICON_TREND_UNCHANGED' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_trend_decreasing iv_text = 'ICON_TREND_DECREASING' ).
    create_text_node( iv_parent = mv_icon_top_node iv_icon = icon_trend_down       iv_text = 'ICON_TREND_DOWN' ).
  ENDMETHOD.


  METHOD create_row_nodes.
    DATA(lr_fields) = mr_tabfield_list->get_fields_ref( ).

    mr_element_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_handle) ).

    LOOP AT lr_fields->* ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE is_text_field = abap_false
                                                              AND is_formula_field = abap_false
      GROUP BY ( alias   = <ls_field>-alias
                 tabname = <ls_field>-tabname )

      ASSIGNING FIELD-SYMBOL(<ls_field_group>).

      DATA(lv_tab_description) = zcl_dbbr_dictionary_helper=>get_table_info( <ls_field_group>-tabname )-ddtext.

      DATA(lt_fields) = VALUE zdbbr_tabfield_info_ui_itab( FOR <ls_fld> IN GROUP <ls_field_group> ( <ls_fld> ) ).

      DATA(lr_table_func_executor) = NEW zcl_uitb_table_func_executor( ir_table = REF #( lt_fields ) ).
      DATA(lv_max_compname_length) = lr_table_func_executor->max_length( 'FIELDNAME' ).

      " create table node
      DATA(lv_table_node) = create_simple_folder_node(
          iv_parent_node  = mv_top_node
          iv_description  = COND #( WHEN <ls_field_group>-alias IS NOT INITIAL THEN
                                      |ROW-{ <ls_field_group>-alias }-Felder - { <ls_field_group>-tabname } - { lv_tab_description }|
                                    ELSE
                                      |ROW-Felder - { <ls_field_group>-tabname } - { lv_tab_description }| )
      ).

      DATA(lv_alias) = COND #( WHEN <ls_field_group>-alias IS NOT INITIAL THEN |{ <ls_field_group>-alias }-| ).

      SORT lt_fields BY ddic_order ASCENDING.

      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_tabfield>).
        DATA(lv_node_key) = get_next_node_key( ).

        mr_tree_model->add_node(
          node_key                = lv_node_key
          relative_node_key       = lv_table_node
          relationship            = cl_list_tree_model=>relat_last_child
          isfolder                = abap_false
          image                   = |{ icon_space }|
          drag_drop_id            = lv_handle
          user_object             = NEW zcl_dbbr_fe_dnd_object( iv_text = |ROW-{ lv_alias }{ <ls_tabfield>-fieldname }| )
          item_table              = VALUE treemlitab(
             ( item_name  = 1
               class      = cl_gui_list_tree=>item_class_text
               font       = cl_gui_list_tree=>item_font_fixed
               length     = lv_max_compname_length
               text       = <ls_tabfield>-fieldname )
             ( item_name  = 2
               class      = cl_gui_list_tree=>item_class_text
               font       = cl_gui_list_tree=>item_font_fixed
               length     = 5
               text       = <ls_tabfield>-datatype )
             ( item_name  = 3
               class      = cl_gui_list_tree=>item_class_text
               font       = cl_gui_list_tree=>item_font_fixed
               length     = 5
               text       = |{ <ls_tabfield>-length ALPHA = OUT }| )
             ( item_name  = 4
               class      = cl_gui_list_tree=>item_class_text
               font       = cl_gui_list_tree=>item_font_prop
               length     = 40
               text       = <ls_tabfield>-field_ddtext )
          )
        ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_saved_formula_node.
    DATA(lv_node_key) = get_next_node_key( ).

    mr_tree_model->add_node(
      node_key                = lv_node_key
      relative_node_key       = iv_top_node
      relationship            = cl_item_tree_model=>relat_last_child
      isfolder                = abap_false
      image                   = |{ icon_space }|
      drag_drop_id            = iv_dnd_handle
      user_object             = NEW zcl_dbbr_fe_dnd_object(
                                    iv_text         = is_formula-formula_string
                                    if_is_long_text = abap_true
                                    iv_db_id        = CONV #( is_formula-id ) )
      item_table              = VALUE treemlitab(
       ( item_name  = 1
         class      = cl_gui_list_tree=>item_class_text
         font       = cl_gui_list_tree=>item_font_prop
         length     = 12
         text       = |{ is_formula-created_date DATE = USER }| )
       ( item_name  = 2
         class      = cl_gui_list_tree=>item_class_text
         font       = cl_gui_list_tree=>item_font_prop
         length     = 12
         text       = |{ is_formula-created_time TIME = USER }| )
       ( item_name  = 3
         class      = cl_gui_list_tree=>item_class_text
         font       = cl_gui_list_tree=>item_font_prop
         length     = 40
         text       = is_formula-description )
      )
    ).

  ENDMETHOD.


  METHOD create_saved_formula_nodes.
    IF if_create_top = abap_true.
      DATA(lv_forms_owner_text) = COND #(
        WHEN iv_user_name = sy-uname THEN
            `from ` && sy-uname
        ELSE
            'from other users'
      ).
      cv_top_node = create_simple_folder_node( iv_parent_node = mv_top_node
                                               iv_description = |Saved Formulas ({ lv_forms_owner_text })|  ).
    ENDIF.

    mr_example_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

    DATA(lt_formulas) = NEW zcl_dbbr_formula_factory( )->get_formulas( iv_user_name ).

    LOOP AT lt_formulas ASSIGNING FIELD-SYMBOL(<ls_formula>).
      create_saved_formula_node(
        is_formula    = <ls_formula>
        iv_top_node   = cv_top_node
        iv_dnd_handle = lv_dnd_handle
      ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create_simple_folder_node.
    rv_new_node_key = get_next_node_key( ).

    mr_tree_model->add_node(
      node_key                = rv_new_node_key
      relative_node_key       = iv_parent_node
      relationship            = cl_item_tree_model=>relat_last_child
      isfolder                = abap_true
      item_table              = VALUE treemlitab(
        ( item_name  = c_column_names-favorite_id
          class      = cl_list_tree_model=>item_class_text
          font       = cl_list_tree_model=>item_font_prop
          alignment  = cl_list_tree_model=>align_auto
          text       = iv_description
        )
      )
    ).
  ENDMETHOD.


  METHOD create_simple_node.
    DATA(lv_node_key) = get_next_node_key( ).

    mr_tree_model->add_node(
      node_key                = lv_node_key
      relative_node_key       = iv_parent_node
      relationship            = cl_item_tree_model=>relat_last_child
      isfolder                = abap_false
      image                   = |{ icon_space }|
      drag_drop_id            = iv_dnd_handle
      user_object             = ir_user_object
      item_table              = VALUE treemlitab(
        ( item_name  = c_column_names-favorite_id
          class      = cl_list_tree_model=>item_class_text
          font       = cl_list_tree_model=>item_font_prop
          alignment  = cl_list_tree_model=>align_auto
          length     = 60
          text       = iv_description
        )
      )
    ).
  ENDMETHOD.


  METHOD create_special_functions_nodes.
    mv_special_func_top_node = create_simple_folder_node( iv_parent_node = mv_top_node
                                                          iv_description = 'Special Commands and Functions'  ).

    mr_element_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_handle) ).

    create_form_command_node(
        iv_parent_node  = mv_special_func_top_node
        iv_keyword      = zif_dbbr_c_fe_keywords=>set_icon_value
        iv_description  = 'Set Icon with Quickinfo'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_set_icon_tmplt ) ).
    create_form_command_node(
        iv_parent_node  = mv_special_func_top_node
        iv_keyword      = zif_dbbr_c_fe_keywords=>set_row_color
        iv_description  = 'Color the current Row'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_set_row_color_template ) ).
    create_form_command_node(
        iv_parent_node  = mv_special_func_top_node
        iv_keyword      = zif_dbbr_c_fe_keywords=>set_cell_color
        iv_description  = 'Color a Cell in the current Row'
        iv_handle       = lv_handle
        ir_user_object  = NEW #( iv_text = zcl_dbbr_fe_templates=>sv_set_cell_color_template ) ).
  ENDMETHOD.


  METHOD create_text_node.
    DATA(lv_node_key) = get_next_node_key( ).

    mr_element_dnd_behaviour->get_handle( IMPORTING handle = DATA(lv_dnd_handle) ).

    mr_tree_model->add_node(
      node_key                = lv_node_key
      relative_node_key       = iv_parent
      relationship            = cl_item_tree_model=>relat_last_child
      isfolder                = abap_false
      image                   = CONV #( iv_icon )
      drag_drop_id            = lv_dnd_handle
      user_object             = NEW zcl_dbbr_fe_dnd_object( iv_text = iv_text )
      item_table              = VALUE treemlitab(
       ( item_name  = 1
         class      = cl_gui_list_tree=>item_class_text
         font       = cl_gui_list_tree=>item_font_prop
         length     = 30
         text       = iv_text )
      )
    ).

  ENDMETHOD.


  METHOD create_top_node.
    mv_top_node = create_simple_folder_node( 'Drag and Drop'  ).
  ENDMETHOD.


  METHOD create_tree.
    DATA: lt_events TYPE cntl_simple_events.

    CHECK mr_tree_model IS INITIAL.

    mr_tree_model = NEW cl_list_tree_model(
        node_selection_mode         = cl_list_tree_model=>node_sel_mode_single
        item_selection              = abap_false
        with_headers                = abap_false
    ).

    """ create drag-n-drop behaviour object
    mr_example_dnd_behaviour = NEW #( ).

    mr_example_dnd_behaviour->add(
        flavor          = zcl_uitb_gui_code_editor=>c_dnd_flavor-replace
        dragsrc         = abap_true
        droptarget      = abap_false
        effect          = cl_dragdrop=>copy
    ).

    mr_element_dnd_behaviour = NEW #( ).
    mr_element_dnd_behaviour->add(
        flavor          = zcl_uitb_gui_code_editor=>c_dnd_flavor-insert
        dragsrc         = abap_true
        droptarget      = abap_false
        effect          = cl_dragdrop=>copy
    ).

    mr_tree_wrapper = NEW zcl_dbbr_list_tree_wrapper( mr_tree_model ).


    mr_tree_model->add_key_stroke( key = cl_list_tree_model=>key_enter ).

    lt_events = VALUE #(
       ( eventid = cl_list_tree_model=>eventid_node_context_menu_req  appl_event = abap_true )
       ( eventid = cl_list_tree_model=>eventid_node_double_click      appl_event = abap_true )
       ( eventid = cl_list_tree_model=>eventid_node_keypress          appl_event = abap_true )
    ).

    mr_tree_model->set_registered_events( lt_events ).

    " create tree control from model
    mr_tree_model->create_tree_control(
      EXPORTING parent  = mr_parent
      IMPORTING control = mr_tree_control
    ).

    """ set event handler methods to tree control
    SET HANDLER:
      on_tree_drag FOR mr_tree_model,
      on_node_context_menu_request FOR mr_tree_model,
      on_node_context_menu_select FOR mr_tree_model,
      on_node_double_click FOR mr_tree_model,
      on_node_enter_key FOR mr_tree_model
    .
  ENDMETHOD.


  METHOD delete_formulas.
    DATA(lv_query) = COND #( WHEN if_all = abap_true THEN
                               |Are you sure you want to delete all saved Formulas?|
                             ELSE
                               |Are you sure you want to delete the selected Formula?| ).

    CHECK zcl_dbbr_appl_util=>popup_to_confirm(
            iv_title                 = 'Delete Formulas'
            iv_query                 = lv_query
            if_display_cancel_button = abap_false
            iv_icon_type             = 'ICON_INFORMATION'
    ) = '1'.

    " delete the selected of all formulas for the current user
    DATA(lr_formula_f) = NEW zcl_dbbr_formula_factory( ).

    IF if_all = abap_true.
      mr_tree_model->node_get_children(
        EXPORTING node_key       = mv_saved_formulas_top_node
        IMPORTING node_key_table = DATA(lt_children)
      ).
      IF lt_children IS NOT INITIAL.
        mr_tree_model->delete_nodes( lt_children ).
        lr_formula_f->delete_formulas_for_user( ).
      ENDIF.
    ELSE.
      mr_tree_model->node_get_user_object(
        EXPORTING node_key    = iv_node_key
        IMPORTING user_object = DATA(lr_user_object)
      ).
      IF lr_user_object IS BOUND.
        DATA(lr_fe_dnd_obj) = CAST zcl_dbbr_fe_dnd_object( lr_user_object ).
        DATA(lv_id) = lr_fe_dnd_obj->get_db_id( ).

        mr_tree_model->delete_node( iv_node_key ).
        lr_formula_f->delete_formula_by_id( CONV #( lv_id ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD expand_top_nodes.
    mr_tree_model->expand_nodes(
        VALUE #(
          ( mv_top_node              )
          ( mv_examples_top_node     )
          ( mv_color_top_node        )
          ( mv_formula_def_top_node  )
          ( mv_special_func_top_node )
          ( mv_saved_formulas_top_node )
        )
    ).
  ENDMETHOD.


  METHOD fill_tree.
    create_top_node( ).
    create_example_nodes( ).
    create_formula_def_templates( ).
    create_available_stmtns_nodes( ).
    create_special_functions_nodes( ).
    create_row_nodes( ).
    create_icon_nodes( ).
    create_color_nodes( ).
    create_saved_formula_nodes(
      EXPORTING if_create_top = abap_true
                iv_user_name  = sy-uname
      CHANGING  cv_top_node   = mv_saved_formulas_top_node
    ).
    create_saved_formula_nodes(
      EXPORTING if_create_top = abap_true
      CHANGING  cv_top_node   = mv_other_saved_forms_top_node
    ).
  ENDMETHOD.


  METHOD free.

  ENDMETHOD.


  METHOD get_next_node_key.
    ADD 1 TO mv_current_node_index.
    rv_nodekey = mv_current_node_index.
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    mr_tree_model->node_get_parent(
      EXPORTING node_key        = node_key
      IMPORTING parent_node_key = DATA(lv_parent_node)
    ).

    IF node_key = mv_saved_formulas_top_node.
      menu->add_function(
          fcode  = 'DEL_SAVED_FORM_ALL'
          text   = 'Delete All Formulas'
      ).
    ENDIF.

    IF lv_parent_node <> mv_saved_formulas_top_node.
      RETURN.
    ENDIF.

    menu->add_function(
        fcode             = 'DEL_SAVED_FORM'
        text              = 'Delete Formula'
    ).
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    CASE fcode.

      WHEN 'DEL_SAVED_FORM'.
        delete_formulas( if_all      = abap_false
                         iv_node_key = node_key ).

      WHEN 'DEL_SAVED_FORM_ALL'.
        delete_formulas( if_all      = abap_true
                         iv_node_key = node_key  ).

    ENDCASE.
  ENDMETHOD.


  METHOD on_node_double_click.
    DATA(lr_selected_node) = mr_tree_wrapper->get_selected_node( ).
    IF lr_selected_node->is_folder( ).
      " check if folder is expanded/collapsed
      IF mr_tree_wrapper->is_node_expanded( lr_selected_node->get_node_key( ) ).
        mr_tree_model->collapse_node( node_key ).
      ELSE.
        mr_tree_model->expand_node( node_key ).
      ENDIF.
    ELSE.
      mr_tree_model->node_get_user_object(
        EXPORTING node_key     = node_key
        IMPORTING user_object  = DATA(lr_user_object)
      ).
      IF lr_user_object IS BOUND.
        DATA(lr_fe_dnd_object) = CAST zcl_dbbr_fe_dnd_object( lr_user_object ).
        IF lr_fe_dnd_object->is_long_text( ).
          NEW zcl_uitb_popup_editor( iv_text = lr_fe_dnd_object->get_text( ) )->zif_uitb_view~show( ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD on_node_enter_key.
    on_node_double_click( node_key ).
  ENDMETHOD.


  METHOD on_toolbar_button.

  ENDMETHOD.


  METHOD on_tree_drag.
    mr_tree_model->node_get_user_object(
      EXPORTING node_key    = node_key
      IMPORTING user_object = DATA(lr_userobject)
    ).

    IF lr_userobject IS INITIAL.
      drag_drop_object->abort( ).
    ELSE.
      drag_drop_object->object = lr_userobject.
    ENDIF.
  ENDMETHOD.


  METHOD refresh_saved_formulas.
    DATA: lt_node_table TYPE treev_ntab,
          lt_item_table TYPE zdbbr_treeitem_itab.

    mr_tree_model->node_get_children(
      EXPORTING node_key       = mv_saved_formulas_top_node
      IMPORTING node_key_table = DATA(lt_saved_formula_nodes)
    ).

    IF lt_saved_formula_nodes IS NOT INITIAL.
      mr_tree_model->delete_nodes( lt_saved_formula_nodes ).
    ENDIF.

    create_saved_formula_nodes(
      EXPORTING if_create_top = abap_false
                iv_user_name  = sy-uname
      CHANGING  cv_top_node   = mv_saved_formulas_top_node
    ).

    mr_tree_model->expand_node( mv_saved_formulas_top_node ).

  ENDMETHOD.


  METHOD register_user_object_for_node.

  ENDMETHOD.


  METHOD show.
    create_tree( ).
    fill_tree( ).

    expand_top_nodes( ).
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.

  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.

  ENDMETHOD.
ENDCLASS.
