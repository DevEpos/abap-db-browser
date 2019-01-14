CLASS zcl_dbbr_selscreen_util DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_dbbr_base_select_tc .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_screen_util
      ABSTRACT METHODS handle_ui_function .
    INTERFACES zif_dbbr_screen_table_util .

    ALIASES free_resources
      FOR zif_dbbr_screen_util~free_resources .
    ALIASES get_deactivated_functions
      FOR zif_dbbr_screen_util~get_deactivated_functions .
    ALIASES handle_pbo
      FOR zif_dbbr_screen_util~handle_pbo .
    ALIASES handle_table_pbo
      FOR zif_dbbr_screen_table_util~handle_pbo .
    ALIASES handle_ui_function
      FOR zif_dbbr_screen_util~handle_ui_function .

    EVENTS request_new_entity
      EXPORTING
        VALUE(ev_id) TYPE zdbbr_entity_id
        VALUE(ev_type) TYPE zdbbr_entity_type .
    CLASS-EVENTS entity_deleted
      EXPORTING
        VALUE(ev_entity_id) TYPE zdbbr_entity_id
        VALUE(ev_entity_type) TYPE zdbbr_entity_type .

    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">Choose select option</p>
    CLASS-METHODS choose_sel_option
      RETURNING
        VALUE(rs_chosen_option) TYPE se16n_sel_option .
    METHODS check_edit_mode .
    METHODS check_mandatory_fields .
    METHODS check_primary_entity
      RETURNING
        VALUE(rf_success) TYPE abap_bool .
    METHODS choose_sort_fields .
    METHODS choose_tabfields
      IMPORTING
        !iv_mode                 TYPE zdbbr_field_chooser_mode
      RETURNING
        VALUE(rf_fields_updated) TYPE abap_bool.
    METHODS clear .
    METHODS constructor
      IMPORTING
        !ir_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data
        !iv_entity_type    TYPE zdbbr_entity_type .
    METHODS delete_join_definition .
    METHODS get_entity_information
      EXPORTING
        !ev_entity      TYPE tabname
        ev_entity_raw   TYPE zdbbr_entity_id_raw
        !ev_type        TYPE zdbbr_favmenu_type
        !ev_description TYPE ddtext .
    METHODS get_id_for_variant .
    METHODS get_title
          ABSTRACT
      RETURNING
        VALUE(result) TYPE string .
    METHODS has_content
      RETURNING
        VALUE(rf_has_content) TYPE abap_bool .
    METHODS is_entity_loaded
      IMPORTING
        !iv_entity_id   TYPE zdbbr_entity_id
        !iv_entity_type TYPE zdbbr_entity_type
      RETURNING
        VALUE(result)   TYPE abap_bool .
    METHODS load_entity
          ABSTRACT
      RETURNING
        VALUE(rf_entity_loaded) TYPE abap_bool .
    METHODS set_custom_functions
        ABSTRACT .
    METHODS update_description_texts
        ABSTRACT .
    METHODS update_entity_type_sh .
    METHODS update_join_definition .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_table_input,
        tablename       TYPE tabname,
        fields          TYPE zdbbr_dfies_itab,
        is_cond_table   TYPE abap_bool,
        is_output       TYPE abap_bool,
        is_selection    TYPE abap_bool,
        selection_order TYPE tabfdpos,
      END OF ty_table_input .

    DATA mr_custom_toolbar_cont TYPE REF TO cl_gui_container .
    DATA mr_custom_toolbar TYPE REF TO cl_gui_toolbar .
    DATA mr_data TYPE REF TO zcl_dbbr_selscreen_data .
    DATA mr_altcoltext_f TYPE REF TO zcl_dbbr_altcoltext_factory .
    DATA mr_favmenu_f TYPE REF TO zcl_dbbr_favmenu_factory .
    DATA mr_usersettings_f TYPE REF TO zcl_dbbr_usersettings_factory .
    DATA ms_join_def_old TYPE zdbbr_join_data .
    DATA mv_entity_id TYPE zdbbr_entity_id .
    DATA mv_entity_type TYPE zdbbr_entity_type .
    DATA mr_custom_menu TYPE REF TO cl_ctmenu .
    CLASS-DATA gr_delete_tb_menu TYPE REF TO cl_ctmenu .

    METHODS update_multiple_table_buttons
      IMPORTING
        !if_has_multiple_tables TYPE abap_bool OPTIONAL .
    METHODS clear_edit_flags .
    METHODS convert_to_selfield
      IMPORTING
        !is_tablefield     TYPE zdbbr_tabfield_info_ui
      RETURNING
        VALUE(rs_selfield) TYPE zdbbr_selfield .
    METHODS create_table_fields
      IMPORTING
        !iv_tablename         TYPE tabname
        !if_is_primary        TYPE abap_bool OPTIONAL
        !if_conditional_table TYPE abap_bool OPTIONAL
        !if_mark_for_output   TYPE boolean OPTIONAL
        !if_selection_active  TYPE boolean OPTIONAL
        !iv_selection_order   TYPE tabfdpos OPTIONAL
      RETURNING
        VALUE(rs_entity_info) TYPE zdbbr_entity_info .
    METHODS create_table_header
      IMPORTING
        !iv_tabname            TYPE tabname
        !iv_tabname_alias      TYPE tabname
        !iv_typename           TYPE zdbbr_entity_type OPTIONAL
      RETURNING
        VALUE(rs_table_header) TYPE zdbbr_selfield .
    METHODS fill_selection_mask .
    METHODS fill_table
        ABSTRACT .
    METHODS fill_toolbar
      IMPORTING
        !if_create_extended_search TYPE abap_bool OPTIONAL .
    METHODS finish_loading .
    METHODS notify_of_deleted_entity
      IMPORTING
        !iv_entity_id   TYPE zdbbr_entity_id
        !iv_entity_type TYPE zdbbr_entity_type .
    METHODS update_alv_variant .
    METHODS update_multi_selection_mask .
    METHODS update_selection_mask .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_selscreen_util IMPLEMENTATION.


  METHOD check_edit_mode ##needed.
  ENDMETHOD.


  METHOD check_mandatory_fields.
  ENDMETHOD.


  METHOD check_primary_entity.
    rf_success = abap_true.
  ENDMETHOD.


  METHOD choose_sort_fields.
    FIELD-SYMBOLS: <lr_tabfields> TYPE REF TO zcl_dbbr_tabfield_list.

    DATA(lf_field_aggr_active) = mr_data->mr_selection_table->aggregation_is_active( ).
    IF lf_field_aggr_active = abap_true.
      ASSIGN mr_data->mr_tabfield_list TO <lr_tabfields>.
    ELSE.
      ASSIGN mr_data->mr_tabfield_list TO <lr_tabfields>.
    ENDIF.

    DATA(lr_sort_controller) = NEW zcl_dbbr_field_sorter_ctrl(
      ir_fields = <lr_tabfields>
    ).

    lr_sort_controller->zif_uitb_screen_controller~call_screen( ).

    IF lr_sort_controller->was_updated( ).
      <lr_tabfields>->overwrite( lr_sort_controller->get_updated_tabfields( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD choose_tabfields.
    FIELD-SYMBOLS: <lr_tabfields> TYPE REF TO zcl_dbbr_tabfield_list.

    DATA(lr_tabfields) = NEW zcl_dbbr_tabfield_list( ).

    DATA(lf_field_aggr_active) = mr_data->mr_selection_table->aggregation_is_active( ).
    IF lf_field_aggr_active = abap_true AND iv_mode = zif_dbbr_global_consts=>gc_field_chooser_modes-output.
      ASSIGN mr_data->mr_tabfield_aggr_list TO <lr_tabfields>.
    ELSE.
      ASSIGN mr_data->mr_tabfield_list TO <lr_tabfields>.
    ENDIF.

    DATA(lv_old_mode) = <lr_tabfields>->get_mode( ).
    <lr_tabfields>->switch_mode( iv_mode ).

    DATA(lr_fields) = <lr_tabfields>->copy( ).
*... delete parameter fields from list if there are any
    lr_fields->delete_where_in_tablist( VALUE #( ( sign = 'I' option = 'EQ' low = zif_dbbr_global_consts=>c_parameter_dummy_table ) ) ).

    DATA(lr_tabfield_manager) = NEW zcl_dbbr_tabfield_manager(
      ir_fields            = lr_fields
      is_join_def          = mr_data->mr_s_join_def->*
      if_field_aggregation = lf_field_aggr_active
      iv_mode              = iv_mode
      iv_entity_type       = mv_entity_type
    ).

*... call main screen for field chooser
    lr_tabfield_manager->zif_uitb_screen_controller~call_screen( ).

**... get chosen fields if screen was not cancelled, otherwise leave ct_fields unchanged
    IF lr_tabfield_manager->data_should_be_transferred( ).
      rf_fields_updated = abap_true.
      <lr_tabfields>->overwrite( lr_tabfield_manager->retrieve_current_data( ) ).
    ENDIF.

    IF iv_mode <> zif_dbbr_global_consts=>gc_field_chooser_modes-selection OR
       rf_fields_updated = abap_false.
      RETURN.
    ENDIF.

*... reset hiding of table fields to prevent unwanted behavior
    mr_data->mr_selection_table->expand_all_hidden_fields( ).

    fill_selection_mask( ).
  ENDMETHOD.


  METHOD class_constructor.
*.. create/fill delete menu
    gr_delete_tb_menu = NEW #( ).
    gr_delete_tb_menu->add_function(
       fcode = zif_dbbr_c_selscreen_functions=>delete_aggregations
       text  = 'Delete all aggregations'
    ).
    gr_delete_tb_menu->add_function(
       fcode = zif_dbbr_c_selscreen_functions=>delete_all_or_tuple
       text  = 'Delete all OR Tuple'
    ).

    DATA(lr_table_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_table_tb( ).
    lr_table_toolbar->add_button_group(
        data_table = VALUE #(
          ( butn_type = cntb_btype_button
            icon      = icon_expand_all
            quickinfo = 'Expand all Tables'
            function  = zif_dbbr_c_selscreen_functions=>expand_all_tables )
          ( butn_type = cntb_btype_button
            icon      = icon_collapse_all
            quickinfo = 'Collapse all tables'
            function  = zif_dbbr_c_selscreen_functions=>collapse_all_tables )
          ( butn_type = cntb_btype_sep )
          ( butn_type = cntb_btype_button
            icon      = icon_previous_page
            quickinfo = 'Go to previous table'
            function  = zif_dbbr_c_selscreen_functions=>go_to_previous_table )
          ( butn_type = cntb_btype_button
            icon      = icon_next_page
            quickinfo = 'Go to next table'
            function  = zif_dbbr_c_selscreen_functions=>go_to_next_table )
          ( butn_type = cntb_btype_sep )
          ( butn_type = cntb_btype_button
            icon      = icon_delete_row
            quickinfo = 'Delete current line values'
            function  = zif_dbbr_c_selscreen_functions=>delete_line_input )
          ( butn_type = cntb_btype_dropdown
            text      = 'All Entries'
            icon      = icon_delete_row
            function  = zif_dbbr_c_selscreen_functions=>delete_all_input )
          ( butn_type = cntb_btype_sep )
          ( butn_type = cntb_btype_button
            quickinfo = 'Choose Selection Fields'
            icon      = icon_align
            function  = zif_dbbr_c_selscreen_functions=>control_sel_fields )
          ( butn_type = cntb_btype_button
            quickinfo = 'Technical View On/Off'
            icon      = icon_tools
            function  = zif_dbbr_c_selscreen_functions=>activate_tech_view )
        )
    ).

    lr_table_toolbar->set_static_ctxmenu(
        fcode   = zif_dbbr_c_selscreen_functions=>delete_all_input
        ctxmenu = gr_delete_tb_menu
    ).
  ENDMETHOD.

  METHOD choose_sel_option.
*& Description: Shows pop-up for choosing a select option (e.g. EQ, BT, ...)
*&---------------------------------------------------------------------*
    DATA: lt_sel_option TYPE STANDARD TABLE OF se16n_sel_option,
          lt_fieldcat   TYPE lvc_t_fcat.

    lt_sel_option = VALUE #(
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-default
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-bt
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-cp
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-np
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-eq
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-nb
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-ne
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-gt
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-lt
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-ge
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-le
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-is_null
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-is_not_null
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-bt
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-cp
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-np
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-eq
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-nb
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-ne
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-gt
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-lt
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-ge
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
      ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-le
                                                       iv_sign      = zif_dbbr_global_consts=>gc_options-e ) )
    ).

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_buffer_active        = ' '
        i_structure_name       = 'SE16N_SEL_OPTION'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      CASE <ls_fieldcat>-fieldname.
        WHEN 'SIGN'.
          <ls_fieldcat>-no_out = abap_true.
        WHEN 'OPTION'.
          <ls_fieldcat>-no_out = abap_true.
        WHEN 'ICON'.
          <ls_fieldcat>-outputlen = 2.
      ENDCASE.
    ENDLOOP.

    DATA: ls_selfield TYPE slis_selfield,
          lf_exit     TYPE abap_bool.

*.. Show popup with the options and give one back
    DATA(lt_status_exclude) = VALUE lvc_t_excl(
      ( func = '&OL0' ) ( func = '&ELP' ) ( func = '&OAD' ) ( func = '&RNT' ) ( func = '&AVE' )
    ).
    CALL FUNCTION 'LVC_SINGLE_ITEM_SELECTION'
      EXPORTING
        i_title         = |{ 'Choose Option'(001) }|
        it_fieldcatalog = lt_fieldcat
        it_status_excl  = lt_status_exclude
      IMPORTING
        es_selfield     = ls_selfield
        e_exit          = lf_exit
      TABLES
        t_outtab        = lt_sel_option.

    IF lf_exit <> abap_true.
      TRY .
          rs_chosen_option = lt_sel_option[ ls_selfield-tabindex ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.



  METHOD clear.
    CLEAR: ms_join_def_old,
           mv_entity_id.

*... Clear all data from data class
    mr_data->clear( ).

    DATA(lr_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_entity_tb( ).
    lr_toolbar->set_button_visible( fcode = 'CUST_FUNC' visible = abap_false ).
  ENDMETHOD.


  METHOD clear_edit_flags.
    CLEAR: mr_data->mr_s_global_data->edit,
           mr_data->mr_s_global_data->delete_mode.
  ENDMETHOD.


  METHOD constructor.
    mr_data = ir_selscreen_data.
    mv_entity_type = iv_entity_type.
    mr_altcoltext_f = NEW #( ).
    mr_favmenu_f = NEW #( ).
    mr_usersettings_f = NEW #( ).

*... update entity type table fields
    mr_data->mr_tabfield_aggr_list->set_entity_type( mv_entity_type ).
    mr_data->mr_tabfield_list->set_entity_type( mv_entity_type ).

    zcl_dbbr_toolbar_util=>get_selscreen_table_tb( )->set_button_state( enabled = abap_true fcode = zif_dbbr_c_selscreen_functions=>control_sel_fields ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD convert_to_selfield.
    rs_selfield = CORRESPONDING #(
      is_tablefield
      MAPPING lowercase        = is_lowercase
              key              = is_key
              option           = default_option
              sign             = default_sign
              low              = default_low
              has_cust_f4_help = has_custom_f4

    ).

*... fill descriptions
    IF is_tablefield-alt_long_text IS NOT INITIAL.
      rs_selfield-description = is_tablefield-alt_long_text.
    ELSEIF is_tablefield-std_long_text IS NOT INITIAL.
      rs_selfield-description = is_tablefield-std_long_text.
    ELSEIF is_tablefield-field_ddtext IS NOT INITIAL.
      rs_selfield-description = is_tablefield-field_ddtext.
    ELSE.
      rs_selfield-description = is_tablefield-fieldname_raw.
    ENDIF.


  ENDMETHOD.


  METHOD create_table_fields.
*&---------------------------------------------------------------------*
*& Description: Creates table fields for given table name
*&---------------------------------------------------------------------*
    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( iv_tablename ).
    CHECK ls_table_info IS NOT INITIAL.

*... read custom f4 helps for join table definitions
    mr_data->mr_custom_f4_map->read_custom_f4_definitions( iv_tablename ).

    DATA(lv_join_table_text) = ls_table_info-ddtext.

    zcl_dbbr_dictionary_helper=>get_table_field_infos( EXPORTING iv_tablename    = iv_tablename
                                                       IMPORTING et_table_fields = DATA(lt_dfies) ).

    DATA(lr_addtext_bl) = zcl_dbbr_addtext_bl=>get_instance( ).

    DATA(lf_first_non_key_processed) = abap_false.

*... build tablefield table
    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_data_element_field>) WHERE datatype <> 'CLNT'.
      DATA(lf_output) = abap_false.
      DATA(ls_altcoltext) = mr_altcoltext_f->find_alternative_text( iv_tabname   = <ls_data_element_field>-tabname
                                                                    iv_fieldname = <ls_data_element_field>-fieldname ).

      DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
        convexit              = <ls_data_element_field>-convexit
        fieldname_raw         = <ls_data_element_field>-fieldname
        selection_active      = if_selection_active
        ddic_order            = <ls_data_element_field>-position
        is_lowercase          = <ls_data_element_field>-lowercase
*... check if there is a custom search field for this table
        has_custom_f4         = mr_data->mr_custom_f4_map->entry_exists(
            iv_tabname   = <ls_data_element_field>-tabname
            iv_fieldname = <ls_data_element_field>-fieldname
        )
*... check if field is numeric
        is_numeric            = zcl_dbbr_dictionary_helper=>is_type_numeric( <ls_data_element_field>-inttype )
*... is there an F4-help for this table field
        f4_available          = <ls_data_element_field>-f4availabl
        field_ddtext          = COND #( WHEN <ls_data_element_field>-scrtext_l IS INITIAL THEN
                                    <ls_data_element_field>-fieldtext
                                  ELSE
                                    <ls_data_element_field>-scrtext_l )
        is_key                = xsdbool( <ls_data_element_field>-keyflag = abap_true )
*... default sign is inclusive
        default_sign          = zif_dbbr_global_consts=>gc_options-i
        is_virtual_join_field = if_conditional_table
        is_foreign_key        = xsdbool( <ls_data_element_field>-checktable IS NOT INITIAL )
        std_short_text        = <ls_data_element_field>-scrtext_s
        std_medium_text       = <ls_data_element_field>-scrtext_m
        std_long_text         = <ls_data_element_field>-scrtext_l
        alt_long_text         = ls_altcoltext-alt_long_text
        alt_medium_text       = ls_altcoltext-alt_short_text
        length                = <ls_data_element_field>-leng
        outputlen             = <ls_data_element_field>-outputlen
        intlen                = <ls_data_element_field>-intlen
        header_text           = <ls_data_element_field>-reptext
        ref_field             = <ls_data_element_field>-reffield
        ref_tab               = <ls_data_element_field>-reftable
      ).

      ls_tabfield = CORRESPONDING #( BASE ( ls_tabfield ) <ls_data_element_field> ).

      lr_addtext_bl->determine_t_fields_for_tab( is_tabfield_info = <ls_data_element_field> ).


      IF ls_altcoltext-alt_long_text IS NOT INITIAL.
        ls_tabfield-field_ddtext = ls_altcoltext-alt_long_text.
      ENDIF.

      IF mr_data->mr_s_global_data->primary_table = ls_tabfield-tabname OR
         ls_tabfield-is_key = abap_true OR
         ( lf_first_non_key_processed = abap_false AND ls_tabfield-is_key = abap_false ).
        lf_output = abap_true.
      ENDIF.

      IF ls_tabfield-is_key = abap_false.
        lf_first_non_key_processed = abap_true.
      ENDIF.

      ls_tabfield-output_active = lf_output.

      DATA(lr_new_field) = CAST zdbbr_tabfield_info_ui( mr_data->mr_tabfield_list->add( REF #( ls_tabfield ) ) ).

      " only continue if normal field did not already exist
      IF lr_new_field IS NOT INITIAL.
        " there is an existing text field for the current table field
        IF lr_addtext_bl->text_exists( is_data_element_info = <ls_data_element_field> ).

          lr_addtext_bl->add_text_fields_to_list(
            ir_tabfields  = mr_data->mr_tabfield_list
            is_ref_tabfield = ls_tabfield
            if_post_select = if_conditional_table
            iv_position   = <ls_data_element_field>-position
            is_altcoltext = ls_altcoltext
          ).

          " connect text field and key field
          lr_new_field->has_text_field = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    rs_entity_info = VALUE zdbbr_entity_info(
      active_selection     = abap_true
      tabname              = iv_tablename
      tabname_alias        = iv_tablename
      type                 = zif_dbbr_c_entity_type=>table
      description          = ls_table_info-ddtext
      fields_are_loaded    = abap_true
      is_primary           = if_is_primary
    ).

    mr_data->mr_tabfield_list->add_table( rs_entity_info ).

  ENDMETHOD.


  METHOD create_table_header.
    rs_table_header = VALUE zdbbr_selfield(
      tabname              = iv_tabname
      is_table_header      = abap_true
      description          = iv_tabname_alias " |{ lt_fix_vals[ low = lv_typename ]-ddtext } { iv_tabname }|
      fieldname_raw        = iv_tabname
      ddic_order           = 0 ).
  ENDMETHOD.


  METHOD delete_join_definition ##needed.
  ENDMETHOD.


  METHOD fill_selection_mask.
  ENDMETHOD.


  METHOD fill_toolbar.
    DATA: lt_data_table TYPE ttb_button.

    DATA(lr_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_entity_tb( ).
*.. clear all buttons
    lr_toolbar->delete_all_buttons( ).
    cl_gui_cfw=>flush( ).

    IF if_create_extended_search = abap_true AND mr_data->mr_f_from_central_search->* = abap_false.
      lt_data_table = VALUE #(
        ( function  = zif_dbbr_c_selscreen_functions=>open_specific_extended_search
          icon      = icon_extended_search
          quickinfo = 'Extended Search' )
      ).
    ENDIF.

    lr_toolbar->add_button_group(
        data_table = VALUE #( BASE lt_data_table
          ( function  = 'CUST_FUNC'
            icon      = icon_reference_list
            quickinfo = 'Custom Functions'
            butn_type = cntb_btype_menu      )
        )
    ).

*.... set custom menu for cust function
    CHECK mr_custom_menu IS BOUND.

    lr_toolbar->set_static_ctxmenu(
        fcode   = 'CUST_FUNC'
        ctxmenu = mr_custom_menu
    ).

  ENDMETHOD.


  METHOD finish_loading.
    DATA(lr_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_entity_tb( ).

    lr_toolbar->set_button_visible( fcode = 'CUST_FUNC' visible = abap_true ).
  ENDMETHOD.


  METHOD get_entity_information.
    ev_description = mr_data->mr_v_selmask_entity_text->*.
  ENDMETHOD.


  METHOD get_id_for_variant.
  ENDMETHOD.


  METHOD has_content.
    rf_has_content = xsdbool( mv_entity_id IS NOT INITIAL ).
  ENDMETHOD.


  METHOD is_entity_loaded.
    result = xsdbool( mv_entity_id = iv_entity_id AND mv_entity_type = iv_entity_type ).
  ENDMETHOD.


  METHOD notify_of_deleted_entity.
    RAISE EVENT entity_deleted
      EXPORTING
        ev_entity_id   = iv_entity_id
        ev_entity_type = iv_entity_type.
  ENDMETHOD.


  METHOD update_alv_variant.
    CLEAR: mr_data->mr_s_global_data->alv_variant,
           mr_data->mr_s_global_data->alv_varianttext.
  ENDMETHOD.


  METHOD update_entity_type_sh.
    DATA(lr_v_entity_type) = CAST zdbbr_entity_type(
      zcl_uitb_data_cache=>get_instance(
        zif_dbbr_c_report_id=>search_help_exit
      )->get_data_ref( zcl_dbbr_entity_sh_helper=>c_v_entity_type )
    ).

    lr_v_entity_type->* = mr_data->mr_s_entity_info->entity_type.
  ENDMETHOD.


  METHOD update_join_definition ##needed.
  ENDMETHOD.


  METHOD update_multiple_table_buttons.
    DATA(lr_table_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_table_tb( ).

    lr_table_toolbar->set_button_state(
        enabled = if_has_multiple_tables
        fcode   = zif_dbbr_c_selscreen_functions=>expand_all_tables
    ).
    lr_table_toolbar->set_button_state(
        enabled = if_has_multiple_tables
        fcode   = zif_dbbr_c_selscreen_functions=>collapse_all_tables
    ).
    lr_table_toolbar->set_button_state(
        enabled = if_has_multiple_tables
        fcode   = zif_dbbr_c_selscreen_functions=>go_to_next_table
    ).
    lr_table_toolbar->set_button_state(
        enabled = if_has_multiple_tables
        fcode   = zif_dbbr_c_selscreen_functions=>go_to_previous_table
    ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD update_multi_selection_mask.
*... set `selection mode` for tabfield list to prevent unwanted behaviour
    mr_data->mr_tabfield_list->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-selection ).

*... first get all active selection tables
    DATA(lt_tables) = mr_data->mr_tabfield_list->get_table_list( ).
    DELETE lt_tables WHERE active_selection = abap_false. " or no_selection_allowed = abap_true.
    SORT lt_tables BY selection_order.

*... cache current list of selection fields
    DATA(lt_selection_fields) = mr_data->mr_t_table_data->*.
    CLEAR mr_data->mr_t_table_data->*.

    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).

      DATA(lr_cache_list) = mr_data->mr_tabfield_list->extract_fields( VALUE #( ( sign = 'I' option = 'EQ' low = <ls_table>-tabname_alias ) ) ).

      lr_cache_list->sort( ).
      lr_cache_list->initialize_iterator( if_for_active = abap_true ).

      CHECK lr_cache_list->has_more_lines( ).

*... add the table header
      mr_data->mr_t_table_data->* = VALUE #(
        BASE mr_data->mr_t_table_data->*
        ( create_table_header(
            iv_tabname       = <ls_table>-tabname
            iv_tabname_alias = <ls_table>-tabname_alias
            iv_typename      = <ls_table>-type
          )
        )
      ).


*... add selection fields for this table
      WHILE lr_cache_list->has_more_lines( ).
        DATA(lr_current_entry) = lr_cache_list->get_next_entry( ).
*... exclude text fields
        CHECK lr_current_entry->is_text_field = abap_false.
*... exclude technical fields
        CHECK lr_current_entry->is_technical = abap_false.

*... search for field in cache
        ASSIGN lt_selection_fields[ tabname   = lr_current_entry->tabname
                                    fieldname = lr_current_entry->fieldname ] TO FIELD-SYMBOL(<ls_existing_selfield>).
        IF sy-subrc = 0.
          mr_data->mr_t_table_data->* = VALUE #( BASE mr_data->mr_t_table_data->* ( <ls_existing_selfield> ) ).
        ELSE.
          mr_data->mr_t_table_data->* = VALUE #(
            BASE mr_data->mr_t_table_data->*
            ( convert_to_selfield( is_tablefield = lr_current_entry->* ) )
          ).
        ENDIF.
      ENDWHILE.

      mr_data->mr_tabfield_list->add_fields( lr_cache_list ).
      mr_data->mr_tabfield_list->sort_tables_by_active( ).
      lr_cache_list->clear( ).
      CLEAR lr_cache_list.
    ENDLOOP. " end of table loop
  ENDMETHOD.


  METHOD update_selection_mask.
*... set `selection mode` for tabfield list to prevent unwanted behaviour
    mr_data->mr_tabfield_list->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-selection ).

    mr_data->mr_tabfield_list->sort( ).

    DATA(lt_selfields) = mr_data->mr_t_table_data->*.
    CLEAR mr_data->mr_t_table_data->*.

    mr_data->mr_tabfield_list->initialize_iterator( if_for_active = abap_true ).
    WHILE mr_data->mr_tabfield_list->has_more_lines( ).
      DATA(lr_current_entry) = mr_data->mr_tabfield_list->get_next_entry( ).

*... exclude text fields
      CHECK lr_current_entry->is_text_field = abap_false.
*... exclude technical fields
      CHECK lr_current_entry->is_technical = abap_false.

      IF line_exists( lt_selfields[ tabname   = lr_current_entry->tabname
                                    fieldname = lr_current_entry->fieldname ] ).
        mr_data->mr_t_table_data->* = VALUE #(
          BASE mr_data->mr_t_table_data->*
          ( lt_selfields[ tabname   = lr_current_entry->tabname
                          fieldname = lr_current_entry->fieldname ] )
        ).
      ELSE.
        mr_data->mr_t_table_data->* = VALUE #(
          BASE mr_data->mr_t_table_data->*
          ( convert_to_selfield( is_tablefield = lr_current_entry->* ) )
        ).
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_dbbr_screen_table_util~handle_pbo.
*... handle expanded mode
    DATA(lv_visibility) = COND i(
      WHEN NOT has_content( )                  THEN 1
      WHEN NOT mr_data->is_multi_table_mode( ) THEN 1
                                               ELSE 0 ).

    LOOP AT mr_data->mr_s_tableview->cols ASSIGNING FIELD-SYMBOL(<ls_column>).
      IF <ls_column>-screen-group1 = 'EXP'.
        <ls_column>-invisible = lv_visibility.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~free_resources.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~get_deactivated_functions.
    IF mv_entity_id IS INITIAL.
      result = VALUE #(
        ( zif_dbbr_c_selscreen_functions=>execute_selection )
        ( zif_dbbr_c_selscreen_functions=>count_lines )
        ( zif_dbbr_c_selscreen_functions=>check_edit_option )
        ( zif_dbbr_c_selscreen_functions=>delete_mode )
        ( zif_dbbr_c_selscreen_functions=>define_joins )
        ( zif_dbbr_c_selscreen_functions=>edit_alternative_coltexts )
        ( zif_dbbr_c_selscreen_functions=>activate_optional_or_select )
        ( zif_dbbr_c_selscreen_functions=>control_sel_fields )
        ( zif_dbbr_c_selscreen_functions=>control_sort_fields )
        ( zif_dbbr_c_selscreen_functions=>control_output_fields )
        ( zif_dbbr_c_selscreen_functions=>select_additional_texts )
        ( zif_dbbr_c_selscreen_functions=>get_variant )
        ( zif_dbbr_c_selscreen_functions=>delete_variant )
        ( zif_dbbr_c_selscreen_functions=>save_variant )
        ( zif_dbbr_c_selscreen_functions=>get_variant )
        ( zif_dbbr_c_selscreen_functions=>save_f4_at_field )
        ( zif_dbbr_c_selscreen_functions=>delete_f4_from_field )
        ( zif_dbbr_c_selscreen_functions=>cross_reference_table )
        ( zif_dbbr_c_selscreen_functions=>copy_query )
        ( zif_dbbr_c_selscreen_functions=>edit_jump_fields )
        ( zif_dbbr_c_selscreen_functions=>multi_or_selection )
        ( zif_dbbr_c_selscreen_functions=>save_query )
        ( zif_dbbr_c_selscreen_functions=>show_formula_manager )
        ( zif_dbbr_c_selscreen_functions=>show_ddls_source )
        ( zif_dbbr_c_selscreen_functions=>go_to_ddic_view_of_cds )
        ( zif_dbbr_c_selscreen_functions=>open_cds_view_with_adt )

      ).
    ELSE.
*.... deactivate functions if advanced mode is not activated
      IF mr_data->mr_s_global_data->settings-advanced_mode = abap_false.
        result = VALUE #(
          ( zif_dbbr_c_selscreen_functions=>delete_mode )
          ( zif_dbbr_c_selscreen_functions=>define_joins )
          ( zif_dbbr_c_selscreen_functions=>delete_joins )
          ( zif_dbbr_c_selscreen_functions=>save_query )
          ( zif_dbbr_c_selscreen_functions=>define_sub_queries )
          ( zif_dbbr_c_selscreen_functions=>maintain_value_helps )
          ( zif_dbbr_c_selscreen_functions=>edit_alternative_coltexts )
          ( zif_dbbr_c_selscreen_functions=>activate_optional_or_select )
          ( zif_dbbr_c_selscreen_functions=>select_additional_texts )
          ( zif_dbbr_c_selscreen_functions=>save_f4_at_field )
          ( zif_dbbr_c_selscreen_functions=>delete_f4_from_field )
          ( zif_dbbr_c_selscreen_functions=>multi_or_selection )
          ( zif_dbbr_c_selscreen_functions=>show_formula_manager )
        ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_screen_util~handle_pbo.
    update_multiple_table_buttons( mr_data->mr_tabfield_list->has_multiple_tables( ) ).
  ENDMETHOD.
ENDCLASS.
