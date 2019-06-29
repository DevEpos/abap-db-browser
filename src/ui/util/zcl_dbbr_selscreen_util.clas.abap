"! <p class="shorttext synchronized" lang="en">Util for Handling Selection Screen Logic</p>
CLASS zcl_dbbr_selscreen_util DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_dbbr_base_select_tc .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_screen_util.
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

    TYPES:
      BEGIN OF ty_s_table_info,
        tabname         TYPE tabname,
        selection_order TYPE tabfdpos,
      END OF ty_s_table_info.
    TYPES: ty_t_table_info TYPE STANDARD TABLE OF ty_s_table_info.

    "! <p class="shorttext synchronized" lang="en">Requesting new entity for selection screen</p>
    EVENTS request_new_entity
      EXPORTING
        VALUE(ev_id) TYPE zdbbr_entity_id
        VALUE(ev_type) TYPE zdbbr_entity_type
        VALUE(ef_force_loading) TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Entity was deleted</p>
    CLASS-EVENTS entity_deleted
      EXPORTING
        VALUE(ev_entity_id) TYPE zdbbr_entity_id
        VALUE(ev_entity_type) TYPE zdbbr_entity_type .

    "! <p class="shorttext synchronized" lang="en">CLASS_CONSTRUCTOR</p>
    CLASS-METHODS init_selscreen_table_tb .
    "! <p class="shorttext synchronized" lang="en">Choose select option</p>
    CLASS-METHODS choose_sel_option
      IMPORTING
        if_allow_null           TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rs_chosen_option) TYPE se16n_sel_option .
    "! <p class="shorttext synchronized" lang="en">Checks edit mode</p>
    METHODS check_edit_mode .
    "! <p class="shorttext synchronized" lang="en">Performs mandatory checks</p>
    METHODS check_mandatory_fields .
    "! <p class="shorttext synchronized" lang="en">Validates the primary entity (table/cds view/query table)</p>
    METHODS check_primary_entity
      RETURNING
        VALUE(rf_success) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Choose Sorting fields</p>
    METHODS choose_sort_fields .
    "! <p class="shorttext synchronized" lang="en">Choose fields for selection/output</p>
    METHODS choose_tabfields
      IMPORTING
        !iv_mode                 TYPE zdbbr_field_chooser_mode
      RETURNING
        VALUE(rf_fields_updated) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Clear all data</p>
    METHODS clear .
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !ir_selscreen_data TYPE REF TO zcl_dbbr_selscreen_data
        !iv_entity_type    TYPE zdbbr_entity_type .
    "! <p class="shorttext synchronized" lang="en">Deletes and Updates the join definition</p>
    METHODS delete_join_definition .
    "! <p class="shorttext synchronized" lang="en">Retrieve information about current entity</p>
    METHODS get_entity_information
      EXPORTING
        ev_entity      TYPE tabname
        ev_entity_raw  TYPE zdbbr_entity_id_raw
        ev_entity_id   TYPE zdbbr_entity_id
        ev_type        TYPE zdbbr_favmenu_type
        ev_description TYPE ddtext .
    "! <p class="shorttext synchronized" lang="en">Retrieve the primary entity type for a join definition</p>
    "!
    METHODS get_entity_type_for_join
      RETURNING
        VALUE(rv_entity_type) TYPE zdbbr_entity_type.
    "! <p class="shorttext synchronized" lang="en">Returns id for variant</p>
    "!
    METHODS get_id_for_variant .
    "! <p class="shorttext synchronized" lang="en">Returns the title for the selection screen</p>
    "!
    METHODS get_title
          ABSTRACT
      RETURNING
        VALUE(result) TYPE string .
    "! <p class="shorttext synchronized" lang="en">Checks if selection screen has loaded entity content</p>
    METHODS has_content
      RETURNING
        VALUE(rf_has_content) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Checks if a certain entity is already loaded into the screen</p>
    METHODS is_entity_loaded
      IMPORTING
        !iv_entity_id   TYPE zdbbr_entity_id
        !iv_entity_type TYPE zdbbr_entity_type
      RETURNING
        VALUE(result)   TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Loads the entity into the selection screen</p>
    METHODS load_entity
      RETURNING
        VALUE(rf_entity_loaded) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets custom entity functions and menus</p>
    METHODS set_custom_functions
        ABSTRACT .
    "! <p class="shorttext synchronized" lang="en">Updates the descriptions texts for the entity</p>
    METHODS update_description_texts
        ABSTRACT .
    "! <p class="shorttext synchronized" lang="en">Update entity type for dynamic search help</p>
    METHODS update_entity_type_sh .
    "! <p class="shorttext synchronized" lang="en">Updates the join definition</p>
    METHODS update_join_definition
      IMPORTING
        it_table_info TYPE ty_t_table_info OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Saves the currently entered filter criteria</p>
    METHODS save_current_criteria.
    "! <p class="shorttext synchronized" lang="en">Update the value help status for given table field</p>
    METHODS update_value_helps_for_field
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname.
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

    DATA mo_custom_toolbar_cont TYPE REF TO cl_gui_container .
    DATA mo_custom_toolbar TYPE REF TO cl_gui_toolbar .
    DATA mo_data TYPE REF TO zcl_dbbr_selscreen_data .
    DATA mo_altcoltext_f TYPE REF TO zcl_dbbr_altcoltext_factory .
    "! <p class="shorttext synchronized" lang="en">Factory for favorite menu</p>
    DATA mo_favmenu_f TYPE REF TO zcl_dbbr_favmenu_factory .
    "! <p class="shorttext synchronized" lang="en">Information for join</p>
    DATA ms_join_def_old TYPE zdbbr_join_data .
    "! <p class="shorttext synchronized" lang="en">ID of an DB Browser entity</p>
    DATA mv_entity_id TYPE zdbbr_entity_id .
    "! <p class="shorttext synchronized" lang="en">Type of Entity</p>
    DATA mv_entity_type TYPE zdbbr_entity_type .
    DATA mo_custom_menu TYPE REF TO cl_ctmenu .
    "! <p class="shorttext synchronized" lang="en">Context Menu</p>
    CLASS-DATA go_delete_tb_menu TYPE REF TO cl_ctmenu .

    "! <p class="shorttext synchronized" lang="en">Internal logic to load an entity</p>
    METHODS load_entity_internal
          ABSTRACT
      RETURNING
        VALUE(rf_entity_loaded) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Update buttons for multiple table mode</p>
    METHODS update_multiple_table_buttons
      IMPORTING
        !if_has_multiple_tables TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Clear the edit flags</p>
    METHODS clear_edit_flags .
    "! <p class="shorttext synchronized" lang="en">convert table info field to selection table field</p>
    METHODS convert_to_selfield
      IMPORTING
        !is_tablefield     TYPE zdbbr_tabfield_info_ui
      RETURNING
        VALUE(rs_selfield) TYPE zdbbr_selfield .
    "! <p class="shorttext synchronized" lang="en">Create Table fields</p>
    "!
    METHODS create_cds_fields
      IMPORTING
        is_entity_info        TYPE zdbbr_entity_info
      RETURNING
        VALUE(rs_entity_info) TYPE zdbbr_entity_info .
    "! <p class="shorttext synchronized" lang="en">Create new table field</p>
    "!
    METHODS create_table_field
      IMPORTING
        is_entity              TYPE zdbbr_entity_info
        is_field               TYPE dfies
        if_first_non_key_field TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Create Table fields</p>
    "!
    METHODS create_table_fields
      IMPORTING
        is_entity_info        TYPE zdbbr_entity_info
      RETURNING
        VALUE(rs_entity_info) TYPE zdbbr_entity_info .
    METHODS create_table_header
      IMPORTING
        !iv_tabname            TYPE tabname
        !iv_tabname_alias      TYPE tabname
        !iv_typename           TYPE zdbbr_entity_type OPTIONAL
      RETURNING
        VALUE(rs_table_header) TYPE zdbbr_selfield .
    "! <p class="shorttext synchronized" lang="en">Fill the fields of the selection table</p>
    "!
    METHODS fill_selection_mask .
    "! <p class="shorttext synchronized" lang="en">Fills the selection screen table</p>
    "!
    METHODS fill_primary_entity
          ABSTRACT
      IMPORTING
        is_primary_entity TYPE zdbbr_entity_info OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Fills the onscreen gui toolbar</p>
    "!
    METHODS fill_toolbar
      IMPORTING
        !if_create_extended_search TYPE abap_bool OPTIONAL
        it_custom_buttons          TYPE ttb_button OPTIONAL
      RETURNING
        VALUE(ro_toolbar)          TYPE REF TO cl_gui_toolbar.
    "! <p class="shorttext synchronized" lang="en">Complete the loading process of an entity</p>
    "!
    METHODS finish_loading .
    "! <p class="shorttext synchronized" lang="en">Trigger event that entity was deleted</p>
    METHODS notify_of_deleted_entity
      IMPORTING
        !iv_entity_id   TYPE zdbbr_entity_id
        !iv_entity_type TYPE zdbbr_entity_type .

    "! <p class="shorttext synchronized" lang="en">Refresh the selection mask for a multi table setup</p>
    "!
    METHODS update_multi_selection_mask .
    "! <p class="shorttext synchronized" lang="en">Refresh the selection mask for a single table setup</p>
    "!
    METHODS update_selection_mask .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_selscreen_util IMPLEMENTATION.

  METHOD constructor.
    mo_data = ir_selscreen_data.
    mv_entity_type = iv_entity_type.
    mo_altcoltext_f = NEW #( ).
    mo_favmenu_f = NEW #( ).

*... update entity type table fields
    mo_data->mo_tabfield_aggr_list->set_entity_type( mv_entity_type ).
    mo_data->mo_tabfield_list->set_entity_type( mv_entity_type ).

    zcl_dbbr_toolbar_util=>get_selscreen_table_tb( )->set_button_state( enabled = abap_true fcode = zif_dbbr_c_selscreen_functions=>control_sel_fields ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD check_edit_mode ##needed.
  ENDMETHOD.

  METHOD save_current_criteria.
    CHECK mo_data->mr_s_global_data->auto_sel_filter_saving = abap_true.

    get_entity_information(
      IMPORTING
        ev_entity_id   = DATA(lv_entity_id)
        ev_type        = DATA(lv_entity_type)
    ).
    DATA(lr_t_multi_or) = mo_data->get_multi_or_all( ).

    DATA(ls_auto_variant) = zcl_dbbr_variant_creator=>create_variant(
        iv_entity_id           = lv_entity_id
        iv_entity_type         = lv_entity_type
        it_selfields           = mo_data->mr_t_table_data->*
        it_multi_selfields     = mo_data->mr_t_selfields_multi->*
        it_multi_or            = lr_t_multi_or->*
    ).

*.. Currently only filter criteria will be stored in automatic variant
*... Later on it could be possible to provide settings which options should be
*... stored inside the automatic variant
    IF ls_auto_variant-has_criteria = abap_true.
      zcl_dbbr_variant_factory=>save_auto_variant( ls_auto_variant ).
    ELSE.
*.... No criteria found, delete any existing automatic variant
      zcl_dbbr_variant_factory=>delete_auto_variant(
          iv_entity_id   = lv_entity_id
          iv_entity_type = lv_entity_type
      ).
    ENDIF.
  ENDMETHOD.

  METHOD update_value_helps_for_field.
    mo_data->mo_custom_f4_map->clear_f4_for_field(
        iv_tabname   = iv_tabname
        iv_fieldname = iv_fieldname
    ).
    mo_data->mo_custom_f4_map->determine_f4_for_field(
        iv_tabname   = iv_tabname
        iv_fieldname = iv_fieldname
    ).
    DATA(lr_t_fields) = mo_data->mo_tabfield_list->get_fields_ref( ).
    LOOP AT lr_t_fields->* ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE tabname = iv_tabname
                                                                AND fieldname = iv_fieldname.
      DATA(lf_custom_f4_old) = <ls_field>-has_custom_f4.
      <ls_field>-has_custom_f4 = mo_data->mo_custom_f4_map->entry_exists(
         iv_tabname       = <ls_field>-tabname
         iv_fieldname     = <ls_field>-fieldname
         iv_rollname      = <ls_field>-rollname
         is_built_in_type = VALUE #(
           datatype = <ls_field>-datatype
           inttype  = <ls_field>-inttype
           leng     = <ls_field>-length
         )
      ).
*.... update field on selscreen if necessary
      IF <ls_field>-has_custom_f4 <> lf_custom_f4_old.
        DATA(lr_selfield) = REF #( mo_data->mr_t_table_data->*[ tabname_alias = <ls_field>-tabname_alias
                                                                fieldname     = <ls_field>-fieldname ] ).
        lr_selfield->has_cust_f4_help = <ls_field>-has_custom_f4.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_mandatory_fields.

    IF mo_data->mo_tabfield_list->has_table( zif_dbbr_global_consts=>c_parameter_dummy_table ).
      LOOP AT mo_data->mr_t_table_data->* ASSIGNING FIELD-SYMBOL(<ls_param>) WHERE is_parameter   =  abap_true
                                                                               AND is_range_param =  abap_false
                                                                               AND low            IS INITIAL.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
*... retrieve the correct line index
        DATA(lv_index) = line_index( mo_data->mr_t_table_data->*[ fieldname = <ls_param>-fieldname
                                                                  tabname   = <ls_param>-tabname ] ).
        DATA(lv_top) = mo_data->mr_s_tableview->top_line.
        IF lv_top > lv_index.
          mo_data->mr_s_tableview->top_line = lv_index.
          lv_index = 1.
        ENDIF.

        RAISE EXCEPTION TYPE zcx_dbbr_validation_exception
          EXPORTING
            textid         = zcx_dbbr_validation_exception=>parameter_value_missing
            msgv1          = |{ <ls_param>-description }|
            parameter_name = 'GS_SELFIELDS-LOW'
            loop_line      = lv_index.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_primary_entity.
    rf_success = abap_true.
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
    ).
    IF if_allow_null = abap_true.
      lt_sel_option = VALUE #( BASE lt_sel_option
        ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-is_null
                                                         iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
        ( zcl_dbbr_icon_handler=>create_sel_option_icon( iv_icon_name = zif_dbbr_global_consts=>gc_options-is_not_null
                                                         iv_sign      = zif_dbbr_global_consts=>gc_options-i ) )
      ).
    ENDIF.
    lt_sel_option = VALUE #( BASE lt_sel_option
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

  METHOD load_entity.
    rf_entity_loaded = load_entity_internal( ).

    IF rf_entity_loaded = abap_false.
      RETURN.
    ENDIF.

    get_entity_information( IMPORTING ev_entity_id = DATA(lv_entity_id)
                                      ev_type      = DATA(lv_entity_type) ).

*.. Check if there is default variant for this entity and if it should be loaded
    DATA(lf_default_var_loaded) = zcl_dbbr_default_variant_util=>load_default_variant(
        io_data           = mo_data
        io_selscreen_util = me
    ).
    IF lf_default_var_loaded = abap_true AND
       mo_data->mr_s_global_data->always_load_def_variant_first = abap_true.
      RETURN.
    ENDIF.

*.. Load automatic variant
    IF mo_data->mr_s_global_data->auto_sel_filter_saving = abap_true.
      zcl_dbbr_variant_loader=>create_auto_variant_loader(
        iv_entity_type       = lv_entity_type
        iv_entity_id         = lv_entity_id
        ir_t_multi_or        = mo_data->get_multi_or_all( )
        ir_t_selfields       = mo_data->mr_t_table_data
        ir_t_selfields_multi = mo_data->mr_t_selfields_multi
        ir_s_global_data     = mo_data->mr_s_global_data
        ir_tabfields         = mo_data->mo_tabfield_list
        ir_tabfields_grouped = mo_data->mo_tabfield_aggr_list
      )->load( ).
    ENDIF.
  ENDMETHOD.

  METHOD choose_sort_fields.
    FIELD-SYMBOLS: <lr_tabfields> TYPE REF TO zcl_dbbr_tabfield_list.

    DATA(lf_field_aggr_active) = mo_data->mo_selection_table->aggregation_is_active( ).
    IF lf_field_aggr_active = abap_true.
      ASSIGN mo_data->mo_tabfield_aggr_list TO <lr_tabfields>.
    ELSE.
      ASSIGN mo_data->mo_tabfield_list TO <lr_tabfields>.
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
    FIELD-SYMBOLS: <lo_tabfields> TYPE REF TO zcl_dbbr_tabfield_list.

    DATA(lf_field_aggr_active) = mo_data->mo_selection_table->aggregation_is_active( ).
    IF lf_field_aggr_active = abap_true AND iv_mode = zif_dbbr_global_consts=>gc_field_chooser_modes-output.
      ASSIGN mo_data->mo_tabfield_aggr_list TO <lo_tabfields>.
    ELSE.
      ASSIGN mo_data->mo_tabfield_list TO <lo_tabfields>.
    ENDIF.

    DATA(lv_old_mode) = <lo_tabfields>->get_mode( ).
    <lo_tabfields>->switch_mode( iv_mode ).

    DATA(lo_fields) = <lo_tabfields>->copy( ).

*... delete parameter fields from list if there are any
    lo_fields->delete_where_in_tablist( VALUE #( ( sign = 'I' option = 'EQ' low = zif_dbbr_global_consts=>c_parameter_dummy_table ) ) ).

    DATA(lr_tabfield_manager) = NEW zcl_dbbr_tabfield_manager(
      ir_fields            = lo_fields
      is_join_def          = mo_data->mr_s_join_def->*
      if_field_aggregation = lf_field_aggr_active
      iv_mode              = iv_mode
      iv_entity_type       = mv_entity_type
    ).

*... call main screen for field chooser
    lr_tabfield_manager->zif_uitb_screen_controller~call_screen( ).

**... get chosen fields if screen was not cancelled, otherwise leave ct_fields unchanged
    IF lr_tabfield_manager->data_should_be_transferred( ).
      rf_fields_updated = abap_true.
*.... Extract paramaters before they are overridden
      IF lf_field_aggr_active = abap_false.
        DATA(lo_params) = <lo_tabfields>->extract_fields( VALUE #( ( sign = 'I' option = 'EQ' low = zif_dbbr_global_consts=>c_parameter_dummy_table ) ) ).
      ENDIF.
      <lo_tabfields>->overwrite( lr_tabfield_manager->retrieve_current_data( ) ).

      IF lo_params IS BOUND.
        <lo_tabfields>->add_fields( lo_params ).
      ENDIF.
    ENDIF.

    IF iv_mode <> zif_dbbr_global_consts=>gc_field_chooser_modes-selection OR
       rf_fields_updated = abap_false.
      RETURN.
    ENDIF.

*... reset hiding of table fields to prevent unwanted behavior
    mo_data->mo_selection_table->expand_all_hidden_fields( ).

    fill_selection_mask( ).
  ENDMETHOD.


  METHOD init_selscreen_table_tb.
*.. create/fill delete menu
    go_delete_tb_menu = NEW #( ).
    go_delete_tb_menu->add_function(
       fcode = zif_dbbr_c_selscreen_functions=>delete_aggregations
       text  = |{ 'Delete all aggregations'(003) }|
    ).
    go_delete_tb_menu->add_function(
       fcode = zif_dbbr_c_selscreen_functions=>delete_all_or_tuple
       text  = |{ 'Delete all OR Tuple'(004) }|
    ).

    DATA(lo_default_var_menu) = NEW cl_ctmenu( ).
    lo_default_var_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>create_default_variant
        text  = |{ 'Create Default Variant'(005) }|
    ).
    lo_default_var_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>delete_default_variant
        text  = |{ 'Delete Default Variant'(006) }|
    ).

    DATA(lo_variant_menu) = NEW cl_ctmenu( ).
    lo_variant_menu->add_function(
        fcode = zif_dbbr_c_selscreen_functions=>save_variant
        text  = |{ 'Save Variant' }  (Ctrl+S)|
    ).

    DATA(lo_table_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_table_tb( ).
    lo_table_toolbar->add_button_group(
        data_table = VALUE #(
          ( butn_type = cntb_btype_button
            icon      = icon_expand_all
            quickinfo = 'Expand all Tables'(007)
            function  = zif_dbbr_c_selscreen_functions=>expand_all_tables )
          ( butn_type = cntb_btype_button
            icon      = icon_collapse_all
            quickinfo = 'Collapse all tables'(008)
            function  = zif_dbbr_c_selscreen_functions=>collapse_all_tables )
          ( butn_type = cntb_btype_sep )
          ( butn_type = cntb_btype_button
            icon      = icon_previous_page
            quickinfo = 'Go to previous table'(009)
            function  = zif_dbbr_c_selscreen_functions=>go_to_previous_table )
          ( butn_type = cntb_btype_button
            icon      = icon_next_page
            quickinfo = 'Go to next table'(010)
            function  = zif_dbbr_c_selscreen_functions=>go_to_next_table )
          ( butn_type = cntb_btype_sep )
          ( butn_type = cntb_btype_button
            icon      = icon_delete_row
            quickinfo = 'Delete current line values'(011)
            function  = zif_dbbr_c_selscreen_functions=>delete_line_input )
          ( butn_type = cntb_btype_dropdown
            text      = 'All Entries'(012)
            icon      = icon_delete_row
            function  = zif_dbbr_c_selscreen_functions=>delete_all_input )
          ( butn_type = cntb_btype_sep )
          ( function  = zif_dbbr_c_selscreen_functions=>get_variant
            icon      = icon_variants
            butn_type = cntb_btype_dropdown
            quickinfo = |{ 'Choose Variant' } (F6)| )
          ( butn_type = cntb_btype_dropdown
            text      = 'Default'(013)
            quickinfo = 'Load Default Variant'(014)
            icon      = icon_variants
            function  = zif_dbbr_c_selscreen_functions=>load_default_variant )
          ( butn_type = cntb_btype_sep )
          ( butn_type = cntb_btype_button
            quickinfo = 'Choose Selection Fields'(015)
            icon      = icon_align
            function  = zif_dbbr_c_selscreen_functions=>control_sel_fields )
          ( butn_type = cntb_btype_button
            quickinfo = 'Technical View On/Off'(016)
            icon      = icon_tools
            function  = zif_dbbr_c_selscreen_functions=>activate_tech_view )
        )
    ).

    lo_table_toolbar->set_static_ctxmenu( fcode = zif_dbbr_c_selscreen_functions=>get_variant ctxmenu = lo_variant_menu ).

    lo_table_toolbar->set_static_ctxmenu(
        fcode   = zif_dbbr_c_selscreen_functions=>delete_all_input
        ctxmenu = go_delete_tb_menu
    ).
    lo_table_toolbar->set_static_ctxmenu(
        fcode   = zif_dbbr_c_selscreen_functions=>load_default_variant
        ctxmenu = lo_default_var_menu
    ).
  ENDMETHOD.


  METHOD clear.
    CLEAR: ms_join_def_old,
           mv_entity_id.

*... Clear all data from data class
    mo_data->clear( ).

    DATA(lr_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_entity_tb( ).
    lr_toolbar->set_button_visible( fcode = 'CUST_FUNC' visible = abap_false ).
  ENDMETHOD.


  METHOD clear_edit_flags.
    CLEAR: mo_data->mr_s_global_data->edit,
           mo_data->mr_s_global_data->delete_mode.
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


  METHOD create_cds_fields.
    TRY.
        DATA(lo_cds_view) = zcl_dbbr_cds_view_factory=>read_cds_view( iv_cds_view = is_entity_info-tabname ).

        mo_data->mo_custom_f4_map->read_custom_f4_definitions( is_entity_info-tabname ).

*...... create parameters
        zcl_dbbr_cds_tabfield_util=>add_parameters(
            ir_tabfield_list = mo_data->mo_tabfield_list
            it_parameters    = lo_cds_view->get_parameters( )
        ).
*...... create table fields for cds view
        DATA(ls_header) = lo_cds_view->get_header( ).

        rs_entity_info = zcl_dbbr_cds_tabfield_util=>add_view_colums(
            ir_tabfield_list = mo_data->mo_tabfield_list
            io_custom_f4_map = mo_data->mo_custom_f4_map
            it_columns       = lo_cds_view->get_columns( )
            iv_name          = is_entity_info-tabname
            if_has_params    = lo_cds_view->has_parameters( )
            iv_alias         = is_entity_info-tabname_alias
            iv_raw_name      = ls_header-entityname_raw
            iv_description   = ls_header-description
            if_is_primary    = is_entity_info-is_primary
        ).
      CATCH zcx_dbbr_data_read_error.
        "handle exception
    ENDTRY.
  ENDMETHOD.

  METHOD create_table_field.
    DATA(lf_output) = abap_false.
    DATA(ls_altcoltext) = mo_altcoltext_f->find_alternative_text(
        iv_tabname   = is_field-tabname
        iv_fieldname = is_field-fieldname
    ).

    DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
      tabname_alias         = is_entity-tabname_alias
      fieldname             = is_field-fieldname
      fieldname_raw         = is_field-fieldname
      selection_active      = is_entity-active_selection
      ddic_order            = is_field-position
      is_lowercase          = is_field-lowercase
*...... check if there is a custom search field for this table
      has_custom_f4         = mo_data->mo_custom_f4_map->entry_exists(
          iv_tabname   = is_field-tabname
          iv_fieldname = is_field-fieldname
          iv_rollname  = is_field-rollname
          is_built_in_type = VALUE #(
            datatype  = is_field-datatype
            leng      = is_field-leng
          )
      )
*...... check if field is numeric
      is_numeric            = zcl_dbbr_dictionary_helper=>is_type_numeric( is_field-inttype )
*...... is there an F4-help for this table field
      f4_available          = is_field-f4availabl
      field_ddtext          = COND #( WHEN is_field-scrtext_l IS INITIAL THEN
                                  is_field-fieldtext
                                ELSE
                                  is_field-scrtext_l )
      is_key                = xsdbool( is_field-keyflag = abap_true )
*...... default sign is inclusive
      default_sign          = zif_dbbr_global_consts=>gc_options-i
      is_virtual_join_field = is_entity-virtual_join_table
      is_foreign_key        = xsdbool( is_field-checktable IS NOT INITIAL )
      std_short_text        = is_field-scrtext_s
      std_medium_text       = is_field-scrtext_m
      std_long_text         = is_field-scrtext_l
      alt_long_text         = ls_altcoltext-alt_long_text
      alt_medium_text       = ls_altcoltext-alt_short_text
      length                = is_field-leng
      outputlen             = is_field-outputlen
      intlen                = is_field-intlen
      header_text           = is_field-reptext
      ref_field             = is_field-reffield
      ref_tab               = is_field-reftable
    ).

*.... Take the rest of the components directly from the table field
    ls_tabfield = CORRESPONDING #( BASE ( ls_tabfield ) is_field ).

    DATA(lo_addtext_bl) = zcl_dbbr_addtext_bl=>get_instance( ).

    lo_addtext_bl->determine_t_fields_for_tab( is_tabfield_info = is_field ).

    IF ls_altcoltext-alt_long_text IS NOT INITIAL.
      ls_tabfield-field_ddtext = ls_altcoltext-alt_long_text.
    ENDIF.

    IF mo_data->mr_s_global_data->primary_table = ls_tabfield-tabname OR
       ls_tabfield-is_key = abap_true OR
       ( if_first_non_key_field = abap_false AND ls_tabfield-is_key = abap_false ).
      lf_output = abap_true.
    ENDIF.

    ls_tabfield-output_active = lf_output.

    DATA(lr_new_field) = CAST zdbbr_tabfield_info_ui( mo_data->mo_tabfield_list->add( REF #( ls_tabfield ) ) ).

    " only continue if normal field did not already exist
    IF lr_new_field IS NOT INITIAL.
      " there is an existing text field for the current table field
      IF lo_addtext_bl->text_exists( is_data_element_info = is_field ).

        lo_addtext_bl->add_text_fields_to_list(
          ir_tabfields  = mo_data->mo_tabfield_list
          is_ref_tabfield = ls_tabfield
          if_post_select = is_entity-virtual_join_table
          iv_position   = is_field-position
          is_altcoltext = ls_altcoltext
        ).

        " connect text field and key field
        lr_new_field->has_text_field = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_table_fields.
*& Description: Creates table fields for given table name
*&---------------------------------------------------------------------*
    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( is_entity_info-tabname ).
    CHECK ls_table_info IS NOT INITIAL.

*... read custom f4 helps for join table definitions
    mo_data->mo_custom_f4_map->read_custom_f4_definitions( is_entity_info-tabname ).

    DATA(lv_join_table_text) = ls_table_info-ddtext.

    zcl_dbbr_dictionary_helper=>get_table_field_infos( EXPORTING iv_tablename    = is_entity_info-tabname
                                                       IMPORTING et_table_fields = DATA(lt_dfies) ).

    DATA(lr_addtext_bl) = zcl_dbbr_addtext_bl=>get_instance( ).

    DATA(lf_first_non_key_field) = abap_false.

*... build tablefield table
    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_data_element_field>) WHERE datatype <> 'CLNT'.
      IF <ls_data_element_field>-keyflag = abap_false.
        lf_first_non_key_field = abap_true.
      ENDIF.
      create_table_field(
        is_entity              = is_entity_info
        if_first_non_key_field = lf_first_non_key_field
        is_field               = <ls_data_element_field>
      ).
    ENDLOOP.

    rs_entity_info = VALUE zdbbr_entity_info(
      active_selection     = abap_true
      tabname              = is_entity_info-tabname
      tabname_alias        = COND #( WHEN is_entity_info-tabname_alias IS NOT INITIAL THEN
                                        is_entity_info-tabname_alias
                                     ELSE
                                        is_entity_info-tabname )
      alias                = is_entity_info-alias
      type                 = zif_dbbr_c_entity_type=>table
      description          = ls_table_info-ddtext
      fields_are_loaded    = abap_true
      is_primary           = is_entity_info-is_primary
      selection_order      = is_entity_info-selection_order
    ).

    mo_data->mo_tabfield_list->add_table( rs_entity_info ).

  ENDMETHOD.



  METHOD create_table_header.
    rs_table_header = VALUE zdbbr_selfield(
      tabname              = iv_tabname
      is_table_header      = abap_true
      description          = iv_tabname_alias
      fieldname_raw        = iv_tabname
      ddic_order           = 0 ).
  ENDMETHOD.


  METHOD delete_join_definition.
    CHECK mo_data->is_join_active( ).

    CLEAR: mo_data->mr_s_join_def->tables.

    ASSIGN mo_data->mr_t_table_data->* TO FIELD-SYMBOL(<lt_selection_fields>).
    ASSIGN mo_data->mr_t_selfields_multi->* TO FIELD-SYMBOL(<lt_selection_fields_multi>).
    ASSIGN mo_data->mr_s_join_def->* TO FIELD-SYMBOL(<ls_join_definition>).
    ASSIGN mo_data->mr_s_global_data->* TO FIELD-SYMBOL(<ls_global_data>).

    mo_data->set_join_active( abap_false ).
    mo_data->mo_tabfield_list->delete_custom( ).

    CLEAR: <lt_selection_fields>,
           <lt_selection_fields_multi>.

    mo_data->mo_tabfield_list->clear_alias_names( ).

    fill_selection_mask( ).
  ENDMETHOD.


  METHOD fill_selection_mask.
  ENDMETHOD.


  METHOD fill_toolbar.
    DATA: lt_data_table TYPE ttb_button.

    DATA(lo_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_entity_tb( ).
*.. clear all buttons
    lo_toolbar->delete_all_buttons( ).
    cl_gui_cfw=>flush( ).

    IF if_create_extended_search = abap_true.
      lt_data_table = VALUE #(
        ( function  = zif_dbbr_c_selscreen_functions=>open_specific_extended_search
          icon      = icon_extended_search
          quickinfo = |{ 'Extended Search'(001) } (Ctrl+Shift+F2)| )
      ).
    ENDIF.

    lo_toolbar->add_button_group(
        data_table = VALUE #( BASE lt_data_table
          ( LINES OF it_custom_buttons )
          ( function  = 'CUST_FUNC'
            icon      = icon_reference_list
            quickinfo = |{ 'Custom Functions'(002) }|
            butn_type = cntb_btype_menu      )
        )
    ).

*.... set custom menu for cust function
    IF mo_custom_menu IS BOUND.
      lo_toolbar->set_static_ctxmenu(
          fcode   = 'CUST_FUNC'
          ctxmenu = mo_custom_menu
      ).
    ENDIF.

    ro_toolbar = lo_toolbar.
  ENDMETHOD.


  METHOD finish_loading.
    DATA(lr_toolbar) = zcl_dbbr_toolbar_util=>get_selscreen_entity_tb( ).

    lr_toolbar->set_button_visible( fcode = 'CUST_FUNC' visible = abap_true ).
  ENDMETHOD.


  METHOD get_entity_information.
    ev_description = mo_data->mr_v_selmask_entity_text->*.
  ENDMETHOD.


  METHOD get_entity_type_for_join.
    rv_entity_type = mv_entity_type.
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


  METHOD update_entity_type_sh.
    DATA(lr_v_entity_type) = CAST zdbbr_entity_type(
      zcl_uitb_data_cache=>get_instance(
        zif_dbbr_c_report_id=>search_help_exit
      )->get_data_ref( zcl_dbbr_entity_sh_helper=>c_v_entity_type )
    ).

    lr_v_entity_type->* = mo_data->mr_s_entity_info->entity_type.
  ENDMETHOD.


  METHOD update_join_definition.
    DATA: ls_new_entity TYPE zdbbr_entity_info.

    FIELD-SYMBOLS: <ls_selfield> TYPE zdbbr_selfield.

    DATA(lt_join_tables_old) = mo_data->mr_s_old_join_def->tables.
    DATA(lv_old_primary_alias) = mo_data->mr_s_old_join_def->primary_table_alias.

    ASSIGN mo_data->mr_t_table_data->* TO FIELD-SYMBOL(<lt_selection_fields>).
    ASSIGN mo_data->mr_t_selfields_multi->* TO FIELD-SYMBOL(<lt_selection_fields_multi>).
    ASSIGN mo_data->mr_s_join_def->* TO FIELD-SYMBOL(<ls_join_definition>).
    ASSIGN mo_data->mr_s_global_data->* TO FIELD-SYMBOL(<ls_global_data>).

    mo_data->set_join_active( xsdbool( <ls_join_definition>-tables IS NOT INITIAL ) ).

    IF mo_data->is_join_active( ).
      CLEAR: mo_data->mr_s_global_data->edit.
    ENDIF.

    DATA(lt_old_tablist) = mo_data->mo_tabfield_list->get_table_list( ).
    SORT lt_old_tablist BY selection_order DESCENDING.
    DATA(lv_max_index) = VALUE #( lt_old_tablist[ 1 ]-selection_order OPTIONAL ).
    SORT lt_old_tablist BY selection_order ASCENDING.

*.. add fields of join table to selfields output
    LOOP AT <ls_join_definition>-tables ASSIGNING FIELD-SYMBOL(<ls_join_table_info>).
      DATA(lf_is_new_table) = abap_false.

*..... check if fields for join table are already in selection fields table
      ASSIGN lt_old_tablist[ KEY alias tabname_alias = <ls_join_table_info>-add_table_alias ] TO FIELD-SYMBOL(<ls_existing_table>).
      IF sy-subrc <> 0.
        lf_is_new_table = abap_true.

        IF <ls_join_table_info>-entity_type = zif_dbbr_c_entity_type=>cds_view.
          ls_new_entity = create_cds_fields( VALUE #(
            tabname            = <ls_join_table_info>-add_table
            tabname_alias      = <ls_join_table_info>-add_table_alias
            virtual_join_table = <ls_join_table_info>-is_virtual
            active_selection   = abap_true
            selection_order    = VALUE #( it_table_info[ tabname = <ls_join_table_info>-add_table ]-selection_order OPTIONAL ) )
          ).
        ELSE.
          ls_new_entity = create_table_fields( VALUE #(
              tabname            = <ls_join_table_info>-add_table
              tabname_alias      = <ls_join_table_info>-add_table_alias
              virtual_join_table = <ls_join_table_info>-is_virtual
              active_selection   = abap_true
              selection_order    = VALUE #( it_table_info[ tabname = <ls_join_table_info>-add_table ]-selection_order OPTIONAL ) )
          ).
        ENDIF.

*...... add new join table to table list
        APPEND ls_new_entity TO lt_old_tablist ASSIGNING <ls_existing_table>.
      ENDIF.

*.... check if join table is of type 'conditional join' to prevent selection
      IF <ls_join_table_info>-is_virtual = abap_true.
        <ls_existing_table>-no_selection_allowed = abap_true.
      ENDIF.

      IF <ls_existing_table>-selection_order IS INITIAL.
        ADD 1 TO lv_max_index.
        <ls_existing_table>-active_selection = abap_true.
        <ls_existing_table>-selection_order = lv_max_index.
      ENDIF.

*.... check if this table's join type changed to or from virtual join
      IF lf_is_new_table = abap_false.
        DATA(ls_old_join_table) = lt_join_tables_old[ add_table = <ls_existing_table>-tabname ].
        IF ls_old_join_table-is_virtual <> <ls_join_table_info>-is_virtual.

*........ update the is_post_join flag of all table fields for this table
          mo_data->mo_tabfield_list->update_virtual_join_for_table(
            iv_table_name  = <ls_existing_table>-tabname
            if_virtual_join = <ls_join_table_info>-is_virtual
          ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    DATA(lt_add_tables_selopt) = VALUE zdbbr_tabname_range_itab(
      FOR add_table IN <ls_join_definition>-tables
      ( sign   = zif_dbbr_global_consts=>gc_options-i
        option = zif_dbbr_global_consts=>gc_options-eq
        low    = add_table-add_table_alias             )
    ).

    IF <ls_join_definition>-primary_table_alias <> lv_old_primary_alias.
      mo_data->mo_tabfield_list->replace_table_alias(
        iv_old_alias = lv_old_primary_alias
        iv_new_alias = <ls_join_definition>-primary_table_alias
      ).
      LOOP AT <lt_selection_fields> ASSIGNING <ls_selfield> WHERE tabname_alias = lv_old_primary_alias.
        <ls_selfield>-tabname_alias = <ls_join_definition>-primary_table_alias.
      ENDLOOP.
      LOOP AT <lt_selection_fields_multi> ASSIGNING <ls_selfield> WHERE tabname_alias = lv_old_primary_alias.
        <ls_selfield>-tabname_alias = <ls_join_definition>-primary_table_alias.
      ENDLOOP.
    ENDIF.

*    DELETE <lt_selection_fields> WHERE tabname_alias NOT IN lt_add_tables_selopt.
*    DELETE <lt_selection_fields_multi> WHERE tabname_alias NOT IN lt_add_tables_selopt.

*.. update tabfield list
    mo_data->mo_tabfield_list->delete_custom(
        it_tabname_alias_range = VALUE #( FOR table IN lt_add_tables_selopt ( sign = 'E' option = table-option low = table-low ) )
    ).

*.. are there any join tables?
    IF <ls_join_definition>-tables IS INITIAL.
      mo_data->mo_tabfield_list->clear_alias_names( ).
    ELSE.
*.... Update table aliases from Join definition
      DATA(lr_tables) = mo_data->mo_tabfield_list->get_tables_ref( ).
      LOOP AT lr_tables->* ASSIGNING FIELD-SYMBOL(<ls_table>) WHERE is_custom = abap_false.
        IF <ls_table>-is_primary = abap_true.
          <ls_table>-alias = <ls_join_definition>-primary_table_alias_alv.
        ELSE.
          <ls_table>-alias = <ls_join_definition>-tables[ add_table_alias = <ls_table>-tabname_alias ]-add_table_alias_alv.
        ENDIF.
      ENDLOOP.

      mo_data->mo_tabfield_list->update_alias_names( ).
    ENDIF.

    IF mo_data->is_join_active( ).
      mo_data->mo_tabfield_list->sort_tables_by_active( ).
*.... manually update join table list of field list to prevent unwanted screen updates
      update_multi_selection_mask( ).
    ELSE.
      update_selection_mask( ).
    ENDIF.
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
    DATA: lt_current_table_list TYPE RANGE OF tabname.

*... set `selection mode` for tabfield list to prevent unwanted behaviour
    mo_data->mo_tabfield_list->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-selection ).

*... first get all active selection tables
    DATA(lt_tables) = mo_data->mo_tabfield_list->get_table_list( ).
    DELETE lt_tables WHERE active_selection = abap_false. " or no_selection_allowed = abap_true.
*.. Build range for deleting invalid selection selection fields
    lt_current_table_list = VALUE #( FOR table IN lt_tables ( sign = 'I' option = 'EQ' low = table-tabname_alias ) ).
    SORT lt_tables BY selection_order.

*.. Always show parameter table at first position
    LOOP AT lt_tables INTO DATA(ls_param_table) WHERE tabname = zif_dbbr_global_consts=>c_parameter_dummy_table.
      CHECK sy-tabix <> 1.
      DELETE lt_tables.
      DATA(lf_insert_params_at_start) = abap_true.
      EXIT.
    ENDLOOP.

    IF lf_insert_params_at_start = abap_true.
      INSERT ls_param_table INTO lt_tables INDEX 1.
    ENDIF.

    DELETE mo_data->mr_t_table_data->* WHERE tabname_alias NOT IN lt_current_table_list.
    DELETE mo_data->mr_t_selfields_multi->* WHERE tabname_alias NOT IN lt_current_table_list.

*... cache current list of selection fields
    DATA(lt_selection_fields) = mo_data->mr_t_table_data->*.
    CLEAR mo_data->mr_t_table_data->*.

    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).

      DATA(lr_cache_list) = mo_data->mo_tabfield_list->extract_fields( VALUE #( ( sign = 'I' option = 'EQ' low = <ls_table>-tabname_alias ) ) ).

      lr_cache_list->sort( ).
      lr_cache_list->initialize_iterator( if_for_active = abap_true ).

      IF NOT lr_cache_list->has_more_lines( ).
        mo_data->mo_tabfield_list->add_fields( lr_cache_list ).
        mo_data->mo_tabfield_list->sort_tables_by_active( ).
        lr_cache_list->clear( ).
        CLEAR lr_cache_list.
        CONTINUE.
      ENDIF.

*... add the table header
      mo_data->mr_t_table_data->* = VALUE #(
        BASE mo_data->mr_t_table_data->*
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
        ASSIGN lt_selection_fields[ tabname_alias = lr_current_entry->tabname_alias
                                    fieldname     = lr_current_entry->fieldname ] TO FIELD-SYMBOL(<ls_existing_selfield>).
        IF sy-subrc = 0.
          mo_data->mr_t_table_data->* = VALUE #( BASE mo_data->mr_t_table_data->* ( <ls_existing_selfield> ) ).
        ELSE.
          mo_data->mr_t_table_data->* = VALUE #(
            BASE mo_data->mr_t_table_data->*
            ( convert_to_selfield( is_tablefield = lr_current_entry->* ) )
          ).
        ENDIF.
      ENDWHILE.

      mo_data->mo_tabfield_list->add_fields( lr_cache_list ).
      mo_data->mo_tabfield_list->sort_tables_by_active( ).
      lr_cache_list->clear( ).
      CLEAR lr_cache_list.
    ENDLOOP. " end of table loop
  ENDMETHOD.


  METHOD update_selection_mask.
*... set `selection mode` for tabfield list to prevent unwanted behaviour
    mo_data->mo_tabfield_list->switch_mode( zif_dbbr_global_consts=>gc_field_chooser_modes-selection ).

    mo_data->mo_tabfield_list->sort( ).

    DATA(lt_selfields) = mo_data->mr_t_table_data->*.
    CLEAR mo_data->mr_t_table_data->*.

    mo_data->mo_tabfield_list->initialize_iterator( if_for_active = abap_true ).
    WHILE mo_data->mo_tabfield_list->has_more_lines( ).
      DATA(lr_current_entry) = mo_data->mo_tabfield_list->get_next_entry( ).

*... exclude text fields
      CHECK lr_current_entry->is_text_field = abap_false.
*... exclude technical fields
      CHECK lr_current_entry->is_technical = abap_false.

      IF line_exists( lt_selfields[ tabname   = lr_current_entry->tabname
                                    fieldname = lr_current_entry->fieldname ] ).
        mo_data->mr_t_table_data->* = VALUE #(
          BASE mo_data->mr_t_table_data->*
          ( lt_selfields[ tabname   = lr_current_entry->tabname
                          fieldname = lr_current_entry->fieldname ] )
        ).
      ELSE.
        mo_data->mr_t_table_data->* = VALUE #(
          BASE mo_data->mr_t_table_data->*
          ( convert_to_selfield( is_tablefield = lr_current_entry->* ) )
        ).
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_dbbr_screen_table_util~handle_pbo.
*... handle expanded mode
    DATA(lv_visibility) = COND i(
      WHEN NOT has_content( )                  THEN 1
      WHEN NOT mo_data->is_multi_table_mode( ) THEN 1
                                               ELSE 0 ).

    LOOP AT mo_data->mr_s_tableview->cols ASSIGNING FIELD-SYMBOL(<ls_column>).
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
        ( zif_dbbr_c_selscreen_functions=>assign_built_in_f4_at_field )
        ( zif_dbbr_c_selscreen_functions=>assign_custom_f4_at_field )
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
      IF mo_data->mr_s_global_data->settings-advanced_mode = abap_false.
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
          ( zif_dbbr_c_selscreen_functions=>assign_built_in_f4_at_field )
          ( zif_dbbr_c_selscreen_functions=>delete_f4_from_field )
          ( zif_dbbr_c_selscreen_functions=>multi_or_selection )
          ( zif_dbbr_c_selscreen_functions=>show_formula_manager )
        ).
      ENDIF.

      IF mo_data->mr_s_global_data->settings-experimental_mode = abap_false.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_ui_function.
    CASE cv_function.

      WHEN zif_dbbr_c_selscreen_functions=>create_default_variant.
        CLEAR cv_function.
        zcl_dbbr_default_variant_util=>create_default_variant(
            io_data           = mo_data
            io_selscreen_util = me
        ).

      WHEN zif_dbbr_c_selscreen_functions=>load_default_variant.
        CLEAR cv_function.
        zcl_dbbr_default_variant_util=>load_default_variant(
            if_show_loaded_message = abap_true
            io_data                = mo_data
            io_selscreen_util      = me
        ).

      WHEN zif_dbbr_c_selscreen_functions=>delete_default_variant.
        CLEAR cv_function.
        zcl_dbbr_default_variant_util=>delete_default_variant( me ).

    ENDCASE.
  ENDMETHOD.

  METHOD zif_dbbr_screen_util~handle_pbo.
    update_multiple_table_buttons( mo_data->mo_tabfield_list->has_multiple_tables( ) ).
  ENDMETHOD.

ENDCLASS.
