CLASS zcl_dbbr_cds_navigator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_dbbr_navigator_creator .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_table_navigator .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_source_cds_view TYPE REF TO zcl_sat_cds_view .
    DATA ms_tech_info TYPE zdbbr_tech_info.
    DATA mr_t_data TYPE REF TO data .
    DATA ms_association TYPE zsat_cds_association .
    DATA mr_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mr_source_tabfields TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mt_source_index TYPE lvc_t_indx .
    DATA mv_entity_type TYPE zsat_entity_type .
    DATA mr_t_for_all_data TYPE REF TO data .
    DATA mt_nav_breadcrumbs TYPE string_table .
    DATA mv_nav_count TYPE i .
    DATA mv_source_params TYPE string.
    DATA mv_params TYPE string.
    DATA mt_where TYPE string_table.

    METHODS create_output_fields
      RAISING
        zcx_dbbr_association_nav_error .
    METHODS export_data_to_memory .
    METHODS fill_selection_fields .
    METHODS constructor
      IMPORTING
        !ir_t_data           TYPE REF TO data
        is_tech_info         TYPE zdbbr_tech_info
        !ir_source_cds_view  TYPE REF TO zcl_sat_cds_view
        !it_source_index     TYPE lvc_t_indx
        !ir_source_tabfields TYPE REF TO zcl_dbbr_tabfield_list
        !is_association      TYPE zsat_cds_association
        !it_nav_breadcrumbs  TYPE string_table
        it_param_values      TYPE zif_sat_ty_global=>ty_t_cds_param_value OPTIONAL
        !iv_nav_count        TYPE i .
    METHODS handle_messages.

    "! <p class="shorttext synchronized" lang="en">Determine Parameter String</p>
    "!
    "! @parameter it_params | <p class="shorttext synchronized" lang="en">List of CDS parameters</p>
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter io_tabfields | <p class="shorttext synchronized" lang="en">Table fields</p>
    METHODS determine_param_string
      IMPORTING
        it_params        TYPE zif_sat_ty_global=>ty_t_cds_parameter
        iv_cds_view_name TYPE zsat_cds_view_name
        io_tabfields     TYPE REF TO zcl_dbbr_tabfield_list
      RAISING
        zcx_dbbr_association_nav_error .

    "! <p class="shorttext synchronized" lang="en">Request parameter value input</p>
    "!
    "! @parameter it_params | <p class="shorttext synchronized" lang="en">List of parameters</p>
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter io_tabfields | <p class="shorttext synchronized" lang="en">Table fields</p>
    "! @parameter rt_param_values | <p class="shorttext synchronized" lang="en">List of parameter/value</p>
    METHODS request_param_values
      IMPORTING
        it_params              TYPE zif_sat_ty_global=>ty_t_cds_parameter
        iv_cds_view_name       TYPE zsat_cds_view_name
        io_tabfields           TYPE REF TO zcl_dbbr_tabfield_list
      RETURNING
        VALUE(rt_param_values) TYPE zif_sat_ty_global=>ty_t_cds_param_value
      RAISING
        zcx_dbbr_association_nav_error.
ENDCLASS.



CLASS zcl_dbbr_cds_navigator IMPLEMENTATION.


  METHOD constructor.
    mr_t_data          = ir_t_data.
    ms_association     = is_association.
    mt_source_index    = it_source_index.
    mr_source_cds_view = ir_source_cds_view.
    mr_source_tabfields = ir_source_tabfields.
    mt_nav_breadcrumbs = it_nav_breadcrumbs.
    mv_source_params = zcl_dbbr_cds_param_util=>build_param_string(
      iv_param_indentation = strlen( ir_source_cds_view->mv_view_name )
      it_param_values      = it_param_values ).
    ms_tech_info = is_tech_info.
    mv_nav_count = iv_nav_count.
  ENDMETHOD.


  METHOD create_output_fields.
    mr_tabfields = NEW #( ).

    CASE ms_association-kind.

      WHEN zif_sat_c_cds_assoc_type=>entity OR
           zif_sat_c_cds_assoc_type=>table_function.

        mv_entity_type = zif_sat_c_entity_type=>cds_view.
        TRY.
            DATA(lr_target_cds) = zcl_sat_cds_view_factory=>read_cds_view( ms_association-ref_cds_view ).
          CATCH zcx_sat_data_read_error INTO DATA(lx_read_error).
            MESSAGE lx_read_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        ENDTRY.
        DATA(ls_target_cds_header) = lr_target_cds->get_header( ).
        zcl_dbbr_cds_tabfield_util=>add_view_colums(
            ir_tabfield_list = mr_tabfields
            if_selection     = abap_false
            io_cds_view      = lr_target_cds
            if_is_primary    = abap_true ).

        IF lr_target_cds->has_parameters( ).
          determine_param_string( iv_cds_view_name = ms_association-ref_cds_view
                                  io_tabfields = mr_tabfields
                                  it_params = lr_target_cds->get_parameters( ) ).
        ENDIF.

      WHEN zif_sat_c_cds_assoc_type=>table OR
           zif_sat_c_cds_assoc_type=>view.

        mv_entity_type = zif_sat_c_entity_type=>table.
        zcl_dbbr_tabfield_builder=>create_tabfields(
            iv_tablename        = ms_association-ref_cds_view
            ir_tabfield_list    = mr_tabfields
            if_output_active    = abap_true
            if_is_primary       = abap_true
        ).
      WHEN OTHERS.
*... TODO: raise exception???
    ENDCASE.

  ENDMETHOD.


  METHOD export_data_to_memory.
    DATA: lv_mem_id          TYPE char32.

    FIELD-SYMBOLS: <lt_for_all_data> TYPE table.

    DATA(ls_tabfield_data) = mr_tabfields->convert_to_structure( ).

    IF mt_nav_breadcrumbs IS INITIAL.
      mt_nav_breadcrumbs = VALUE #( ( mr_source_cds_view->get_header( )-entityname_raw ) ).
    ENDIF.

    mt_nav_breadcrumbs = VALUE #( BASE mt_nav_breadcrumbs
      ( |{ ms_association-ref_cds_view_raw }| )
    ).

    DATA(ls_controller_data) = VALUE zif_dbbr_ty_global=>ty_sel_ctrl_serialized(
        entity_id                  = ms_association-ref_cds_view
        entity_type                = mv_entity_type
        entity_params              = mv_params
        technical_info             = ms_tech_info
        tabfields_data             = ls_tabfield_data
        tabfields_all_data         = ls_tabfield_data
        navigation_info            = ms_association
        navigation_breadcrumbs     = mt_nav_breadcrumbs
        navigation_count           = mv_nav_count + 1
        source_entity_id           = mr_source_cds_view->get_header( )-entityname
        source_entity_where_cond   = mt_where
        source_entity_params       = mv_source_params ).

    lv_mem_id = zif_dbbr_c_report_id=>main && sy-uname.

    EXPORT
      serialized = ls_controller_data
    TO MEMORY ID lv_mem_id.

  ENDMETHOD.


  METHOD fill_selection_fields.
    DATA: lt_and_tab  TYPE zif_sat_ty_global=>ty_t_and_seltab_sql,
          lt_or_tab   TYPE zif_sat_ty_global=>ty_t_or_seltab_sql,
          lt_cond_tab TYPE zif_sat_ty_global=>ty_t_seltab_sql.

    FIELD-SYMBOLS: <lt_source>       TYPE table,
                   <lt_for_all_data> TYPE table.

    ASSIGN mr_t_data->* TO <lt_source>.

*... create dynamic table to hold FOR ALL data
    DATA(lt_key_fields) = mr_source_cds_view->get_columns( ).
    DELETE lt_key_fields WHERE datatype = 'CLNT'
                            OR keyflag  = abap_false.
    ASSIGN mr_t_for_all_data->* TO <lt_for_all_data>.

    LOOP AT mt_source_index ASSIGNING FIELD-SYMBOL(<lv_index>).
      ASSIGN <lt_source>[ <lv_index> ] TO FIELD-SYMBOL(<ls_data>).

      CLEAR lt_cond_tab.
      LOOP AT lt_key_fields ASSIGNING FIELD-SYMBOL(<ls_key_field>).
        ASSIGN COMPONENT <ls_key_field>-fieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_key_field_val>).
        CHECK sy-subrc = 0.
        lt_cond_tab = VALUE #( BASE lt_cond_tab
          ( sqlfieldname = <ls_key_field>-fieldname
            sign         = 'I'
            option       = 'EQ'
            low          = <lv_key_field_val> )
        ).
      ENDLOOP.

      IF lt_cond_tab IS NOT INITIAL AND lines( lt_key_fields ) = lines( lt_cond_tab ).
        lt_or_tab = VALUE #( BASE lt_or_tab ( values = lt_cond_tab ) ).
      ENDIF.
    ENDLOOP.

*.. Create "NOT NULL" condition for a single field of the target
    DATA(lr_t_target_fields) = mr_tabfields->get_fields_ref( ).
    LOOP AT lr_t_target_fields->* ASSIGNING FIELD-SYMBOL(<ls_target_key_field>) WHERE datatype <> 'CLNT'.
      EXIT.
    ENDLOOP.
    CHECK <ls_target_key_field> IS ASSIGNED.

    lt_cond_tab = VALUE #(
      ( sign = 'I' option = zif_sat_c_options=>is_not_null sqlfieldname = |\\{ ms_association-raw_name }{ mv_params }-{ <ls_target_key_field>-fieldname_raw }| )
    ).
    DATA(lt_not_null_cond) = VALUE zif_sat_ty_global=>ty_t_or_seltab_sql( ( values = lt_cond_tab ) ).

    lt_and_tab = VALUE #(
      ( lt_not_null_cond ) ).

    IF lt_or_tab IS NOT INITIAL.
      APPEND lt_or_tab TO lt_and_tab.
    ENDIF.

    mt_where = zcl_sat_where_clause_builder=>create_and_condition( lt_and_tab ).

  ENDMETHOD.


  METHOD handle_messages.
    DATA(lv_memid) = CONV char32( |{ zif_dbbr_c_report_id=>output }{ sy-uname }MSG| ).
    DATA: lv_message  TYPE string,
          lv_msg_type TYPE sy-msgty.

    IMPORT
      message      = lv_message
      message_type = lv_msg_type
    FROM MEMORY ID lv_memid.

    IF sy-subrc = 0 AND lv_message IS NOT INITIAL.
      FREE MEMORY ID lv_memid.
      MESSAGE lv_message TYPE lv_msg_type.
    ENDIF.
  ENDMETHOD.


  METHOD zif_dbbr_table_navigator~navigate.
*... Maximum call stack level in ABAP is 9.
*... The first and second screen of the DB Browser already raise the number to 2,
*... so there are only 7 navigations possible by using the CALL TRANSACTION solution.
*...................................................................
*... A possible work around to increase the number of possible navigations would
*... to always buffer the current information about the selection screen to memory
*... and replace the current screen information with the target entity
    IF mv_nav_count = 7.
      RAISE EXCEPTION TYPE zcx_dbbr_association_nav_error
        EXPORTING
          textid = zcx_dbbr_association_nav_error=>max_navigation_level_reached.
    ENDIF.

*... create output fields
    create_output_fields( ).

*... create selection fields
    fill_selection_fields( ).

*... fill structure for memory export and
*... export data to memory
    export_data_to_memory( ).

*... start new transaction for assocation select
    CALL TRANSACTION 'ZDBBR_START_SEL'.

*... check if there was an error message
    handle_messages( ).
  ENDMETHOD.

  METHOD request_param_values.

    DATA(lo_custom_f4_map) = NEW zcl_dbbr_custom_f4_map( ).

    lo_custom_f4_map->read_custom_f4_definitions( iv_cds_view_name ).
    lo_custom_f4_map->read_same_type_custom_f4_defs( ).

*... create parameters
    zcl_dbbr_cds_tabfield_util=>add_parameters(
      ir_tabfield_list = io_tabfields
      io_custom_f4_map = lo_custom_f4_map
      it_parameters    = it_params ).

    DATA(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
      io_tabfields     = io_tabfields
      iv_cds_view_name = iv_cds_view_name ).

    lo_param_popup->show( ).
    rt_param_values = lo_param_popup->get_param_values( ).

    IF rt_param_values IS INITIAL.
      RAISE EXCEPTION TYPE zcx_dbbr_association_nav_error
        EXPORTING
          textid = zcx_dbbr_association_nav_error=>assoc_param_not_available.
    ENDIF.

  ENDMETHOD.

  METHOD determine_param_string.

    mv_params = zcl_dbbr_cds_param_util=>build_param_string(
      iv_param_indentation    = strlen( iv_cds_view_name )
      if_sep_param_by_newline = abap_false
      it_param_values         = request_param_values(
        it_params        = it_params
        iv_cds_view_name = iv_cds_view_name
        io_tabfields     = io_tabfields ) ).

  ENDMETHOD.

ENDCLASS.
