CLASS zcl_dbbr_dictionary_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_table_definition,
             tabname TYPE tabname,
             ddtext  TYPE ddtext,
             is_view TYPE abap_bool,
           END OF ty_table_definition.

    TYPES: tt_table_definition TYPE STANDARD TABLE OF ty_table_definition WITH EMPTY KEY.

    "! <p class="shorttext synchronized" lang="en">Retrieve database entity for the given name</p>
    "!
    CLASS-METHODS get_entity
      IMPORTING
        iv_entity_id     TYPE zdbbr_entity_id
      RETURNING
        VALUE(rs_entity) TYPE zdbbr_entity
      RAISING
        zcx_dbbr_data_read_error.
    "! <p class="shorttext synchronized" lang="en">Retrieve entities in search range</p>
    "!
    CLASS-METHODS get_entity_by_range
      IMPORTING
        it_entity_range TYPE zif_dbbr_global_types=>ty_tabname_range
      RETURNING
        VALUE(rt_entities) TYPE zdbbr_entity_t.
    "! <p class="shorttext synchronized" lang="en">Retrieve all mappings of domain to convexit</p>
    "!
    "! @parameter rt_convext2domain | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_domain_to_convexit_entries
      RETURNING
        VALUE(rt_convext2domain) TYPE zif_dbbr_global_types=>tt_domain2convext.

    "! <p class="shorttext synchronized" lang="en">Retrieve table/view/cds from clipboard</p>
    "!
    "! @parameter rt_entities | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_entities_from_clipboard
      EXPORTING
        VALUE(ev_clipboard_count) TYPE sy-tabix
      RETURNING
        VALUE(rt_entities)        TYPE zdbbr_entity_t.
    "! <p class="shorttext synchronized" lang="en">Find database tables/views</p>
    "!
    "! @parameter iv_package | <p class="shorttext synchronized" lang="en">Package for several </p>
    "! @parameter if_all | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_type | <p class="shorttext synchronized" lang="en">Specifies which entities should be searched</p>
    "! @parameter iv_tabname | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_tabname_range | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">List of search results</p>
    CLASS-METHODS find_database_tab_view
      IMPORTING
        iv_package       TYPE devclass OPTIONAL
        if_all           TYPE abap_bool OPTIONAL
        iv_type          TYPE zdbbr_entity_type OPTIONAL
        iv_tabname       TYPE tabname OPTIONAL
        it_tabname_range TYPE zif_dbbr_global_types=>ty_tabname_range OPTIONAL
      RETURNING
        VALUE(result)    TYPE zdbbr_entity_t.
    "! <p class="shorttext synchronized" lang="en">Find base tables for database view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rt_result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS find_base_tables_of_view
      IMPORTING
        iv_view_name     TYPE tabname
      RETURNING
        VALUE(rt_result) TYPE zdbbr_entity_t.
    CLASS-METHODS create_initial_raw_value
      IMPORTING
        iv_length     TYPE ddleng
      RETURNING
        VALUE(result) TYPE string.
    CLASS-METHODS call_language_value_help
      RETURNING
        VALUE(result) TYPE langu .
    CLASS-METHODS get_tables_for_package
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE zdbbr_tabinfo_itab .
    "! <p class="shorttext synchronized" lang="en">Navigate to Table/View via SE11</p>
    "!
    "! @parameter iv_tabname | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS navigate_to_table
      IMPORTING
        !iv_tabname TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Retrieve Text table</p>
    "!
    "! @parameter iv_tabname | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ev_text_table | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ev_text_key_field | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_text_table
      IMPORTING
        !iv_tabname              TYPE tabname
      EXPORTING
        VALUE(ev_text_table)     TYPE tabname
        VALUE(ev_text_key_field) TYPE fieldname .
    "! <p class="shorttext synchronized" lang="en">Checks if type is numeric</p>
    "!
    "! @parameter iv_internal_type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rf_is_numeric | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS is_type_numeric
      IMPORTING
        !iv_internal_type    TYPE inttype
      RETURNING
        VALUE(rf_is_numeric) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Checks if Data element is numeric</p>
    "!
    "! @parameter iv_dtel_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rf_is_numeric | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS is_dtel_numeric
      IMPORTING
        !iv_dtel_name        TYPE rollname
      RETURNING
        VALUE(rf_is_numeric) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter iv_data | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_ddtext | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_domain_fix_value_text
      IMPORTING
        !iv_data         TYPE any
      RETURNING
        VALUE(rv_ddtext) TYPE ddtext .
    CLASS-METHODS get_table_info
      IMPORTING
        !iv_tablename  TYPE tabname
      RETURNING
        VALUE(rs_info) TYPE dd02v .
    CLASS-METHODS get_table_field_infos
      IMPORTING
        !iv_tablename    TYPE tabname
      EXPORTING
        !et_table_fields TYPE dfies_table .
    CLASS-METHODS get_table_field_info
      IMPORTING
        !iv_tablename   TYPE tabname
        !iv_fieldname   TYPE fieldname
      RETURNING
        VALUE(rs_dfies) TYPE dfies .
    "! Validates the given table name if it is a valid
    "! database table or database view that can be used for
    "! selected data
    "! @parameter iv_table_name | the table name to be validated
    "! @parameter if_customizing_view_allowed | specify if you want to allow customizing views
    "! @parameter if_print_error_message | OBSOLETE: do not use anymore
    CLASS-METHODS validate_table_name
      IMPORTING
        !iv_table_name               TYPE tabname
        !iv_dynpro_fieldname         TYPE dynfnam OPTIONAL
        !if_customizing_view_allowed TYPE abap_bool OPTIONAL
        !if_print_error_message      TYPE abap_bool DEFAULT abap_true
      EXPORTING
        ef_is_view                   TYPE abap_bool.
    CLASS-METHODS validate_table_field
      IMPORTING
        !iv_table_field TYPE fieldname
        !iv_dynfname    TYPE dynfnam OPTIONAL
        !iv_loop_line   TYPE sy-tabix OPTIONAL
        !iv_table_name  TYPE tabname
      RETURNING
        VALUE(result)   TYPE dfies .
    CLASS-METHODS call_table_field_f4
      IMPORTING
        !iv_current_line       LIKE sy-tabix OPTIONAL
        !iv_dynpname_tablename TYPE dynfnam
        !iv_dynpname_fieldname TYPE dynfnam
        !iv_repid              TYPE sy-repid
      CHANGING
        !cv_fieldname          TYPE fieldname .
    CLASS-METHODS call_table_f4
      IMPORTING
        !iv_dynp_field_name TYPE devparname
        !iv_repid           TYPE sy-repid
      CHANGING
        !cv_table           TYPE tabname .
    CLASS-METHODS build_dynamic_sorted_table
      IMPORTING
        !it_fields          TYPE zdbbr_dfies_itab
        !if_unique_key      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rr_table_ref) TYPE REF TO data .
    CLASS-METHODS build_dynamic_hash_table
      IMPORTING
        !it_fields    TYPE zdbbr_dfies_itab
      RETURNING
        VALUE(result) TYPE REF TO data .
    CLASS-METHODS build_dynamic_table_with_keys
      IMPORTING
        !it_fields          TYPE zdbbr_dfies_itab
        !it_key_tab         TYPE abap_table_keydescr_tab
      RETURNING
        VALUE(rr_table_ref) TYPE REF TO data .
    CLASS-METHODS fill_data_cache_for_add_texts
      CHANGING
        !ct_add_texts TYPE zdbbr_additional_text_itab .
    CLASS-METHODS validate_data_element
      IMPORTING
        !iv_data_element TYPE rollname .
    CLASS-METHODS get_data_element
      IMPORTING
        !iv_data_element    TYPE rollname
      RETURNING
        VALUE(rs_dtel_info) TYPE dd04v .
    CLASS-METHODS get_dtel_inttype
      IMPORTING
        !iv_data_element        TYPE rollname
      RETURNING
        VALUE(rv_internal_type) TYPE inttype .
    CLASS-METHODS cross_reference_table
      IMPORTING
        !iv_tabname TYPE tabname .
    CLASS-METHODS check_for_possible_systype
      IMPORTING
        !iv_inttype           TYPE inttype
        !iv_system_value_type TYPE zdbbr_syst_value_type
      RETURNING
        VALUE(rf_possible)    TYPE boolean .
    CLASS-METHODS is_transaction_valid
      IMPORTING
        !iv_transaction TYPE tcode
      RETURNING
        VALUE(rf_valid) TYPE boolean .
    CLASS-METHODS build_dynamic_std_table
      IMPORTING
        !it_fields    TYPE zdbbr_dfies_itab
      RETURNING
        VALUE(result) TYPE REF TO data .
    CLASS-METHODS call_customizing_view
      IMPORTING
        !iv_view_name TYPE tabname .
    CLASS-METHODS check_package
      IMPORTING
        iv_package TYPE devclass .
    "! <p class="shorttext synchronized" lang="en">Read description for table/view</p>
    "!
    CLASS-METHODS get_table_description
      IMPORTING
        is_table_info         TYPE dd02v
      RETURNING
        VALUE(rv_description) TYPE dd02v-ddtext.
    "! <p class="shorttext synchronized" lang="en">Retrieve field information for rollname</p>
    "!
    CLASS-METHODS get_dfies_info_for_rollname
      IMPORTING
        iv_rollname     TYPE rollname
      RETURNING
        VALUE(rs_dfies) TYPE dfies.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS create_dynamic_table
      IMPORTING
        !it_fields         TYPE zdbbr_dfies_itab
        !it_key_defs       TYPE abap_table_keydescr_tab OPTIONAL
        !if_unique_key     TYPE abap_bool OPTIONAL
        !if_sorted_table   TYPE abap_bool OPTIONAL
        !if_hashed_table   TYPE abap_bool OPTIONAL
        !if_standard_table TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)      TYPE REF TO data .
ENDCLASS.



CLASS zcl_dbbr_dictionary_helper IMPLEMENTATION.


  METHOD build_dynamic_hash_table.
    result = create_dynamic_table(
       it_fields       = it_fields
       if_hashed_table = abap_true
    ).
  ENDMETHOD.


  METHOD build_dynamic_sorted_table.
*&---------------------------------------------------------------------*
*& Description: Builds a dynamic hashtable for the given list of
*& data elements.
*&---------------------------------------------------------------------*
    rr_table_ref = create_dynamic_table(
      it_fields       = it_fields
      if_sorted_table = abap_true
      if_unique_key   = if_unique_key
    ).
  ENDMETHOD.


  METHOD build_dynamic_std_table.
    result = create_dynamic_table(
      it_fields         = it_fields
      if_standard_table = abap_true
    ).
  ENDMETHOD.


  METHOD build_dynamic_table_with_keys.
    rr_table_ref = create_dynamic_table(
        it_fields         = it_fields
        it_key_defs       = it_key_tab
    ).
  ENDMETHOD.


  METHOD call_customizing_view.
    DATA: lt_selection_list        TYPE STANDARD TABLE OF vimsellist,
          lt_exclude_cua_functions TYPE STANDARD TABLE OF vimexclfun.


    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'SM30'
      EXCEPTIONS
        ok     = 0
        not_ok = 1.
    IF sy-subrc NE 0.
      MESSAGE e059(eu) WITH 'SM30'.   " keine Berechtigung
    ENDIF.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action              = 'S'
        view_name           = iv_view_name
        check_ddic_mainflag = 'X'        "n'1148568
      TABLES
        dba_sellist         = lt_selection_list
        excl_cua_funct      = lt_exclude_cua_functions
                              EXCEPTIONS OTHERS.
    IF sy-subrc NE 0.
      MESSAGE e404(mo) WITH iv_view_name.
    ENDIF.

  ENDMETHOD.


  METHOD call_language_value_help.
    DATA: lv_chosen_language TYPE se16n_value.

    zcl_dbbr_f4_helper=>call_built_in_f4(
      EXPORTING
        iv_tablename            = 'ZDBBR_TECH_INFO'
        iv_fieldname            = 'CURRENT_LANGUAGE'
      CHANGING
        cv_value                = lv_chosen_language
    ).
    result = lv_chosen_language.
  ENDMETHOD.


  METHOD call_table_f4.
*&---------------------------------------------------------------------*
*& Description: Calls f4 help for table name
*&---------------------------------------------------------------------*
    DATA: lt_dynpfields TYPE TABLE OF dynpread,
          lt_returntab  TYPE STANDARD TABLE OF ddshretval,
          lf_user_reset TYPE boolean.

    lt_dynpfields = VALUE #( ( fieldname = iv_dynp_field_name ) ).
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = iv_repid
        dynumb             = sy-dynnr
        translate_to_upper = abap_true
      TABLES
        dynpfields         = lt_dynpfields.

    DATA(ls_dynpfield) = lt_dynpfields[ 1 ].
    cv_table = ls_dynpfield-fieldvalue.
    IF cv_table = space.
      cv_table = '*'.
    ENDIF.

    DATA(lv_table_temp) = cv_table.

    CALL FUNCTION 'F4_DD_TABLES'
      EXPORTING
        object             = lv_table_temp
        suppress_selection = ''
      IMPORTING
        result             = lv_table_temp.

    cv_table = lv_table_temp.

  ENDMETHOD.


  METHOD call_table_field_f4.
*&---------------------------------------------------------------------*
*& Description: Calls value help for fields of db table
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF lty_value_tab,
             fieldname TYPE fieldname,
             key       TYPE keyflag,
             fieldtext TYPE ddtext,
           END OF lty_value_tab.

    DATA: lv_table  TYPE tabname,
          lv_field  TYPE dynfieldvalue,
          lt_values TYPE TABLE OF lty_value_tab,
          lt_return TYPE TABLE OF ddshretval,
          lf_reset  TYPE boolean.

    DATA(lr_dynpfield_mng) = NEW zcl_uitb_screen_field_manager( iv_repid = iv_repid ).
    lr_dynpfield_mng->read_values(
        it_fieldname_selopt = VALUE #( ( sign = 'I' option = 'EQ' low = iv_dynpname_tablename )
                                       ( sign = 'I' option = 'EQ' low = iv_dynpname_fieldname ) )
        iv_step_loop_index  = iv_current_line
    ).

    lr_dynpfield_mng->get_value( EXPORTING iv_fieldname       = iv_dynpname_tablename
                                           iv_step_loop_index = iv_current_line
                                 IMPORTING ev_value           = lv_table                ).
    lr_dynpfield_mng->get_value( EXPORTING iv_fieldname       = iv_dynpname_fieldname
                                           iv_step_loop_index = iv_current_line
                                 IMPORTING ev_value           = lv_field                ).

    " no search help without table name
    IF lv_table = space.
      RETURN.
    ENDIF.

    zcl_dbbr_dictionary_helper=>get_table_field_infos(
      EXPORTING iv_tablename    = lv_table
      IMPORTING et_table_fields = DATA(lt_table_fields)
    ).

    lt_values = VALUE #( FOR dfies IN lt_table_fields ( fieldname = dfies-fieldname
                                                        key       = dfies-keyflag
                                                        fieldtext = dfies-fieldtext ) ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'FIELDNAME'
        value_org       = 'S'
        multiple_choice = space
        value           = lv_field
      IMPORTING
        user_reset      = lf_reset
      TABLES
        value_tab       = lt_values
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF lt_return IS NOT INITIAL AND lf_reset = abap_false.
      cv_fieldname = lt_return[ 1 ]-fieldval.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_for_possible_systype.
    rf_possible = abap_true.

    CASE iv_system_value_type.
      WHEN zif_dbbr_global_consts=>gc_system_value_types-date.
        IF iv_inttype <> cl_abap_typedescr=>typekind_date.
          rf_possible = abap_false.
        ENDIF.

      WHEN zif_dbbr_global_consts=>gc_system_value_types-time.
        IF iv_inttype <> cl_abap_typedescr=>typekind_time.
          rf_possible = abap_false.
        ENDIF.

      WHEN zif_dbbr_global_consts=>gc_system_value_types-user.
        IF iv_inttype <> cl_abap_typedescr=>typekind_char.
          rf_possible = abap_false.
        ENDIF.

      WHEN zif_dbbr_global_consts=>gc_system_value_types-language.
        IF iv_inttype <> cl_abap_typedescr=>typekind_char.
          rf_possible = abap_false.
        ENDIF.

      WHEN OTHERS.
        rf_possible = abap_false.

    ENDCASE.
  ENDMETHOD.


  METHOD check_package.
    cl_package_helper=>check_package_existence(
      EXPORTING
        i_package_name          = iv_package
      IMPORTING
        e_package_exists        = DATA(lf_exists)
      EXCEPTIONS
        intern_err              = 1
        package_hierarchy_error = 2
        OTHERS                  = 3
    ).

    IF sy-subrc <> 0.
      zcx_dbbr_validation_exception=>raise_from_sy( ).
    ELSE.
      IF lf_exists = abap_false.
        RAISE EXCEPTION TYPE zcx_dbbr_validation_exception
          EXPORTING
            textid = zcx_dbbr_validation_exception=>package_not_existing
            msgv1  = |{ iv_package }|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_dynamic_table.
    DATA: lt_component_table TYPE abap_component_tab,
          lv_type_name       TYPE string,
          lt_key_table       TYPE abap_keydescr_tab.

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>).

      """ collect key fields
      IF <ls_field>-keyflag = abap_true.
        APPEND VALUE abap_keydescr( name = <ls_field>-fieldname ) TO lt_key_table.
      ENDIF.


      IF <ls_field>-rollname IS NOT INITIAL.
        lv_type_name = <ls_field>-rollname.
      ELSE.
        lv_type_name = <ls_field>-tabname && '-' && <ls_field>-fieldname.
      ENDIF.

      cl_abap_typedescr=>describe_by_name(
        EXPORTING
          p_name         = lv_type_name
        RECEIVING
          p_descr_ref    = DATA(lr_type)
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2
      ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      APPEND VALUE abap_componentdescr(
          name       = <ls_field>-fieldname
          type       = CAST cl_abap_datadescr( lr_type )
      ) TO lt_component_table.

    ENDLOOP.

    IF it_key_defs IS INITIAL AND lt_key_table IS INITIAL AND lt_component_table IS INITIAL.
      RETURN.
    ENDIF.

    """ create the hashed table
    DATA(lr_new_type) = cl_abap_structdescr=>create( lt_component_table ).

    " create the table type for the structure type
    DATA(lr_new_tab) = COND #(
      WHEN it_key_defs IS INITIAL THEN
        cl_abap_tabledescr=>create(
          p_line_type  = lr_new_type
          p_table_kind = COND #( WHEN if_sorted_table = abap_true THEN
                                    cl_abap_tabledescr=>tablekind_sorted
                                 WHEN if_hashed_table = abap_true THEN
                                    cl_abap_tabledescr=>tablekind_hashed
                                 WHEN if_standard_table = abap_true THEN
                                    cl_abap_tabledescr=>tablekind_std )
          p_key_kind   = COND #( WHEN if_standard_table = abap_true THEN cl_abap_tabledescr=>keydefkind_default ELSE cl_abap_tabledescr=>keydefkind_user )
          p_key        = COND #( WHEN if_standard_table = abap_false THEN lt_key_table )
          p_unique     = COND #( WHEN if_hashed_table = abap_true THEN abap_true ELSE if_unique_key )
        )
      ELSE
        cl_abap_tabledescr=>create_with_keys(
            p_line_type = lr_new_type
            p_keys      = it_key_defs )
    ).

    CREATE DATA result TYPE HANDLE lr_new_tab.

  ENDMETHOD.


  METHOD cross_reference_table.
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM'
      EXPORTING
        object_type = 'TABL'
        action      = 'C'
        object_name = iv_tabname
*       suppress_selection = 'X'
*       show_as_popup      = 'X'
      EXCEPTIONS
        cancel      = 1
        wrong_type  = 2
        OTHERS      = 3.

  ENDMETHOD.


  METHOD fill_data_cache_for_add_texts.
*& Description: Fills data caches for additional text selection
*&---------------------------------------------------------------------*
    LOOP AT ct_add_texts ASSIGNING FIELD-SYMBOL(<ls_add_text>).
      CASE <ls_add_text>-selection_type.

        WHEN zif_dbbr_c_text_selection_type=>domain_value.
          DATA(lr_elemdescr) = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( <ls_add_text>-id_field_rollname ) ).
          <ls_add_text>-domain_fix_values = lr_elemdescr->get_ddic_fixed_values( ).

        WHEN zif_dbbr_c_text_selection_type=>table OR
             zif_dbbr_c_text_selection_type=>text_table.

          " build dynamic hashtable for text selection
          " 2) collect fields for hash table
          DATA(lt_fields) = VALUE zdbbr_dfies_itab(
              ( tabname   = <ls_add_text>-text_table
                fieldname = <ls_add_text>-key_field
                keyflag   = abap_true )
          ).

          IF <ls_add_text>-key_field2 IS NOT INITIAL.
            lt_fields = VALUE #(
                BASE lt_fields
                ( tabname   = <ls_add_text>-text_table
                  fieldname = <ls_add_text>-key_field2
                  keyflag   = abap_true )
            ).
          ENDIF.
          lt_fields = VALUE #(
              BASE lt_fields
              ( tabname   = <ls_add_text>-text_table
                fieldname = <ls_add_text>-text_field )
          ).

          <ls_add_text>-table_cache = build_dynamic_sorted_table(
            it_fields     = lt_fields
            if_unique_key = abap_false
          ).
          IF <ls_add_text>-table_cache IS INITIAL.
            DELETE ct_add_texts.
            CONTINUE.
          ENDIF.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_data_element.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = iv_data_element     " Name des zu lesenden Datenelements
        langu         = zcl_dbbr_appl_util=>get_description_language( )
      IMPORTING
        dd04v_wa      = rs_dtel_info    " Header des Datenelements
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
  ENDMETHOD.


  METHOD get_dfies_info_for_rollname.
    DATA(lr_elem_descr) = CAST cl_abap_elemdescr(
        cl_abap_elemdescr=>describe_by_name( iv_rollname )
    ).
    rs_dfies = lr_elem_descr->get_ddic_field( p_langu = zcl_dbbr_appl_util=>get_description_language( ) ).
  ENDMETHOD.


  METHOD get_domain_fix_value_text.
*& Description: Returns the text for the given domain fix value
*&---------------------------------------------------------------------*
    CHECK iv_data IS NOT INITIAL.

    DATA(lr_data_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( iv_data ) ).
    lr_data_descr->get_ddic_fixed_values(
      EXPORTING
        p_langu        = zcl_dbbr_appl_util=>get_description_language( )
      RECEIVING
        p_fixed_values = DATA(lt_fix_values)
      EXCEPTIONS
        not_found      = 1
        no_ddic_type   = 2
        OTHERS         = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      rv_ddtext = lt_fix_values[ low = CONV #( iv_data ) ]-ddtext.
    ENDIF.
  ENDMETHOD.


  METHOD get_dtel_inttype.
    DATA(lr_dtel_type) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( iv_data_element ) ).
    rv_internal_type = lr_dtel_type->get_ddic_field( )-inttype.
  ENDMETHOD.


  METHOD get_tables_for_package.
    DATA: lt_tadir TYPE STANDARD TABLE OF tabname.

    SELECT obj_name INTO TABLE lt_tadir
      FROM tadir
      WHERE pgmid    = 'R3TR'
        AND object   = 'TABL'
        AND devclass = iv_package.

    IF lt_tadir IS INITIAL.
      RETURN.
    ENDIF.

    " only return db tables
    SELECT tabname ddtext AS description INTO CORRESPONDING FIELDS OF TABLE result
      FROM dd02v
      FOR ALL ENTRIES IN lt_tadir
      WHERE tabname = lt_tadir-table_line
        AND tabclass = 'TRANSP'
        AND ddlanguage = sy-langu.

  ENDMETHOD.


  METHOD get_table_field_info.
*& Description: Returns field infos for table field
*&---------------------------------------------------------------------*
    DATA: lv_fieldname TYPE dfies-lfieldname.

    lv_fieldname = iv_fieldname.
    " get components for table
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = iv_tablename
        lfieldname     = lv_fieldname
        langu          = zcl_dbbr_appl_util=>get_description_language( )
      IMPORTING
        dfies_wa       = rs_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

  ENDMETHOD.


  METHOD get_table_field_infos.
*& Description: Returns infos for table fields of the specified table
*&---------------------------------------------------------------------*
    CLEAR et_table_fields.

    " get components for table
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = iv_tablename
        langu          = zcl_dbbr_appl_util=>get_description_language( )
      TABLES
        dfies_tab      = et_table_fields
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CLEAR et_table_fields.
    ENDIF.

  ENDMETHOD.


  METHOD get_table_info.
*& Description: Returns infos for the specified table
*&---------------------------------------------------------------------*
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = iv_tablename
        langu         = zcl_dbbr_appl_util=>get_description_language( )
      IMPORTING
        dd02v_wa      = rs_info
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      CLEAR rs_info.
    ELSE.
      IF rs_info-tabclass = 'VIEW' OR rs_info-ddtext IS INITIAL.
        rs_info-ddtext = get_table_description( rs_info ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_text_table.
*& Description: Returns the text table (if one exists) for the given
*& table name
*&---------------------------------------------------------------------*
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname    = iv_tabname
      IMPORTING
        texttable  = ev_text_table
        checkfield = ev_text_key_field.
  ENDMETHOD.


  METHOD is_dtel_numeric.
    DATA(lr_dtel_type) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( iv_dtel_name ) ).
    DATA(lv_inttype) = lr_dtel_type->get_ddic_field( )-inttype.

    rf_is_numeric = is_type_numeric( lv_inttype ).
  ENDMETHOD.


  METHOD is_transaction_valid.
    SELECT SINGLE tcode FROM tstc INTO @DATA(lv_tcode)
      WHERE tcode = @iv_transaction.

    IF sy-subrc = 0.
      rf_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_type_numeric.
    IF iv_internal_type = cl_abap_datadescr=>typekind_int OR
       iv_internal_type = cl_abap_datadescr=>typekind_float OR
       iv_internal_type = cl_abap_datadescr=>typekind_packed.
      rf_is_numeric = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD navigate_to_table.
    SELECT SINGLE @abap_true
      FROM dd02l
      WHERE tabname = @iv_tabname
        AND tabclass = 'VIEW'
    INTO @DATA(lf_is_view).

    DATA(lv_object_type) = COND ddeutype( WHEN lf_is_view = abap_true THEN 'V' ELSE 'T' ).

    CALL FUNCTION 'RS_DD_SHOW'
      EXPORTING
        objname = iv_tabname
        objtype = lv_object_type
*       popup   = space
*       secname =
*       monitor_activate     = 'X'
*    IMPORTING
*       fcode   =
*    EXCEPTIONS
*       object_not_found     = 1
*       object_not_specified = 2
*       permission_failure   = 3
*       type_not_valid       = 4
*       others  = 5
      .
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD validate_data_element.
    DATA(ls_dtel) = get_data_element( iv_data_element ).
    IF ls_dtel IS INITIAL.
      MESSAGE e513(0k) WITH iv_data_element ''.
    ENDIF.

  ENDMETHOD.


  METHOD validate_table_field.
    result = get_table_field_info(
       iv_tablename = iv_table_name
       iv_fieldname = iv_table_field
    ).

    IF result IS INITIAL.
      MESSAGE e018(zdbbr_info) WITH iv_table_field iv_table_name INTO DATA(lv_dummy).
      zcx_dbbr_validation_exception=>raise_from_sy(
          iv_parameter = iv_dynfname
          iv_line      = iv_loop_line
      ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_table_name.
*&---------------------------------------------------------------------*
*& Description: Validation of the input table name
*&---------------------------------------------------------------------*
    DATA: lv_object_type TYPE objstate.

    CHECK: iv_table_name <> space.

    DATA(ls_table_info) = get_table_info( iv_table_name ).

    IF ls_table_info IS NOT INITIAL.
      IF ls_table_info-tabclass <> 'TRANSP' AND
         ls_table_info-tabclass <> 'VIEW' AND
         ls_table_info-tabclass <> 'POOL'.
        MESSAGE e001(wusl) WITH iv_table_name lv_object_type INTO DATA(lv_message).
        zcx_dbbr_validation_exception=>raise_from_sy(
          iv_parameter = iv_dynpro_fieldname
        ).
      ELSE.
        IF ( ls_table_info-tabclass = 'VIEW' AND ls_table_info-viewclass = 'C' ) AND
             if_customizing_view_allowed = abap_false.
          MESSAGE e001(wusl) WITH iv_table_name lv_object_type INTO lv_message.
          zcx_dbbr_validation_exception=>raise_from_sy(
            iv_parameter = iv_dynpro_fieldname
          ).
        ENDIF.

      ENDIF.
    ELSE.
      MESSAGE e007(e2) WITH iv_table_name INTO lv_message.
      zcx_dbbr_validation_exception=>raise_from_sy(
        iv_parameter = iv_dynpro_fieldname
      ).
    ENDIF.

    ef_is_view = ls_table_info-tabclass = 'VIEW'.

  ENDMETHOD.

  METHOD create_initial_raw_value.
    DATA: lr_v_raw TYPE REF TO data,
          lv_raw   TYPE c LENGTH 128.

    DATA(lr_raw_descr) = cl_abap_elemdescr=>get_x( p_length = CONV #( iv_length ) ).
    CREATE DATA lr_v_raw TYPE HANDLE lr_raw_descr.
    ASSIGN lr_v_raw->* TO FIELD-SYMBOL(<lv_raw_value>).

    WRITE |{ <lv_raw_value> }| TO lv_raw.
    result = lv_raw.
  ENDMETHOD.

  METHOD find_database_tab_view.
    DATA: lt_db_range          TYPE RANGE OF tabname,
          lt_package_range     TYPE RANGE OF devclass,
          lt_entity_type_range TYPE RANGE OF zdbbr_entity_type,
          lt_tables            LIKE result,
          lt_views             LIKE result.

    DATA(lv_max_rows) = COND #( WHEN if_all = abap_true THEN 0 ELSE 50 ).

    IF it_tabname_range IS NOT INITIAL.
      lt_db_range = it_tabname_range.
    ELSEIF iv_tabname IS NOT INITIAL.
      lt_db_range = VALUE #( ( sign = 'I' option = 'CP' low = iv_tabname ) ).
    ELSEIF iv_package IS NOT INITIAL.
      lt_package_range = VALUE #( ( sign = 'I' option = 'EQ' low = iv_package ) ).
    ELSE.
      RETURN.
    ENDIF.


    DATA(lv_descr_language) = zcl_dbbr_appl_util=>get_description_language( ).

    IF iv_type IS NOT INITIAL.

      CASE iv_type.

        WHEN zif_dbbr_c_entity_type=>table.
          SELECT tablename AS entity_id, tablename AS entity_id_raw, type AS entity_type, description
            FROM zdbbr_i_databasetable( p_language = @lv_descr_language )
            WHERE developmentpackage IN @lt_package_range
              AND tablename             IN @lt_db_range
          INTO CORRESPONDING FIELDS OF TABLE @result
            UP TO @lv_max_rows ROWS.
        WHEN zif_dbbr_c_entity_type=>view.
          SELECT viewname AS entity_id, viewname AS entity_id_raw, type AS entity_type, description
            FROM zdbbr_i_databaseview( p_language = @lv_descr_language )
            WHERE developmentpackage IN @lt_package_range
              AND viewname             IN @lt_db_range
          INTO CORRESPONDING FIELDS OF TABLE @result
            UP TO @lv_max_rows ROWS.
      ENDCASE.
    ELSE.

      SELECT entity AS entity_id, entity AS entity_id_raw, type AS entity_type, description
        FROM zdbbr_i_databaseentity( p_language = @lv_descr_language )
        WHERE developmentpackage IN @lt_package_range
          AND entity             IN @lt_db_range
          AND type               <> @zif_dbbr_c_entity_type=>cds_view
      INTO CORRESPONDING FIELDS OF TABLE @result
        UP TO @lv_max_rows ROWS.

    ENDIF.


  ENDMETHOD.

  METHOD get_entities_from_clipboard.
    DATA: lt_clipboard_data    TYPE filetable,
          lv_entity            TYPE tabname,
          lt_possible_entities TYPE STANDARD TABLE OF tabname,
          lt_dd02l_filter      TYPE TABLE OF dd02l-tabname,
          lt_dd02b_filter      TYPE TABLE OF dd02b-strucobjn,
          lv_rest_content      TYPE string.


    cl_gui_frontend_services=>clipboard_import( IMPORTING data = lt_clipboard_data ).

    CHECK lt_clipboard_data IS NOT INITIAL.

    LOOP AT lt_clipboard_data ASSIGNING FIELD-SYMBOL(<ls_clipboard_data>).
      SPLIT <ls_clipboard_data>-filename AT space INTO lv_entity lv_rest_content.
      CHECK lv_entity IS NOT INITIAL.

      lv_entity = to_upper( lv_entity ).
      lt_possible_entities = VALUE #( BASE lt_possible_entities ( lv_entity ) ).
    ENDLOOP.

    IF lt_possible_entities IS INITIAL.
      RETURN.
    ENDIF.

    ev_clipboard_count = lines( lt_possible_entities ).

    DATA(lv_description_language) = zcl_dbbr_appl_util=>get_description_language( ).

    SELECT *
      FROM zdbbr_i_databaseentity( p_language = @lv_description_language )
      FOR ALL ENTRIES IN @lt_possible_entities
      WHERE entity = @lt_possible_entities-table_line
    INTO TABLE @DATA(lt_entities).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*.. table/view already have their descriptions and can be moved directly to the result
    rt_entities = VALUE #(
        FOR table_view IN lt_entities
        WHERE ( type <> zif_dbbr_c_entity_type=>cds_view )
        ( entity_id     = table_view-entity
          entity_id_raw = table_view-entityraw
          entity_type   = table_view-type
          description   = table_view-description )
    ).

*.. CDS Views have to be handled separately to get the platform specific description
    IF line_exists( lt_entities[ type = zif_dbbr_c_entity_type=>cds_view ] ).
      rt_entities = VALUE #( BASE rt_entities
        FOR cds_header IN zcl_dbbr_cds_view_factory=>read_cds_view_header_multi(
            it_cds_view_name = VALUE #( FOR cds IN lt_entities WHERE ( type = zif_dbbr_c_entity_type=>cds_view ) ( cds-entity ) )
        )
        ( entity_id     = cds_header-strucobjn
          entity_id_raw = cds_header-strucobjn_raw
          description   = cds_header-ddtext
          entity_type   = zif_dbbr_c_entity_type=>cds_view )
      ).
    ENDIF.

  ENDMETHOD.

  METHOD find_base_tables_of_view.
    DATA(lv_descr_language) = zcl_dbbr_appl_util=>get_description_language( ).

    SELECT basetable AS entity_id,
           basetable AS entity_id_raw,
           CASE ddictype WHEN 'VIEW' THEN 'V' WHEN 'TABL' THEN 'T' END AS entity_type,
           ddtext AS description
      FROM zdbbrdd26s_v AS base
        LEFT OUTER JOIN dd02t AS text ON base~basetable = text~tabname
                                     AND text~ddlanguage = @lv_descr_language
      WHERE ddlview = @iv_view_name
    INTO CORRESPONDING FIELDS OF TABLE @rt_result.

  ENDMETHOD.


  METHOD get_table_description.
    DATA(lv_language) = zcl_dbbr_appl_util=>get_description_language( ).

    IF is_table_info-tabclass = 'VIEW'.
      SELECT SINGLE ddtext INTO @rv_description
          FROM dd25t
          WHERE viewname = @is_table_info-tabname
            AND ( ddlanguage = @lv_language
               OR ddlanguage = 'EN' ).
    ELSE.
      SELECT SINGLE ddtext INTO @rv_description
        FROM dd02t
        WHERE tabname = @is_table_info-tabname
          AND ( ddlanguage = @lv_language
             OR ddlanguage = 'EN' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_domain_to_convexit_entries.
    TYPES: BEGIN OF lty_convexit2dom.
        INCLUDE TYPE zdbbr_conve2dom.
    TYPES: funcname TYPE funcname.
    TYPES: END OF lty_convexit2dom.

    DATA: lt_conv2domain TYPE TABLE OF lty_convexit2dom.

*.. Select all defined mapping entries of conversion exits to domain
    SELECT *
      FROM zdbbr_conve2dom
    INTO CORRESPONDING FIELDS OF TABLE @lt_conv2domain.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_conv2domain ASSIGNING FIELD-SYMBOL(<ls_conv2dom>).
      <ls_conv2dom>-funcname = |CONVERSION_EXIT_{ <ls_conv2dom>-convexit }_OUTPUT|.
    ENDLOOP.

*.. Validate the conversion exits
    SELECT funcname
      FROM tfdir
      FOR ALL ENTRIES IN @lt_conv2domain
      WHERE funcname = @lt_conv2domain-funcname
    INTO TABLE @DATA(lt_found_func_output).

    IF sy-subrc <> 0.
      CLEAR rt_convext2domain.
    ELSE.
      LOOP AT lt_conv2domain ASSIGNING <ls_conv2dom>.
        IF line_exists( lt_found_func_output[ table_line = <ls_conv2dom>-funcname ] ).
          rt_convext2domain = VALUE #( BASE rt_convext2domain ( CORRESPONDING #( <ls_conv2dom> ) ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_entity.
    DATA(lv_language) = zcl_dbbr_appl_util=>get_description_language( ).

    SELECT SINGLE entity AS entity_id,
                  entityraw AS entity_id_raw,
                  type AS entity_type,
                  description
      FROM zdbbr_i_databaseentity( p_language = @lv_language )
      WHERE entity = @iv_entity_id
    INTO CORRESPONDING FIELDS OF @rs_entity.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dbbr_data_read_error
        EXPORTING
          textid = zcx_dbbr_data_read_error=>db_entity_not_existing
          msgv1  = |{ iv_entity_id } |.
    ENDIF.
  ENDMETHOD.


  METHOD get_entity_by_range.
    DATA(lv_language) = zcl_dbbr_appl_util=>get_description_language( ).

    SELECT entity AS entity_id,
           entityraw AS entity_id_raw,
           type AS entity_type,
           description
      FROM zdbbr_i_databaseentity( p_language = @lv_language )
      WHERE entity IN @it_entity_range
    INTO CORRESPONDING FIELDS OF TABLE @rt_entities.
  ENDMETHOD.

ENDCLASS.
