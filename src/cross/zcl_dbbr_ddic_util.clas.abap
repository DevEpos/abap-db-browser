"! <p class="shorttext synchronized" lang="en">Helper for DDIC objects</p>
CLASS zcl_dbbr_ddic_util DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_domain2convext TYPE STANDARD TABLE OF zdbbr_conve2dom WITH KEY convexit domname.
    TYPES: BEGIN OF ty_table_definition,
             tabname TYPE tabname,
             ddtext  TYPE ddtext,
             is_view TYPE abap_bool,
           END OF ty_table_definition.

    TYPES: tt_table_definition TYPE STANDARD TABLE OF ty_table_definition WITH EMPTY KEY.

    "! <p class="shorttext synchronized" lang="en">Check if there is a single DB entry for the given entity</p>
    CLASS-METHODS exists_data_for_entity
      IMPORTING
        iv_entity_id          TYPE zsat_entity_id
      RETURNING
        VALUE(rf_data_exists) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Retrieve all mappings of domain to convexit</p>
    "!
    "! @parameter rt_convext2domain | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_domain_to_convexit_entries
      RETURNING
        VALUE(rt_convext2domain) TYPE tt_domain2convext.

    "! <p class="shorttext synchronized" lang="en">Retrieve table/view/cds from clipboard</p>
    "!
    "! @parameter rt_entities | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_entities_from_clipboard
      EXPORTING
        VALUE(ev_clipboard_count) TYPE sy-tabix
      RETURNING
        VALUE(rt_entities)        TYPE zsat_entity_t.
    "! <p class="shorttext synchronized" lang="en">Create initial RAW value</p>
    CLASS-METHODS create_initial_raw_value
      IMPORTING
        iv_length     TYPE ddleng
      RETURNING
        VALUE(result) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Navigate to Table/View via SE11</p>
    "!
    "! @parameter iv_tabname | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS navigate_to_table
      IMPORTING
        !iv_tabname TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Checks if type is numeric</p>
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

    "! <p class="shorttext synchronized" lang="en">Validates the given table name</p>
    "! Validates the given table name if it is a valid
    "! database table or database view that can be used for
    "! selected data
    CLASS-METHODS validate_table_name
      IMPORTING
        !iv_table_name               TYPE tabname
        !iv_dynpro_fieldname         TYPE dynfnam OPTIONAL
        !if_customizing_view_allowed TYPE abap_bool OPTIONAL
        !if_print_error_message      TYPE abap_bool DEFAULT abap_true
      EXPORTING
        ef_is_view                   TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Validates table field</p>
    CLASS-METHODS validate_table_field
      IMPORTING
        !iv_table_field TYPE fieldname
        !iv_dynfname    TYPE dynfnam OPTIONAL
        !iv_loop_line   TYPE sy-tabix OPTIONAL
        !iv_table_name  TYPE tabname
      RETURNING
        VALUE(result)   TYPE dfies .
    "! <p class="shorttext synchronized" lang="en">Call table field f4</p>
    CLASS-METHODS call_table_field_f4
      IMPORTING
        !iv_current_line       LIKE sy-tabix OPTIONAL
        !iv_dynpname_tablename TYPE dynfnam
        !iv_dynpname_fieldname TYPE dynfnam
        !iv_repid              TYPE sy-repid
      CHANGING
        !cv_fieldname          TYPE fieldname .
    "! <p class="shorttext synchronized" lang="en">Call Table F4</p>
    CLASS-METHODS call_table_f4
      IMPORTING
        !iv_dynp_field_name TYPE devparname
        !iv_repid           TYPE sy-repid
      CHANGING
        !cv_table           TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Build dynamic standard table</p>
    CLASS-METHODS build_dynamic_sorted_table
      IMPORTING
        !it_fields          TYPE zsat_dfies_itab
        !if_unique_key      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rr_table_ref) TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Build dynamic hash table</p>
    CLASS-METHODS build_dynamic_hash_table
      IMPORTING
        !it_fields    TYPE zsat_dfies_itab
      RETURNING
        VALUE(result) TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Build dynamic table with keys</p>
    CLASS-METHODS build_dynamic_table_with_keys
      IMPORTING
        !it_fields          TYPE zsat_dfies_itab
        !it_key_tab         TYPE abap_table_keydescr_tab
      RETURNING
        VALUE(rr_table_ref) TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Fills data cache for additional texts</p>
    CLASS-METHODS fill_data_cache_for_add_texts
      CHANGING
        !ct_add_texts TYPE zdbbr_additional_text_itab .
    "! <p class="shorttext synchronized" lang="en">Validates data element name</p>
    CLASS-METHODS validate_data_element
      IMPORTING
        !iv_data_element TYPE rollname .
    "! <p class="shorttext synchronized" lang="en">Trigger where used </p>
    CLASS-METHODS cross_reference_table
      IMPORTING
        !iv_tabname TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Check if possible system type</p>
    CLASS-METHODS check_for_possible_systype
      IMPORTING
        !iv_inttype           TYPE inttype
        !iv_system_value_type TYPE zsat_syst_value_type
      RETURNING
        VALUE(rf_possible)    TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Check if the given transaction is valid</p>
    CLASS-METHODS is_transaction_valid
      IMPORTING
        !iv_transaction TYPE tcode
      RETURNING
        VALUE(rf_valid) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Builds dynamic standard table</p>
    CLASS-METHODS build_dynamic_std_table
      IMPORTING
        !it_fields    TYPE zsat_dfies_itab
      RETURNING
        VALUE(result) TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Call the customizing view of the given table</p>
    CLASS-METHODS call_customizing_view
      IMPORTING
        !iv_view_name TYPE tabname .
    "! <p class="shorttext synchronized" lang="en">Checks the given package</p>
    CLASS-METHODS check_package
      IMPORTING
        iv_package TYPE devclass .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS create_dynamic_table
      IMPORTING
        !it_fields         TYPE zsat_dfies_itab
        !it_key_defs       TYPE abap_table_keydescr_tab OPTIONAL
        !if_unique_key     TYPE abap_bool OPTIONAL
        !if_sorted_table   TYPE abap_bool OPTIONAL
        !if_hashed_table   TYPE abap_bool OPTIONAL
        !if_standard_table TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)      TYPE REF TO data .
ENDCLASS.



CLASS zcl_dbbr_ddic_util IMPLEMENTATION.

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

    zcl_sat_ddic_repo_access=>get_table_field_infos(
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
      WHEN zif_sat_c_system_value_type=>date.
        IF iv_inttype <> cl_abap_typedescr=>typekind_date.
          rf_possible = abap_false.
        ENDIF.

      WHEN zif_sat_c_system_value_type=>time.
        IF iv_inttype <> cl_abap_typedescr=>typekind_time.
          rf_possible = abap_false.
        ENDIF.

      WHEN zif_sat_c_system_value_type=>user.
        IF iv_inttype <> cl_abap_typedescr=>typekind_char.
          rf_possible = abap_false.
        ENDIF.

      WHEN zif_sat_c_system_value_type=>language.
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
      zcx_sat_validation_exception=>raise_from_sy( ).
    ELSE.
      IF lf_exists = abap_false.
        RAISE EXCEPTION TYPE zcx_sat_validation_exception
          EXPORTING
            textid = zcx_sat_validation_exception=>package_not_existing
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
    DELETE ADJACENT DUPLICATES FROM lt_component_table COMPARING name.

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
          <ls_add_text>-domain_fix_values = lr_elemdescr->get_ddic_fixed_values( p_langu = zcl_sat_system_helper=>get_system_language( ) ).

        WHEN zif_dbbr_c_text_selection_type=>table OR
             zif_dbbr_c_text_selection_type=>text_table.

          " build dynamic hashtable for text selection
          " 2) collect fields for hash table
          DATA(lt_fields) = VALUE zsat_dfies_itab(
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
    DATA(ls_dtel) = zcl_sat_ddic_repo_access=>get_data_element( iv_data_element ).
    IF ls_dtel IS INITIAL.
      MESSAGE e513(0k) WITH iv_data_element ''.
    ENDIF.

  ENDMETHOD.


  METHOD validate_table_field.
    result = zcl_sat_ddic_repo_access=>get_table_field_info(
       iv_tablename = iv_table_name
       iv_fieldname = iv_table_field
    ).

    IF result IS INITIAL.
      MESSAGE e018(zdbbr_info) WITH iv_table_field iv_table_name INTO DATA(lv_dummy).
      zcx_sat_validation_exception=>raise_from_sy(
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

    DATA(ls_table_info) = zcl_sat_ddic_repo_access=>get_table_info( iv_table_name ).

    IF ls_table_info IS NOT INITIAL.
      IF ls_table_info-tabclass <> 'TRANSP' AND
         ls_table_info-tabclass <> 'VIEW' AND
         ls_table_info-tabclass <> 'POOL'.
        MESSAGE e001(wusl) WITH iv_table_name lv_object_type INTO DATA(lv_message).
        zcx_sat_validation_exception=>raise_from_sy(
          iv_parameter = iv_dynpro_fieldname
        ).
      ELSE.
        IF ( ls_table_info-tabclass = 'VIEW' AND ls_table_info-viewclass = 'C' ) AND
             if_customizing_view_allowed = abap_false.
          MESSAGE e001(wusl) WITH iv_table_name lv_object_type INTO lv_message.
          zcx_sat_validation_exception=>raise_from_sy(
            iv_parameter = iv_dynpro_fieldname
          ).
        ENDIF.

      ENDIF.
    ELSE.
      MESSAGE e007(e2) WITH iv_table_name INTO lv_message.
      zcx_sat_validation_exception=>raise_from_sy(
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

*.. Try to find queries first
    SELECT
      FROM zdbbr_queryh
      FIELDS query_name,
             description
      FOR ALL ENTRIES IN @lt_possible_entities
      WHERE query_name = @lt_possible_entities-table_line
    INTO TABLE @DATA(lt_queries_from_clipboard).

    IF sy-subrc = 0.
      rt_entities = VALUE #(
        FOR query IN lt_queries_from_clipboard
        ( entity_id     = query-query_name
          entity_id_raw = query-query_name
          entity_type   = zif_sat_c_entity_type=>query
          description   = query-description )
      ).
    ENDIF.

*.. Now try to find tables/views/cds views
    SELECT *
      FROM zsat_i_databaseentity
      FOR ALL ENTRIES IN @lt_possible_entities
      WHERE entity = @lt_possible_entities-table_line
    INTO TABLE @DATA(lt_entities).

    IF sy-subrc = 0.

      rt_entities = VALUE #( BASE rt_entities
          FOR table_view IN lt_entities
          ( entity_id     = table_view-entity
            entity_id_raw = table_view-entityraw
            entity_type   = table_view-type
            description   = table_view-description )
      ).
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


  METHOD exists_data_for_entity.
    DATA: lt_selfields TYPE string_table,
          lr_result    TYPE REF TO data.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    CHECK: iv_entity_id IS NOT INITIAL.

    zcl_sat_ddic_repo_access=>get_table_field_infos(
      EXPORTING iv_tablename    = iv_entity_id
      IMPORTING et_table_fields = DATA(lt_fields)
    ).

    IF lt_fields IS INITIAL.
      RETURN.
    ENDIF.

    DELETE lt_fields WHERE datatype = 'CLNT' OR fieldname CP '.*'.

    DATA(ls_field) = VALUE #( lt_fields[ 1 ] OPTIONAL ).

    IF ls_field IS INITIAL.
      RETURN.
    ENDIF.

*.. Create result data data type
    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name      = |{ to_upper( iv_entity_id ) }-{ ls_field-fieldname }|
      RECEIVING  p_descr_ref = DATA(lr_typedescr)
      EXCEPTIONS OTHERS      = 1
    ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_datadescr) = CAST cl_abap_datadescr( lr_typedescr ).
    CREATE DATA lr_result TYPE HANDLE lr_datadescr.
    ASSIGN lr_result->* TO <lv_data>.
    lt_selfields = VALUE #( ( |{ ls_field-fieldname }| ) ).

    TRY.
        SELECT
          SINGLE
          FROM (iv_entity_id)
          FIELDS (lt_selfields)
        INTO @<lv_data>.

        IF sy-subrc = 0 AND sy-dbcnt > 0.
          rf_data_exists = abap_true.
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
*        MESSAGE lx_root->get_text( ) TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
