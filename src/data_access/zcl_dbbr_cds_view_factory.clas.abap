"! <p class="shorttext synchronized" lang="en">Factory for Accessing CDS View information</p>
CLASS zcl_dbbr_cds_view_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Activate CDS View</p>
    CLASS-METHODS activate_cds_view
      IMPORTING
        !iv_cds_view TYPE ddlname
      RAISING
        zcx_dbbr_application_exc .
    "! <p class="shorttext synchronized" lang="en">Checks Syntax of CDS View</p>
    CLASS-METHODS check_cds_view
      IMPORTING
        !iv_cds_view TYPE ddlname
        !iv_source   TYPE string
      RAISING
        zcx_dbbr_application_exc .
    "! <p class="shorttext synchronized" lang="en">CLASS_CONSTRUCTOR</p>
    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">Checks if the given cds view exists</p>
    "!
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS exists
      IMPORTING
        !iv_cds_view_name TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(result)     TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Retrieve annotation/value for a range of annotation names</p>
    "!
    CLASS-METHODS get_annotations
      IMPORTING
        it_annotation_name TYPE zif_dbbr_global_types=>tt_cds_anno_name_range
      RETURNING
        VALUE(rt_anno)     TYPE zif_dbbr_global_types=>tt_cds_annotation.

    "! <p class="shorttext synchronized" lang="en">Finds header information for cds view(s)</p>
    "!
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_description | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_package | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_max_rows | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS find_cds_views
      IMPORTING
        !iv_cds_view_name TYPE zdbbr_cds_view_name OPTIONAL
        !iv_description   TYPE ddtext OPTIONAL
        !iv_package       TYPE devclass OPTIONAL
        !iv_max_rows      TYPE sy-tabix DEFAULT 50
      RETURNING
        VALUE(result)     TYPE zdbbr_entity_t .
    "! <p class="shorttext synchronized" lang="en">Read API states for Cds View</p>
    "!
    "! @parameter iv_cds_view | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_api_states
      IMPORTING
        !iv_cds_view         TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rt_api_states) TYPE zif_dbbr_global_types=>tt_cds_api_state .
    "! <p class="shorttext synchronized" lang="en">Get author of given CDS View</p>
    "!
    "! @parameter iv_cds_view | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rs_tadir | <p class="shorttext synchronized" lang="en">Repository information for CDS view</p>
    CLASS-METHODS get_tadir_entry
      IMPORTING
        !iv_cds_view    TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rs_tadir) TYPE zif_dbbr_global_types=>ty_cds_tadir.
    "! <p class="shorttext synchronized" lang="en">Retrieve DDL Name for CDS Entity name</p>
    "!
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_ddl_name | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_ddl_for_entity_name
      IMPORTING
        !iv_cds_view_name  TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rv_ddl_name) TYPE ddlname
      RAISING
        zcx_dbbr_data_read_error .
    "! <p class="shorttext synchronized" lang="en">Get CDS view for package</p>
    "!
    "! @parameter iv_package | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_ddl_for_package
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE zdbbr_cds_view_header_t .
    "! <p class="shorttext synchronized" lang="en">Get description of cds view</p>
    "!
    "! @parameter iv_cds_view | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_description | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_description
      IMPORTING
        !iv_cds_view          TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rv_description) TYPE ddtext .
    "! <p class="shorttext synchronized" lang="en">Reads CDS View reference</p>
    "!
    "! @parameter iv_cds_view | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_dbbr_data_read_error | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS read_cds_view
      IMPORTING
        !iv_cds_view  TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(result) TYPE REF TO zcl_dbbr_cds_view
      RAISING
        zcx_dbbr_data_read_error .
    "! <p class="shorttext synchronized" lang="en">Reads head information of cds view</p>
    CLASS-METHODS read_cds_view_header
      IMPORTING
        !iv_cds_view  TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(result) TYPE zdbbr_cds_view_header .
    "! <p class="shorttext synchronized" lang="en">Reads multiple headers for cds views</p>
    CLASS-METHODS read_cds_view_header_multi
      IMPORTING
        !it_cds_view_name TYPE zdbbr_cds_view_name_t
      RETURNING
        VALUE(result)     TYPE dd02bvtab .
    "! <p class="shorttext synchronized" lang="en">Reads the source of the CDS View</p>
    CLASS-METHODS read_ddls_source
      IMPORTING
        !iv_cds_view_name TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rv_source)  TYPE string
      RAISING
        zcx_dbbr_application_exc .
    "! <p class="shorttext synchronized" lang="en">Read DDIC View for CDS View</p>
    CLASS-METHODS read_ddl_ddic_view
      IMPORTING
        !iv_ddl_name        TYPE ddlname
      RETURNING
        VALUE(rv_ddic_view) TYPE viewname .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF lty_cds_view_cache,
        ddl_view_name TYPE viewname,
        cds_view_name TYPE zdbbr_cds_view_name,
        ref           TYPE REF TO zcl_dbbr_cds_view,
        language      TYPE langu,
      END OF lty_cds_view_cache .

    CLASS-DATA:
      st_cds_view_cache TYPE SORTED TABLE OF lty_cds_view_cache WITH UNIQUE KEY cds_view_name language .
    "! <p class="shorttext synchronized" lang="en">Description of Domain Fixed Values</p>
    CLASS-DATA st_cds_source_vals TYPE ddfixvalues .

    "! <p class="shorttext synchronized" lang="en">Fills associations read from ddic</p>
    CLASS-METHODS fill_associations
      IMPORTING
        !it_header    TYPE dd08bvtab
        !it_fields    TYPE dd05bvtab
      RETURNING
        VALUE(result) TYPE zdbbr_cds_association_t .
    "! <p class="shorttext synchronized" lang="en">Retrieves the source type of a cds view</p>
    "!
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_source_type | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_source_type
      IMPORTING
        !iv_cds_view_name     TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rv_source_type) TYPE zdbbr_cds_source_type .
    "! <p class="shorttext synchronized" lang="en">Handler for REQUEST_ANNOTATIONS</p>
    "!
    CLASS-METHODS on_annotation_read_request
          FOR EVENT request_annotations OF zcl_dbbr_cds_view
      IMPORTING
          et_anno_name_range
          sender.
    "! <p class="shorttext synchronized" lang="en">Event Handler for lazy loading of CDS View API states</p>
    "!
    "! @parameter sender | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS on_api_states_loading_request
          FOR EVENT request_api_states OF zcl_dbbr_cds_view
      IMPORTING
          !sender .
    "! <p class="shorttext synchronized" lang="en">Event Handler for lazy loading of CDS View author</p>
    "!
    "! @parameter sender | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS on_tadir_info_loading_request
          FOR EVENT request_tadir_info OF zcl_dbbr_cds_view
      IMPORTING
          !sender .
    "! <p class="shorttext synchronized" lang="en">Event handler for lazy loading of CDS Views base tables</p>
    CLASS-METHODS on_base_table_loading_request
          FOR EVENT request_base_tables OF zcl_dbbr_cds_view
      IMPORTING
          !sender .
    "! <p class="shorttext synchronized" lang="en">Event Handler for lazy loading of CDS View description</p>
    "!
    "! @parameter sender | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS on_description_loading_request
          FOR EVENT request_description OF zcl_dbbr_cds_view
      IMPORTING
          !sender .
    "! <p class="shorttext synchronized" lang="en">Reads base tables of DDL View</p>
    CLASS-METHODS read_cds_base_tables
      IMPORTING
        !iv_view_name         TYPE viewname
      RETURNING
        VALUE(rt_base_tables) TYPE zdbbr_cds_view_base_table_t .
    "! <p class="shorttext synchronized" lang="en">Reads annotations for parameters</p>
    CLASS-METHODS read_param_annotations
      IMPORTING
        !iv_cds_view  TYPE zdbbr_cds_view_name
      CHANGING
        !ct_parameter TYPE zdbbr_cds_parameter_t .
ENDCLASS.



CLASS zcl_dbbr_cds_view_factory IMPLEMENTATION.


  METHOD activate_cds_view.
    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        lr_ddl->activate(
            name = iv_cds_view
        ).
      CATCH cx_dd_ddl_activate  INTO DATA(lx_dd_ddl_activate).
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc
          EXPORTING
            previous = lx_dd_ddl_activate.
    ENDTRY.

  ENDMETHOD.


  METHOD check_cds_view.
    DATA: lt_warnings   TYPE ddl2ddicwarnings,
          ls_ddl_source TYPE ddddlsrcv.

    ls_ddl_source-source = iv_source.
    ls_ddl_source-ddlanguage = sy-langu.
*.. TODO: Fill other components

    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        lr_ddl->check(
          EXPORTING
            name            = iv_cds_view    " Name of a DDL Source
*            prid            =     " ID for Log Writer
          IMPORTING
            warnings        = lt_warnings
          CHANGING
            ddlsrcv_wa      = ls_ddl_source
        ).
      CATCH cx_dd_ddl_check INTO DATA(lx_dd_ddl_check).
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc
          EXPORTING
            previous = lx_dd_ddl_check.
    ENDTRY.
  ENDMETHOD.


  METHOD class_constructor.
    DATA(lr_cds_sourc_typedescr) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_data( VALUE zdbbr_cds_source_type( ) )
    ).

    st_cds_source_vals = lr_cds_sourc_typedescr->get_ddic_fixed_values(
        p_langu = 'E'
    ).
  ENDMETHOD.


  METHOD exists.
    SELECT SINGLE *
    FROM dd02b
    INTO @DATA(ls_view)
    WHERE strucobjn = @iv_cds_view_name.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD fill_associations.
    DATA: lv_and_or_value         TYPE vsconj VALUE zif_dbbr_c_selection_condition=>and,
          lt_db_tables            TYPE STANDARD TABLE OF zdbbr_i_databasetable,
          lv_entity_id_raw        TYPE zdbbr_entity_id_raw,
          lv_description          TYPE ddtext,
          lt_db_table_range       TYPE RANGE OF tabname,
          lt_assoc_cds_view_range TYPE RANGE OF zdbbr_cds_view_name,
          lf_no_and_or_field      TYPE abap_bool.

    lt_assoc_cds_view_range = VALUE #(
      FOR <ls_assoc_cds> IN it_header
      WHERE ( typekind_t <> zif_dbbr_c_cds_assoc_type=>table )
      ( sign = 'I' option = 'EQ' low = <ls_assoc_cds>-strucobjn_t )
    ).
    SORT lt_assoc_cds_view_range.
    DELETE ADJACENT DUPLICATES FROM lt_assoc_cds_view_range.

    DATA(lv_descr_lang) = zcl_dbbr_appl_util=>get_description_language( ).

*.. Read database table+text if there are any association which point to database tables
    IF line_exists( it_header[ typekind_t = zif_dbbr_c_cds_assoc_type=>table ] ).
      lt_db_table_range = VALUE #(
          FOR dbtab IN it_header WHERE ( typekind_t = zif_dbbr_c_cds_assoc_type=>table ) ( sign = 'I' option = 'EQ' low = dbtab-strucobjn_t )
      ).
      SELECT *
          FROM zdbbr_i_databasetable( p_language = @lv_descr_lang )
          WHERE tablename IN @lt_db_table_range
      INTO CORRESPONDING FIELDS OF TABLE @lt_db_tables.
    ENDIF.

*... read cds view texts for associations
    SELECT entityid, rawentityid, description
      FROM zdbbr_i_cdsentity( p_language = @lv_descr_lang )
      WHERE entityid IN @lt_assoc_cds_view_range
    INTO TABLE @DATA(lt_assoc_cds_view_header).

    LOOP AT it_header ASSIGNING FIELD-SYMBOL(<ls_header>).
      IF <ls_header>-typekind_t = zif_dbbr_c_cds_assoc_type=>table.
        lv_entity_id_raw = <ls_header>-strucobjn_t.
        DATA(ls_dbtab) = VALUE #( lt_db_tables[ tablename = <ls_header>-strucobjn_t ] OPTIONAL ).
        lv_description = ls_dbtab-description.
      ELSE.
        DATA(ls_assoc_cds) = VALUE #( lt_assoc_cds_view_header[ entityid = <ls_header>-strucobjn_t ] OPTIONAL ).
        lv_entity_id_raw = ls_assoc_cds-rawentityid.
        lv_description = ls_assoc_cds-description.
      ENDIF.

      DATA(ls_assoc) = VALUE zdbbr_cds_association(
          name             = <ls_header>-associationname
          raw_name         = <ls_header>-assocname_raw
          ref_cds_view     = <ls_header>-strucobjn_t
          ref_cds_view_raw = lv_entity_id_raw
          kind             = <ls_header>-typekind_t
          entity_type      = SWITCH #(
            <ls_header>-typekind_t
            WHEN zif_dbbr_c_cds_assoc_type=>entity OR
                 zif_dbbr_c_cds_assoc_type=>table_function THEN zif_dbbr_c_entity_type=>cds_view
            WHEN zif_dbbr_c_cds_assoc_type=>table OR
                 zif_dbbr_c_cds_assoc_type=>view THEN zif_dbbr_c_entity_type=>table
          )
          cardinality      = <ls_header>-cardinality
          cardinality_text = |[{ <ls_header>-card_min }..| &&
                             COND #( WHEN <ls_header>-card_max > 10 THEN |*| ELSE |{ <ls_header>-card_max }| ) &&
                             |]|
          card_min         = <ls_header>-card_min
          card_max         = <ls_header>-card_max
          ddtext           = lv_description
      ).

      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE associationname = <ls_header>-associationname.

        ASSIGN COMPONENT 'AND_OR' OF STRUCTURE <ls_field> TO FIELD-SYMBOL(<lv_and_or>).
        IF sy-subrc = 0.
          lv_and_or_value = <lv_and_or>.
        ELSE.
          lf_no_and_or_field = abap_true.
        ENDIF.
        ls_assoc-fields = VALUE #(
          BASE ls_assoc-fields
          (
            name     = <ls_field>-fieldname_t
            ref_name = <ls_field>-fieldname
            position = <ls_field>-fdposition
            operator = <ls_field>-operator
            and_or   = lv_and_or_value
          )
        ).
      ENDLOOP.

      IF lf_no_and_or_field = abap_true.
        CLEAR: ls_assoc-fields[ lines( ls_assoc-fields ) ]-and_or.
      ENDIF.

      result = VALUE #( BASE result ( ls_assoc ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD find_cds_views.
    DATA: lt_cds_view_range    TYPE RANGE OF ddstrucobjname,
          lt_description_range TYPE RANGE OF ddtext,
          lt_package_range     TYPE RANGE OF devclass.

    IF iv_cds_view_name IS NOT INITIAL.
      lt_cds_view_range = VALUE #( ( sign = 'I' option = COND #( WHEN iv_cds_view_name CS '*' THEN 'CP' ELSE 'EQ' ) low = iv_cds_view_name ) ).
    ELSEIF iv_package IS NOT INITIAL.
      lt_package_range = VALUE #( ( sign = 'I' option = 'EQ' low = iv_package ) ).
    ENDIF.

    IF iv_description IS NOT INITIAL.
      lt_description_range = VALUE #( ( sign = 'I' option = COND #( WHEN iv_description CS '*' THEN 'CP' ELSE 'EQ' ) low = iv_description ) ).
    ENDIF.

    DATA(lv_descr_language) = zcl_dbbr_appl_util=>get_description_language( ).

    SELECT entityid AS entity_id,
           rawentityid AS entity_id_raw,
           'C' AS entity_type,
           description,
           developmentpackage AS devclass
      FROM zdbbr_i_cdsentity( p_language = @lv_descr_language )
      WHERE entityid           IN @lt_cds_view_range
        AND developmentpackage IN @lt_package_range
        AND description        IN @lt_description_range
      ORDER BY entity_id
    INTO CORRESPONDING FIELDS OF TABLE @result
      UP TO @iv_max_rows ROWS.

  ENDMETHOD.


  METHOD get_api_states.
    SELECT filtervalue
     FROM zdbbr_i_apistates
     WHERE objectname = @iv_cds_view
       AND objecttype = 'DDLS'
     INTO TABLE @rt_api_states.

    IF sy-subrc <> 0.
      rt_api_states = VALUE #( ( zif_dbbr_c_cds_api_state=>not_released ) ).
    ELSE.

    ENDIF.
  ENDMETHOD.


  METHOD get_tadir_entry.
    SELECT SINGLE author AS created_by, created_on AS created_date
      FROM tadir
      WHERE object   = 'STOB'
        AND pgmid    = 'R3TR'
        AND obj_name = @iv_cds_view
    INTO @rs_tadir.
  ENDMETHOD.

  METHOD get_annotations.
    SELECT *
      FROM zdbbr_i_cdsannotation
      WHERE name IN @it_annotation_name
    INTO CORRESPONDING FIELDS OF TABLE @rt_anno.
  ENDMETHOD.


  METHOD get_ddl_for_entity_name.
    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        rv_ddl_name = lr_ddl->get_ddl_name_4_dd_artefact(
            ddname = iv_cds_view_name
        ).
      CATCH cx_dd_ddl_exception INTO DATA(lx_ddl_read).
        RAISE EXCEPTION TYPE zcx_dbbr_data_read_error
          EXPORTING
            previous = lx_ddl_read.
    ENDTRY.
  ENDMETHOD.


  METHOD get_ddl_for_package.
    SELECT obj_name AS ddl
      FROM tadir
        WHERE object = 'STOB'
          AND devclass = @iv_package
    INTO TABLE @DATA(lt_cds_views).

    CHECK sy-subrc = 0.

    result = CORRESPONDING #(
        read_cds_view_header_multi(
            it_cds_view_name = VALUE #(
                FOR cds IN lt_cds_views
                ( CONV #( cds-ddl ) )
            )
        ) MAPPING entityname  = strucobjn
                  description = ddtext
    ).
  ENDMETHOD.


  METHOD get_description.
    DATA(lv_description_language) = zcl_dbbr_appl_util=>get_description_language( ).

    SELECT SINGLE description
      FROM zdbbr_i_cdsentity( p_language = @lv_description_language )
      WHERE entityid = @iv_cds_view
    INTO @rv_description.

  ENDMETHOD.


  METHOD get_source_type.

    IF sy-saprl >= 751.
      DATA(lv_select) = 'SOURCE_TYPE'.
      SELECT SINGLE (lv_select)
        FROM ddddlsrc
        WHERE ddlname = @iv_cds_view_name
      INTO @rv_source_type.

      IF sy-subrc <> 0.
        rv_source_type = zif_dbbr_c_cds_view_type=>view.
      ENDIF.
    ELSE.
*... assume ddls is a normal view
      rv_source_type = zif_dbbr_c_cds_view_type=>view.
    ENDIF.
  ENDMETHOD.


  METHOD on_annotation_read_request.
    SELECT *
      FROM zdbbr_i_cdsannotation
      WHERE name IN @et_anno_name_range
    APPENDING CORRESPONDING FIELDS OF TABLE @sender->mt_annotations.

    IF sy-subrc = 0.
      SORT sender->mt_annotations BY fieldname name.
      DELETE ADJACENT DUPLICATES FROM sender->mt_annotations.
    ENDIF.
  ENDMETHOD.


  METHOD on_api_states_loading_request.
    sender->mt_api_states = get_api_states( sender->mv_view_name ).

    SET HANDLER:
        on_api_states_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.


  METHOD on_tadir_info_loading_request.
    sender->ms_tadir_info = get_tadir_entry( sender->mv_view_name ).

    SET HANDLER:
        on_tadir_info_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.


  METHOD on_base_table_loading_request.
    sender->mt_base_tables = read_cds_base_tables( iv_view_name = sender->ms_header-ddlview  ).

*... remove handler for this cds view
    SET HANDLER:
      on_base_table_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.


  METHOD on_description_loading_request.
    sender->mv_description = get_description( sender->mv_view_name ).

    SET HANDLER:
        on_description_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.


  METHOD read_cds_base_tables.
    DATA: lt_ddnames      TYPE if_dd_ddl_types=>ty_t_ddobj,
          lt_db_obj_range TYPE RANGE OF tabname.

    SELECT *
      FROM zdbbrdd26s_v
      WHERE ddlview = @iv_view_name
      ORDER BY tabpos
      INTO TABLE @DATA(lt_base_tables).

    CHECK sy-subrc = 0.


*... collect all base tables of type VIEW and try to retrieve then ddl entities for them
    lt_ddnames = VALUE #(
      FOR view IN lt_base_tables
      WHERE ( ddictype = 'VIEW' )
      ( name = view-basetable )
    ).

    DATA(lr_dd_handler) = cl_dd_ddl_handler_factory=>create( ).

    lr_dd_handler->get_entityname_from_viewname(
      EXPORTING
        ddnames        = lt_ddnames
      IMPORTING
        entity_of_view = DATA(lt_entity_of_view)
    ).

*... read headers for ddl found entities
    DATA(lt_header) = read_cds_view_header_multi( it_cds_view_name = VALUE #( FOR ddl IN lt_entity_of_view ( ddl-entityname ) ) ).

    LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
*... exclude some tables which some get mixed up inside dd26s ( they are not really used in the select clause )
      CHECK: <ls_base_table>-basetable <> 'DDDDLCHARTYPES',
             <ls_base_table>-basetable <> 'DDDDLCURRTYPES',
             <ls_base_table>-basetable <> 'DDDDLDECTYPES',
             <ls_base_table>-basetable <> 'DDDDLNUMTYPES',
             <ls_base_table>-basetable <> 'DDDDLQUANTYPES'.

      DATA(ls_base_table) = VALUE zdbbr_cds_view_base_table(
          table_order    = <ls_base_table>-tabpos
      ).
      IF <ls_base_table>-ddictype = 'VIEW'.
        DATA(lr_s_entity) = REF #( lt_entity_of_view[ viewname = <ls_base_table>-basetable ] OPTIONAL ).
        IF lr_s_entity IS BOUND.
          ls_base_table-entityname = lr_s_entity->entityname.
          ls_base_table-table_kind = zif_dbbr_c_entity_type=>cds_view.
*........ retrieve header to get raw name and description
          DATA(lr_s_header) = REF #( lt_header[ strucobjn = ls_base_table-entityname ] ).
          ls_base_table-entityname_raw = lr_s_header->strucobjn_raw.
          ls_base_table-description = lr_s_header->ddtext.
        ELSE.
*........ view is a normal database view
          ls_base_table-table_kind = zif_dbbr_c_entity_type=>table.
          ls_base_table-is_db_view = abap_true.
          ls_base_table-entityname =
          ls_base_table-entityname_raw = <ls_base_table>-basetable.
          lt_db_obj_range = VALUE #( BASE lt_db_obj_range ( sign = 'I' option = 'EQ' low = <ls_base_table>-basetable ) ).
        ENDIF.
      ELSE.
        ls_base_table-table_kind = zif_dbbr_c_entity_type=>table.
        ls_base_table-entityname =
        ls_base_table-entityname_raw = <ls_base_table>-basetable.
        lt_db_obj_range = VALUE #( BASE lt_db_obj_range ( sign = 'I' option = 'EQ' low = <ls_base_table>-basetable ) ).
      ENDIF.

      rt_base_tables = VALUE #( BASE rt_base_tables ( ls_base_table ) ).
    ENDLOOP.

*... get descriptions for db tables and views
    IF lt_db_obj_range IS NOT INITIAL.
      DATA(lv_description_language) = zcl_dbbr_appl_util=>get_description_language( ).
      SELECT tabname, ddtext
        FROM dd02t
        WHERE tabname   IN @lt_db_obj_range
          AND ddlanguage = @lv_description_language
      INTO TABLE @DATA(lt_db_obj_descr).

      IF sy-subrc = 0.
        LOOP AT rt_base_tables ASSIGNING FIELD-SYMBOL(<ls_db_obj>) WHERE table_kind = zif_dbbr_c_entity_type=>table.
          <ls_db_obj>-description = VALUE #( lt_db_obj_descr[ tabname = <ls_db_obj>-entityname ]-ddtext OPTIONAL ).
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD read_cds_view.
    DATA(lv_description_language) = zcl_dbbr_appl_util=>get_description_language( ).

*... try to read view from cache
    result = VALUE #( st_cds_view_cache[ cds_view_name = iv_cds_view
                                         language      = lv_description_language ]-ref OPTIONAL ).
    CHECK result IS NOT BOUND.

    DATA(lr_dd_sobject) = cl_dd_sobject_factory=>create( ).


    TRY.

        lr_dd_sobject->read(
          EXPORTING
            withtext     = abap_true
            langu        = lv_description_language
            sobjnames    = VALUE #( ( iv_cds_view ) )
          IMPORTING
            dd02bv_tab   = DATA(lt_headers)
            dd02bndv_tab = DATA(lt_nodes)
            dd03ndv_tab  = DATA(lt_col)
            dd08bv_tab   = DATA(lt_assoc_header)
            dd05bv_tab   = DATA(lt_assoc_fields)
            dd10bv_tab   = DATA(lt_params_db)
        ).

        " entity was not found -> maybe it has been deleted
        IF lt_headers IS INITIAL.
          RAISE EXCEPTION TYPE zcx_dbbr_data_read_error
            EXPORTING
              textid = zcx_dbbr_data_read_error=>cds_view_not_existing
              msgv1  = |{ iv_cds_view }|.
        ENDIF.


        DATA(ls_dd02bv) = lt_headers[ 1 ].

        DATA(ls_header) = CORRESPONDING zdbbr_cds_view_header(
          ls_dd02bv
          MAPPING entityname      = strucobjn
                  entityname_raw  = strucobjn_raw
                  description     = ddtext
        ).

        ls_header-ddlname = get_ddl_for_entity_name( ls_header-entityname ).

*... supply generated ddl database view of ddls
*... and read base tables of cds view ( if view name was supplied )
        ls_header-ddlview  = VALUE #( lt_nodes[ 1 ]-dbtabname OPTIONAL ).

*... if this cds is an extend view the source type can be determined right here
        IF ls_dd02bv-strucobjclass = 'APPEND'.
          ls_header-source_type = zif_dbbr_c_cds_view_type=>extend.
        ELSE.
*... determine the correct source type of the cds
          ls_header-source_type = get_source_type( iv_cds_view_name = iv_cds_view ).
        ENDIF.

        IF ls_header-source_type IS NOT INITIAL.
          ls_header-source_type_name = st_cds_source_vals[ low = ls_header-source_type ]-ddtext.
        ENDIF.

        DATA(lt_params) = CORRESPONDING zdbbr_cds_parameter_t(
          lt_params_db
          MAPPING inttype = abaptype
        ).

*... complete parameters with annotations (if existing)
        IF sy-saprl >= 750.
          read_param_annotations(
            EXPORTING iv_cds_view  = iv_cds_view
            CHANGING ct_parameter = lt_params
          ).
        ENDIF.

        IF lt_assoc_header IS NOT INITIAL.

          DATA(lt_association) = fill_associations(
            EXPORTING
              it_header = lt_assoc_header
              it_fields = lt_assoc_fields
          ).
        ENDIF.

        result = NEW zcl_dbbr_cds_view(
          is_header             = ls_header
          it_association        = lt_association
          it_columns            = lt_col
          it_parameters         = lt_params
        ).

*... set handler for lazy loading of base tables
        SET HANDLER:
          on_base_table_loading_request FOR result,
          on_api_states_loading_request FOR result,
          on_tadir_info_loading_request FOR result,
          on_description_loading_request FOR result.

*... fill cache
        INSERT VALUE #(
          ddl_view_name = ls_header-ddlview
          cds_view_name = ls_header-entityname
          language      = lv_description_language
          ref           = result
        ) INTO TABLE st_cds_view_cache.
      CATCH cx_dd_sobject_get INTO DATA(lx_sobject_get). " Read error
        RAISE EXCEPTION TYPE zcx_dbbr_data_read_error
          EXPORTING
            previous = lx_sobject_get.
    ENDTRY.

  ENDMETHOD.


  METHOD read_cds_view_header.
    DATA(lr_dd_util) = cl_dd_sobject_factory=>create_util( ).

    TRY.
        lr_dd_util->get_header(
          EXPORTING
            entitynames       = VALUE #( ( iv_cds_view ) )
            withtext          = abap_true    " ABAP_true: Read texts also
            langu             = zcl_dbbr_appl_util=>get_description_language( )
          IMPORTING
            header            = DATA(lt_header)
        ).
        result = CORRESPONDING #( VALUE #( lt_header[ 1 ] OPTIONAL ) ).
      CATCH cx_dd_sobject_get.
    ENDTRY.
  ENDMETHOD.


  METHOD read_cds_view_header_multi.
    CHECK it_cds_view_name IS NOT INITIAL.

    DATA(lr_dd_sobject) = cl_dd_sobject_factory=>create( ).

    TRY.
        lr_dd_sobject->read(
          EXPORTING
            langu             = zcl_dbbr_appl_util=>get_description_language( )
            withtext          = abap_true
            sobjnames         = it_cds_view_name
          IMPORTING
            dd02bv_tab        = result
        ).
      CATCH cx_dd_sobject_get.
    ENDTRY.
  ENDMETHOD.


  METHOD read_ddls_source.

    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        DATA(lv_ddl_name) = lr_ddl->get_ddl_name_4_dd_artefact(
            ddname = iv_cds_view_name
        ).

        lr_ddl->read(
          EXPORTING
            name           = lv_ddl_name
            get_state      = 'A'    " Version of DDL source to be read
            langu          = zcl_dbbr_appl_util=>get_description_language( )
          IMPORTING
            ddddlsrcv_wa   = DATA(ls_ddddl_source)
        ).
        rv_source = ls_ddddl_source-source.
      CATCH cx_dd_ddl_exception INTO DATA(lx_ddl_read).
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc
          EXPORTING
            previous = lx_ddl_read.
    ENDTRY.
  ENDMETHOD.


  METHOD read_ddl_ddic_view.
    DATA(lr_dd_sobject) = cl_dd_sobject_factory=>create( ).
    DATA(lr) = cl_dd_ddl_handler_factory=>create( ).
    TRY.
        lr->get_ddl_content_object_names(
          EXPORTING
            ddlname        = to_upper( iv_ddl_name )
            get_state      = 'A'    " Version of ABAP Dictionary Object
          IMPORTING
            viewname       = rv_ddic_view    " Name of SQL View Defined in DDL Source
        ).
      CATCH cx_dd_ddl_read.
    ENDTRY.
  ENDMETHOD.


  METHOD read_param_annotations.
    TYPES: BEGIN OF ty_param_anno,
             parametername TYPE ddparname,
             name          TYPE c LENGTH 240,
             position      TYPE i,
             value         TYPE zdbbr_annotation_value,
           END OF ty_param_anno.

    DATA: lt_paramanno TYPE STANDARD TABLE OF ty_param_anno WITH EMPTY KEY.
    DATA(lt_select) = VALUE zdbbr_string_t(
      ( |PARAMETERNAME, | )
      ( |NAME, | )
      ( |PARPOS as POSITION, | )
      ( |VALUE | )
    ).
    DATA(lv_from) = 'DDPARAMETERANNO'.
    DATA(lv_order_by) = |POSITION|.
    DATA(lt_where) = VALUE zdbbr_string_t(
      ( |STRUCOBJN = @iv_cds_view| )
    ).

    TRY.
        SELECT (lt_select)
          FROM (lv_from)
          WHERE (lt_where)
          ORDER BY (lv_order_by)
        INTO CORRESPONDING FIELDS OF TABLE @lt_paramanno.
      CATCH cx_sy_open_sql_db INTO DATA(lr_osql_db_exc).
      CATCH cx_sy_dynamic_osql_semantics INTO DATA(lr_osql_semantics_exc).
      CATCH cx_sy_dynamic_osql_syntax INTO DATA(lr_osql_syntax_exc).
    ENDTRY.

*... enrich parameters with annotation values
    LOOP AT lt_paramanno ASSIGNING FIELD-SYMBOL(<ls_paramanno>)
      GROUP BY ( parameter = <ls_paramanno>-parametername )
      ASSIGNING FIELD-SYMBOL(<ls_anno_group>).

      DATA(lr_s_param) = REF #( ct_parameter[ parametername = <ls_anno_group>-parameter ] OPTIONAL ).
      CHECK lr_s_param IS BOUND.

      LOOP AT GROUP <ls_anno_group> ASSIGNING FIELD-SYMBOL(<ls_anno>).
        lr_s_param->annotations = VALUE #(
          BASE lr_s_param->annotations
          ( CORRESPONDING #( <ls_anno> ) )
        ).

*...... do some annotation checks
        IF <ls_anno>-name = to_upper( zif_dbbr_c_cds_anno_definition=>consumption_defaultvalue ).
          DATA(lv_val) = <ls_anno>-value.
          DATA(lv_length) = strlen( <ls_anno>-value ).
**....... remove leading single quote
          IF lv_val(1) = ''''.
            lv_length = lv_length - 1.
            lv_val = lv_val+1(lv_length).
          ENDIF.

**....... remove trailing single quote
          lv_length = lv_length - 1.
          IF lv_val+lv_length(1) = ''''.
            lv_val = lv_val(lv_length).
          ENDIF.
          lr_s_param->default_value = lv_val.
        ELSEIF <ls_anno>-name = to_upper( zif_dbbr_c_cds_anno_definition=>environment_systemfield ).
*........ Handle system environment annotations
          IF <ls_anno>-value = zif_dbbr_c_cds_anno_value=>c_environment_system_field-client.
            lr_s_param->has_system_anno = abap_true.
          ELSE.
            CASE <ls_anno>-value.
              WHEN zif_dbbr_c_cds_anno_value=>c_environment_system_field-date.
                lr_s_param->default_value = sy-datum.
              WHEN zif_dbbr_c_cds_anno_value=>c_environment_system_field-language.
                lr_s_param->default_value = sy-langu.
              WHEN zif_dbbr_c_cds_anno_value=>c_environment_system_field-time.
                lr_s_param->default_value = sy-timlo.
              WHEN zif_dbbr_c_cds_anno_value=>c_environment_system_field-user.
                lr_s_param->default_value = sy-uname.
            ENDCASE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
