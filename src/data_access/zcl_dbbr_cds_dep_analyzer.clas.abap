"! <p class="shorttext synchronized" lang="en">Dependency Analyzer for CDS View</p>
CLASS zcl_dbbr_cds_dep_analyzer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_parent,
        name TYPE string,
      END OF ty_s_parent.
    TYPES: ty_t_parent TYPE STANDARD TABLE OF ty_s_parent WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_dependency,
        name                TYPE zdbbr_entity_id,
        raw_name            TYPE zdbbr_entity_id_raw,
        object_type         TYPE zdbbr_entity_type,
        adt_type            TYPE string,
        uri                 TYPE string,
        package             TYPE string,
        description         TYPE ddtext,
        parent_nodes        TYPE ty_t_parent,
        source_type         TYPE char1,
        api_state           TYPE zdbbr_cds_api_state,
        occurrence          TYPE i,
        used_entities_count TYPE i,
        used_join_count     TYPE i,
        used_union_count    TYPE i,
      END OF ty_s_dependency.

    TYPES: ty_t_dependency TYPE STANDARD TABLE OF ty_s_dependency WITH KEY name object_type.

    TYPES:
      BEGIN OF ty_s_dependency_info,
        cds_view          TYPE zdbbr_cds_view_name,
        used_entity_count TYPE i,
        dependencies      TYPE ty_t_dependency,
      END OF ty_s_dependency_info.

    "! <p class="shorttext synchronized" lang="en">Anaylyzes dependencies of View and returns tree</p>
    CLASS-METHODS analyze_dependency
      IMPORTING
        iv_cds_view_name           TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rs_dependency_graph) TYPE cl_ddls_dependency_visitor=>ty_s_dependency_graph_node.
    "! <p class="shorttext synchronized" lang="en">Retrieve distinct entities and count</p>
    CLASS-METHODS get_used_entities
      IMPORTING
        iv_cds_view_name          TYPE zdbbr_cds_view_name
        if_for_adt                TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_dependency_info) TYPE ty_s_dependency_info.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_dependency_graph_node,
        parent                   TYPE string,
        name                     TYPE string, " db name, e.g. SQL view name
        type                     TYPE string, " DDIC view, CDS view or table
        relation                 TYPE string,
        entity_name              TYPE string,
        user_defined_entity_name TYPE string, " with camel case
        ddls_name                TYPE string,
        children                 TYPE REF TO data,
      END OF ty_s_dependency_graph_node,
      ty_t_dependency_graph_nodes TYPE STANDARD TABLE OF ty_s_dependency_graph_node WITH DEFAULT KEY.
    "! <p class="shorttext synchronized" lang="en">Retrieve metrics about dependencies</p>
    CLASS-METHODS get_used_entity_count
      IMPORTING
        it_children          TYPE cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes
      CHANGING
        cv_used_entity_count TYPE i OPTIONAL
        cv_used_joins        TYPE i OPTIONAL
        cv_used_unions       TYPE i OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Fill additional information of dependencies</p>
    CLASS-METHODS fill_additional_information
      IMPORTING
        if_for_adt      TYPE abap_bool OPTIONAL
      CHANGING
        ct_dependencies TYPE zcl_dbbr_cds_dep_analyzer=>ty_t_dependency.
ENDCLASS.



CLASS zcl_dbbr_cds_dep_analyzer IMPLEMENTATION.

  METHOD analyze_dependency.
    DATA(lr_dependency_visitor) = NEW cl_ddls_dependency_visitor( ).
    lr_dependency_visitor->compute_dependency_information( to_upper( iv_cds_view_name ) ).
    rs_dependency_graph = lr_dependency_visitor->get_dependency_graph( ).
  ENDMETHOD.

  METHOD get_used_entities.
    DATA: lv_entity_type TYPE zdbbr_entity_type,
          lv_entity_id   TYPE zdbbr_entity_id.

    DATA(lo_dependency_visitor) = NEW cl_ddls_dependency_visitor( ).
    lo_dependency_visitor->compute_dependency_information( to_upper( iv_cds_view_name ) ).

    DATA(lt_dependencies) = VALUE ty_t_dependency_graph_nodes( ( CORRESPONDING #( DEEP lo_dependency_visitor->get_dependency_graph( ) ) ) ).

    LOOP AT lt_dependencies ASSIGNING FIELD-SYMBOL(<ls_dependency>).
      DATA(lv_used_entity_count) = 0.
      DATA(lv_used_union_count) = 0.
      DATA(lv_used_join_count) = 0.

      DATA(lv_tabix) = sy-tabix.
      IF <ls_dependency>-children IS BOUND.
        DATA(lt_children) = CAST cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes( <ls_dependency>-children )->*.
        get_used_entity_count(
          EXPORTING
            it_children          = lt_children
          CHANGING
            cv_used_entity_count = lv_used_entity_count
            cv_used_joins        = lv_used_join_count
            cv_used_unions       = lv_used_union_count
        ).
        LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<ls_child>).
          APPEND INITIAL LINE TO lt_dependencies ASSIGNING FIELD-SYMBOL(<ls_child_enhanced>).
          <ls_child_enhanced> = CORRESPONDING #( DEEP <ls_child> ).
          IF <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-select OR
             <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-result OR
             <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-union OR
             <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-union_all.
            <ls_child_enhanced>-parent = <ls_dependency>-parent.
          ELSE.
            <ls_child_enhanced>-parent = COND #( WHEN <ls_dependency>-entity_name IS NOT INITIAL THEN <ls_dependency>-entity_name ELSE <ls_dependency>-name ).
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_tabix = 1.
*...... This is the starting CDS view
        rs_dependency_info-cds_view = <ls_dependency>-entity_name.
        rs_dependency_info-used_entity_count = lv_used_entity_count.
        INSERT VALUE #( name                = to_upper( iv_cds_view_name )
                        object_type         = zif_dbbr_c_entity_type=>cds_view
                        occurrence          = 1
                        used_entities_count = lv_used_entity_count
                        used_join_count     = lv_used_join_count
                        used_union_count    = lv_used_union_count ) INTO TABLE rs_dependency_info-dependencies.
      ELSE.
*...... Add the entity to the dependency list
        IF <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-cds_view OR
          <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-cds_table_function OR
          <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-cds_db_view.
          lv_entity_type = zif_dbbr_c_entity_type=>cds_view.
        ELSEIF <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-table.
          lv_entity_type = zif_dbbr_c_entity_type=>table.
        ELSEIF <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-view.
          lv_entity_type = zif_dbbr_c_entity_type=>view.
        ELSE.
          CLEAR lv_entity_type.
        ENDIF.

        IF lv_entity_type IS NOT INITIAL.
          lv_entity_id = COND #( WHEN <ls_dependency>-entity_name IS NOT INITIAL THEN <ls_dependency>-entity_name ELSE <ls_dependency>-name ).
          ASSIGN rs_dependency_info-dependencies[ name = lv_entity_id object_type = lv_entity_type ] TO FIELD-SYMBOL(<ls_dep>).
          IF sy-subrc <> 0.
            INSERT VALUE #( name                = lv_entity_id
                            object_type         = lv_entity_type
                            occurrence          = 1
                            used_entities_count = lv_used_entity_count
                            used_join_count     = lv_used_join_count
                            used_union_count    = lv_used_union_count
                            parent_nodes        = VALUE #( ( name = <ls_dependency>-parent ) ) ) INTO TABLE rs_dependency_info-dependencies.
          ELSE.
            <ls_dep>-occurrence = <ls_dep>-occurrence + 1.
            <ls_dep>-parent_nodes = VALUE #( BASE <ls_dep>-parent_nodes ( name = <ls_dependency>-parent ) ).
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    fill_additional_information( EXPORTING if_for_adt      = if_for_adt
                                 CHANGING  ct_dependencies = rs_dependency_info-dependencies ).
  ENDMETHOD.

  METHOD get_used_entity_count.
    LOOP AT it_children ASSIGNING FIELD-SYMBOL(<ls_child>).
*.... Add the entity to the dependency list
      IF <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-cds_view OR
        <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-cds_table_function OR
        <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-cds_db_view.
        ADD 1 TO cv_used_entity_count.
      ELSEIF <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-table.
        ADD 1 TO cv_used_entity_count.
      ELSEIF <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-view.
        ADD 1 TO cv_used_entity_count.
      ENDIF.

      IF <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-inner_join OR
         <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-right_outer_join OR
         <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-left_outer_join.
        ADD 1 TO cv_used_joins.
      ELSEIF <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-union OR
             <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-union_all.
        ADD 1 TO cv_used_unions.
      ENDIF.

      IF <ls_child>-children IS BOUND.
        get_used_entity_count(
          EXPORTING
            it_children          = CAST cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes( <ls_child>-children )->*
          CHANGING
            cv_used_entity_count = cv_used_entity_count
            cv_used_joins        = cv_used_joins
            cv_used_unions       = cv_used_unions
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_additional_information.
    DATA: lt_entity_range  TYPE RANGE OF zdbbr_entity_id,
          lt_tabname_range TYPE RANGE OF tabname.

    lt_entity_range = VALUE #( FOR cds IN ct_dependencies
                               WHERE ( object_type = zif_dbbr_c_entity_type=>cds_view )
                               ( sign = 'I' option = 'EQ' low = cds-name ) ).
    lt_tabname_range = VALUE #( FOR table IN ct_dependencies
                                WHERE ( object_type <> zif_dbbr_c_entity_type=>cds_view )
                                ( sign = 'I' option = 'EQ' low = table-name ) ).

*.. Enrich dependencies by description
    IF lt_entity_range IS NOT INITIAL.
      IF if_for_adt = abap_true.
        SELECT entityid AS entity,
               rawentityid AS entityraw,
               cdsbase~ddlname AS secondary_entity_id,
               source_type,
               api~filtervalue AS api_state,
               description,
               developmentpackage,
               createdby
          FROM zdbbr_p_cdsviewbase AS cdsbase
            INNER JOIN ddddlsrc AS source
              ON cdsbase~ddlname = source~ddlname
            LEFT OUTER JOIN zdbbr_i_apistates AS api
              ON  cdsbase~ddlname = api~objectname
              AND api~filtervalue <> '4'
            LEFT OUTER JOIN zdbbr_i_cdsviewt AS text
              ON cdsbase~ddlname = text~ddlname
              AND text~language = @sy-langu
          WHERE cdsbase~ddlname IN @lt_entity_range
             OR cdsbase~entityid IN @lt_entity_range
        INTO TABLE @DATA(lt_entity_info).
      ELSE.
        SELECT entityid AS entity,
               rawentityid AS entityraw,
               cdsbase~ddlname AS secondary_entity_id,
               description,
               developmentpackage,
               createdby
          FROM zdbbr_p_cdsviewbase AS cdsbase
            LEFT OUTER JOIN zdbbr_i_cdsviewt AS text
              ON cdsbase~ddlname = text~ddlname
              AND text~language = @sy-langu
          WHERE cdsbase~ddlname IN @lt_entity_range
             OR cdsbase~entityid IN @lt_entity_range
        INTO CORRESPONDING FIELDS OF TABLE @lt_entity_info.
      ENDIF.
    ENDIF.

    IF lt_tabname_range IS NOT INITIAL.
      SELECT entity,
             entityraw,
             description,
             developmentpackage,
             createdby
        FROM zdbbr_i_databasetablesandviews( p_language = @sy-langu )
        WHERE entity IN @lt_tabname_range
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_entity_info.
    ENDIF.

    LOOP AT ct_dependencies ASSIGNING FIELD-SYMBOL(<ls_cds_dep>).
      SORT <ls_cds_dep>-parent_nodes BY name.
      DELETE ADJACENT DUPLICATES FROM <ls_cds_dep>-parent_nodes COMPARING name.
      ASSIGN lt_entity_info[ entity = <ls_cds_dep>-name ] TO FIELD-SYMBOL(<ls_entity_descr>).
      IF sy-subrc <> 0.
        ASSIGN lt_entity_info[ secondary_entity_id = <ls_cds_dep>-name ] TO <ls_entity_descr>.
      ENDIF.
      CHECK sy-subrc = 0.
      <ls_cds_dep>-name  = <ls_entity_descr>-entity.
      <ls_cds_dep>-raw_name = <ls_entity_descr>-entityraw.
      <ls_cds_dep>-api_state = <ls_entity_descr>-api_state.
      <ls_cds_dep>-source_type = <ls_entity_descr>-source_type.
      <ls_cds_dep>-description = <ls_entity_descr>-description.
      <ls_cds_dep>-package = <ls_entity_descr>-developmentpackage.
      IF if_for_adt = abap_true.
        DATA(ls_obj_ref) = zcl_dbbr_adt_util=>create_adt_uri(
            iv_type  = <ls_cds_dep>-object_type
            iv_name  = <ls_entity_descr>-entity
            iv_name2 = CONV #( <ls_entity_descr>-secondary_entity_id )
        ).
        <ls_cds_dep>-adt_type = ls_obj_ref-type.
        <ls_cds_dep>-uri = ls_obj_ref-uri.
      ENDIF.
    ENDLOOP.

    SORT ct_dependencies BY used_entities_count DESCENDING occurrence DESCENDING.
  ENDMETHOD.

ENDCLASS.
