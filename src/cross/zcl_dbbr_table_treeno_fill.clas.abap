"! <p class="shorttext synchronized" lang="en">Fills tree nodes/items out of table field list</p>
CLASS zcl_dbbr_table_treeno_fill DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_tree_node_filler .

    METHODS constructor
      IMPORTING
        !it_join_tables     TYPE zdbbr_entity_t
        !is_join_field_info TYPE dfies OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_table_node_prefix TYPE string VALUE 'TABLE'.
    CONSTANTS c_tablefield_node_prefix TYPE string VALUE 'FIELD'.
    CONSTANTS c_matched_node TYPE tm_nodekey VALUE '<MATCHED>'.
    DATA mv_field_node_counter TYPE i.
    DATA mv_first_table_node TYPE tm_nodekey.
    DATA mv_prio TYPE sy-tabix.

    ALIASES tt_node_map
      FOR zif_dbbr_tree_node_filler~tt_node_map .

    TYPES:
      BEGIN OF ty_s_matched_type_fields,
        prio          TYPE sy-tabix,
        fieldname     TYPE fieldname,
        tabname       TYPE zdbbr_entity_id,
        tabname_alias TYPE zdbbr_entity_alias,
        description   TYPE ddtext,
        domain_match  TYPE abap_bool,
        type_match    TYPE abap_bool,
      END OF ty_s_matched_type_fields .

    DATA mt_join_tables TYPE zdbbr_entity_t.
    DATA ms_join_field_info TYPE dfies .
    DATA:
      mt_matched_fields TYPE STANDARD TABLE OF ty_s_matched_type_fields WITH EMPTY KEY .

    "! <p class="shorttext synchronized" lang="en">Create field nodes for given table</p>
    METHODS create_table_field_nodes
      IMPORTING
        !ir_nodes      TYPE REF TO zcl_uitb_ctm_nodes
        !iv_table_node TYPE tm_nodekey
        is_entity      TYPE zdbbr_entity
      CHANGING
        !ct_node_map   TYPE tt_node_map
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Create node for fieldnames with domain match / type match</p>
    METHODS create_matched_node_and_fields
      IMPORTING
        !ir_nodes    TYPE REF TO zcl_uitb_ctm_nodes
      CHANGING
        !ct_node_map TYPE tt_node_map
      RAISING
        zcx_uitb_tree_error .
ENDCLASS.



CLASS zcl_dbbr_table_treeno_fill IMPLEMENTATION.


  METHOD constructor.
    mt_join_tables = it_join_tables.
    ms_join_field_info = is_join_field_info.
  ENDMETHOD.


  METHOD create_matched_node_and_fields.
*.. Now add all the matched nodes to a special node on top
    CHECK mt_matched_fields IS NOT INITIAL.

*.. Collapse the table field nodes
    ir_nodes->add_node(
        iv_node_key          = c_matched_node
        iv_relative_node_key = mv_first_table_node
        iv_relationship      = cl_tree_model=>relat_first_sibling
        if_folder            = abap_true
        iv_style             = zif_uitb_c_ctm_style=>light_orange
        it_item_table        = VALUE #(
          ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
            font      = cl_item_tree_model=>item_font_prop
            class     = cl_item_tree_model=>item_class_text
            text      = |Suitable Table fields| )
        )
    ).
    ct_node_map = VALUE #( BASE ct_node_map
      ( node_key = c_matched_node
        node_type = zif_dbbr_tree_node_filler=>c_node_type-matched_fields )
    ).

*.. Add matched fields - 1) domain matched, 2) type matches
    SORT mt_matched_fields BY domain_match DESCENDING prio ASCENDING.
    LOOP AT mt_matched_fields ASSIGNING FIELD-SYMBOL(<ls_matched_fields>).
      DATA(lv_icon) = COND tv_image(
        WHEN <ls_matched_fields>-domain_match = abap_true THEN
          zcl_dbbr_icon_handler=>create_icon_with_tip( iv_icon = icon_led_green
                                                       iv_quicktip = 'Domain matches' )
        ELSE
          zcl_dbbr_icon_handler=>create_icon_with_tip( iv_icon = icon_led_yellow
                                                       iv_quicktip = 'Datatype and Length match' )
      ).
      DATA(lv_matched_field_node) = |{ c_matched_node }{ sy-tabix }|.
      ct_node_map = VALUE #( BASE ct_node_map
        ( node_key  = lv_matched_field_node
          node_type = zif_dbbr_tree_node_filler=>c_node_type-matched_field
          fieldname = <ls_matched_fields>-fieldname
          tabname   = <ls_matched_fields>-tabname
          alias     = <ls_matched_fields>-tabname_alias )
      ).
      ir_nodes->add_node(
          iv_node_key          = lv_matched_field_node
          iv_relative_node_key = c_matched_node
          iv_image             = lv_icon
          iv_style             = COND #( WHEN <ls_matched_fields>-domain_match = abap_true AND
                                              <ls_matched_fields>-fieldname = ms_join_field_info-fieldname THEN zif_uitb_c_ctm_style=>light_green )
          it_item_table        = VALUE #(
            ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = |{ <ls_matched_fields>-tabname_alias }-{ <ls_matched_fields>-fieldname }| )
            ( item_name = zcl_dbbr_tabfield_tree_f4=>c_hier_col2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = <ls_matched_fields>-description )
          )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD create_table_field_nodes.
    zcl_dbbr_dictionary_helper=>get_table_field_infos(
      EXPORTING iv_tablename    = is_entity-entity_id
      IMPORTING et_table_fields = DATA(lt_table_fields)
    ).

    LOOP AT lt_table_fields ASSIGNING FIELD-SYMBOL(<ls_table_field>) WHERE tabname = is_entity-entity_id
                                                                       AND fieldname <> '.NODE1'
                                                                       AND datatype <> 'CLNT'.
      DATA(lv_current_index) = sy-tabix.

      DATA(lv_field_node_key) = |{ c_tablefield_node_prefix }{ mv_field_node_counter }|.
*.... add node to node map
      ct_node_map = VALUE #( BASE ct_node_map
        ( node_key  = lv_field_node_key
          node_type = zif_dbbr_tree_node_filler=>c_node_type-field
          tabname   = is_entity-entity_id
          alias     = is_entity-entity_alias
          fieldname = <ls_table_field>-fieldname )
      ).
*.... add icon for key field
      DATA(lv_key_image) = COND tv_image(
        WHEN <ls_table_field>-keyflag = abap_true THEN |@{ icon_foreign_key+1(2) }\\QKey Field@| ELSE |{ icon_space }|
      ).

*.... Collect field for matched fields node if domain or type/length match
      IF <ls_table_field>-datatype = ms_join_field_info-datatype AND
         <ls_table_field>-leng     = ms_join_field_info-leng.

        DATA(lf_domain_matched) = xsdbool( ms_join_field_info-domname = <ls_table_field>-domname ).
        IF lf_domain_matched = abap_true.
          ADD 1 TO mv_prio.
        ENDIF.

        mt_matched_fields = VALUE #( BASE mt_matched_fields
          ( fieldname     = <ls_table_field>-fieldname
            tabname       = <ls_table_field>-tabname
            tabname_alias = is_entity-entity_alias
            description   = <ls_table_field>-fieldtext
            type_match    = abap_true
            domain_match  = lf_domain_matched
            prio          = mv_prio )
        ).
      ENDIF.

*.... Add the field to the nodes
      ir_nodes->add_node(
          iv_node_key          = lv_field_node_key
          iv_relative_node_key = iv_table_node
          if_folder            = abap_false
          iv_image             = lv_key_image
          it_item_table        = VALUE #(
            ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = |{ <ls_table_field>-fieldname }| )
            ( item_name = zcl_dbbr_tabfield_tree_f4=>c_hier_col2
              style     = zif_uitb_c_ctm_style=>inverted_gray
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = <ls_table_field>-fieldtext )
          )
      ).

      ADD 1 TO mv_field_node_counter.
    ENDLOOP. " Loop over table fields
  ENDMETHOD.


  METHOD zif_dbbr_tree_node_filler~fill_node_item_tables.
    mv_field_node_counter = 1.

    TRY.
        LOOP AT mt_join_tables ASSIGNING FIELD-SYMBOL(<ls_join_table_sel>).
          DATA(lv_current_index) = sy-tabix.
          DATA(lv_type_prefix) = ``.
          DATA(lv_icon) = VALUE tv_image( ).

*........ Get table description
          DATA(ls_tab_info) = zcl_dbbr_dictionary_helper=>get_table_info( |{ <ls_join_table_sel>-entity_id }| ).

          DATA(lv_table_node_key) = |{ c_table_node_prefix }{ lv_current_index }|.
          IF lv_current_index = 1.
            mv_first_table_node = lv_table_node_key.
          ENDIF.

          CASE <ls_join_table_sel>-entity_type.

            WHEN zif_dbbr_c_entity_type=>cds_view.
              lv_icon = zif_dbbr_c_icon=>cds_view.

            WHEN zif_dbbr_c_entity_type=>view.
              lv_icon = zif_dbbr_c_icon=>database_view.

            WHEN zif_dbbr_c_entity_type=>table.
              lv_icon = zif_dbbr_c_icon=>database_table.
          ENDCASE.

          ir_nodes->add_node(
              iv_node_key          = lv_table_node_key
              if_folder            = abap_true
              iv_image             = lv_icon
              iv_expanded_image    = lv_icon
              it_item_table        = VALUE #(
                ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                  font      = cl_item_tree_model=>item_font_prop
                  class     = cl_item_tree_model=>item_class_text
                  text      = |{ <ls_join_table_sel>-entity_id_raw }| )
                ( item_name = zcl_dbbr_tabfield_tree_f4=>c_hier_col2
                  font      = cl_item_tree_model=>item_font_prop
                  text      = |({ <ls_join_table_sel>-entity_alias })|
                  class     = cl_item_tree_model=>item_class_text
                  style     = zif_uitb_c_ctm_style=>inverted_blue )
                ( item_name = zcl_dbbr_tabfield_tree_f4=>c_hier_col3
                  style     = zif_uitb_c_ctm_style=>inverted_gray
                  font      = cl_item_tree_model=>item_font_prop
                  class     = cl_item_tree_model=>item_class_text
                  text      = ls_tab_info-ddtext )
              )
          ).

          create_table_field_nodes(
            EXPORTING
              ir_nodes      = ir_nodes
              iv_table_node = lv_table_node_key
              is_entity     = <ls_join_table_sel>
            CHANGING
              ct_node_map   = rt_node_map
          ).

        ENDLOOP. " Loop over tables

        create_matched_node_and_fields(
          EXPORTING ir_nodes    = ir_nodes
          CHANGING  ct_node_map = rt_node_map
        ).
      CATCH zcx_uitb_tree_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
