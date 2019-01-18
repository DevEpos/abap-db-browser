"! <p class="shorttext synchronized" lang="en">Fills tree nodes/items out of table field list</p>
CLASS zcl_dbbr_table_treeno_fill DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_tree_node_filler .

    METHODS constructor
      IMPORTING
        !it_join_tables     TYPE zdbbr_selopt_itab
        !is_join_field_info TYPE dfies OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_table_node_prefix TYPE string VALUE 'TABLE'.
    CONSTANTS c_tablefield_node_prefix TYPE string VALUE 'FIELD'.
    CONSTANTS c_matched_node TYPE tm_nodekey VALUE '<MATCHED>'.
    DATA mv_field_node_counter TYPE i.
    DATA mv_first_table_node TYPE tm_nodekey.
    data mv_prio type sy-tabix.

    ALIASES tt_node_map
      FOR zif_dbbr_tree_node_filler~tt_node_map .

    TYPES:
      BEGIN OF ty_matched_type_fields,
        prio         TYPE sy-tabix,
        fieldname    TYPE fieldname,
        tabname      TYPE tabname,
        description  TYPE ddtext,
        domain_match TYPE abap_bool,
        type_match   TYPE abap_bool,
      END OF ty_matched_type_fields .

    DATA mt_join_tables_selopt TYPE zdbbr_selopt_itab .
    DATA ms_join_field_info TYPE dfies .
    DATA:
      mt_matched_fields TYPE STANDARD TABLE OF ty_matched_type_fields WITH EMPTY KEY .

    "! <p class="shorttext synchronized" lang="en">Create field nodes for given table</p>
    METHODS create_table_field_nodes
      IMPORTING
        !ir_nodes       TYPE REF TO zcl_uitb_ctm_nodes
        !iv_table_node  TYPE tm_nodekey
        iv_table_prefix TYPE string OPTIONAL
        !iv_tabname     TYPE tabname
      CHANGING
        !ct_node_map    TYPE tt_node_map
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



CLASS ZCL_DBBR_TABLE_TREENO_FILL IMPLEMENTATION.


  METHOD constructor.
    mt_join_tables_selopt = it_join_tables.
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
    ct_node_map = value #( base ct_node_map
      ( node_key = c_matched_node
        node_type = zif_dbbr_tree_node_filler=>c_node_type-matched_fields )
    ).

*.. Add matched fields - 1) domain matched, 2) type matches
    SORT mt_matched_fields BY domain_match DESCENDING prio ascending.
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
          tabname   = <ls_matched_fields>-tabname )
      ).
      ir_nodes->add_node(
          iv_node_key          = lv_matched_field_node
          iv_relative_node_key = c_matched_node
          iv_image             = lv_icon
          iv_style             = cond #( when <ls_matched_fields>-domain_match = abap_true and
                                              <ls_matched_fields>-fieldname = ms_join_field_info-fieldname then zif_uitb_c_ctm_style=>light_green )
          it_item_table        = VALUE #(
            ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = |{ <ls_matched_fields>-tabname }-{ <ls_matched_fields>-fieldname }| )
            ( item_name = 'DESC'
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = <ls_matched_fields>-description )
          )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD create_table_field_nodes.
    zcl_dbbr_dictionary_helper=>get_table_field_infos(
      EXPORTING iv_tablename    = iv_tabname
      IMPORTING et_table_fields = DATA(lt_table_fields)
    ).

    LOOP AT lt_table_fields ASSIGNING FIELD-SYMBOL(<ls_table_field>) WHERE tabname = iv_tabname
                                                                       AND datatype <> 'CLNT'.
      DATA(lv_current_index) = sy-tabix.

      DATA(lv_field_node_key) = |{ c_tablefield_node_prefix }{ mv_field_node_counter }|.
*.... add node to node map
      ct_node_map = VALUE #( BASE ct_node_map
        ( node_key  = lv_field_node_key
          node_type = zif_dbbr_tree_node_filler=>c_node_type-field
          tabname   = iv_tabname
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
          ( fieldname    = <ls_table_field>-fieldname
            tabname      = <ls_table_field>-tabname
            description  = <ls_table_field>-fieldtext
            type_match   = abap_true
            domain_match = lf_domain_matched
            prio         = mv_prio )
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
            ( item_name = 'DESC'
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
        LOOP AT mt_join_tables_selopt ASSIGNING FIELD-SYMBOL(<ls_join_table_sel>).
          DATA(lv_current_index) = sy-tabix.
*........ Get table description
          DATA(ls_tab_info) = zcl_dbbr_dictionary_helper=>get_table_info( |{ <ls_join_table_sel>-low }| ).

          DATA(lv_table_node_key) = |{ c_table_node_prefix }{ lv_current_index }|.
          IF lv_current_index = 1.
            mv_first_table_node = lv_table_node_key.
          ENDIF.

          ir_nodes->add_node(
              iv_node_key          = lv_table_node_key
              if_folder            = abap_true
              iv_image             = |{ icon_database_table }|
              iv_expanded_image    = |{ icon_database_table }|
              it_item_table        = VALUE #(
                ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                  font      = cl_item_tree_model=>item_font_prop
                  class     = cl_item_tree_model=>item_class_text
                  text      = |Table { <ls_join_table_sel>-low }| )
                ( item_name = 'DESC'
                  font      = cl_item_tree_model=>item_font_prop
                  class     = cl_item_tree_model=>item_class_text
                  text      = ls_tab_info-ddtext )
              )
          ).

          create_table_field_nodes(
            EXPORTING
              ir_nodes      = ir_nodes
              iv_table_node = lv_table_node_key
              iv_tabname    = |{ <ls_join_table_sel>-low }|
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
