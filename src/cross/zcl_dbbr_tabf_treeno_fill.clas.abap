CLASS zcl_dbbr_tabf_treeno_fill DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_dbbr_tree_node_filler .

    METHODS constructor
      IMPORTING
        !ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab.
    DATA mr_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list.
ENDCLASS.



CLASS zcl_dbbr_tabf_treeno_fill IMPLEMENTATION.


  METHOD constructor.
    mr_tabfield_list = ir_tabfield_list.

    mt_table_to_alias_map = mr_tabfield_list->build_table_to_alias_map( ).
  ENDMETHOD.


  METHOD zif_dbbr_tree_node_filler~fill_node_item_tables.
    DATA: lv_alias_prefix     TYPE string.

    mr_tabfield_list->get_fields( EXPORTING if_consider_all = abap_true
                                  IMPORTING et_fields       = DATA(lt_fields) ).
    DATA(lt_table_list) = mr_tabfield_list->get_table_list( ).

    DATA(lv_table_node_prefix) = 'TABLE'.
    DATA(lv_table_node_counter) = 1.
    DATA(lv_tablefield_node_prefix) = 'FIELD'.
    DATA(lv_tablefield_node_counter) = 1.

    LOOP AT lt_table_list ASSIGNING FIELD-SYMBOL(<ls_table>).
      CLEAR: lv_alias_prefix.
*..... check if there is an alias for the given table
      IF line_exists( mt_table_to_alias_map[ tabname = <ls_table>-tabname ] ).
        DATA(lv_table_alias) = mt_table_to_alias_map[ tabname = <ls_table>-tabname ]-alias.
      ENDIF.

      IF lv_table_alias IS NOT INITIAL.
        lv_alias_prefix = |({ lv_table_alias })|.
      ENDIF.

      DATA(lv_table_node_key) = |{ lv_table_node_prefix }{ lv_table_node_counter }|.
      ir_nodes->add_node(
          iv_node_key          = lv_table_node_key
          if_folder            = abap_true
          iv_image             = |{ icon_database_table }|
          iv_expanded_image    = |{ icon_database_table }|
          it_item_table        = VALUE #(
            ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = |{ lv_alias_prefix } Table { <ls_table>-tabname }| )
            ( item_name = 'DESC'
              font      = cl_item_tree_model=>item_font_prop
              class     = cl_item_tree_model=>item_class_text
              text      = <ls_table>-description )
          )
      ).
      rt_node_map = VALUE #( BASE rt_node_map
        ( node_key        = lv_table_node_key
          tabname         = <ls_table>-tabname
          node_type       = zif_dbbr_tree_node_filler=>c_node_type-table )
      ).

      ADD 1 TO lv_table_node_counter.

      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_table_field>) WHERE tabname = <ls_table>-tabname.
        DATA(lv_current_index) = sy-tabix.
*...... add node for table field to tree

        DATA(lv_field_node_key) = |{ lv_tablefield_node_prefix }{ lv_tablefield_node_counter }|.
*...... add node to node map
        rt_node_map = VALUE #( BASE rt_node_map
          ( node_key        = lv_field_node_key
            tabname         = <ls_table>-tabname
            node_type       = zif_dbbr_tree_node_filler=>c_node_type-field
            alias_fieldname = <ls_table_field>-sql_fieldname
            fieldname       = <ls_table_field>-fieldname )
        ).
*...... Add the field to the nodes
*...... add items for table field to tree
*...... add icon for key field
        DATA(lv_key_image) = COND tv_image(
          WHEN <ls_table_field>-is_key = abap_true THEN |@{ icon_foreign_key+1(2) }\\QKey Field@| ELSE |{ icon_space }|
        ).

        ir_nodes->add_node(
            iv_node_key          = lv_field_node_key
            iv_relative_node_key = lv_table_node_key
            if_folder            = abap_false
            iv_image             = lv_key_image
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                font      = cl_item_tree_model=>item_font_prop
                class     = cl_item_tree_model=>item_class_text
                text      = |{ <ls_table_field>-sql_fieldname }| )
              ( item_name = 'DESC'
                font      = cl_item_tree_model=>item_font_prop
                class     = cl_item_tree_model=>item_class_text
                text      = <ls_table_field>-std_medium_text )
            )
        ).

        ADD 1 TO lv_tablefield_node_counter.
      ENDLOOP. " Loop over table fields

    ENDLOOP. " Loop over tables

  ENDMETHOD.
ENDCLASS.
