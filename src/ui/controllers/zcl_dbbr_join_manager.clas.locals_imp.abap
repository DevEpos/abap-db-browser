*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

 CLASS lcl_join_field IMPLEMENTATION.
   METHOD constructor.
     mv_node_key = iv_node_key.
     mv_table = iv_table.
     mv_table_alias = iv_alias.
     ms_field = is_field.
   ENDMETHOD.

   METHOD get_field.
     result = ms_field.
   ENDMETHOD.

   METHOD update_field.
     mf_changed = xsdbool(
        ms_field-field <> value-field OR
        ms_field-off_length <> value-off_length OR
        ms_field-off_offset <> value-off_offset OR
        ms_field-operator <> value-operator OR
        ms_field-ref_table <> value-ref_table OR
        ms_field-ref_table_alias <> value-ref_table_alias OR
        ms_field-ref_field <> value-ref_field
     ).
     rf_changed = mf_changed.
     ms_field = value.
   ENDMETHOD.
 ENDCLASS.

 CLASS lcl_join_filter IMPLEMENTATION.
   METHOD constructor.
     mv_node_key = iv_node_key.
     mv_table = iv_table.
     mv_table_alias = iv_alias.
     ms_filter = is_filter.
   ENDMETHOD.

   METHOD set_and_or.
     ms_filter-and_or = value.
   ENDMETHOD.

   METHOD set_or_group_node.
     mv_or_group_node = iv_node_key.
   ENDMETHOD.

   METHOD get_filter.
     result = ms_filter.
   ENDMETHOD.

   METHOD update_filter.
     mf_changed = xsdbool(
        ms_filter-fieldname <> value-fieldname OR
        ms_filter-operator <> value-operator OR
        ms_filter-tabname <> value-tabname OR
        ms_filter-tabname_alias <> value-tabname_alias OR
        ms_filter-value <> value-value OR
        ms_filter-value2 <> value-value2 OR
        ms_filter-value_type <> value-value_type
     ).
     rf_changed = mf_changed.
     ms_filter = value.
   ENDMETHOD.
 ENDCLASS.

 CLASS lcl_join_table IMPLEMENTATION.
   METHOD constructor.
     mv_node_key = iv_node_key.
     mv_tabname = is_join_table-add_table.
     mv_alias = is_join_table-add_table_alias.
     ms_table_info = is_join_table.
     mv_type = is_join_table-entity_type.
   ENDMETHOD.

   METHOD clear_offset_from_fields.
     LOOP AT mt_field_cond ASSIGNING FIELD-SYMBOL(<ls_field>).
       CLEAR: <ls_field>-field_ref->ms_field-off_length,
              <ls_field>-field_ref->ms_field-off_offset.
     ENDLOOP.
   ENDMETHOD.

   METHOD has_changes.
     result = mf_changed.
     IF result = abap_true.
       RETURN.
     ENDIF.

*... check field conditions
     LOOP AT mt_field_cond ASSIGNING FIELD-SYMBOL(<ls_field>).
       result = <ls_field>-field_ref->mf_changed.
       IF result = abap_true.
         RETURN.
       ENDIF.
     ENDLOOP.

*... check filter conditions
     LOOP AT mt_filter_cond ASSIGNING FIELD-SYMBOL(<ls_filter>).
       result = <ls_filter>-filter_ref->mf_changed.
       IF result = abap_true.
         RETURN.
       ENDIF.
     ENDLOOP.

   ENDMETHOD.

   METHOD clear_changed_flag.
     CLEAR mf_changed.

     LOOP AT mt_field_cond ASSIGNING FIELD-SYMBOL(<ls_field>).
       CLEAR <ls_field>-field_ref->mf_changed.
     ENDLOOP.

     LOOP AT mt_filter_cond ASSIGNING FIELD-SYMBOL(<ls_filter>).
       CLEAR <ls_filter>-filter_ref->mf_changed.
     ENDLOOP.
   ENDMETHOD.

   METHOD get_tab_info.
     result = ms_table_info.
   ENDMETHOD.

   METHOD set_tab_info.
     mf_changed = xsdbool(
      ms_table_info-is_virtual <> value-is_virtual OR
      ms_table_info-join_type <> value-join_type OR
      ms_table_info-add_table_alias <> value-add_table_alias
     ).
     rf_changed = mf_changed.
     ms_table_info = value.
   ENDMETHOD.

   METHOD delete_dependent.

     LOOP AT mt_field_cond ASSIGNING FIELD-SYMBOL(<ls_field>).
       IF <ls_field>-field_ref->ms_field-ref_table = iv_table.
*...... delete field and its node
         RAISE EVENT lif_tree_node_events~request_deletion
           EXPORTING
             ev_node_key = <ls_field>-node_key.

         DELETE mt_field_cond.
       ENDIF.
     ENDLOOP.

     LOOP AT mt_filter_cond ASSIGNING FIELD-SYMBOL(<ls_filter>).
       IF <ls_filter>-filter_ref->ms_filter-tabname = iv_table.
*....... delete filter and its node
         RAISE EVENT lif_tree_node_events~request_deletion
           EXPORTING
             ev_node_key = <ls_filter>-node_key.

         DELETE mt_filter_cond.
       ENDIF.
     ENDLOOP.
   ENDMETHOD.

   METHOD add_filter.
*... get index of predecessor filter
     DATA(lo_node) = io_filter_node->get_previous_sibling( ).
     IF lo_node IS INITIAL.
       mt_filter_cond = VALUE #( BASE mt_filter_cond
         ( node_key   = io_filter->mv_node_key
           filter_ref = io_filter )
        ).
     ELSE.
       DATA(lv_relat_index) = line_index( mt_filter_cond[ node_key = lo_node->mv_node_key ] ).
       IF lv_relat_index = 0.
         mt_filter_cond = VALUE #(
           BASE mt_filter_cond
           ( node_key   = io_filter->mv_node_key
             filter_ref = io_filter )
         ).
       ELSE.
         INSERT VALUE #(
          node_key   = io_filter->mv_node_key
          filter_ref = io_filter
         ) INTO mt_filter_cond INDEX lv_relat_index + 1.
       ENDIF.
     ENDIF.

     mf_changed = abap_true.
   ENDMETHOD.

   METHOD delete_filter.
     DELETE mt_filter_cond WHERE node_key = iv_node_key.
     mf_changed = abap_true.
   ENDMETHOD.

   METHOD delete_field.
     DELETE mt_field_cond WHERE node_key = iv_node_key.
     mf_changed = abap_true.
   ENDMETHOD.

   METHOD validate.
     IF mt_field_cond IS INITIAL.
       zcx_dbbr_validation_exception=>raise_with_text(
           iv_text = |Join Table { mv_tabname } has no single Field condition|
       ).
     ENDIF.
   ENDMETHOD.

   METHOD add_field.
*... get index of predecessor field
     DATA(lo_node) = io_field_node->get_previous_sibling( ).
     IF lo_node IS INITIAL.
       mt_field_cond = VALUE #( BASE mt_field_cond
        ( node_key  = io_field->mv_node_key
          field_ref = io_field )
       ).
     ELSE.
       DATA(lv_relat_index) = line_index( mt_field_cond[ node_key = lo_node->mv_node_key ] ).
       INSERT VALUE #(
        node_key  = io_field->mv_node_key
        field_ref = io_field
       ) INTO mt_field_cond INDEX lv_relat_index + 1.
     ENDIF.
     mf_changed = abap_true.

   ENDMETHOD.

   METHOD has_references_to_table.
     LOOP AT mt_field_cond ASSIGNING FIELD-SYMBOL(<ls_field>).
       IF <ls_field>-field_ref->ms_field-ref_table = iv_table.
         rf_dependencies_exist = abap_true.
         RETURN.
       ENDIF.
     ENDLOOP.

     LOOP AT mt_filter_cond ASSIGNING FIELD-SYMBOL(<ls_filter>).
       IF <ls_filter>-filter_ref->ms_filter-tabname = iv_table.
         rf_dependencies_exist = abap_true.
         RETURN.
       ENDIF.
     ENDLOOP.
   ENDMETHOD.

   METHOD to_structure.
     FIELD-SYMBOLS: <ls_last_filter> TYPE zdbbr_joinfil.

     rs_table = CORRESPONDING #(
       get_tab_info( )
     ).
     rs_table-add_table_alias_alv = zcl_dbbr_entity_alias_util=>get_next_free_alv_alias( ).

*... Fill field conditions for table
     LOOP AT mt_field_cond ASSIGNING FIELD-SYMBOL(<ls_field_info>).
       rs_table-field_conditions = VALUE #(
         BASE rs_table-field_conditions
         ( <ls_field_info>-field_ref->get_field( ) )
       ).
     ENDLOOP.

     DATA(lv_or_group) = ||.

*... Fill filter conditions for table
     DATA(lv_filter_count) = lines( mt_filter_cond ).
     LOOP AT mt_filter_cond ASSIGNING FIELD-SYMBOL(<ls_filter_info>).
       DATA(lv_current_index) = sy-tabix.
       DATA(lv_filter_or_group_node) = <ls_filter_info>-filter_ref->mv_or_group_node.
       DATA(ls_filter_info) = <ls_filter_info>-filter_ref->get_filter( ).

       IF lv_or_group IS INITIAL.
         lv_or_group = lv_filter_or_group_node.
       ELSEIF lv_or_group <> lv_filter_or_group_node. " AND
*              lv_filter_or_group_node IS NOT INITIAL.
*....... The last filter's OR needs to be switched to AND
         <ls_last_filter>-and_or = zif_dbbr_c_selection_condition=>and.
         lv_or_group = lv_filter_or_group_node.
       ENDIF.

       APPEND ls_filter_info TO rs_table-filter_conditions ASSIGNING <ls_last_filter>.

       IF lv_current_index = lv_filter_count.
*...... clear the and/or of the last filter of the table
         CLEAR <ls_last_filter>-and_or.
       ENDIF.
     ENDLOOP.
   ENDMETHOD.

   METHOD delete_filters.
     CLEAR mt_filter_cond.
     mf_changed = abap_true.
   ENDMETHOD.


   METHOD delete_fields.
     CLEAR mt_field_cond.
     mf_changed = abap_true.
   ENDMETHOD.


   METHOD update_alias.
     LOOP AT mt_field_cond ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
       IF <ls_field_cond>-field_ref->ms_field-ref_table_alias = iv_alias_old.
         <ls_field_cond>-field_ref->ms_field-ref_table_alias = iv_alias_new.
         rt_nodes_to_update = VALUE #( BASE rt_nodes_to_update
           ( node_key  = <ls_field_cond>-node_key
             item_key  = c_columns-value
             new_value = |{ iv_alias_new }-{ <ls_field_cond>-field_ref->ms_field-ref_field }| )
         ).
       ENDIF.
     ENDLOOP.

     LOOP AT mt_filter_cond ASSIGNING FIELD-SYMBOL(<ls_filter_cond>).
       IF <ls_filter_cond>-filter_ref->ms_filter-tabname_alias = iv_alias_old.
         <ls_filter_cond>-filter_ref->ms_filter-tabname_alias = iv_alias_new.
         rt_nodes_to_update = VALUE #( BASE rt_nodes_to_update
           ( node_key  = <ls_filter_cond>-node_key
             item_key  = c_columns-field
             new_value = |{ iv_alias_new }-{ <ls_filter_cond>-filter_ref->ms_filter-fieldname }| )
         ).
       ENDIF.
     ENDLOOP.
   ENDMETHOD.

 ENDCLASS.

 CLASS lcl_join IMPLEMENTATION.
   METHOD constructor.
     mv_primary_entity_type = is_primary_entity-entity_type.
     mv_primary_entity = is_primary_entity-entity_id.
     mv_primary_entity_raw = is_primary_entity-entity_id_raw.
     mv_primary_entity_alias = is_primary_entity-entity_alias.
   ENDMETHOD.

   METHOD to_structure.
     zcl_dbbr_entity_alias_util=>initialize_aliases( if_alv_alias_only = abap_true ).
     rs_join_def-primary_table = mv_primary_entity.
     rs_join_def-primary_table_alias = mv_primary_entity_alias.
     rs_join_def-primary_table_alias_alv = zcl_dbbr_entity_alias_util=>get_next_free_alv_alias( ).
     rs_join_def-primary_table_entity_type = mv_primary_entity_type.

*... Fill join tables
     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       rs_join_def-tables = VALUE #(
          BASE rs_join_def-tables
          ( <ls_table>-tab_ref->to_structure( io_tree ) )
       ).
     ENDLOOP.
   ENDMETHOD.

   METHOD update_alias.

     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       rt_nodes_to_update = VALUE #(
         BASE rt_nodes_to_update
         ( LINES OF CORRESPONDING #(
              <ls_table>-tab_ref->update_alias(
                  iv_alias_old = mv_primary_entity_alias
                  iv_alias_new = iv_alias )
           )
         )
       ).
     ENDLOOP.

     mv_primary_entity_alias = iv_alias.

   ENDMETHOD.

   METHOD has_changes.
     result = mf_changed.
     IF result = abap_true.
       RETURN.
     ENDIF.
*... check join tables for changes
     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       result = <ls_table>-tab_ref->has_changes( ).
       IF result = abap_true.
         RETURN.
       ENDIF.
     ENDLOOP.
   ENDMETHOD.

   METHOD get_possible_entities_for_f4.
     rt_entities = VALUE #(
       ( entity_id        = mv_primary_entity
         entity_id_raw    = mv_primary_entity
         entity_alias     = mv_primary_entity_alias
         entity_type      = mv_primary_entity_type )
     ).
     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       rt_entities = VALUE #(
         BASE rt_entities
         ( entity_id        = <ls_table>-tabname
           entity_id_raw    = <ls_table>-tabname
           entity_alias     = <ls_table>-alias
           entity_type      = <ls_table>-tab_ref->mv_type      )
       ).
       IF <ls_table>-alias = iv_entity_alias.
         RETURN.
       ENDIF.
     ENDLOOP.
   ENDMETHOD.

   METHOD has_entity.
     rf_exists = xsdbool( line_exists( mt_tables[ alias = iv_alias ] ) ).
   ENDMETHOD.

   METHOD get_entity.
     rr_table = mt_tables[ alias = iv_alias ]-tab_ref.
   ENDMETHOD.

   METHOD add_table.
     mt_tables = VALUE #( BASE mt_tables
       ( tab_ref  = io_table
         node_key = io_table->mv_node_key
         alias    = io_table->mv_alias
         tabname  = io_table->mv_tabname )
     ).
     mf_changed = abap_true.
   ENDMETHOD.

   METHOD delete_table.
*... delete all dependent filters / fields for this table
     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       zcl_dbbr_entity_alias_util=>unregister_alias( <ls_table>-alias ).
       <ls_table>-tab_ref->delete_dependent( iv_table ).
     ENDLOOP.

*.... delete the table itself
     DELETE mt_tables WHERE tabname = iv_table.
     mf_changed = abap_true.
   ENDMETHOD.

   METHOD delete_all_tables.
*... unregister used aliases
     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       zcl_dbbr_entity_alias_util=>unregister_alias( <ls_table>-alias ).
     ENDLOOP.
     CLEAR mt_tables.
     mf_changed = abap_true.
   ENDMETHOD.

   METHOD clear_changed_flag.
     CLEAR mf_changed.

     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       <ls_table>-tab_ref->clear_changed_flag( ).
     ENDLOOP.
   ENDMETHOD.

   METHOD validate.
     LOOP AT mt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
       <ls_table>-tab_ref->validate( ).
     ENDLOOP.
   ENDMETHOD.
 ENDCLASS.
