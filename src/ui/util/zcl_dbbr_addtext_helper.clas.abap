CLASS zcl_dbbr_addtext_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS fill_text_field_value
      IMPORTING
        iv_text_field     TYPE fieldname
        iv_alv_text_field TYPE fieldname
        is_text_cache_row TYPE any
        ir_row            TYPE REF TO data
        it_fix_values     TYPE ddfixvalues OPTIONAL.
    CLASS-METHODS determine_text_field_info
      IMPORTING
        it_text_fields   TYPE zdbbr_additional_text_itab
        is_row           TYPE any
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
        iv_alv_fieldname TYPE fieldname
      EXPORTING
        ev_text_table    TYPE tabname
        ev_text_field    TYPE fieldname
        ev_text_ddtext   TYPE ddtext
        ef_domain_text   TYPE abap_bool
        ev_domain        TYPE domname.
    CLASS-METHODS prepare_text_fields
      IMPORTING
        ir_fields    TYPE REF TO zcl_dbbr_tabfield_list
      CHANGING
        ct_add_texts TYPE zdbbr_additional_text_itab.
ENDCLASS.



CLASS zcl_dbbr_addtext_helper IMPLEMENTATION.
  METHOD fill_text_field_value.
    FIELD-SYMBOLS: <ls_row> TYPE any.

    CHECK iv_text_field IS NOT INITIAL.

    ASSIGN ir_row->* TO <ls_row>.

    ASSIGN COMPONENT iv_alv_text_field OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_text_value>).
    ASSIGN COMPONENT iv_text_field     OF STRUCTURE is_text_cache_row TO FIELD-SYMBOL(<lv_table_text_value>).
    IF <lv_text_value> IS ASSIGNED AND
       <lv_table_text_value> IS ASSIGNED.

      " check if there are domain fix values for this text field
      IF it_fix_values IS NOT INITIAL.
        DATA(lv_domain_fix_value) = condense( val = CONV domvalue_l( <lv_table_text_value> ) ).
        DATA(ls_text_value) = VALUE #( it_fix_values[ low = lv_domain_fix_value ] OPTIONAL ).
        <lv_text_value> = COND string( WHEN ls_text_value IS INITIAL THEN <lv_table_text_value> ELSE ls_text_value-ddtext ).
      ELSE.
        <lv_text_value> = <lv_table_text_value>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD determine_text_field_info.
    LOOP AT it_text_fields ASSIGNING FIELD-SYMBOL(<ls_add_text>) WHERE id_table = iv_tabname
                                                                   AND id_field = iv_fieldname.
      " get domain key value from selection table
      ASSIGN COMPONENT <ls_add_text>-id_field_alv_int OF STRUCTURE is_row TO FIELD-SYMBOL(<lv_text_key_value>).
      CHECK sy-subrc = 0.

      IF <ls_add_text>-id_field2_alv_int IS NOT INITIAL.
        ASSIGN COMPONENT <ls_add_text>-id_field2_alv_int OF STRUCTURE is_row TO FIELD-SYMBOL(<lv_text_key2_value>).
        CHECK sy-subrc = 0.
      ENDIF.

      " is this a condition field?
      IF <ls_add_text>-condition_field IS NOT INITIAL.
        ASSIGN COMPONENT <ls_add_text>-condition_field_alv_int OF STRUCTURE is_row TO FIELD-SYMBOL(<lv_cond_key_value>).
        IF sy-subrc <> 0.
          CONTINUE.
        ELSE. " check the condition value
          CASE <ls_add_text>-condition_op.
            WHEN 'EQ'.
              CHECK <lv_cond_key_value> EQ <ls_add_text>-condition_value.
            WHEN 'CP'.
              CHECK <lv_cond_key_value> CP <ls_add_text>-condition_value.
            WHEN 'CO'.
              CHECK <lv_cond_key_value> CO <ls_add_text>-condition_value.
            WHEN 'CA'.
              CHECK <lv_cond_key_value> CA <ls_add_text>-condition_value.
          ENDCASE.
        ENDIF.
      ENDIF.

      " read corresponding text value
      IF <ls_add_text>-selection_type = zif_dbbr_c_text_selection_type=>domain_value.
        ef_domain_text = abap_true.
        DATA(lr_dtel_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( |{ <ls_add_text>-id_table }-{ <ls_add_text>-id_field }| ) ).
        ev_domain = lr_dtel_descr->get_ddic_field( )-domname.
        ev_text_ddtext = lr_dtel_descr->get_ddic_field( )-fieldtext.
        RETURN.
      ELSE.
        lr_dtel_descr = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( |{ <ls_add_text>-text_table }-{ <ls_add_text>-text_field }| ) ).
        ev_text_field = <ls_add_text>-text_field.

        ev_text_table = <ls_add_text>-text_table.
        ev_text_ddtext = lr_dtel_descr->get_ddic_field( )-fieldtext.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD prepare_text_fields.
    " get all active text fields from tabfields
    ir_fields->switch_mode( zif_dbbr_c_global=>c_field_chooser_modes-output ).
    ir_fields->initialize_iterator( if_for_active = abap_true ).

    WHILE ir_fields->has_more_lines( ).
      DATA(lr_current_field) = ir_fields->get_next_entry( ).

      CHECK: lr_current_field->is_text_field = abap_true.

      IF NOT line_exists( ct_add_texts[ id_table = lr_current_field->tabname
                                        id_field = lr_current_field->fieldname ] ).
        DATA(lt_addtexts) = zcl_dbbr_addtext_bl=>get_instance( )->get_text_fields( iv_tablename = lr_current_field->tabname
                                                                                    iv_fieldname = lr_current_field->fieldname ).
        LOOP AT lt_addtexts ASSIGNING FIELD-SYMBOL(<ls_addtext>).
          DATA(ls_new_text_field) = CORRESPONDING zdbbr_additional_text( <ls_addtext> ).

          " update the id field name to so that the text field in the output structure can be found
          ls_new_text_field-id_field_alv_int = lr_current_field->reference_alv_fieldname.

          " is there a second key field?
          IF ls_new_text_field-id_field2 IS NOT INITIAL.
            DATA(lr_id_field2) = ir_fields->get_field_ref( iv_tabname_alias = <ls_addtext>-id_table
                                                           iv_fieldname     = <ls_addtext>-id_field2 ).
            ls_new_text_field-id_field2_alv_int = lr_id_field2->alv_fieldname.
          ENDIF.

          " check if there is a condition field present
          IF ls_new_text_field-condition_field IS NOT INITIAL.
            TRY.
                " it is possible because of grouping by that the condition field was
                " not selected for the aggregation
                DATA(lr_cond_field) = ir_fields->get_field_ref(
                    iv_tabname_alias = ls_new_text_field-id_table
                    iv_fieldname     = ls_new_text_field-condition_field
                ).
              CATCH cx_sy_itab_line_not_found.
                " text field cannot be used
                CONTINUE.
            ENDTRY.

            ls_new_text_field-condition_field_alv_int = lr_cond_field->alv_fieldname.
          ENDIF.

          APPEND ls_new_text_field TO ct_add_texts.
        ENDLOOP.
      ENDIF.

      " fill alv text field names of text fields
      LOOP AT ct_add_texts ASSIGNING FIELD-SYMBOL(<ls_addtext_info>) WHERE id_table = lr_current_field->tabname
                                                                       AND id_field = lr_current_field->fieldname.

        <ls_addtext_info>-text_field_alv_int = lr_current_field->alv_fieldname.
      ENDLOOP.
    ENDWHILE.

    zcl_dbbr_ddic_util=>fill_data_cache_for_add_texts( CHANGING ct_add_texts = ct_add_texts ).
  ENDMETHOD.
ENDCLASS.
