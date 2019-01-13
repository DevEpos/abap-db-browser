FORM init_prog_data_cache USING ir_data_cache TYPE REF TO zcl_uitb_data_cache.
*& Description: Initializes global data cache
*&---------------------------------------------------------------------*
  DATA: lt_global_data TYPE STANDARD TABLE OF zuitb_global_data_ref.
  FIELD-SYMBOLS: <lv_global_data_var> TYPE any,
                 <lv_comp_value>      TYPE any.

  DATA(lt_components) = zcl_uitb_appl_util=>get_program_variables( sy-repid ).

  LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_global_data>).
    UNASSIGN <lv_global_data_var>.
    " get reference of global variable
    ASSIGN (<ls_global_data>-name) TO <lv_global_data_var>.
    IF sy-subrc = 0.
      ir_data_cache->register_data(
          ir_variable_ref  = REF #( <lv_global_data_var> )
          iv_variable_name = |{ <ls_global_data>-name }|
      ).

      DATA(lr_type_descr) = cl_abap_typedescr=>describe_by_data( <lv_global_data_var> ).
*.... Resolve structured components and register also component names
      IF lr_type_descr->kind = cl_abap_typedescr=>kind_struct.
        DATA(lt_struct_comp) = CAST cl_abap_structdescr( lr_type_descr )->components.

        LOOP AT lt_struct_comp ASSIGNING FIELD-SYMBOL(<ls_component>).
          DATA(lv_comp_name) = |{ <ls_global_data>-name }-{ <ls_component>-name }|.
          ASSIGN (lv_comp_name) TO <lv_comp_value>.
          IF sy-subrc = 0.
            ir_data_cache->register_data(
                ir_variable_ref  = REF #( <lv_comp_value> )
                iv_variable_name = lv_comp_name
            ).
          ENDIF.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
