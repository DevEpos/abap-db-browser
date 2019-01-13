CLASS zcl_dbbr_cds_tabfield_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS add_view_colums
      IMPORTING
        !ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list
        !if_output        TYPE abap_bool DEFAULT abap_true
        !if_selection     TYPE abap_bool DEFAULT abap_true
        !it_columns       TYPE dd03ndvtab
        !if_is_primary    TYPE abap_bool OPTIONAL
        !iv_name          TYPE ddstrucobjname
        !iv_raw_name      TYPE ddstrucobjname_raw
        !iv_description   TYPE ddtext .
    CLASS-METHODS add_parameters
      IMPORTING
        !ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list
        !it_parameters    TYPE zdbbr_cds_parameter_t .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_cds_tabfield_util IMPLEMENTATION.


  METHOD add_parameters.
    DATA: ls_datel    TYPE dd04v,
          lv_rollname TYPE rollname.

    CHECK it_parameters IS NOT INITIAL.

    LOOP AT it_parameters ASSIGNING FIELD-SYMBOL(<ls_param>) WHERE has_system_anno = abap_false.
      CLEAR: lv_rollname,
             ls_datel.

*... determine the rollname of the parameter
      lv_rollname = COND rollname(
        WHEN <ls_param>-rollname IS NOT INITIAL THEN
           <ls_param>-rollname
        WHEN <ls_param>-datatype = 'LANG' THEN
           'LANGU'
        WHEN <ls_param>-datatype = 'DATS' THEN
           'DATS'
      ).

      DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
        tabname          = zif_dbbr_global_consts=>c_parameter_dummy_table
        fieldname        = <ls_param>-parametername
        fieldname_raw    = <ls_param>-parametername_raw
        is_parameter     = abap_true
        selection_active = abap_true
        default_option   = zif_dbbr_c_options=>equals
        default_sign     = zif_dbbr_c_options=>includes
        default_low      = <ls_param>-default_value
        is_technical     = <ls_param>-has_system_anno
        ddic_order       = <ls_param>-posnr
        length           = <ls_param>-leng
        outputlen        = <ls_param>-leng
        decimals         = <ls_param>-decimals
        datatype         = <ls_param>-datatype
        inttype          = <ls_param>-inttype
        field_ddtext     = <ls_param>-parametername_raw
        std_short_text   = ls_datel-scrtext_s
        std_medium_text  = ls_datel-scrtext_m
        std_long_text    = ls_datel-scrtext_l
        header_text      = ls_datel-reptext
        is_numeric       = zcl_dbbr_dictionary_helper=>is_type_numeric( <ls_param>-inttype )
        rollname         = lv_rollname
      ).

*... get the data element
      IF ls_tabfield-rollname IS NOT INITIAL.
        ls_datel = zcl_dbbr_dictionary_helper=>get_data_element( ls_tabfield-rollname ).
        ls_tabfield-domname = ls_datel-domname.
        ls_tabfield-outputlen = ls_datel-outputlen.
        ls_tabfield-convexit = ls_datel-convexit.
      ENDIF.

      ir_tabfield_list->add( REF #( ls_tabfield ) ).
    ENDLOOP.


    " add cds view to list of tables
    ir_tabfield_list->add_table(
      VALUE zdbbr_entity_info(
        active_selection     = abap_true
        tabname              = zif_dbbr_global_consts=>c_parameter_dummy_table
        type                 = zif_dbbr_c_entity_type=>table
        description          = 'Parameters'
        no_output            = abap_true
      )
    ).

  ENDMETHOD.


  METHOD add_view_colums.
    DATA: ls_datel    TYPE dd04v,
          lv_rollname TYPE rollname.

    LOOP AT it_columns ASSIGNING FIELD-SYMBOL(<ls_column>).
      CLEAR: lv_rollname,
             ls_datel.

*... determine the rollname of the parameter
      lv_rollname = COND rollname(
        WHEN <ls_column>-rollname IS NOT INITIAL THEN
           <ls_column>-rollname
        WHEN <ls_column>-datatype = 'LANG' THEN
           'LANGU'
      ).

      DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
        tabname          = iv_name
        tabname_alias    = iv_raw_name
        fieldname        = <ls_column>-fieldname
        fieldname_raw    = <ls_column>-fieldname_raw
        is_key           = <ls_column>-keyflag
        selection_active = if_selection
        output_active    = if_output
        default_sign     = zif_dbbr_c_options=>includes
        ddic_order       = <ls_column>-position
        length           = <ls_column>-leng
        decimals         = <ls_column>-decimals
        datatype         = <ls_column>-datatype
        rollname         = lv_rollname
        ref_field        = <ls_column>-reffield
        ref_tab          = <ls_column>-reftable
      ).


*... get additional information from rollname
      IF ls_tabfield-rollname IS NOT INITIAL.
        ls_datel = zcl_dbbr_dictionary_helper=>get_data_element( ls_tabfield-rollname ).
        " TODO: Adjust addtext bl to load existing value helps for rollname
*        zcl_dbbr_addtext_bl=>get_instance( )->determine_text_fields( is_data_element_info = ls_datel ).
        ls_tabfield-domname = ls_datel-domname.
        ls_tabfield-is_lowercase = ls_datel-lowercase.
        ls_tabfield-outputlen = ls_datel-outputlen.
        ls_tabfield-f4_available = xsdbool( ls_datel-shlpname IS NOT INITIAL ).
        ls_tabfield-std_short_text = ls_datel-scrtext_s.
        ls_tabfield-std_medium_text = ls_datel-scrtext_m.
        ls_tabfield-std_long_text = ls_datel-scrtext_l.
        ls_tabfield-header_text = ls_datel-reptext.
        ls_tabfield-field_ddtext = ls_datel-ddtext.
        ls_tabfield-convexit = ls_datel-convexit.
        ls_tabfield-inttype = zcl_dbbr_dictionary_helper=>get_dtel_inttype( iv_data_element = ls_datel-rollname  ).
      ELSE.
        TRY.
            DATA(lr_typedescr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( |{ <ls_column>-strucobjn }-{ <ls_column>-fieldname }| ) ).
            ls_tabfield-inttype = lr_typedescr->type_kind.
            ls_tabfield-outputlen = lr_typedescr->output_length.

          CATCH cx_sy_move_cast_error.
        ENDTRY.
      ENDIF.

      ls_tabfield-is_numeric = zcl_dbbr_dictionary_helper=>is_type_numeric( ls_tabfield-inttype ).
      DATA(lr_new_field) = ir_tabfield_list->zif_uitb_data_ref_list~add( REF #( ls_tabfield ) ).
      " TODO: Add additional text field if any exists for the new field @see zcl_dbbr_selscreen_util->create_table_fields( )
    ENDLOOP.

    " add cds view to list of tables
    ir_tabfield_list->add_table(
      VALUE zdbbr_entity_info(
        active_selection     = if_selection
        tabname              = iv_name
        tabname_alias        = iv_raw_name
        type                 = zif_dbbr_c_entity_type=>cds_view
        description          = iv_description
        fields_are_loaded    = abap_true
        is_primary           = if_is_primary
      )
    ).
  ENDMETHOD.
ENDCLASS.
