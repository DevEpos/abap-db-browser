"! <p class="shorttext synchronized" lang="en">Utilities for CDS View Tablefield list</p>
CLASS zcl_dbbr_cds_tabfield_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Adds columns to tab field list</p>
    "!
    CLASS-METHODS add_view_colums
      IMPORTING
        !ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list
        io_custom_f4_map  TYPE REF TO zcl_dbbr_custom_f4_map OPTIONAL
        !if_output        TYPE abap_bool DEFAULT abap_true
        !if_selection     TYPE abap_bool DEFAULT abap_true
        !if_is_primary    TYPE abap_bool OPTIONAL
        !io_cds_view      TYPE REF TO zcl_sat_cds_view
        iv_alias          TYPE zsat_entity_alias OPTIONAL
      RETURNING
        VALUE(rs_entity)  TYPE zdbbr_entity_info .
    "! <p class="shorttext synchronized" lang="en">Add parameters to tab field list</p>
    "!
    CLASS-METHODS add_parameters
      IMPORTING
        !ir_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list
        io_custom_f4_map  TYPE REF TO zcl_dbbr_custom_f4_map OPTIONAL
        !it_parameters    TYPE zif_sat_ty_global=>ty_t_cds_parameter
      RETURNING
        VALUE(rs_entity)  TYPE zdbbr_entity_info .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_annotation_objectmodel,
        virtual_element TYPE string VALUE 'OBJECTMODEL.VIRTUALELEMENT',
      END OF c_annotation_objectmodel.
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
        WHEN <ls_param>-datatype = 'TIMS' THEN
           'TIMS'
      ).

      DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
        tabname          = zif_dbbr_c_global=>c_parameter_dummy_table
        fieldname        = <ls_param>-parametername
        fieldname_raw    = <ls_param>-parametername_raw
        is_parameter     = abap_true
        selection_active = abap_true
        default_option   = zif_sat_c_options=>equals
        default_sign     = zif_sat_c_options=>including
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
        is_numeric       = zcl_dbbr_ddic_util=>is_type_numeric( <ls_param>-inttype )
        rollname         = lv_rollname
      ).

*... get the data element
      IF ls_tabfield-rollname IS NOT INITIAL.
        ls_datel = zcl_sat_ddic_repo_access=>get_data_element( ls_tabfield-rollname ).
        ls_tabfield-domname = ls_datel-domname.
        ls_tabfield-outputlen = ls_datel-outputlen.
        ls_tabfield-convexit = ls_datel-convexit.
        ls_tabfield-is_lowercase = ls_datel-lowercase.
      ENDIF.

      IF io_custom_f4_map IS BOUND.
        ls_tabfield-has_custom_f4 = io_custom_f4_map->entry_exists(
          iv_tabname       = <ls_param>-strucobjn
          iv_fieldname     = ls_tabfield-fieldname
          iv_rollname      = ls_tabfield-rollname
          is_built_in_type = VALUE #(
            datatype  = ls_tabfield-datatype
            leng      = ls_tabfield-length
          )
        ).
      ENDIF.

      ir_tabfield_list->add( REF #( ls_tabfield ) ).
    ENDLOOP.

*.. add cds view to list of tables
    rs_entity = VALUE zdbbr_entity_info(
       active_selection     = abap_true
       tabname              = zif_dbbr_c_global=>c_parameter_dummy_table
       tabname_alias        = zif_dbbr_c_global=>c_parameter_dummy_table
       type                 = zif_sat_c_entity_type=>table
       description          = 'Parameters'
       no_output            = abap_true
    ).

    ir_tabfield_list->add_table( rs_entity ).

  ENDMETHOD.


  METHOD add_view_colums.
    DATA: ls_datel    TYPE dfies,
          lv_rollname TYPE rollname.

    DATA(lr_addtext_bl) = zcl_dbbr_addtext_bl=>get_instance( ).
    DATA(lo_altcoltext_f) = NEW zcl_dbbr_altcoltext_factory( ).

    DATA(lt_annotation) = io_cds_view->get_annotations(
      it_annotation_name = VALUE #( ( sign = 'I' option = 'EQ' low = c_annotation_objectmodel-virtual_element ) ) ).

    LOOP AT io_cds_view->get_columns( ) ASSIGNING FIELD-SYMBOL(<ls_column>).
      CLEAR: lv_rollname,
             ls_datel.

*.... determine the rollname of the parameter
      lv_rollname = COND rollname(
        WHEN <ls_column>-rollname IS NOT INITIAL THEN
           <ls_column>-rollname
        WHEN <ls_column>-datatype = 'LANG' THEN
           'LANGU'
      ).

      DATA(ls_altcoltext) = lo_altcoltext_f->find_alternative_text(
          iv_tabname   = <ls_column>-strucobjn
          iv_fieldname = <ls_column>-fieldname
      ).

      DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
        tabname            = io_cds_view->mv_view_name
        tabname_raw        = io_cds_view->get_header( )-entityname_raw
        tabname_alias      = COND #( WHEN iv_alias IS NOT INITIAL THEN iv_alias ELSE io_cds_view->mv_view_name )
        fieldname          = <ls_column>-fieldname
        fieldname_raw      = <ls_column>-fieldname_raw
        is_key             = <ls_column>-keyflag
        selection_active   = if_selection
        output_active      = if_output
        default_sign       = zif_sat_c_options=>including
        ddic_order         = <ls_column>-position
        length             = <ls_column>-leng
        decimals           = <ls_column>-decimals
        datatype           = <ls_column>-datatype
        rollname           = lv_rollname
        ref_field          = <ls_column>-reffield
        ref_tab            = <ls_column>-reftable
        alt_long_text      = ls_altcoltext-alt_long_text
        alt_medium_text    = ls_altcoltext-alt_short_text
        is_virtual_element = xsdbool( line_exists( lt_annotation[ fieldname = <ls_column>-fieldname ] ) )
      ).

      IF io_custom_f4_map IS BOUND.
        ls_tabfield-has_custom_f4 = io_custom_f4_map->entry_exists(
          iv_tabname       = io_cds_view->mv_view_name
          iv_fieldname     = ls_tabfield-fieldname
          iv_rollname      = ls_tabfield-rollname
          is_built_in_type = VALUE #(
            datatype  = ls_tabfield-datatype
            leng      = ls_tabfield-length
          )
        ).
      ENDIF.

*... get additional information from rollname
      IF ls_tabfield-rollname IS NOT INITIAL.
        ls_datel = zcl_sat_ddic_repo_access=>get_dfies_info_for_rollname( ls_tabfield-rollname ).
        lr_addtext_bl->determine_t_flds_for_cds_field(
            iv_cds_view      = io_cds_view->mv_view_name
            iv_cds_field     = <ls_column>-fieldname
            is_tabfield_info = ls_datel
        ).
        ls_tabfield-domname = ls_datel-domname.
        ls_tabfield-is_lowercase = ls_datel-lowercase.
        ls_tabfield-outputlen = ls_datel-outputlen.
        ls_tabfield-f4_available = ls_datel-f4availabl.
        ls_tabfield-std_short_text = ls_datel-scrtext_s.
        ls_tabfield-std_medium_text = ls_datel-scrtext_m.
        ls_tabfield-std_long_text = ls_datel-scrtext_l.
        ls_tabfield-header_text = ls_datel-reptext.
        ls_tabfield-field_ddtext = ls_datel-fieldtext.
        ls_tabfield-convexit = ls_datel-convexit.
        ls_tabfield-inttype = zcl_sat_ddic_repo_access=>get_dtel_inttype( iv_data_element = ls_datel-rollname  ).
      ELSE.
        TRY.
            DATA(lr_typedescr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( |{ <ls_column>-strucobjn }-{ <ls_column>-fieldname }| ) ).
            ls_tabfield-inttype = lr_typedescr->type_kind.
            ls_tabfield-outputlen = lr_typedescr->output_length.

          CATCH cx_sy_move_cast_error.
        ENDTRY.
      ENDIF.

*.... Fill the field label
      IF <ls_column>-fieldlabel IS NOT INITIAL.
        ls_tabfield-field_ddtext = <ls_column>-fieldlabel.
      ELSEIF <ls_column>-ddtext IS NOT INITIAL.
        ls_tabfield-field_ddtext = <ls_column>-ddtext.
      ENDIF.

      ls_tabfield-is_numeric = zcl_dbbr_ddic_util=>is_type_numeric( ls_tabfield-inttype ).
      DATA(lr_new_field) = CAST zdbbr_tabfield_info_ui( ir_tabfield_list->zif_uitb_data_ref_list~add( REF #( ls_tabfield ) ) ).

      IF ls_datel IS NOT INITIAL
        AND lr_addtext_bl->text_exists( VALUE #( tabname = io_cds_view->mv_view_name fieldname = ls_tabfield-fieldname ) ).

        lr_addtext_bl->add_text_fields_to_list(
          ir_tabfields  = ir_tabfield_list
          is_ref_tabfield = ls_tabfield
          iv_position   = ls_tabfield-ddic_order
          is_altcoltext = VALUE #( )
        ).

*...... connect text field and key field
        lr_new_field->has_text_field = abap_true.
      ENDIF.
    ENDLOOP.

*.. add cds view to list of tables
    rs_entity = VALUE zdbbr_entity_info(
      active_selection     = if_selection
      tabname              = io_cds_view->mv_view_name
      tabname_alias        = COND #( WHEN iv_alias IS NOT INITIAL THEN iv_alias ELSE io_cds_view->mv_view_name )
      has_params           = io_cds_view->has_parameters( )
      type                 = zif_sat_c_entity_type=>cds_view
      description          = io_cds_view->get_header( )-description
      fields_are_loaded    = abap_true
      is_primary           = if_is_primary
    ).
    ir_tabfield_list->add_table( rs_entity ).
  ENDMETHOD.
ENDCLASS.
