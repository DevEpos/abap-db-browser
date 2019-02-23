"! <p class="shorttext synchronized" lang="en">Starting DB Browser with variant</p>
CLASS zcl_dbbr_variant_starter DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "! @parameter IV_VARIANT_ID | <p class="shorttext synchronized" lang="en"></p>
    METHODS constructor
      IMPORTING
        !iv_variant_id TYPE zdbbr_variant_id .
  PROTECTED SECTION.

    DATA mv_variant_id TYPE zdbbr_variant_id .
    DATA ms_global_data TYPE zdbbr_global_data .
    DATA mo_tabfield_list TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mo_tabfield_list_grouped TYPE REF TO zcl_dbbr_tabfield_list .
    DATA mt_selfields TYPE zdbbr_selfield_itab .
    DATA mt_selfields_multi TYPE zdbbr_selfield_itab .
    DATA mt_selfields_or TYPE zdbbr_or_seltab_itab .
    DATA mt_table_to_alias_map TYPE zdbbr_table_to_alias_map_itab .
    DATA mo_variant_f TYPE REF TO zcl_dbbr_variant_factory .
    DATA ms_variant TYPE zdbbr_variant_data .
    "! Factory for alternative column texts
    DATA mo_altcoltext_f TYPE REF TO zcl_dbbr_altcoltext_factory .

    "! <p class="shorttext synchronized" lang="en">Create table field</p>
    "!
    METHODS create_table_field
      IMPORTING
        is_entity              TYPE zdbbr_entity_info
        is_field               TYPE dfies
        if_first_non_key_field TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Create Table fields</p>
    "!
    METHODS create_cds_fields
      IMPORTING
        is_entity_info        TYPE zdbbr_entity_info
      RETURNING
        VALUE(rs_entity_info) TYPE zdbbr_entity_info .
    "! <p class="shorttext synchronized" lang="en">Create table fields</p>
    "!
    METHODS create_table_fields
      IMPORTING
        is_entity_info TYPE zdbbr_entity_info.
    "! <p class="shorttext synchronized" lang="en">Fills data from variant</p>
    "!
    METHODS fill_data_from_variant
      RAISING
        zcx_dbbr_variant_error.
    "! <p class="shorttext synchronized" lang="en">Fill selection screen with tuple data</p>
    "!
    METHODS fill_selscreen_with_tupledata
      IMPORTING
        !it_variant_data TYPE zdbbr_vardata_itab .
    "! <p class="shorttext synchronized" lang="en">Fill selection screen with normal variant data</p>
    "!
    METHODS fill_selscreen_with_vardata
      IMPORTING
        !it_variant_data TYPE zdbbr_vardata_itab .
    "! <p class="shorttext synchronized" lang="en">Fill selection screen table</p>
    "!
    METHODS fill_primary_entity
        ABSTRACT.
    "! <p class="shorttext synchronized" lang="en">Retrieve table field references for execution</p>
    "!
    METHODS get_tabfields
      EXPORTING
        !er_tabfields     TYPE REF TO zcl_dbbr_tabfield_list
        !er_tabfields_all TYPE REF TO zcl_dbbr_tabfield_list .
    "! <p class="shorttext synchronized" lang="en">Loads the variant</p>
    "!
    METHODS load_variant .
    "! <p class="shorttext synchronized" lang="en">Shows progress text during execution</p>
    "!
    METHODS show_start_progress_text .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_variant_starter IMPLEMENTATION.


  METHOD constructor.
    mv_variant_id = iv_variant_id.
    mo_tabfield_list = NEW #( ).
    mo_tabfield_list_grouped = NEW #( ).
    mo_variant_f = NEW #( ).
    mo_altcoltext_f = NEW #( ).
  ENDMETHOD.


  METHOD create_table_field.
    DATA(lr_addtext_bl) = zcl_dbbr_addtext_bl=>get_instance( ).

    DATA(ls_altcoltext) = mo_altcoltext_f->find_alternative_text(
        iv_tabname   = is_field-tabname
        iv_fieldname = is_field-fieldname ).

    DATA(ls_tabfield) = VALUE zdbbr_tabfield_info_ui(
      fieldname_raw         = is_field-fieldname
      selection_active      = is_entity-active_selection
      output_active         = xsdbool( is_entity-no_output = abap_false )
      ddic_order            = is_field-position
      is_lowercase          = is_field-lowercase
*... check if field is numeric
      is_numeric            = zcl_dbbr_dictionary_helper=>is_type_numeric( is_field-inttype )
*... is there an F4-help for this table field
      f4_available          = is_field-f4availabl
      field_ddtext          = COND #( WHEN is_field-scrtext_l IS INITIAL THEN
                                  is_field-fieldtext
                                ELSE
                                  is_field-scrtext_l )
      is_key                = is_field-keyflag
*... default sign is inclusive
      default_sign          = zif_dbbr_global_consts=>gc_options-i
      is_virtual_join_field = is_entity-virtual_join_table
      is_foreign_key        = xsdbool( is_field-checktable IS NOT INITIAL )
      std_short_text        = is_field-scrtext_s
      std_medium_text       = is_field-scrtext_m
      std_long_text         = is_field-scrtext_l
      alt_long_text         = ls_altcoltext-alt_long_text
      alt_medium_text       = ls_altcoltext-alt_short_text
      length                = is_field-leng
      header_text           = is_field-reptext
      ref_field             = is_field-reffield
      ref_tab               = is_field-reftable
    ).

    ls_tabfield = CORRESPONDING #( BASE ( ls_tabfield ) is_field ).

    lr_addtext_bl->determine_t_fields_for_tab( is_tabfield_info = is_field ).

    DATA(lr_new_field) = CAST zdbbr_tabfield_info_ui( mo_tabfield_list->add( REF #( ls_tabfield ) ) ).

    " only continue if normal field did not already exist
    IF lr_new_field IS NOT INITIAL.
      " there is an existing text field for the current table field
      IF lr_addtext_bl->text_exists( is_data_element_info = is_field ).

        lr_addtext_bl->add_text_fields_to_list(
          ir_tabfields    = mo_tabfield_list
          is_ref_tabfield = ls_tabfield
          if_post_select  = is_entity-virtual_join_table
          iv_position     = is_field-position
          is_altcoltext   = ls_altcoltext
        ).

        " connect text field and key field
        lr_new_field->has_text_field = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_cds_fields.
    TRY.
        DATA(lo_cds_view) = zcl_dbbr_cds_view_factory=>read_cds_view( iv_cds_view = is_entity_info-tabname ).

*...... create parameters
        zcl_dbbr_cds_tabfield_util=>add_parameters(
            ir_tabfield_list = mo_tabfield_list
            it_parameters    = lo_cds_view->get_parameters( )
        ).
*...... create table fields for cds view
        DATA(ls_header) = lo_cds_view->get_header( ).

        rs_entity_info = zcl_dbbr_cds_tabfield_util=>add_view_colums(
            ir_tabfield_list = mo_tabfield_list
            it_columns       = lo_cds_view->get_columns( )
            iv_name          = is_entity_info-tabname
            iv_alias         = is_entity_info-tabname_alias
            iv_raw_name      = ls_header-entityname_raw
            iv_description   = ls_header-description
            if_is_primary    = is_entity_info-is_primary
        ).
      CATCH zcx_dbbr_data_read_error.
        "handle exception
    ENDTRY.
  ENDMETHOD.

  METHOD create_table_fields.
    DATA(ls_table_info) = zcl_dbbr_dictionary_helper=>get_table_info( is_entity_info-tabname ).
    CHECK ls_table_info IS NOT INITIAL.

    zcl_dbbr_dictionary_helper=>get_table_field_infos( EXPORTING iv_tablename    = is_entity_info-tabname
                                                       IMPORTING et_table_fields = DATA(lt_dfies) ).

    " build tablefield table
    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_data_element_field>) WHERE datatype <> 'CLNT'.
      create_table_field(
          is_field  = <ls_data_element_field>
          is_entity = is_entity_info
      ).
    ENDLOOP.

    mo_tabfield_list->add_table(
      VALUE zdbbr_entity_info(
        active_selection     = is_entity_info-active_selection
        tabname              = is_entity_info-tabname
        tabname_alias        = is_entity_info-tabname_alias
        index                = is_entity_info-selection_order
        selection_order      = is_entity_info-selection_order
        description          = ls_table_info-ddtext
        fields_are_loaded    = abap_true
        is_primary           = is_entity_info-is_primary
      )
    ).

  ENDMETHOD.


  METHOD fill_data_from_variant.

    DATA: ls_alv_variant TYPE disvariant.

    CHECK mv_variant_id <> zif_dbbr_global_consts=>c_dummy_variant.

    " if variant holds output fields clear all active formulas
    SORT ms_variant-variant_data BY tuple_id tabname fieldname counter.

    " check if variant stores grouping information
    LOOP AT ms_variant-variant_data ASSIGNING FIELD-SYMBOL(<ls_vard>) WHERE data_type = zif_dbbr_global_consts=>gc_variant_datatypes-group_by OR
                                                                            data_type = zif_dbbr_global_consts=>gc_variant_datatypes-aggregation.
      DATA(lf_grouping_is_active) = abap_true.
      EXIT.
    ENDLOOP.

    " fill selection screen with variant data
    LOOP AT ms_variant-variant_data ASSIGNING FIELD-SYMBOL(<ls_vardata>)
      GROUP BY ( tuple = <ls_vardata>-tuple_id )
      ASSIGNING FIELD-SYMBOL(<ls_vardata_tuple>).

      IF <ls_vardata_tuple>-tuple IS NOT INITIAL.
        fill_selscreen_with_tupledata( VALUE #( FOR data IN GROUP <ls_vardata_tuple> ( data ) ) ).
      ELSE.
        fill_selscreen_with_vardata( VALUE #( FOR data IN GROUP <ls_vardata_tuple> ( data ) ) ).
      ENDIF.
    ENDLOOP.

    IF lf_grouping_is_active = abap_false AND
       ( ms_variant-has_output_fields = abap_true OR
         ms_variant-has_sort_fields = abap_true ).
      mo_tabfield_list->clear_active_flag(
        if_clear_output = ms_variant-has_output_fields
        if_clear_sort   = ms_variant-has_sort_fields
      ).
    ENDIF.


    " fill output and sorting fields from variant
    IF ms_variant-has_output_fields = abap_true OR ms_variant-has_sort_fields = abap_true.

      LOOP AT ms_variant-fields ASSIGNING FIELD-SYMBOL(<ls_tabfield>).

        TRY .
            DATA(lr_field_stru) = mo_tabfield_list->get_field_ref(
               iv_tabname_alias           = <ls_tabfield>-tabname
               iv_fieldname         = <ls_tabfield>-fieldname
               if_is_text_field     = <ls_tabfield>-is_text_field
            ).
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.

        IF lf_grouping_is_active = abap_true.
          DATA(ls_field) = lr_field_stru->*.
          ASSIGN ls_field TO FIELD-SYMBOL(<ls_field>).

        ELSE.
          ASSIGN lr_field_stru->* TO <ls_field>.
        ENDIF.

        IF ms_variant-has_output_fields = abap_true.
          <ls_field>-output_active = <ls_tabfield>-output_active.
          <ls_field>-output_order = <ls_tabfield>-output_order.
          <ls_field>-is_text_field = <ls_tabfield>-is_text_field.
        ENDIF.

        IF ms_variant-has_sort_fields = abap_true.
          <ls_field>-sort_active = <ls_tabfield>-sort_active.
          <ls_field>-sort_direction = <ls_tabfield>-sort_direction.
          <ls_field>-sort_order = <ls_tabfield>-sort_order.
        ENDIF.

        IF lf_grouping_is_active = abap_true.
          mo_tabfield_list_grouped->append_tabfield_info( <ls_field> ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD fill_selscreen_with_tupledata.

    DATA(ls_multi_or) = VALUE zdbbr_or_seltab(
        pos    = it_variant_data[ 1 ]-tuple_id
    ).

    LOOP AT it_variant_data ASSIGNING FIELD-SYMBOL(<ls_vardata>)
         GROUP BY ( tablename = <ls_vardata>-tabname
                    fieldname = <ls_vardata>-fieldname )
         ASSIGNING FIELD-SYMBOL(<ls_vardata_group>).

      DATA(lf_first_iteration) = abap_true.

      LOOP AT GROUP <ls_vardata_group> ASSIGNING FIELD-SYMBOL(<ls_vardata_group_entry>).
        IF lf_first_iteration = abap_true.
          DATA(ls_selfield_info) = VALUE zdbbr_selfield_info(
              tabname   = <ls_vardata_group_entry>-tabname
              fieldname = <ls_vardata_group_entry>-fieldname
              sign      = <ls_vardata_group_entry>-sign_val
              option    = <ls_vardata_group_entry>-data_type
              low       = <ls_vardata_group_entry>-low_val
              high      = <ls_vardata_group_entry>-high_val
          ).
          CLEAR lf_first_iteration.
        ELSE.
          ls_selfield_info-multi_values = VALUE #(
            BASE ls_selfield_info-multi_values
            ( sign   = <ls_vardata_group_entry>-sign_val
               option = <ls_vardata_group_entry>-data_type
               low    = <ls_vardata_group_entry>-low_val
               high   = <ls_vardata_group_entry>-high_val )
          ).
        ENDIF.
      ENDLOOP.

      ls_multi_or-values = VALUE #( BASE ls_multi_or-values ( ls_selfield_info ) ).
    ENDLOOP.

    mt_selfields_or = VALUE #( BASE mt_selfields_or ( ls_multi_or ) ).
  ENDMETHOD.


  METHOD fill_selscreen_with_vardata.

    LOOP AT it_variant_data ASSIGNING FIELD-SYMBOL(<ls_vardata>)
      GROUP BY ( tablename = <ls_vardata>-tabname
                 fieldname = <ls_vardata>-fieldname )
      ASSIGNING FIELD-SYMBOL(<ls_vardata_group>).

      DATA(lt_vardata_group) = VALUE zdbbr_vardata_itab( FOR data IN GROUP <ls_vardata_group> ( data ) ).
      SORT lt_vardata_group BY counter.

*.... read correct selection field
      ASSIGN mt_selfields[ tabname   = <ls_vardata_group>-tablename
                           fieldname = <ls_vardata_group>-fieldname ] TO FIELD-SYMBOL(<ls_selfield>).
      IF sy-subrc <> 0.
*...... Get alias name of table
        DATA(lv_tabname_alias) = mo_tabfield_list->get_table_alias_name( <ls_vardata_group>-tablename ).

*...... find the table field to get general information about selfield
        TRY.
            DATA(lr_field_ref) = mo_tabfield_list->get_field_ref(
               iv_tabname_alias = lv_tabname_alias
               iv_fieldname     = <ls_vardata_group>-fieldname
            ).
            APPEND CORRESPONDING #( lr_field_ref->* ) TO mt_selfields ASSIGNING <ls_selfield>.
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.
      ENDIF.

      DATA(lf_first_selvalue) = abap_true.

      LOOP AT lt_vardata_group ASSIGNING FIELD-SYMBOL(<ls_vardata_group_entry>).
        CASE <ls_vardata_group_entry>-data_type.

          WHEN zif_dbbr_global_consts=>gc_variant_datatypes-aggregation.
            <ls_selfield>-aggregation = <ls_vardata_group_entry>-low_val.

          WHEN zif_dbbr_global_consts=>gc_variant_datatypes-group_by.
            <ls_selfield>-group_by = abap_true.
          WHEN OTHERS. " normal selection field
            IF lf_first_selvalue = abap_true.

              IF <ls_vardata_group_entry>-system_value_type IS NOT INITIAL.
                zcl_dbbr_system_helper=>get_system_value( EXPORTING iv_system_value_type = <ls_vardata_group_entry>-system_value_type
                                                           IMPORTING ev_system_value      = <ls_selfield>-low ).
                <ls_selfield>-system_value_type = <ls_vardata_group_entry>-system_value_type.
                <ls_selfield>-sign  = <ls_vardata_group_entry>-sign_val.
                <ls_selfield>-option = <ls_vardata_group_entry>-data_type.
              ELSE.

                <ls_selfield>-sign = <ls_vardata_group_entry>-sign_val.
                <ls_selfield>-option = <ls_vardata_group_entry>-data_type.
                <ls_selfield>-low = <ls_vardata_group_entry>-low_val.
                <ls_selfield>-high = <ls_vardata_group_entry>-high_val.
                lf_first_selvalue = abap_false.
              ENDIF.
            ELSE. " further values will be added to multi selection field table
              DATA(ls_multi_select) = <ls_selfield>.
              ls_multi_select-low = <ls_vardata_group_entry>-low_val.
              ls_multi_select-high = <ls_vardata_group_entry>-high_val.
              ls_multi_select-sign = <ls_vardata_group_entry>-sign_val.
              ls_multi_select-option = <ls_vardata_group_entry>-data_type.
              APPEND ls_multi_select TO mt_selfields_multi.
              " obviously there is more than one value for this selection field
              " so the push optin will be active
              <ls_selfield>-push = abap_true.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_tabfields.
    IF mo_tabfield_list_grouped->zif_uitb_data_ref_list~is_empty( ).
      er_tabfields = mo_tabfield_list.
      er_tabfields_all = mo_tabfield_list.
    ELSE.
      er_tabfields = mo_tabfield_list_grouped.
      er_tabfields_all = mo_tabfield_list.
    ENDIF.
  ENDMETHOD.


  METHOD load_variant.
    IF mv_variant_id = zif_dbbr_global_consts=>c_dummy_variant.
      DATA(lv_tabname) = ms_global_data-primary_table.
    ELSE.
      mo_variant_f->get_variant(
        EXPORTING iv_variant_id = mv_variant_id
        IMPORTING es_variant    = ms_variant
      ).
    ENDIF.

    " read global data from current global data.
    DATA(lr_s_global_data) = CAST zdbbr_global_data( zcl_uitb_data_cache=>get_instance(
      zif_dbbr_c_report_id=>main
    )->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).

    ms_global_data = lr_s_global_data->*.
    ms_global_data-primary_table = lv_tabname.
  ENDMETHOD.


  METHOD show_start_progress_text.
    zcl_dbbr_screen_helper=>show_progress(
      iv_progress = 1
      iv_text     = |{ 'Variant'(001) } { ms_variant-variant_name } { 'is being executed...'(002) }|
    ).
  ENDMETHOD.
ENDCLASS.
