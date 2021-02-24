CLASS zcl_dbbr_addtext_bl DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_dbbr_addtext_bl .

    "! <p class="shorttext synchronized" lang="en">Check if there is a text field for the given field</p>
    "!
    METHODS text_exists
      IMPORTING
        !is_data_element_info TYPE dfies
      RETURNING
        VALUE(rf_exists)      TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Retrieve text fields</p>
    "!
    METHODS get_text_fields
      IMPORTING
        !iv_tablename       TYPE tabname
        !iv_fieldname       TYPE fieldname
      RETURNING
        VALUE(rt_text_info) TYPE zdbbr_addtext_data_itab .
    METHODS add_text_fields_to_list
      IMPORTING
        ir_tabfields    TYPE REF TO zcl_dbbr_tabfield_list
        is_ref_tabfield TYPE zdbbr_tabfield_info_ui
        if_post_select  TYPE abap_bool OPTIONAL
        iv_position     TYPE tabfdpos
        is_altcoltext   TYPE zdbbr_altcoltext_data .
    "! <p class="shorttext synchronized" lang="en">Determine Text fields for cds view</p>
    "!
    METHODS determine_t_flds_for_cds_field
      IMPORTING
        iv_cds_view      TYPE zsat_cds_view_name
        iv_cds_field     TYPE fieldname
        is_tabfield_info TYPE dfies .
    "! <p class="shorttext synchronized" lang="en">Determine text fields for table</p>
    "!
    METHODS determine_t_fields_for_tab
      IMPORTING
        is_tabfield_info TYPE dfies OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Determines manually created text fields for table</p>
    METHODS determine_manual_text_fields
      IMPORTING
        iv_entity TYPE tabname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_text_tab_map,
             checktable    TYPE tabname,
             no_text_table TYPE abap_bool,
             texttable     TYPE tabname,
             keyfield      TYPE fieldname,
             textfield     TYPE fieldname,
             sprasfield    TYPE fieldname,
           END OF ty_text_tab_map.

    DATA mt_text_table_map TYPE HASHED TABLE OF ty_text_tab_map WITH UNIQUE KEY checktable.
    DATA mt_addtext_info TYPE zdbbr_addtext_data_itab.
    DATA mv_id_table TYPE tabname.
    DATA mv_id_field TYPE fieldname.
    DATA ms_dtel_info TYPE dfies.
    DATA mr_addtext_factory TYPE REF TO zcl_dbbr_addtext_factory.
    CLASS-DATA sr_instance TYPE REF TO zcl_dbbr_addtext_bl.

    "! <p class="shorttext synchronized" lang="en">Create text from f4 data</p>
    "!
    METHODS create_addtext_from_f4_data
      IMPORTING
        is_f4_infos TYPE zdbbr_sh_infos.
    "! <p class="shorttext synchronized" lang="en">Create text from manual data</p>
    "!
    METHODS create_addtext_from_manual
      IMPORTING
        is_manual_addtext TYPE zdbbr_addtext.
    "! <p class="shorttext synchronized" lang="en">Determine text fields via text table</p>
    "!
    METHODS determine_textfield_via_txttab.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.
    "! <p class="shorttext synchronized" lang="en">Delete all existing fields for the given key</p>
    "!
    METHODS delete_existing_for_key.
    "! <p class="shorttext synchronized" lang="en">Determine Text fields</p>
    "!
    METHODS determine_text_fields.
ENDCLASS.



CLASS zcl_dbbr_addtext_bl IMPLEMENTATION.

  METHOD add_text_fields_to_list.

    LOOP AT mt_addtext_info ASSIGNING FIELD-SYMBOL(<ls_text_field>) USING KEY key_for_source WHERE id_table = is_ref_tabfield-tabname
                                                                                               AND id_field = is_ref_tabfield-fieldname.

      IF ( <ls_text_field>-text_field IS NOT INITIAL
           OR <ls_text_field>-selection_type = zif_dbbr_c_text_selection_type=>domain_value ).

        DATA(ls_text_tabfield) = VALUE zdbbr_tabfield_info_ui(
          tabname                = is_ref_tabfield-tabname
          tabname_alias          = is_ref_tabfield-tabname_alias
          fieldname              = is_ref_tabfield-fieldname
          fieldname_raw          = is_ref_tabfield-fieldname_raw
          rollname               = is_ref_tabfield-rollname
          domname                = is_ref_tabfield-domname
          is_key                 = abap_false
          is_foreign_key         = abap_false
          field_ddtext           = is_ref_tabfield-field_ddtext
          ddic_order             = iv_position " text field gets the same ddic position for the moment
          selection_active       = abap_false " text fields are not selectedable
          output_active          = abap_false
          f4_available           = abap_false
          is_numeric             = abap_false
          is_text_field          = abap_true
          is_virtual_join_field  = if_post_select
          std_short_text         = is_ref_tabfield-std_short_text
          std_medium_text        = is_ref_tabfield-std_medium_text
          std_long_text          = is_ref_tabfield-std_long_text
          alt_medium_text        = is_altcoltext-alt_short_text
          alt_long_text          = is_altcoltext-alt_long_text
        ).
        ir_tabfields->add( REF #( ls_text_tabfield ) ).
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD constructor.
    mr_addtext_factory = NEW #( ).
  ENDMETHOD.


  METHOD create_addtext_from_f4_data.
*.. add field to global list of possible additional text columns
    CASE is_f4_infos-type.
      WHEN zif_dbbr_c_global=>c_searchhelp_types-domain_fix_values.
      WHEN zif_dbbr_c_global=>c_searchhelp_types-search_help.
        IF is_f4_infos-unique_text_field = abap_false.
          RETURN.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    DATA(ls_add_text) = VALUE zdbbr_addtext_data( ).

*.. determine selection type
    CASE is_f4_infos-type.

      WHEN zif_dbbr_c_global=>c_searchhelp_types-search_help.

        CASE is_f4_infos-sel_method_type.

          WHEN zif_dbbr_c_sh_selmethod_type=>table_selection OR
               zif_dbbr_c_sh_selmethod_type=>view_selection.
            ls_add_text-selection_type = zif_dbbr_c_text_selection_type=>table.
            ls_add_text-text_table = is_f4_infos-sel_method.
            ls_add_text-key_field = is_f4_infos-key_field.

          WHEN zif_dbbr_c_sh_selmethod_type=>with_text_table_selection.
            ls_add_text-selection_type = zif_dbbr_c_text_selection_type=>text_table.
            ls_add_text-text_table = is_f4_infos-text_table.
            ls_add_text-key_field = is_f4_infos-key_field.
            ls_add_text-language_field = is_f4_infos-language_field.

          WHEN OTHERS.
            RETURN.
        ENDCASE.

*...... fill name of internal text field
        ls_add_text-text_field = is_f4_infos-text_field.

      WHEN zif_dbbr_c_global=>c_searchhelp_types-domain_fix_values.
        ls_add_text-selection_type = zif_dbbr_c_text_selection_type=>domain_value.
        ls_add_text-key_field = ms_dtel_info-fieldname.
        ls_add_text-id_field_rollname = ms_dtel_info-rollname.
        ls_add_text-id_field_domname = ms_dtel_info-domname.
    ENDCASE.

    ls_add_text-id_field = mv_id_field.
    ls_add_text-id_table = mv_id_table.

    APPEND ls_add_text TO mt_addtext_info.

  ENDMETHOD.


  METHOD create_addtext_from_manual.
    DATA(ls_manual_addtext) = CORRESPONDING zdbbr_addtext_data( is_manual_addtext ).
    ls_manual_addtext-is_manual = abap_true.
    ls_manual_addtext-selection_type = zif_dbbr_c_text_selection_type=>text_table.

    APPEND ls_manual_addtext TO mt_addtext_info.
  ENDMETHOD.


  METHOD delete_existing_for_key.
    DELETE mt_addtext_info WHERE id_table = mv_id_table
                             AND id_field = mv_id_field.
  ENDMETHOD.


  METHOD determine_textfield_via_txttab.
    DATA: lv_checktable TYPE tabname.

    lv_checktable = ms_dtel_info-checktable.

    IF lv_checktable = '*' OR lv_checktable IS INITIAL.
      " retrieve possible checktable from data element
      SELECT SINGLE entitytab FROM dd04l INTO @lv_checktable WHERE rollname = @ms_dtel_info-rollname.
    ENDIF.

    IF lv_checktable IS INITIAL.
      RETURN. " no checktable -> no text table
    ENDIF.

    DATA(ls_text_table) = VALUE #( mt_text_table_map[ checktable = lv_checktable ] OPTIONAL ).
    IF ls_text_table IS INITIAL.

      " check if there is a defined text table
      zcl_sat_ddic_repo_access=>get_text_table(
        EXPORTING
          iv_tabname        = lv_checktable
        IMPORTING
          ev_text_table     = DATA(lv_text_tab)
          ev_text_key_field = DATA(lv_text_key_field)
      ).

      IF lv_text_tab IS NOT INITIAL AND
         lv_text_key_field IS NOT INITIAL.

        " retrieve the first non-key field from the text table
        zcl_sat_ddic_repo_access=>get_table_field_infos(
          EXPORTING iv_tablename    = lv_text_tab
          IMPORTING et_table_fields = DATA(lt_text_table_fields)
        ).

        DATA(lv_language_field) = VALUE #( lt_text_table_fields[ datatype = 'LANGU' ]-fieldname OPTIONAL ).

        DELETE lt_text_table_fields WHERE keyflag = abap_true
                                       OR datatype = 'CLNT'
                                       OR fieldname = lv_text_key_field.
        DATA(lv_first_text_field) = VALUE #( lt_text_table_fields[ 1 ]-fieldname OPTIONAL ).

        IF lv_first_text_field IS NOT INITIAL AND
           lv_language_field IS NOT INITIAL.

          ls_text_table = VALUE #(
            checktable    = lv_checktable
            keyfield      = lv_text_key_field
            sprasfield    = lv_language_field
            textfield     = lv_first_text_field
            texttable     = lv_text_tab
          ).

          INSERT ls_text_table INTO TABLE mt_text_table_map.
        ELSE.
          INSERT VALUE ty_text_tab_map(
              checktable    = lv_checktable
              no_text_table = abap_true
          ) INTO TABLE mt_text_table_map.
        ENDIF.

      ELSE.
        INSERT VALUE ty_text_tab_map(
            checktable    = lv_checktable
            no_text_table = abap_true
        ) INTO TABLE mt_text_table_map.
      ENDIF.

    ENDIF.

    IF ls_text_table IS NOT INITIAL AND
       ls_text_table-no_text_table = abap_false.

      create_addtext_from_f4_data(
        VALUE zdbbr_sh_infos(
          type              = zif_dbbr_c_global=>c_searchhelp_types-search_help
          is_simple         = abap_true
          sel_method_type   = zif_dbbr_c_sh_selmethod_type=>with_text_table_selection
          sel_method        = zif_dbbr_c_text_selection_type=>text_table
          text_table        = ls_text_table-texttable
          key_field         = ls_text_table-keyfield
          language_field    = ls_text_table-sprasfield
          text_field        = ls_text_table-textfield
          unique_text_field = abap_true
        )
      ).
    ENDIF.

  ENDMETHOD.

  METHOD determine_t_flds_for_cds_field.
*& Description: Starting point for determining optional text field
*&---------------------------------------------------------------------*
*& Description: Determines text field for given cds view field
*& by evaluating any defined elementary f4 helps, through defined checktables
*& or value tabs at domains
*&---------------------------------------------------------------------*

    IF is_tabfield_info IS NOT INITIAL.
      ms_dtel_info = is_tabfield_info.
      mv_id_table = iv_cds_view.
      mv_id_field = iv_cds_field.
    ELSE.
      RETURN.
    ENDIF.

    determine_text_fields( ).
  ENDMETHOD.

  METHOD determine_t_fields_for_tab.
*& Description: Starting point for determining optional text field
*&---------------------------------------------------------------------*
*& Description: Determines text field for given table field
*& by evaluating any defined elementary f4 helps, through defined checktables
*& or value tabs at domains
*&---------------------------------------------------------------------*
    DATA: lf_f4_exists TYPE boolean.

    IF is_tabfield_info IS NOT INITIAL.
      ms_dtel_info = is_tabfield_info.
      mv_id_table = is_tabfield_info-tabname.
      mv_id_field = is_tabfield_info-fieldname.
    ELSE.
      RETURN.
    ENDIF.

    determine_text_fields( ).

  ENDMETHOD.


  METHOD get_instance.
    IF sr_instance IS INITIAL.
      sr_instance = NEW #( ).
    ENDIF.

    rr_instance = sr_instance.
  ENDMETHOD.


  METHOD get_text_fields.
    rt_text_info = FILTER #(
      mt_addtext_info USING KEY key_for_source
      WHERE id_table = CONV #( iv_tablename )
        AND id_field = iv_fieldname
    ).
  ENDMETHOD.


  METHOD text_exists.
    rf_exists = xsdbool( line_exists( mt_addtext_info[ KEY key_for_source id_table = is_data_element_info-tabname
                                                                          id_field = is_data_element_info-fieldname ] ) ).
  ENDMETHOD.

  METHOD determine_text_fields.

*.. delete any existing text fields for table/fieldname combination
    delete_existing_for_key( ).

    " 1) try getting text field through f4 information defined at the data element
    IF ms_dtel_info-f4availabl = abap_true.
      DATA(ls_f4_infos) = zcl_dbbr_f4_helper=>get_f4_infos(
          iv_fieldname = ms_dtel_info-fieldname
          iv_tablename = ms_dtel_info-tabname
      ).

      IF ls_f4_infos IS NOT INITIAL.
        create_addtext_from_f4_data( ls_f4_infos ).
      ELSE.
        determine_textfield_via_txttab( ).
      ENDIF.
    ELSE.
      determine_textfield_via_txttab( ).
    ENDIF.

  ENDMETHOD.


  METHOD determine_manual_text_fields.
    DELETE mt_addtext_info WHERE addtext_id IS NOT INITIAL
                             AND id_table = iv_entity.

    NEW zcl_dbbr_addtext_factory( )->find_add_texts( EXPORTING iv_id_table  = iv_entity
                                                     IMPORTING et_add_texts = DATA(lt_addtext) ).

    LOOP AT lt_addtext ASSIGNING FIELD-SYMBOL(<ls_addtext_db>).
      APPEND CORRESPONDING #( <ls_addtext_db> ) TO mt_addtext_info ASSIGNING FIELD-SYMBOL(<ls_addtext_info>).
      <ls_addtext_info>-selection_type = zif_dbbr_c_text_selection_type=>text_table.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
