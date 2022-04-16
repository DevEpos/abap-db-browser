CLASS zcl_dbbr_addtext_bl DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_dbbr_addtext_bl .

    "! <p class="shorttext synchronized" lang="en">Check if there is a text field for the given field</p>
    METHODS text_exists
      IMPORTING
        !is_data_element_info TYPE dfies
      RETURNING
        VALUE(rf_exists)      TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Retrieve text fields</p>
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
    "! <p class="shorttext synchronized" lang="en">Determines text fields for table (new)</p>
    METHODS determine_text_fields
      IMPORTING
        iv_entity type zsat_entity_id
        iv_entity_type type zsat_entity_type.

    "! <p class="shorttext synchronized" lang="en">Determines manually created text fields for table</p>
    METHODS determine_manual_text_fields
      IMPORTING
        iv_entity TYPE tabname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_text_tab_map,
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

    "! <p class="shorttext synchronized" lang="en">Create text from manual data</p>
    METHODS create_addtext_from_manual
      IMPORTING
        is_manual_addtext TYPE zdbbr_addtext.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor.
    "! <p class="shorttext synchronized" lang="en">Delete all existing fields for the given key</p>
    METHODS delete_existing_for_key.
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


  METHOD determine_text_fields.
    lcl_text_field_reader=>determine_text_fields(
      EXPORTING
        iv_entity     = iv_entity
        iv_entity_type = iv_entity_type
      CHANGING
        ct_addtext     = mt_addtext_info ).
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
      WHERE id_table = iv_tablename
        AND id_field = iv_fieldname ).
  ENDMETHOD.


  METHOD text_exists.
    rf_exists = xsdbool( line_exists( mt_addtext_info[ KEY key_for_source id_table = is_data_element_info-tabname
                                                                          id_field = is_data_element_info-fieldname ] ) ).
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
