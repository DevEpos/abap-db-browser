CLASS zcl_dbbr_tabfield_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create_tabfields
      IMPORTING
        !iv_tablename        TYPE tabname
        !if_is_primary       TYPE abap_bool OPTIONAL
        !ir_tabfield_list    TYPE REF TO zcl_dbbr_tabfield_list
        !if_selection_active TYPE boolean OPTIONAL
        !if_output_active    TYPE boolean OPTIONAL .
    CLASS-METHODS create_tabfields_for_tabs
      IMPORTING
        !it_tabname_range TYPE zdbbr_tabname_range_itab
      RETURNING
        VALUE(rr_list)    TYPE REF TO zcl_dbbr_tabfield_list .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_tabfield_builder IMPLEMENTATION.


  METHOD create_tabfields.

    DATA(ls_table_info) = zcl_sat_ddic_repo_access=>get_table_info( iv_tablename ).
    CHECK ls_table_info IS NOT INITIAL.

    zcl_sat_ddic_repo_access=>get_table_field_infos( EXPORTING iv_tablename    = iv_tablename
                                                       IMPORTING et_table_fields = DATA(lt_dfies) ).

    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>) WHERE datatype <> 'CLNT'.

      DATA(ls_tabfield) = CORRESPONDING zdbbr_tabfield_info_ui(
        <ls_dfies>
        MAPPING ddic_order      = position
                std_short_text  = scrtext_s
                std_medium_text = scrtext_m
                std_long_text   = scrtext_l
                header_text     = reptext
                f4_available    = f4availabl
                is_key          = keyflag
                length          = leng
      ).
      ls_tabfield-field_ddtext = cond #( when <ls_dfies>-scrtext_l is initial then
                                            <ls_dfies>-fieldtext
                                         else <ls_dfies>-scrtext_l ).

      ls_tabfield-is_numeric = zcl_dbbr_ddic_util=>is_type_numeric( <ls_dfies>-inttype ).
      ls_tabfield-is_foreign_key = xsdbool( <ls_dfies>-checktable IS NOT INITIAL ).
      ls_tabfield-selection_active = if_selection_active.
      ls_tabfield-output_active = if_output_active.

      ir_tabfield_list->add( REF #( ls_tabfield ) ).
    ENDLOOP.

    ir_tabfield_list->add_table(
      VALUE zdbbr_entity_info(
        active_selection     = abap_true
        tabname              = iv_tablename
        tabname_alias        = iv_tablename
        type                 = zif_sat_c_entity_type=>table
        selection_order      = 1
        description          = ls_table_info-ddtext
        fields_are_loaded    = abap_true
        is_primary           = if_is_primary
      )
    ).
  ENDMETHOD.


  METHOD create_tabfields_for_tabs.
    rr_list = NEW #( ).

*... fill list for each table
    LOOP AT it_tabname_range ASSIGNING FIELD-SYMBOL(<ls_tab>).
      create_tabfields(
          iv_tablename        = <ls_tab>-low
          ir_tabfield_list    = rr_list
*          if_selection_active =
*          if_output_active    =
      ).
    ENDLOOP.

    rr_list->update_tables( ).
    rr_list->update_alias_names( ).
    rr_list->build_complete_fieldnames( ).
  ENDMETHOD.
ENDCLASS.
