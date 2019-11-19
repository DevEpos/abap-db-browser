class ZCL_DBBR_TABFIELD_BUILDER definition
  public
  final
  create public .

public section.

  class-methods CREATE_TABFIELDS
    importing
      !IV_TABLENAME type TABNAME
      !IF_IS_PRIMARY type ABAP_BOOL optional
      !IR_TABFIELD_LIST type ref to ZCL_DBBR_TABFIELD_LIST
      !IF_SELECTION_ACTIVE type BOOLEAN optional
      !IF_OUTPUT_ACTIVE type BOOLEAN optional .
  class-methods CREATE_TABFIELDS_FOR_TABS
    importing
      !IT_TABNAME_RANGE type ZDBBR_TABNAME_RANGE_ITAB
    returning
      value(RR_LIST) type ref to ZCL_DBBR_TABFIELD_LIST .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_DBBR_TABFIELD_BUILDER IMPLEMENTATION.


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
                field_ddtext    = scrtext_l
                length          = leng
      ).

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
        type                 = ZIF_SAT_C_ENTITY_TYPE=>table
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

    rr_list->build_complete_fieldnames( ).
  ENDMETHOD.
ENDCLASS.
