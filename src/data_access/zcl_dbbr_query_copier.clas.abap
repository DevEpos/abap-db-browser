class ZCL_DBBR_query_COPIER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_SRC_query_NAME type ZSAT_QUERY_NAME
      !IV_SRC_query_DESC type DDTEXT
      !IV_SRC_query_ID type ZDBBR_query_ID
      !IV_TRG_query_NAME type ZSAT_QUERY_NAME
      !IV_TRG_query_DESC type DDTEXT
      !IF_TRG_IS_GLOBAL type BOOLEAN
      !IF_COPY_VARIANTS type BOOLEAN .
  methods COPY_query
    returning
      value(RS_NEW_query) type ZDBBR_query_INFO_UI .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_src_query_name TYPE ZSAT_QUERY_NAME .
    DATA mv_src_query_desc TYPE ddtext .
    DATA mv_src_query_id TYPE ZDBBR_query_id .
    DATA mv_trg_query_name TYPE ZSAT_QUERY_NAME .
    DATA mv_trg_query_desc TYPE ddtext .
    DATA mf_trg_is_global TYPE boolean .
    DATA ms_new_query TYPE ZDBBR_query_info_ui .
    DATA mr_query_f TYPE REF TO ZCL_DBBR_query_factory .
    DATA mf_copy_variants TYPE boolean .

    METHODS fill_id_values
      IMPORTING
        !iv_new_query_id TYPE ZDBBR_query_id
      CHANGING
        !cs_query_data   TYPE ZDBBR_query_data .
    METHODS validate_query .
ENDCLASS.



CLASS ZCL_DBBR_query_COPIER IMPLEMENTATION.


  METHOD constructor.
    mv_src_query_name = iv_src_query_name.
    mv_src_query_desc = iv_src_query_desc.
    mv_src_query_id = iv_src_query_id.
    mv_trg_query_name = iv_trg_query_name.
    mv_trg_query_desc = iv_trg_query_desc.
    mf_trg_is_global = if_trg_is_global.
    mf_copy_variants = if_copy_variants.

    mr_query_f = NEW #( ).
  ENDMETHOD.


  METHOD copy_query.
    validate_query( ).

    " delete existing query for target parameters
    DATA(ls_existing_query) = mr_query_f->get_query(
        iv_query_name     = mv_trg_query_name
    ).

    IF ls_existing_query IS NOT INITIAL.
      " delete existing query <- source query will replace anything anyway
      mr_query_f->delete_query_by_id( ls_existing_query-query_id ).
    ENDIF.

    " no error until this point -> copy process can be started
    " 1) read complete source query
    DATA(ls_query_data) = mr_query_f->get_query_by_id(
        iv_query_id              = mv_src_query_id
        if_load_formulas          = abap_true
        if_load_variants          = mf_copy_variants
        if_load_jump_destinations = abap_true
    ).

    " fill target info into new query
    ls_query_data-is_global = mf_trg_is_global.
    ls_query_data-created_date = sy-datum.
    ls_query_data-created_by = sy-uname.
    CLEAR ls_query_data-changed_date.
    ls_query_data-query_name = mv_trg_query_name.
    ls_query_data-description = mv_trg_query_desc.

    DATA(lv_new_query_id) = ZCL_SAT_SYSTEM_HELPER=>create_guid_22( ).

    fill_id_values(
      EXPORTING iv_new_query_id = lv_new_query_id
      CHANGING  cs_query_data   = ls_query_data ).

    mr_query_f->save_query( ls_query_data ).

    MESSAGE s037(ZDBBR_info) WITH mv_src_query_name mv_trg_query_name.

    rs_new_query = CORRESPONDING #( ls_query_data ).
  ENDMETHOD.


  METHOD fill_id_values.
    cs_query_data-query_id = iv_new_query_id.

    ASSIGN cs_query_data-join_def TO FIELD-SYMBOL(<ls_join_def>).
    ASSIGN cs_query_data-variants TO FIELD-SYMBOL(<lt_variants>).
    ASSIGN cs_query_data-jump_fields TO FIELD-SYMBOL(<lt_jump_fields>).

    " 1) clear join definition of ids
    CLEAR: <ls_join_def>-join_id,
           cs_query_data-ref_join_id.

    " 4) clear variants of ids
    LOOP AT <lt_variants> ASSIGNING FIELD-SYMBOL(<ls_variant_data>).
      <ls_variant_data>-entity_id = iv_new_query_id.
      CLEAR: <ls_variant_data>-variant_id.
    ENDLOOP.

    " 6) clear jump fields of ids
    LOOP AT <lt_jump_fields> ASSIGNING FIELD-SYMBOL(<ls_jump_field>).
      <ls_jump_field>-ref_query_id = iv_new_query_id.
      CLEAR <ls_jump_field>-jumpdest_id.
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_query.
    " 1) query cannot be copied itself
    " read the source query
    DATA(ls_source_query) = mr_query_f->get_query_by_id( iv_query_id       = mv_src_query_id
                                                            if_load_completely = abap_false ).

    IF mv_src_query_name = mv_trg_query_name.
        RAISE EXCEPTION TYPE ZCX_DBBR_exception
          EXPORTING
            textid = ZCX_DBBR_exception=>query_self_copy_impossible
            msgv1  = |{ mv_src_query_name }|.
    ENDIF.

    " 2) validate query name
    ZCL_DBBR_query_helper=>check_query_name( iv_query_name = mv_trg_query_name
                                                if_global      = mf_trg_is_global ).

    " 3) query cannot be copied to query that was not created by the user
    " -> determine if query with given target name exists
    DATA(ls_existing_query) = mr_query_f->get_query(
        iv_query_name     = mv_trg_query_name
        if_load_completely = abap_false
    ).

    IF ls_existing_query IS NOT INITIAL.

      IF ls_existing_query-created_by <> sy-uname.
        RAISE EXCEPTION TYPE ZCX_DBBR_exception
          EXPORTING
            textid = ZCX_DBBR_exception=>query_overwrite_not_possible
            msgv1  = |{ mv_trg_query_name }|.
      ENDIF.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
