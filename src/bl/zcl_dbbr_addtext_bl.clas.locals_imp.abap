*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_text_field_reader IMPLEMENTATION.

  METHOD determine_text_fields.
    IF iv_entity_type = zif_sat_c_entity_type=>cds_view.
      NEW lcl_cds_text_field_reader( iv_entity )->determine_text_fields( CHANGING ct_addtext = ct_addtext ).
    ELSE.
      NEW lcl_table_text_field_reader( iv_entity )->determine_text_fields( CHANGING ct_addtext = ct_addtext ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_text_field_reader_base IMPLEMENTATION.

  METHOD constructor.
    mv_entity = iv_entity.
  ENDMETHOD.


  METHOD determine_text_fields.
    DELETE ct_addtext WHERE id_table = mv_entity.

    determine_fields( ).
    determine_f_w_shlp( ).
    determine_f_w_fixed_val_shlp( ).
    determine_f_w_dtel_shlp( ).
    determine_text_table_fields( ).

    " are there any text fields at all?
    IF mt_dtel_with_shlp IS INITIAL AND
        mt_field_with_shlp IS INITIAL AND
        mt_field_with_fix_val_shlp IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      DATA(ls_text_field) = VALUE zdbbr_addtext_data( ).

      " 1) Search help with text table at field
      DATA(lr_field_shlp) = REF #( mt_field_with_shlp[ fieldname = <ls_field>-fieldname ] OPTIONAL ).
      IF lr_field_shlp IS NOT INITIAL.
        ls_text_field-selection_type = zif_dbbr_c_text_selection_type=>text_table.
        ls_text_field-text_table = lr_field_shlp->texttab.
        ls_text_field-key_field = lr_field_shlp->shlpfield.

        fill_text_field_infos( CHANGING cs_text_field = ls_text_field ).
      ENDIF.

      IF ls_text_field-selection_type IS INITIAL.
        " 2) or Search help via domain fixed values
        IF line_exists( mt_field_with_fix_val_shlp[ fieldname = <ls_field>-fieldname ] ).
          ls_text_field-selection_type = zif_dbbr_c_text_selection_type=>domain_value.
          ls_text_field-key_field = <ls_field>-fieldname.
          ls_text_field-id_field_rollname = <ls_field>-rollname.
          ls_text_field-id_field_domname = <ls_field>-domname.
        ENDIF.

      ENDIF.

      IF ls_text_field-selection_type IS INITIAL.

        " 3) Search help at data element?
        "    check table or explicit search help
        DATA(lr_dtel_shlp) = REF #( mt_dtel_with_shlp[ rollname = <ls_field>-rollname ] OPTIONAL ).
        IF lr_dtel_shlp IS NOT INITIAL.

          IF lr_dtel_shlp->entitytab = 'T002'.
            ls_text_field-selection_type = zif_dbbr_c_text_selection_type=>text_table.
            ls_text_field-key_field = 'SPRSL'.
            ls_text_field-text_table = 'T002T'.
            ls_text_field-text_field = 'SPTXT'.
            ls_text_field-language_field = 'SPRAS'.
          ELSEIF lr_dtel_shlp->shlpname IS NOT INITIAL AND
              lr_dtel_shlp->texttab IS NOT INITIAL.
            ls_text_field-selection_type = zif_dbbr_c_text_selection_type=>text_table.
            ls_text_field-text_table = lr_dtel_shlp->texttab.
            ls_text_field-key_field = lr_dtel_shlp->shlpfield.
            fill_text_field_infos( CHANGING cs_text_field = ls_text_field ).
          ELSEIF lr_dtel_shlp->dtel_text_table IS NOT INITIAL.
            ls_text_field-selection_type = zif_dbbr_c_text_selection_type=>text_table.
            ls_text_field-text_table = lr_dtel_shlp->dtel_text_table.
            ls_text_field-key_field = lr_dtel_shlp->dtel_text_table_key.
            fill_text_field_infos( CHANGING cs_text_field = ls_text_field ).
          ENDIF.

        ENDIF.

      ENDIF.

      IF ls_text_field-selection_type IS NOT INITIAL AND
          ls_text_field-key_field IS NOT INITIAL AND
          ( ( ls_text_field-text_field IS NOT INITIAL AND
              ls_text_field-language_field IS NOT INITIAL ) OR
            ls_text_field-selection_type = zif_dbbr_c_text_selection_type=>domain_value ).

        ls_text_field-id_table = mv_entity.
        ls_text_field-id_field = <ls_field>-fieldname.

        ct_addtext = VALUE #( BASE ct_addtext ( ls_text_field ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD determine_f_w_shlp.
    SELECT shlp_field~fieldname,
           shlp_field~shlpname,
           shlp_f_param~shlpfield,
           shlp~texttab
      FROM dd35l AS shlp_field
        INNER JOIN dd30l AS shlp
          ON  shlp_field~shlpname = shlp~shlpname
          AND shlp~as4local = 'A'
        INNER JOIN dd36s AS shlp_f_param
          ON  shlp_field~tabname = shlp_f_param~tabname
          AND shlp_field~fieldname = shlp_f_param~fieldname
          AND shlp_field~fieldname = shlp_f_param~shfield
      WHERE shlp_field~as4local = 'A'
        AND shlp_field~fieldname IS NOT INITIAL
        AND shlp_field~tabname = @mv_entity
        AND shlp~selmtype = @zif_dbbr_c_sh_selmethod_type=>with_text_table_selection
        AND shlp~issimple = @abap_true
      ORDER BY shlp_field~fieldname
      INTO TABLE @DATA(lt_result).

    DELETE ADJACENT DUPLICATES FROM lt_result COMPARING fieldname.
    mt_field_with_shlp = lt_result.
  ENDMETHOD.


  METHOD determine_f_w_fixed_val_shlp.
    SELECT fieldname
      FROM dd03l AS field
      WHERE tabname = @mv_entity
        AND shlporigin = @c_shlporigin-fixed_values
        AND as4local = 'A'
      INTO CORRESPONDING FIELDS OF TABLE @mt_field_with_fix_val_shlp.
  ENDMETHOD.


  METHOD determine_f_w_dtel_shlp.
    DATA(lt_f_w_dtel_shlp) = select_f_w_dtel_shlp( ).

    DELETE ADJACENT DUPLICATES FROM lt_f_w_dtel_shlp COMPARING rollname.
    DELETE lt_f_w_dtel_shlp WHERE texttab IS INITIAL AND
                                  dtel_text_table IS INITIAL.
    mt_dtel_with_shlp = lt_f_w_dtel_shlp.
  ENDMETHOD.


  METHOD determine_text_table_fields.
    DATA: lt_tablename_range TYPE RANGE OF tabname.

    lt_tablename_range = VALUE #(
      ( LINES OF VALUE #( FOR <dtel_shlp> IN mt_dtel_with_shlp
        ( sign = 'I' option = 'EQ' low = <dtel_shlp>-texttab  )
        ( sign = 'I' option = 'EQ' low = <dtel_shlp>-dtel_text_table ) ) )
      ( LINES OF VALUE #( FOR <field_shlp> IN mt_field_with_shlp
        ( sign = 'I' option = 'EQ' low = <field_shlp>-texttab  ) ) ) ).

    DELETE lt_tablename_range WHERE low IS INITIAL.
    IF lt_tablename_range IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_tablename_range BY low.
    DELETE ADJACENT DUPLICATES FROM lt_tablename_range.

    SELECT tabname,
           fieldname,
           rollname,
           datatype
      FROM dd03l
      WHERE tabname IN @lt_tablename_range
        AND datatype <> 'CLNT'
        AND fieldname <> '.INCLUDE'
      INTO CORRESPONDING FIELDS OF TABLE @mt_text_table_field.
  ENDMETHOD.


  METHOD fill_text_field_infos.
    cs_text_field-language_field = VALUE #( mt_text_table_field[ tabname = cs_text_field-text_table
                                                                 datatype = 'LANG' ]-fieldname OPTIONAL ).

    LOOP AT mt_text_table_field ASSIGNING FIELD-SYMBOL(<text_tab_field>) WHERE datatype <> 'LANG'
                                                                           AND tabname = cs_text_field-text_table
                                                                           AND fieldname <> cs_text_field-key_field.
      cs_text_field-text_field = <text_tab_field>-fieldname.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_cds_text_field_reader IMPLEMENTATION.

  METHOD determine_fields.
    SELECT fieldname,
           rollname,
           domname
      FROM dd03nd
      WHERE strucobjn = @mv_entity
      INTO CORRESPONDING FIELDS OF TABLE @mt_fields.
  ENDMETHOD.


  METHOD select_f_w_dtel_shlp.
    SELECT dtel~rollname,
           dtel~shlpname,
           dtel~shlpfield,
           entitytab,
           text_table~tabname AS dtel_text_table,
           text_table~fieldname AS dtel_text_table_key,
           shlp~texttab
      FROM dd03nd AS field
        INNER JOIN dd04l AS dtel
          ON  field~rollname = dtel~rollname
        LEFT OUTER JOIN dd30l AS shlp
          ON  dtel~shlpname = shlp~shlpname
        LEFT OUTER JOIN dd08l AS text_table
          ON  dtel~entitytab = text_table~checktable
          AND text_table~frkart = 'TEXT' " foreign key type
          AND dtel~entitytab <> 'T002' " exclude language table
      WHERE field~strucobjn = @mv_entity
        AND (
                 (     dtel~shlpname IS NOT INITIAL
                   AND shlp~texttab IS NOT INITIAL )
              OR dtel~entitytab IS NOT INITIAL )
      ORDER BY dtel~rollname
      INTO CORRESPONDING FIELDS OF TABLE @rt_result.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_table_text_field_reader IMPLEMENTATION.

  METHOD determine_fields.
    SELECT fieldname,
           rollname,
           domname
      FROM dd03l
      WHERE tabname = @mv_entity
      INTO CORRESPONDING FIELDS OF TABLE @mt_fields.
  ENDMETHOD.


  METHOD select_f_w_dtel_shlp.
    SELECT dtel~rollname,
           dtel~shlpname,
           dtel~shlpfield,
           entitytab,
           text_table~tabname AS dtel_text_table,
           text_table~fieldname AS dtel_text_table_key,
           shlp~texttab
      FROM dd03l AS field
        INNER JOIN dd04l AS dtel
          ON  field~rollname = dtel~rollname
        LEFT OUTER JOIN dd30l AS shlp
          ON  dtel~shlpname = shlp~shlpname
        LEFT OUTER JOIN dd08l AS text_table
          ON  dtel~entitytab = text_table~checktable
          AND text_table~frkart = 'TEXT' " foreign key type
          AND dtel~entitytab <> 'T002' " exclude language table
      WHERE field~tabname = @mv_entity
        AND (
                 (     dtel~shlpname IS NOT INITIAL
                   AND shlp~texttab IS NOT INITIAL )
              OR dtel~entitytab IS NOT INITIAL )
      ORDER BY dtel~rollname
      INTO CORRESPONDING FIELDS OF TABLE @rt_result.
  ENDMETHOD.

ENDCLASS.
