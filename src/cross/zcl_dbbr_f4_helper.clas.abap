CLASS zcl_dbbr_f4_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Retrieve infos of Value help</p>
    "!
    CLASS-METHODS get_f4_infos
      IMPORTING
        !iv_fieldname      TYPE fieldname
        !iv_tablename      TYPE tabname
      RETURNING
        VALUE(rs_f4_infos) TYPE zdbbr_sh_infos .
    "! <p class="shorttext synchronized" lang="en">Get value help information for cds view field</p>
    "!
    CLASS-METHODS get_cds_field_f4_infos
      IMPORTING
        iv_fieldname       TYPE fieldname
        iv_cds_view_name   TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rs_f4_infos) TYPE zdbbr_sh_infos.
    "! <p class="shorttext synchronized" lang="en">Call built in selection field value help</p>
    "!
    CLASS-METHODS call_built_in_selfield_f4
      IMPORTING
        !if_low                  TYPE boolean
        !if_do_not_convert_alpha TYPE boolean OPTIONAL
        !iv_selfield_name        TYPE dynfnam
        !iv_selvalue             TYPE dynfieldvalue
        !iv_tablename            TYPE tabname
        !iv_fieldname            TYPE fieldname
      CHANGING
        !cs_selfields            TYPE zdbbr_selfield .
    "! <p class="shorttext synchronized" lang="en">Call built-in value help with multiple selection</p>
    "!
    CLASS-METHODS call_built_in_f4_multi
      IMPORTING
        !if_low                  TYPE boolean
        !if_do_not_convert_alpha TYPE boolean OPTIONAL
        !iv_tablename            TYPE tabname
        !iv_fieldname            TYPE fieldname
        VALUE(iv_current_line)   LIKE sy-tabix
      CHANGING
        !cs_selfield             TYPE zdbbr_selfield
        !ct_selfield             TYPE zdbbr_selfield_itab .
    "! <p class="shorttext synchronized" lang="en">Call built-in value help</p>
    "!
    CLASS-METHODS call_built_in_f4
      IMPORTING
        !iv_tablename            TYPE tabname
        !iv_fieldname            TYPE fieldname OPTIONAL
        !iv_selfield_name        TYPE dynfnam OPTIONAL
        !iv_repid                TYPE sy-repid OPTIONAL
        !iv_selvalue             TYPE dynfieldvalue OPTIONAL
        !iv_current_line         LIKE sy-tabix OPTIONAL
        !if_determine_dynp_value TYPE boolean OPTIONAL
        !iv_search_help          TYPE shlpname OPTIONAL
      CHANGING
        !cv_value                TYPE zdbbr_value .
    "! <p class="shorttext synchronized" lang="en">Call value help for selection field</p>
    "!
    CLASS-METHODS call_selfield_f4
      IMPORTING
        !if_low           TYPE boolean OPTIONAL
        !iv_repid         TYPE sy-repid
        !iv_selfield_name TYPE dynfnam
        !iv_current_line  LIKE sy-tabix
        !ir_custom_f4_map TYPE REF TO zcl_dbbr_custom_f4_map
      CHANGING
        !cs_selfield      TYPE zdbbr_selfield .
    "! <p class="shorttext synchronized" lang="en">Call value help with internal table</p>
    "!
    CLASS-METHODS call_int_table_f4
      IMPORTING
        !it_table_search      TYPE STANDARD TABLE
        !iv_f4_window_title   TYPE ddtext DEFAULT space
        !iv_return_field_name TYPE dfies-fieldname OPTIONAL
      RETURNING
        VALUE(rv_return)      TYPE shvalue_d .
    "! <p class="shorttext synchronized" lang="en">Call value help for specific ddic value help definition</p>
    "!
    CLASS-METHODS call_f4_with_search_help
      IMPORTING
        !iv_sh_name            TYPE shlpname
        !iv_max_records        TYPE i DEFAULT '500'
        !if_multi_selection    TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_chosen_value) TYPE zdbbr_f4_choice_t .
    "! <p class="shorttext synchronized" lang="en">Call value help for table field</p>
    "!
    CLASS-METHODS call_table_field_f4
      IMPORTING
        !iv_tablename          TYPE tabname OPTIONAL
        !iv_dynpname_tablename TYPE dynfnam OPTIONAL
        !iv_dynpname_fieldname TYPE dynfnam OPTIONAL
        !iv_repid              TYPE sy-repid OPTIONAL
        !iv_current_line       LIKE sy-tabix OPTIONAL
      CHANGING
        !cv_fieldname          TYPE fieldname .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_f4_helper IMPLEMENTATION.

  METHOD get_cds_field_f4_infos.
    " TODO: read cds view field foreignkey and valuehelp association
  ENDMETHOD.

  METHOD get_f4_infos.
    DATA: ls_search_help_info TYPE shlp_descr.

    CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
      EXPORTING
        tabname           = iv_tablename
        fieldname         = iv_fieldname
      IMPORTING
        shlp              = ls_search_help_info
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        OTHERS            = 4.
    IF sy-subrc <> 0 OR ls_search_help_info-intdescr-issimple = abap_false.
      RETURN.
    ENDIF.

    TRY.
*...... convert into internal structure
        DATA(ls_f4_infos) = VALUE zdbbr_sh_infos(
            type            = ls_search_help_info-shlptype
            is_simple       = ls_search_help_info-intdescr-issimple
            key_field       = ls_search_help_info-interface[ valfield = iv_fieldname ]-shlpfield
            sel_method_type = ls_search_help_info-intdescr-selmtype
            sel_method      = ls_search_help_info-intdescr-selmethod
            text_table      = ls_search_help_info-intdescr-texttab
        ).

*...... get unique output field
        IF ls_f4_infos-type = zif_dbbr_global_consts=>gc_searchhelp_types-domain_fix_values.
          ls_f4_infos-unique_text_field = abap_true.
        ELSEIF ls_f4_infos-type = zif_dbbr_global_consts=>gc_searchhelp_types-search_help AND
           ( ls_f4_infos-sel_method_type = zif_dbbr_c_sh_selmethod_type=>table_selection OR
             ls_f4_infos-sel_method_type = zif_dbbr_c_sh_selmethod_type=>view_selection OR
             ls_f4_infos-sel_method_type = zif_dbbr_c_sh_selmethod_type=>with_text_table_selection ).

          DATA(lv_sel_table) = COND #( WHEN ls_f4_infos-text_table IS NOT INITIAL THEN
                                         ls_f4_infos-text_table
                                       ELSE
                                         ls_f4_infos-sel_method ).

          DATA(lt_output_fields) = VALUE zdbbr_sh_output_field_itab(
              FOR field_descr IN ls_search_help_info-fielddescr
              WHERE ( fieldname <> ls_f4_infos-key_field AND
                      tabname    = lv_sel_table )
              ( fieldname = field_descr-fieldname
                rollname  = field_descr-rollname )
          ).
          ls_f4_infos-text_field = lt_output_fields[ 1 ].

          IF lines( lt_output_fields ) = 1.
            ls_f4_infos-unique_text_field = abap_true.
          ENDIF.

*........ determine language field for text tables
          IF ls_f4_infos-sel_method_type = zif_dbbr_c_sh_selmethod_type=>with_text_table_selection.
*.......... special case for language table T002T
            IF ls_f4_infos-text_table = 'T002T'.
              ls_f4_infos-language_field = 'SPRAS'.
              ls_f4_infos-key_field = 'SPRSL'.
            ELSE.
              zcl_dbbr_dictionary_helper=>get_table_field_infos(
                EXPORTING iv_tablename    = ls_f4_infos-text_table
                IMPORTING et_table_fields = DATA(lt_text_table_fields) ).
              TRY.
                  ls_f4_infos-language_field = lt_text_table_fields[ datatype = 'LANG' ]-fieldname.
                CATCH cx_sy_itab_line_not_found.
                  RETURN.
              ENDTRY.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    rs_f4_infos = ls_f4_infos.

  ENDMETHOD.

  METHOD call_built_in_f4.
*&---------------------------------------------------------------------*
*& Description: Calls built-in f4 help for table field
*&---------------------------------------------------------------------*
    DATA: lv_selvalue   TYPE dynfieldvalue,
          lt_return_tab TYPE TABLE OF ddshretval,
          lf_reset      TYPE boolean.

    DATA: BEGIN OF ls_f4_dummy,
            tab      TYPE dbnam,
            minus(1),
            field    TYPE fdnam,
          END OF ls_f4_dummy.


    IF if_determine_dynp_value = abap_true.
      DATA(lr_dynp_field_manager) = NEW zcl_uitb_screen_field_manager( iv_repid = iv_repid ).
      lr_dynp_field_manager->read_single_step_loop_value( EXPORTING iv_fieldname       = iv_selfield_name
                                                                    iv_step_loop_index = iv_current_line
                                                          IMPORTING ev_value           = lv_selvalue ).
    ELSE.
      lv_selvalue = iv_selvalue.
    ENDIF.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = iv_tablename
        fieldname         = iv_fieldname
        searchhelp        = iv_search_help
        value             = lv_selvalue
        selection_screen  = 'X'
      IMPORTING
        user_reset        = lf_reset
      TABLES
        return_tab        = lt_return_tab
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND lf_reset = abap_false.
      ls_f4_dummy = VALUE #(
         tab   = iv_tablename
         minus = '-'
         field = iv_fieldname
      ).
      CONDENSE ls_f4_dummy NO-GAPS.

      ASSIGN lt_return_tab[ retfield = ls_f4_dummy ] TO FIELD-SYMBOL(<ls_return>).
      IF sy-subrc = 0.
        cv_value = <ls_return>-fieldval.
      ELSE.
        " in some cases the search help does not give back the expected
        " retfield -> try to read with fieldname
        ASSIGN lt_return_tab[ fieldname = iv_fieldname ] TO <ls_return>.
        IF sy-subrc = 0.
          cv_value = <ls_return>-fieldval.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD call_built_in_f4_multi.
*&---------------------------------------------------------------------*
*& Description: Call built in f4 method with multi selection option and
*& return the selected value(s)
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF f4_dummy,
             tab      TYPE dbnam,
             minus(1),
             field    TYPE fdnam,
           END OF f4_dummy.

    DATA: lv_tabname    TYPE tabname,
          lv_fieldname  TYPE fieldname,
          lv_selvalue   TYPE dynfieldvalue,
          lt_return_tab TYPE TABLE OF ddshretval,

          ls_f4_dummy   TYPE f4_dummy.

    FIELD-SYMBOLS: <lv_selvalue> TYPE zdbbr_value.

    IF if_low = abap_true.
      ASSIGN cs_selfield-low TO <lv_selvalue>.
    ELSE.
      ASSIGN cs_selfield-high TO <lv_selvalue>.
    ENDIF.


    IF cs_selfield-is_parameter = abap_true.
      lv_tabname = cs_selfield-rollname.
    ELSE.
      lv_tabname = cs_selfield-tabname.
      lv_fieldname = cs_selfield-fieldname.
    ENDIF.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = lv_tabname
        fieldname         = lv_fieldname
        multiple_choice   = abap_true
        selection_screen  = abap_true
      TABLES
        return_tab        = lt_return_tab
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc = 0.
      ls_f4_dummy = VALUE #(
         tab = lv_tabname
         minus = '-'
         field = lv_fieldname
      ).
      CONDENSE ls_f4_dummy NO-GAPS.

      LOOP AT lt_return_tab ASSIGNING FIELD-SYMBOL(<ls_return_value>) WHERE retfield  = ls_f4_dummy
                                                                         OR fieldname = iv_fieldname.

        DATA(lv_tabix) = sy-tabix.
        " perform alpha conversion if necessary
        IF if_do_not_convert_alpha = abap_false AND
           cs_selfield-fieldname <> iv_fieldname.
          zcl_dbbr_data_converter=>perform_alpha_conversion_input( EXPORTING iv_tabname   = lv_tabname
                                                                             iv_fieldname = lv_fieldname
                                                                             iv_value     = <ls_return_value>-fieldval
                                                                   IMPORTING ev_output    = <ls_return_value>-fieldval ).
        ENDIF.
        <lv_selvalue> = <ls_return_value>-fieldval.
        " in case of multi-f4 PBO will be called afterwards, without
        " doing PAI for the input fields --> input will not be
        " converted to internal view
        IF <lv_selvalue> <> space AND
           <lv_selvalue> <> '#'.
          IF cs_selfield-lowercase <> abap_true.
            TRANSLATE cs_selfield-low TO UPPER CASE.     "#EC TRANSLANG
          ENDIF.

          IF cs_selfield-is_parameter = abap_true.
            zcl_dbbr_data_converter=>convert_values_to_int_format(
              EXPORTING iv_rollname  = cs_selfield-rollname
                        iv_type      = cs_selfield-inttype
                        iv_decimals  = CONV #( cs_selfield-decimals )
                        iv_length    = CONV #( cs_selfield-intlen )
              CHANGING  cv_value1    = <lv_selvalue>
            ).
          ELSE.
            zcl_dbbr_data_converter=>convert_selopt_to_int_format(
              EXPORTING iv_tabname   = cs_selfield-tabname
                        iv_fieldname = cs_selfield-fieldname
              CHANGING  cv_value1    = <lv_selvalue>
            ).
          ENDIF.
        ENDIF.

        IF lv_tabix = 1 OR if_low = abap_false.
          MODIFY ct_selfield FROM cs_selfield INDEX iv_current_line.
        ELSE.
          INSERT cs_selfield INTO ct_selfield INDEX iv_current_line.
        ENDIF.
        ADD 1 TO iv_current_line.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD call_built_in_selfield_f4.
*&---------------------------------------------------------------------*
*& Description: Call built in f4 method and return selected value
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lv_selvalue> TYPE zdbbr_value.

    DATA: lv_selvalue   TYPE dynfieldvalue,
          lt_return_tab TYPE TABLE OF ddshretval.

    DATA: BEGIN OF ls_f4_dummy,
            tab      TYPE dbnam,
            minus(1),
            field    TYPE fdnam,
          END OF ls_f4_dummy.

    IF if_low = abap_true.
      ASSIGN cs_selfields-low TO <lv_selvalue>.
      DATA(lv_dynpfield_name) = iv_selfield_name && '-LOW'.
    ELSE.
      ASSIGN cs_selfields-high TO <lv_selvalue>.
      lv_dynpfield_name = iv_selfield_name && '-HIGH'.
    ENDIF.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = iv_tablename
        fieldname         = iv_fieldname
        value             = iv_selvalue
        selection_screen  = 'X'
      TABLES
        return_tab        = lt_return_tab
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc = 0.
      ls_f4_dummy = VALUE #(
         tab   = iv_tablename
         minus = '-'
         field = iv_fieldname
      ).
      CONDENSE ls_f4_dummy NO-GAPS.

      ASSIGN lt_return_tab[ retfield = ls_f4_dummy ] TO FIELD-SYMBOL(<ls_return>).
      IF sy-subrc = 0.
        IF if_do_not_convert_alpha = abap_false AND cs_selfields-fieldname <> iv_fieldname.
          zcl_dbbr_data_converter=>perform_alpha_conversion_input( EXPORTING iv_tabname   = iv_tablename
                                                                              iv_fieldname = iv_fieldname
                                                                              iv_value     = <ls_return>-fieldval
                                                                    IMPORTING ev_output    = <ls_return>-fieldval ).
        ENDIF.
        <lv_selvalue> = <ls_return>-fieldval.
      ELSE.
        " in some cases the search help does not give back the expected
        " retfield -> try to read with fieldname
        ASSIGN lt_return_tab[ fieldname = iv_fieldname ] TO <ls_return>.
        IF sy-subrc = 0.
          IF if_do_not_convert_alpha = abap_false AND cs_selfields-fieldname <> iv_fieldname.
            zcl_dbbr_data_converter=>perform_alpha_conversion_input( EXPORTING iv_tabname   = iv_tablename
                                                                                iv_fieldname = iv_fieldname
                                                                                iv_value     = <ls_return>-fieldval
                                                                      IMPORTING ev_output    = <ls_return>-fieldval ).
          ENDIF.
          <lv_selvalue> = <ls_return>-fieldval.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD call_f4_with_search_help.
    DATA: lv_user_reset TYPE char1.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = space    " Table/structure name from Dictionary
        fieldname         = space    " Field name from Dictionary
        searchhelp        = iv_sh_name
        multiple_choice   = if_multi_selection
*       dynpnr            = '0100'
*       dynpprog          = 'SAPLZUITB_GUI_TEMPLATE'
      IMPORTING
        user_reset        = lv_user_reset
      TABLES
        return_tab        = rt_chosen_value
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD call_int_table_f4.
*&---------------------------------------------------------------------*
*& Description: Calls generic function for displaying a value help
*&---------------------------------------------------------------------*
    DATA: lt_return TYPE TABLE OF ddshretval.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = iv_return_field_name
        window_title    = iv_f4_window_title
        value_org       = 'S'
        display         = 'F'
      TABLES
        value_tab       = it_table_search
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    " Transfer selected value result to dynprofield
    IF sy-subrc = 0 AND NOT lt_return IS INITIAL.
      TRY.
          rv_return = lt_return[ 1 ]-fieldval.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD call_selfield_f4.
    DATA: lv_dynpfield_name TYPE dynfnam,
          lv_selvalue       TYPE dynfieldvalue.

    IF if_low = abap_true.
      lv_dynpfield_name = iv_selfield_name && '-LOW'.
    ELSE.
      lv_dynpfield_name = iv_selfield_name && '-HIGH'.
    ENDIF.

    DATA(lr_dynp_field_manager) = NEW zcl_uitb_screen_field_manager( iv_repid = iv_repid ).
    lr_dynp_field_manager->read_single_step_loop_value( EXPORTING iv_fieldname       = lv_dynpfield_name
                                                                  iv_step_loop_index = iv_current_line
                                                        IMPORTING ev_value           = lv_selvalue ).

    IF cs_selfield-has_cust_f4_help = abap_false.
      DATA: lv_tabname   TYPE tabname,
            lv_fieldname TYPE fieldname.

      IF cs_selfield-is_parameter = abap_true.
        lv_tabname = cs_selfield-rollname.
      ELSE.
        lv_tabname = cs_selfield-tabname.
        lv_fieldname = cs_selfield-fieldname.
      ENDIF.

      zcl_dbbr_f4_helper=>call_built_in_selfield_f4(
        EXPORTING
          if_low                  = if_low
          if_do_not_convert_alpha = abap_true
          iv_selfield_name        = iv_selfield_name
          iv_selvalue             = lv_selvalue
          iv_tablename            = lv_tabname
          iv_fieldname            = lv_fieldname
        CHANGING
          cs_selfields            = cs_selfield
      ).
    ELSE. " Call custom search help
      " 1) read f4 help definition
      ir_custom_f4_map->read_custom_f4_definition(
        EXPORTING
          iv_tablename             = cs_selfield-tabname
          iv_fieldname             = cs_selfield-fieldname
        IMPORTING
          et_custom_f4_definitions = DATA(lt_f4_definition) ).
      IF lt_f4_definition IS INITIAL.
        MESSAGE `Suchhilfe in ZDBBROIN_F4 ist nicht gültig` TYPE 'S'.
        RETURN.
      ENDIF.

      zcl_dbbr_custom_f4_helper=>call_custom_f4(
        EXPORTING
          iv_selfield_name   = iv_selfield_name
          iv_selvalue        = lv_selvalue
          it_f4_definition   = lt_f4_definition
          if_low             = if_low
        CHANGING
          cs_selfield        = cs_selfield
      ).

    ENDIF.
  ENDMETHOD.


  METHOD call_table_field_f4.
*&---------------------------------------------------------------------*
*& Description: Calls value help for fields of db table
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF lty_value_tab,
             fieldname TYPE fieldname,
             key       TYPE keyflag,
             fieldtext TYPE ddtext,
           END OF lty_value_tab.

    DATA: lv_table  TYPE tabname,
          lv_field  TYPE dynfieldvalue,
          lt_values TYPE TABLE OF lty_value_tab,
          lt_return TYPE TABLE OF ddshretval,
          lf_reset  TYPE boolean.

*... Set table value to passed tabname value
    lv_table = iv_tablename.

    IF iv_repid IS NOT INITIAL AND
       ( iv_dynpname_fieldname IS NOT INITIAL OR iv_dynpname_tablename IS NOT INITIAL ).

      DATA(lr_dynpfield_mng) = NEW zcl_uitb_screen_field_manager( iv_repid = iv_repid ).

      DATA(lt_fieldname_selopt) = VALUE zuitb_generic_range_itab( ).
      IF iv_dynpname_fieldname IS NOT INITIAL.
        lt_fieldname_selopt = VALUE #( ( sign = 'I' option = 'EQ' low = iv_dynpname_fieldname ) ).
      ENDIF.
      IF iv_dynpname_tablename IS NOT INITIAL.
        lt_fieldname_selopt = VALUE #( BASE lt_fieldname_selopt ( sign = 'I' option = 'EQ' low = iv_dynpname_tablename ) ).
      ENDIF.

      lr_dynpfield_mng->read_values(
          it_fieldname_selopt = lt_fieldname_selopt
          iv_step_loop_index  = iv_current_line
      ).

      IF iv_dynpname_tablename IS NOT INITIAL.
        lr_dynpfield_mng->get_value( EXPORTING iv_fieldname       = iv_dynpname_tablename
                                               iv_step_loop_index = iv_current_line
                                     IMPORTING ev_value           = lv_table  ).
      ENDIF.
      IF iv_dynpname_fieldname IS NOT INITIAL.
        lr_dynpfield_mng->get_value( EXPORTING iv_fieldname       = iv_dynpname_fieldname
                                               iv_step_loop_index = iv_current_line
                                     IMPORTING ev_value           = lv_field  ).
      ENDIF.
    ENDIF.

*.... no search help without table name
    IF lv_table = space.
      RETURN.
    ENDIF.

    zcl_dbbr_dictionary_helper=>get_table_field_infos(
      EXPORTING iv_tablename    = lv_table
      IMPORTING et_table_fields = DATA(lt_table_fields)
    ).

    lt_values = VALUE #( FOR dfies IN lt_table_fields
                         WHERE ( fieldname <> '.NODE1' )
                         ( fieldname = dfies-fieldname
                           key       = dfies-keyflag
                           fieldtext = dfies-fieldtext ) ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'FIELDNAME'
        value_org       = 'S'
        multiple_choice = space
        value           = lv_field
      IMPORTING
        user_reset      = lf_reset
      TABLES
        value_tab       = lt_values
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF lt_return IS NOT INITIAL AND lf_reset = abap_false.
      cv_fieldname = lt_return[ 1 ]-fieldval.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
