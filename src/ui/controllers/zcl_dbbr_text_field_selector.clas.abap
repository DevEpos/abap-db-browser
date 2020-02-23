"! <p class="shorttext synchronized" lang="en">Selection dialog for choosing text fields for columns</p>
CLASS zcl_dbbr_text_field_selector DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_selection_dialog
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter io_tabfields | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_fieldcat | <p class="shorttext synchronized" lang="en"></p>
    METHODS constructor
      IMPORTING
        io_tabfields TYPE REF TO zcl_dbbr_tabfield_list
        it_fieldcat  TYPE lvc_t_fcat.
    "! <p class="shorttext synchronized" lang="en">Returns selected/deselected text fields of the dialog</p>
    "!
    "! @parameter rt_text_fields | <p class="shorttext synchronized" lang="en">Text fields with visibility status</p>
    METHODS get_text_fields
      RETURNING
        VALUE(rt_text_fields) TYPE zif_dbbr_ty_global=>ty_t_text_field.
  PROTECTED SECTION.
    METHODS get_output_table
        REDEFINITION.

    METHODS adjust_column
        REDEFINITION.
    METHODS matches_filter
        REDEFINITION.
    METHODS has_selections
        REDEFINITION.
    METHODS get_mark_field_description
        REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_field_with_text,
        mark          TYPE abap_bool,
        visible       TYPE abap_bool,
        filtered      TYPE abap_bool,
        type_icon     TYPE char40,
        description   TYPE ddtext,
        alv_fieldname TYPE fieldname,
        tabname       TYPE tabname,
        fieldname     TYPE fieldname,
      END OF ty_s_field_with_text.
    DATA mt_text_fields TYPE STANDARD TABLE OF ty_s_field_with_text WITH EMPTY KEY.
    DATA mo_tabfields TYPE REF TO zcl_dbbr_tabfield_list.
    DATA mt_fieldcat TYPE lvc_t_fcat.
ENDCLASS.



CLASS zcl_dbbr_text_field_selector IMPLEMENTATION.


  METHOD adjust_column.
    CASE io_column->get_name( ).

      WHEN 'TYPE_ICON'.
        io_column->set_icon( ).
        io_column->set_output_length( 5 ).
        io_column->set_descriptions( iv_long = 'Type' ).

      WHEN 'ALV_FIELDNAME'.
        io_column->set_optimized( ).

      WHEN 'FIELDNAME'.
        io_column->set_descriptions( iv_long =  |{ 'Ref. Field' }| ).
        io_column->set_optimized( ).

      WHEN 'TABNAME'.
        io_column->set_descriptions( iv_long = |{ 'Ref. Table' }| ).
        io_column->set_optimized( ).

      WHEN 'DESCRIPTION'.
        io_column->set_descriptions( iv_long = |{ 'Description' }| ).
        io_column->set_optimized( ).

      WHEN 'VISIBLE'.
        io_column->set_technical( ).

      WHEN OTHERS.
        io_column->set_optimized( ).
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
        iv_title          = |{ 'Available Text Fields'(001) }|
        if_multi_select   = abap_true
        if_use_alv_filter = abap_true
    ).
    mo_tabfields = io_tabfields.
    mt_fieldcat = it_fieldcat.
  ENDMETHOD.


  METHOD get_output_table.
    rr_table = REF #( mt_text_fields ).
  ENDMETHOD.


  METHOD get_text_fields.
*.. Determine text fields
    DATA(lr_t_fields) = mo_tabfields->get_fields_ref( ).
    LOOP AT lr_t_fields->* ASSIGNING FIELD-SYMBOL(<ls_text_field>) WHERE is_text_field = abap_true.

      DATA(lt_addtexts) = zcl_dbbr_addtext_bl=>get_instance( )->get_text_fields( iv_tablename = <ls_text_field>-tabname
                                                                                 iv_fieldname = <ls_text_field>-fieldname ).
      SORT lt_addtexts BY selection_type.
      DELETE ADJACENT DUPLICATES FROM lt_addtexts COMPARING selection_type.

      CHECK lt_addtexts IS NOT INITIAL.

      APPEND INITIAL LINE TO mt_text_fields ASSIGNING FIELD-SYMBOL(<ls_new_text_field>).
      <ls_new_text_field>-alv_fieldname = <ls_text_field>-alv_fieldname.
      <ls_new_text_field>-description = <ls_text_field>-field_ddtext.
      <ls_new_text_field>-tabname = <ls_text_field>-tabname.
      <ls_new_text_field>-fieldname = <ls_text_field>-fieldname.
      ASSIGN mt_fieldcat[ fieldname = <ls_new_text_field>-alv_fieldname ] TO FIELD-SYMBOL(<ls_fieldcat>).
      IF sy-subrc = 0.
        <ls_text_field>-output_active = xsdbool( <ls_fieldcat>-no_out = abap_false ).
      ENDIF.
      <ls_new_text_field>-mark = <ls_text_field>-output_active.
      <ls_new_text_field>-visible = <ls_text_field>-output_active.

      IF lines( lt_addtexts ) = 1.
        DATA(ls_addtext) = lt_addtexts[ 1 ].
        IF ls_addtext-selection_type = zif_dbbr_c_text_selection_type=>domain_value.
          <ls_new_text_field>-type_icon = |@{ icon_catalog+1(2) }\\QDomain { <ls_text_field>-domname }@|.
        ELSE.
          <ls_new_text_field>-type_icon = |@{ icon_text_ina+1(2) }\\QText Table { ls_addtext-text_table }@|.
        ENDIF.
      ELSE.
        <ls_new_text_field>-type_icon = |@{ icon_display_more+1(2) }\\QSeveral Text fields@|.
      ENDIF.
    ENDLOOP.

*.. Do not show dialog if no text fields exist
    IF sy-subrc <> 0.
      MESSAGE |No Text Fields found in the current Selection| TYPE 'S'.
      RETURN.
    ENDIF.

    show(
        iv_top    = 4
        iv_left   = 20
        iv_width  = 120
        iv_height = 15
    ).

    LOOP AT mt_text_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      CHECK <ls_field>-mark <> <ls_field>-visible.
      lr_t_fields->*[ KEY alv_fieldname alv_fieldname = <ls_field>-alv_fieldname ]-output_active = <ls_field>-mark.
      rt_text_fields = VALUE #( BASE rt_text_fields ( alv_fieldname = <ls_field>-alv_fieldname visible = <ls_field>-mark ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD matches_filter.
    FIELD-SYMBOLS: <ls_text_field> TYPE ty_s_field_with_text.

    ASSIGN is_data TO <ls_text_field>.
    CHECK sy-subrc = 0.

    IF <ls_text_field>-alv_fieldname CS iv_filter OR
       <ls_text_field>-description CS iv_filter OR
       <ls_text_field>-tabname CS iv_filter OR
       <ls_text_field>-fieldname CS iv_filter.
      rf_matches = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD has_selections.
    rf_has_selections = abap_true.
  ENDMETHOD.

  METHOD get_mark_field_description.
    ev_short =
    ev_medium =
    ev_long = |{ 'Show?'(002) }|.
  ENDMETHOD.
ENDCLASS.
