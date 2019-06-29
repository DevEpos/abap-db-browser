"! <p class="shorttext synchronized" lang="en">Selection dialog for custom F4</p>
CLASS zcl_dbbr_custom_f4_selector DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_selection_dialog
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        is_tabfield TYPE zdbbr_tabfield_info_ui.
    "! <p class="shorttext synchronized" lang="en">Assign value helps to the given field</p>
    METHODS assign_value_helps
      RETURNING
        VALUE(rf_updated) TYPE abap_bool.
  PROTECTED SECTION.
    METHODS: get_output_table REDEFINITION,
      matches_filter REDEFINITION,
      set_selected_element REDEFINITION,
      adjust_column REDEFINITION,
      has_selections REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_f4,
        mark         TYPE abap_bool,
        f4_id        TYPE zdbbr_f4_id,
        description  TYPE zdbbr_search_descr,
        search_table TYPE zdbbr_searchtab,
        search_field TYPE zdbbr_search_field,
        is_built_in  TYPE zdbbr_built_in,
        created_by   TYPE zdbbr_created_by,
        filtered     TYPE abap_bool,
      END OF ty_s_f4.
    TYPES: ty_t_f4 TYPE STANDARD TABLE OF ty_s_f4 WITH EMPTY KEY.

    DATA mt_custom_f4 TYPE ty_t_f4.
    DATA ms_tabfield TYPE zdbbr_tabfield_info_ui.
ENDCLASS.



CLASS zcl_dbbr_custom_f4_selector IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
        iv_title          = |{ 'Assign F4 to Field' } { is_tabfield-fieldname }|
        if_multi_select   = abap_true
        if_use_alv_filter = abap_true
    ).
    ms_tabfield = is_tabfield.
  ENDMETHOD.

  METHOD assign_value_helps.
    CLEAR: mt_custom_f4.

*.. Read all possible value helps for the fieldname and datatype of the field
    zcl_dbbr_custom_f4_factory=>find_f4_for_datatype(
      EXPORTING iv_rollname      = ms_tabfield-rollname
                is_built_in_type = VALUE #(
                  datatype = ms_tabfield-datatype
                  inttype  = ms_tabfield-inttype
                )
      IMPORTING et_f4            = DATA(lt_f4)
    ).

    CHECK lt_f4 IS NOT INITIAL.

*.... read assignments for F4
    DATA(lt_f4_assgmt) = zcl_dbbr_custom_f4_factory=>get_f4_assignments( ).

    DATA(lv_mark_count) = 0.
    LOOP AT lt_f4 ASSIGNING FIELD-SYMBOL(<ls_f4_data>).
      DATA(ls_search_field) = <ls_f4_data>-fields[ is_search_key = abap_true ].

      IF ms_tabfield-rollname = ls_search_field-rollname OR
         ( ms_tabfield-datatype = ls_search_field-datatype AND
           ms_tabfield-inttype = ls_search_field-inttype ).

        CHECK ms_tabfield-length >= ls_search_field-leng.
      ENDIF.

      APPEND INITIAL LINE TO mt_custom_f4 ASSIGNING FIELD-SYMBOL(<ls_f4>).
      <ls_f4> = CORRESPONDING #( <ls_f4_data> ).
      <ls_f4>-search_field = ls_search_field-search_field.
      <ls_f4>-search_table = ls_search_field-search_table.
      <ls_f4>-mark = xsdbool( line_exists( lt_f4_assgmt[ ref_f4_id = <ls_f4_data>-f4_id
                                                         fieldname = ms_tabfield-fieldname
                                                         entity_id = ms_tabfield-tabname ] ) ).
      IF <ls_f4>-mark = abap_true.
        ADD 1 TO lv_mark_count.
      ENDIF.
    ENDLOOP.

    IF mt_custom_f4 IS INITIAL.
      MESSAGE |{ 'There a no suitable value helps for this table field' }| TYPE 'S'.
      RETURN.
    ENDIF.

    show(
        iv_top    = 2
        iv_left   = 10
        iv_width  = 105
        iv_height = 17
    ).

    CHECK mf_data_selected = abap_true.

    rf_updated =  zcl_dbbr_custom_f4_factory=>update_f4_assignments(
        it_f4_assignments = VALUE #(
          FOR f4 IN mt_custom_f4
          WHERE ( mark = abap_true )
          ( entity_id  = ms_tabfield-tabname
            fieldname  = ms_tabfield-fieldname
            ref_f4_id  = f4-f4_id )
        )
        it_f4_assgnmt_delete = VALUE #(
          FOR f4 IN mt_custom_f4
          WHERE ( mark = abap_false )
          ( entity_id  = ms_tabfield-tabname
            fieldname  = ms_tabfield-fieldname
            ref_f4_id  = f4-f4_id )
        )
    ).

    CHECK rf_updated = abap_true.

    MESSAGE s100(zdbbr_info) WITH ms_tabfield-tabname ms_tabfield-fieldname.
  ENDMETHOD.

  METHOD adjust_column.

    CASE io_column->get_name( ).

      WHEN 'F4_ID'.
        io_column->set_technical( ).

      WHEN 'DESCRIPTION'.
        io_column->set_optimized( ).
        io_column->set_key( ).
        io_column->set_color( zif_uitb_c_alv_colors=>bluegreen ).

      WHEN 'IS_BUILT_IN'.
        io_column->set_optimized( ).
        io_column->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox ).

      WHEN OTHERS.
        io_column->set_optimized( ).

    ENDCASE.

  ENDMETHOD.

  METHOD matches_filter.

    FIELD-SYMBOLS: <ls_custom_f4> TYPE ty_s_f4.

    ASSIGN is_data TO <ls_custom_f4>.

    DATA(lv_filter) = |*{ to_upper( iv_filter ) }*|.

    IF to_upper( <ls_custom_f4>-description ) CP lv_filter OR
       to_upper( <ls_custom_f4>-search_field ) CP lv_filter OR
       to_upper( <ls_custom_f4>-search_table ) CP lv_filter.
      rf_matches = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_output_table.
    rr_table = REF #( mt_custom_f4 ).
  ENDMETHOD.

  METHOD set_selected_element.
    DATA(lr_s_row) = REF #( mt_custom_f4[ iv_row ] ).
    zcl_uitb_appl_util=>toggle( CHANGING value = lr_s_row->mark ).
  ENDMETHOD.

  METHOD has_selections.
    rf_has_selections = abap_true.
  ENDMETHOD.

ENDCLASS.
