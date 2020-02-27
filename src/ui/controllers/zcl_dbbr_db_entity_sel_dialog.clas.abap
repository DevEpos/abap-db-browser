"! <p class="shorttext synchronized" lang="en">Selection Dialog for Database entity</p>
CLASS zcl_dbbr_db_entity_sel_dialog DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_selection_dialog
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_db_entity,
        entity_id   TYPE zsat_entity_id,
        description TYPE ddtext,
        type        TYPE zsat_entity_type,
      END OF ty_s_db_entity,
      ty_t_db_entity TYPE STANDARD TABLE OF ty_s_db_entity WITH EMPTY KEY.
    METHODS constructor
      IMPORTING
        if_multi_select TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Creates dialog for finding and selected db entities</p>
    CLASS-METHODS get_db_entities
      IMPORTING
        if_multi_select  TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_entity) TYPE ty_t_db_entity.
  PROTECTED SECTION.
    METHODS get_output_table
        REDEFINITION.
    METHODS get_filtered_data
        REDEFINITION.
    METHODS adjust_column
        REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_db_entity_int,
        mark        TYPE abap_bool,
        type_icon   TYPE c LENGTH 40,
        entity_id   TYPE zsat_entity_id,
        description TYPE ddtext,
        type        TYPE zsat_entity_type,
        devclass    TYPE devclass,
      END OF ty_s_db_entity_int.

    DATA mt_db_entity TYPE TABLE OF ty_s_db_entity_int.
ENDCLASS.



CLASS zcl_dbbr_db_entity_sel_dialog IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
        iv_title          = 'Choose Database entity'
        iv_filter_prompt  = 'Database Entity...'
        if_multi_select   = if_multi_select
        iv_initial_focus  = c_focus_on_filter
    ).

  ENDMETHOD.

  METHOD get_output_table.
    rr_table = REF #( mt_db_entity ).
  ENDMETHOD.

  METHOD get_db_entities.
    DATA(lo_dialog) = NEW zcl_dbbr_db_entity_sel_dialog( if_multi_select = if_multi_select ).
*.. Determine text fields
    lo_dialog->show(
        iv_top    = 4
        iv_left   = 20
        iv_width  = 120
        iv_height = 20
    ).

    IF lo_dialog->is_multi_select( ).
      rt_entity = VALUE #(
        FOR <ls_entity> IN lo_dialog->mt_db_entity
        WHERE ( mark = abap_true )
        ( CORRESPONDING #( <ls_entity> ) )
      ).
    ELSE.
      rt_entity = VALUE #( ( CORRESPONDING #( lo_dialog->mt_db_entity[ lo_dialog->mv_selected_row ] ) ) ).
    ENDIF.

  ENDMETHOD.

  METHOD adjust_column.
    CASE io_column->get_name( ).

      WHEN 'MARK'.
        io_column->set_technical( xsdbool( NOT is_multi_select(  ) ) ).

      WHEN 'TYPE_ICON'.
        io_column->set_descriptions( iv_long = 'Type' ).
        io_column->set_icon( ).
        io_column->set_output_length( 5 ).

      WHEN 'TYPE'.
        io_column->set_technical( ).

    ENDCASE.
  ENDMETHOD.

  METHOD get_filtered_data.
    DATA: lt_entity_range TYPE RANGE OF tabname,
          lt_filter       TYPE TABLE OF string.

    CHECK iv_name_filter IS NOT INITIAL.

    SPLIT iv_name_filter AT ',' INTO TABLE lt_filter.

    LOOP AT lt_filter INTO DATA(lv_filter).
      lt_entity_range = VALUE #( BASE lt_entity_range ( sign = 'I' option = 'CP' low = |{ to_upper( lv_filter ) }| ) ).
    ENDLOOP.

    SELECT
      FROM zsat_i_databaseentity
      FIELDS entityraw AS entity_id,
             description,
             developmentpackage AS devclass,
             type
      WHERE entity IN @lt_entity_range
    INTO CORRESPONDING FIELDS OF TABLE @mt_db_entity
      UP TO 200 ROWS.

    LOOP AT mt_db_entity ASSIGNING FIELD-SYMBOL(<ls_db_entity>).
      <ls_db_entity>-type_icon = SWITCH #( <ls_db_entity>-type
        WHEN zif_sat_c_entity_type=>cds_view THEN zif_dbbr_c_icon=>cds_view
        WHEN zif_sat_c_entity_type=>view     THEN zif_dbbr_c_icon=>database_view
        WHEN zif_sat_c_entity_type=>table    THEN zif_dbbr_c_icon=>database_table
      ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
