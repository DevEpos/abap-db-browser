CLASS zcl_dbbr_join_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS save_join
      IMPORTING
        !is_join_def           TYPE zdbbr_join_def
      RETURNING
        VALUE(rv_new_query_id) TYPE zdbbr_query_id .
    METHODS read_join
      IMPORTING
        !iv_join_id    TYPE zdbbr_join_id
      RETURNING
        VALUE(rs_join) TYPE zdbbr_join_def .
    METHODS delete_join
      IMPORTING
        !iv_join_id TYPE zdbbr_join_id .
    METHODS delete_multiple_joins
      IMPORTING
        !it_join_range TYPE zdbbr_selopt_itab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS find_join_by_query_id
      IMPORTING
        iv_query_id       TYPE zdbbr_query_id
      RETURNING
        VALUE(rv_join_id) TYPE zdbbr_join_id .
    "! <p class="shorttext synchronized" lang="en">Fill missing alias names and entity types</p>
    "!
    METHODS repair_join
      CHANGING
        cs_join TYPE zdbbr_join_def.
ENDCLASS.



CLASS zcl_dbbr_join_factory IMPLEMENTATION.


  METHOD delete_join.
    CHECK iv_join_id IS NOT INITIAL.

    DELETE FROM zdbbr_joinfil WHERE ref_join_id = iv_join_id.
    DELETE FROM zdbbr_joinfld WHERE ref_join_id = iv_join_id.
    DELETE FROM zdbbr_joint WHERE ref_join_id = iv_join_id.
    DELETE FROM zdbbr_joinh WHERE join_id = iv_join_id.

    COMMIT WORK.
  ENDMETHOD.


  METHOD delete_multiple_joins.
    CHECK it_join_range IS NOT INITIAL.

    DELETE FROM zdbbr_joinh WHERE join_id IN it_join_range.
    DELETE FROM zdbbr_joint WHERE ref_join_id IN it_join_range.
    DELETE FROM zdbbr_joinfil WHERE ref_join_id IN it_join_range.
    DELETE FROM zdbbr_joinfld WHERE ref_join_id IN it_join_range.

    COMMIT WORK.
  ENDMETHOD.


  METHOD find_join_by_query_id.
    SELECT SINGLE ref_join_id
      FROM zdbbr_queryh
      WHERE query_id = @iv_query_id
      INTO @rv_join_id.
  ENDMETHOD.


  METHOD read_join.
    SELECT SINGLE *
      FROM zdbbr_joinh
      WHERE join_id = @iv_join_id
      INTO @DATA(ls_join_head).

    CHECK sy-subrc = 0.

    SELECT *
      FROM zdbbr_joint
      WHERE ref_join_id = @iv_join_id
      INTO TABLE @DATA(lt_tables).

    CHECK sy-subrc = 0.

    SELECT *
      FROM zdbbr_joinfil
      WHERE ref_join_id = @iv_join_id
      INTO TABLE @DATA(lt_filter_cond).

    SELECT *
      FROM zdbbr_joinfld
      WHERE ref_join_id = @iv_join_id
      INTO TABLE @DATA(lt_field_cond).

    rs_join = CORRESPONDING #( ls_join_head ).

    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      DATA(ls_join_table) = CORRESPONDING zdbbr_join_table_ui( <ls_table> ).

*.... Find field conditions for this join table
      ls_join_table-field_conditions = VALUE #(
        FOR field IN lt_field_cond
        WHERE ( ref_join_table_id = <ls_table>-join_table_id )
        ( field )
      ).
*.... Find filter conditions for this join table
      ls_join_table-filter_conditions = VALUE #(
        FOR filter IN lt_filter_cond
        WHERE ( ref_join_table_id = <ls_table>-join_table_id )
        ( filter )
      ).

      rs_join-tables = VALUE #( BASE rs_join-tables ( ls_join_table ) ).
    ENDLOOP.

    repair_join( CHANGING cs_join = rs_join ).
  ENDMETHOD.


  METHOD save_join.
    DATA: ls_join_head   TYPE zdbbr_joinh,
          lt_tables      TYPE STANDARD TABLE OF zdbbr_joint,
          lt_field_cond  TYPE STANDARD TABLE OF zdbbr_joinfld,
          lt_filter_cond TYPE STANDARD TABLE OF zdbbr_joinfil.

*.. Now save the join definition to the data base
    DATA(ls_join_def) = is_join_def.

    rv_new_query_id =
    ls_join_def-join_id = zcl_dbbr_system_helper=>create_guid_22( ).
    ls_join_head = CORRESPONDING #( ls_join_def ).

    LOOP AT ls_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      <ls_table>-join_table_id = zcl_dbbr_system_helper=>create_guid_22( ).
      <ls_table>-ref_join_id = ls_join_def-join_id.
      lt_tables = VALUE #( BASE lt_tables ( CORRESPONDING #( <ls_table> ) ) ).

*.... Fill field conditions
      LOOP AT <ls_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
        <ls_field_cond>-join_field_id = zcl_dbbr_system_helper=>create_guid_22( ).
        <ls_field_cond>-ref_join_table_id = <ls_table>-join_table_id.
        <ls_field_cond>-ref_join_id = ls_join_def-join_id.

        lt_field_cond = VALUE #( BASE lt_field_cond ( <ls_field_cond> ) ).
      ENDLOOP.

*.... Fill Filter conditions
      LOOP AT <ls_table>-filter_conditions ASSIGNING FIELD-SYMBOL(<ls_filter_cond>).
        <ls_filter_cond>-join_filter_id = zcl_dbbr_system_helper=>create_guid_22( ).
        <ls_filter_cond>-ref_join_table_id = <ls_table>-join_table_id.
        <ls_filter_cond>-ref_join_id = ls_join_def-join_id.

        lt_filter_cond = VALUE #( BASE lt_filter_cond ( <ls_filter_cond> ) ).
      ENDLOOP.
    ENDLOOP.

*... perform the db inserts
    INSERT zdbbr_joinh FROM ls_join_head.
    INSERT zdbbr_joint FROM TABLE lt_tables.
    INSERT zdbbr_joinfld FROM TABLE lt_field_cond.
    INSERT zdbbr_joinfil FROM TABLE lt_filter_cond.

    COMMIT WORK.
  ENDMETHOD.

  METHOD repair_join.
    CHECK cs_join-primary_table_alias IS INITIAL.

*.. Repair join definition to match new pattern
    zcl_dbbr_entity_alias_util=>initialize_aliases( ).
    IF cs_join-primary_table_alias IS INITIAL.
      cs_join-primary_table_alias = cs_join-primary_table.
*.... if long alias name is not supplied the short will probably empty as well
      cs_join-primary_table_alias_alv = zcl_dbbr_entity_alias_util=>get_next_free_alv_alias( ).

      LOOP AT cs_join-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>).
*...... Consider every entity as a normal table if it is not filled
        IF <ls_join_table>-entity_type IS INITIAL.
          <ls_join_table>-entity_type = zif_dbbr_c_entity_type=>table.
        ENDIF.
        <ls_join_table>-add_table_alias = <ls_join_table>-add_table.
        <ls_join_table>-add_table_alias_alv = zcl_dbbr_entity_alias_util=>get_next_free_alv_alias( ).

        LOOP AT <ls_join_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_field_condition>).
          <ls_field_condition>-ref_table_alias = <ls_field_condition>-ref_table.
        ENDLOOP.

        LOOP AT <ls_join_table>-filter_conditions ASSIGNING FIELD-SYMBOL(<ls_filter_condition>).
          <ls_filter_condition>-tabname_alias = <ls_filter_condition>-tabname.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
