class ZCL_DBBR_JOIN_FACTORY definition
  public
  final
  create public .

public section.

  methods SAVE_JOIN
    importing
      !IS_JOIN_DEF type ZDBBR_JOIN_DEF
    returning
      value(RV_NEW_query_ID) type ZDBBR_query_ID .
  methods READ_JOIN
    importing
      !IV_JOIN_ID type ZDBBR_JOIN_ID
    returning
      value(RS_JOIN) type ZDBBR_JOIN_DEF .
  methods DELETE_JOIN
    importing
      !IV_JOIN_ID type ZDBBR_JOIN_ID .
  methods DELETE_MULTIPLE_JOINS
    importing
      !IT_JOIN_RANGE type ZDBBR_SELOPT_ITAB .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS find_join_by_query_id
      IMPORTING
        !iv_query_id     TYPE zdbbr_query_id
      RETURNING
        VALUE(rv_join_id) TYPE zdbbr_join_id .
ENDCLASS.



CLASS ZCL_DBBR_JOIN_FACTORY IMPLEMENTATION.


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
ENDCLASS.
