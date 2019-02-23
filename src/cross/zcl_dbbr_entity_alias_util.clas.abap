"! <p class="shorttext synchronized" lang="en">Retrieves free Alias for Join definition</p>
"! Singleton for managing the alias names for tables in a single join definition
CLASS zcl_dbbr_entity_alias_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.


    "! <p class="shorttext synchronized" lang="en">Initialize aliases</p>
    "!
    CLASS-METHODS initialize_aliases
      IMPORTING
        if_alv_alias_only TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Retrieves the next free ALV Alias</p>
    "!
    CLASS-METHODS get_next_free_alv_alias
      RETURNING
        VALUE(rv_alias) TYPE zdbbr_entity_alias_alv.
    "! <p class="shorttext synchronized" lang="en">Creates alias for the entity and registers it</p>
    "!
    CLASS-METHODS create_entity_alias
      IMPORTING
        iv_entity              TYPE zdbbr_entity_id
      RETURNING
        VALUE(rv_entity_alias) TYPE zdbbr_entity_alias.
    "! <p class="shorttext synchronized" lang="en">Add new Alias</p>
    "!
    CLASS-METHODS add_entity_alias
      IMPORTING
        iv_entity_alias TYPE zdbbr_entity_alias
      RETURNING
        VALUE(rf_added) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Unregisters given alias from Alias pool</p>
    "!
    CLASS-METHODS unregister_alv_alias
      IMPORTING
        iv_alias TYPE zdbbr_entity_alias_alv.
    "! <p class="shorttext synchronized" lang="en">Unregister long alias name</p>
    "!
    CLASS-METHODS unregister_alias
      IMPORTING
        iv_alias TYPE zdbbr_entity_alias.
    CLASS-METHODS check_entity_alias
      IMPORTING
        iv_alias TYPE zdbbr_entity_alias.
    "! <p class="shorttext synchronized" lang="en">Add ALV Alias for entity</p>
    "!
    CLASS-METHODS add_entity_alv_alias
      IMPORTING
        iv_alias        TYPE zdbbr_entity_alias_alv
      RETURNING
        VALUE(rf_added) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gt_alv_alias_in_use TYPE STANDARD TABLE OF zdbbr_entity_alias_alv.
    CLASS-DATA gt_alias_in_use TYPE STANDARD TABLE OF zdbbr_entity_alias.
ENDCLASS.



CLASS zcl_dbbr_entity_alias_util IMPLEMENTATION.

  METHOD add_entity_alv_alias.
    CLEAR rf_added.

    IF NOT line_exists( gt_alv_alias_in_use[ table_line = iv_alias ] ).
      gt_alv_alias_in_use = VALUE #( BASE gt_alv_alias_in_use ( iv_alias ) ).
      rf_added = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD add_entity_alias.
    CLEAR rf_added.

    IF NOT line_exists( gt_alias_in_use[ table_line = iv_entity_alias ] ).
      gt_alias_in_use = VALUE #( BASE gt_alias_in_use ( iv_entity_alias ) ).
      rf_added = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_entity_alias.
    CHECK line_exists( gt_alias_in_use[ table_line = to_upper( iv_alias ) ] ).

    zcx_dbbr_validation_exception=>raise_with_text(
        iv_text = |{ 'Alias' } { iv_alias } { 'is already in use!' }|
    ).
    .
  ENDMETHOD.

  METHOD create_entity_alias.
    DATA: lt_slash_match TYPE match_result_tab.

*** As a first step the original entity will be the alias, for a better
*    IF iv_entity CP '/*'.
*      FIND ALL OCCURRENCES OF '/' IN iv_entity RESULTS lt_slash_match.
*      DATA(lv_last_index) = lt_slash_match[ 2 ]-offset + 1.
*      rv_entity_alias = iv_entity+lv_last_index.
*    ELSE.
      rv_entity_alias = iv_entity.
*    ENDIF.

  ENDMETHOD.

  METHOD initialize_aliases.
    CLEAR gt_alv_alias_in_use.

    IF if_alv_alias_only = abap_true.
      CLEAR gt_alias_in_use.
    ENDIF.
  ENDMETHOD.

  METHOD get_next_free_alv_alias.
    DATA(lv_counter) = 1.
    DATA(lf_alias_found) = abap_false.

    WHILE lf_alias_found = abap_false.
      rv_alias = zcl_dbbr_alias_map=>get_alias( lv_counter ).

      IF NOT line_exists( gt_alv_alias_in_use[ table_line = rv_alias ] ).
        gt_alv_alias_in_use = VALUE #( BASE gt_alv_alias_in_use ( rv_alias ) ).
        lf_alias_found = abap_true.
      ELSE.
        lv_counter = lv_counter + 1.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD unregister_alv_alias.
    DELETE gt_alv_alias_in_use WHERE table_line = iv_alias.
  ENDMETHOD.

  METHOD unregister_alias.
    DELETE gt_alias_in_use WHERE table_line = iv_alias.
  ENDMETHOD.


ENDCLASS.
