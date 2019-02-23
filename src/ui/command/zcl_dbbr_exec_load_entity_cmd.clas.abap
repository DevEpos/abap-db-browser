"! <p class="shorttext synchronized" lang="en">Command for loading DB Browser entity</p>
CLASS zcl_dbbr_exec_load_entity_cmd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_command_exec.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor
      IMPORTING
        iv_entity_id    TYPE zdbbr_entity_id
        iv_entity_type  TYPE zdbbr_entity_type
        if_force_reload TYPE abap_bool OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_entity_id TYPE zdbbr_entity_id.
    DATA mf_force_reload TYPE abap_bool.
    DATA mv_entity_type TYPE zdbbr_entity_type.
ENDCLASS.



CLASS zcl_dbbr_exec_load_entity_cmd IMPLEMENTATION.
  METHOD zif_uitb_gui_command_exec~execute.
    zcl_dbbr_selscr_nav_events=>raise_entity_chosen(
        iv_entity_id   = mv_entity_id
        iv_entity_type = mv_entity_type
        if_force_loading = mf_force_reload
    ).
  ENDMETHOD.

  METHOD constructor.
    mv_entity_id = iv_entity_id.
    mv_entity_type = iv_entity_type.
  ENDMETHOD.

ENDCLASS.
