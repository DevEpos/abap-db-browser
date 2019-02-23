CLASS zcl_dbbr_exec_dep_tree_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_command_exec.
    "! <p class="shorttext synchronized" lang="en">Create command</p>
    "!
    METHODS constructor
      IMPORTING
        iv_entity_id TYPE zdbbr_entity_id.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_entity_id TYPE zdbbr_entity_id.
ENDCLASS.



CLASS zcl_dbbr_exec_dep_tree_command IMPLEMENTATION.
  METHOD constructor.
    mv_entity_id = iv_entity_id.
  ENDMETHOD.

  METHOD zif_uitb_gui_command_exec~execute.
    NEW zcl_dbbr_cds_dependency_tree( mv_entity_id )->show( ).
  ENDMETHOD.

ENDCLASS.
