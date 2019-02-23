"! <p class="shorttext synchronized" lang="en">Next Gen Easy-to-Use Query Creator</p>
CLASS zcl_dbbr_query_creator DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_query_creator IMPLEMENTATION.
  METHOD create_content.

  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.

  ENDMETHOD.

  METHOD do_before_dynpro_output.

  ENDMETHOD.

ENDCLASS.
