CLASS zcl_dbbr_ob_fav_manager DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_modal_dialog
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
  PRIVATE SECTION.
    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model.
ENDCLASS.



CLASS zcl_dbbr_ob_fav_manager IMPLEMENTATION.
  METHOD create_content.

  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.

  ENDMETHOD.

ENDCLASS.
