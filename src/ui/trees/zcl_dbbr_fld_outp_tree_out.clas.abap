CLASS ZCL_DBBR_fld_outp_tree_out DEFINITION
  PUBLIC
  INHERITING FROM ZCL_DBBR_field_output_tree
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS create_nodes_from_tabfields REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_fld_outp_tree_out IMPLEMENTATION.
  METHOD create_nodes_from_tabfields.
    super->create_nodes_from_tabfields( iv_dnd_handle = iv_dnd_handle ).
  ENDMETHOD.

ENDCLASS.
