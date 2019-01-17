CLASS zcl_dbbr_cds_dep_analyzer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Anaylyzes dependencies of View and returns tree</p>
    "!
    CLASS-METHODS analyze_dependency
      IMPORTING
        iv_cds_view_name           TYPE zdbbr_cds_view_name
      RETURNING
        VALUE(rs_dependency_graph) TYPE cl_ddls_dependency_visitor=>ty_s_dependency_graph_node.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_cds_dep_analyzer IMPLEMENTATION.

  METHOD analyze_dependency.
    DATA(lr_dependency_visitor) = NEW cl_ddls_dependency_visitor( ).
    lr_dependency_visitor->compute_dependency_information( to_upper( iv_cds_view_name ) ).
    rs_dependency_graph = lr_dependency_visitor->get_dependency_graph( ).
  ENDMETHOD.

ENDCLASS.
