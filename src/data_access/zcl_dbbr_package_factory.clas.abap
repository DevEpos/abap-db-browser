CLASS zcl_dbbr_package_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_package,
             package TYPE devclass,
             ddtext  TYPE ddtext,
           END OF ty_package.

    TYPES: tt_package TYPE STANDARD TABLE OF ty_package WITH EMPTY KEY.

    CLASS-METHODS get_package
      IMPORTING
        iv_package        TYPE devclass
      RETURNING
        VALUE(rr_package) TYPE REF TO if_package
      RAISING
        zcx_dbbr_data_read_error.
    CLASS-METHODS find_packages
      IMPORTING
        iv_package    TYPE devclass
      RETURNING
        VALUE(result) TYPE tt_package.
    CLASS-METHODS get_packages_by_appl_hierarchy
      RETURNING
        VALUE(result) TYPE REF TO zif_uitb_list.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_package_factory IMPLEMENTATION.

  METHOD get_package.
    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package
*        i_force_reload             =     " Force System to Read from Database (X=Yes)
      IMPORTING
        e_package                  = rr_package    " Package Instance
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6
    ).
    IF sy-subrc <> 0.
      zcx_dbbr_data_read_error=>raise_data_read_error_sy( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_packages_by_appl_hierarchy.
    TYPES: BEGIN OF ty_s_app_comp,
             node_key TYPE snodetext-id,
             ref      TYPE REF TO zcl_dbbr_application_component,
           END OF ty_s_app_comp.

    DATA: lt_nodes              TYPE STANDARD TABLE OF snodetext,
          ls_last_top_hierarchy TYPE snodetext,
          lt_app_comp           TYPE HASHED TABLE OF ty_s_app_comp WITH UNIQUE KEY node_key,
          lr_app_component      TYPE REF TO zcl_dbbr_application_component.

    DATA(lr_app_comp_list) = CAST zif_uitb_list( NEW zcl_uitb_object_list( ) ).

    CALL FUNCTION 'RS_COMPONENT_VIEW'
      EXPORTING
        object_type = 'HF'
*       refresh     =
*       without_tree_setting =
*       ignore_sfw_switches  = SPACE
*       refresh_all = SPACE
      TABLES
        nodetab     = lt_nodes.

    " build package list from created node table
    LOOP AT lt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>) FROM 2.
      IF <ls_node>-type = 'HF'.
        " top level components go directly into the result list
        IF <ls_node>-tlevel = 2.
          lr_app_component = NEW zcl_dbbr_application_component(
              iv_app_component      = CONV #( <ls_node>-name )
              iv_app_component_text = CONV #( <ls_node>-text )
          ).
          lr_app_comp_list->add( lr_app_component ).
        ELSE.
          " determine parent and add component to that parent
          DATA(lr_parent_comp) = lt_app_comp[ node_key = <ls_node>-parent ]-ref.
          lr_app_component = lr_parent_comp->add_application_component(
            iv_app_component      = CONV #( <ls_node>-name )
            iv_app_component_text = CONV #( <ls_node>-text )
          ).
        ENDIF.
        " store app component for later assignment
        lt_app_comp = VALUE #( BASE lt_app_comp ( VALUE #( node_key = <ls_node>-id ref = lr_app_component ) ) ).
      ELSEIF <ls_node>-type = 'DEVC'.
        lr_app_component->add_package(
            iv_package_name = <ls_node>-name
            iv_package_text = CONV #( <ls_node>-text )
        ).
      ENDIF.
    ENDLOOP.

    result = lr_app_comp_list.
  ENDMETHOD.

  METHOD find_packages.
    DATA: lt_package_range TYPE RANGE OF devclass.

    lt_package_range = VALUE #( ( sign = 'I' option = 'CP' low = iv_package ) ).

    DATA(lv_descr_language) = zcl_dbbr_system_helper=>get_system_language( ).

    SELECT package~devclass AS package, text~ctext AS ddtext
      FROM tdevc AS package
        LEFT OUTER JOIN tdevct AS text
          ON package~devclass = text~devclass
         AND text~spras = @lv_descr_language
      WHERE package~devclass IN @lt_package_range
      ORDER BY package~devclass
    into corresponding fields of table @RESULT
      UP TO 50 ROWS.
  ENDMETHOD.

ENDCLASS.
