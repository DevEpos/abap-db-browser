CLASS zcl_dbbr_virtual_elem_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Determine fields needed for virtual element calculation</p>
    "!
    "! @parameter it_fields | <p class="shorttext synchronized" lang="en">Field list</p>
    "! @parameter io_cds_view | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter rt_requested_elements | <p class="shorttext synchronized" lang="en">List of requested elements</p>
    METHODS determine_requested_elements
      IMPORTING
        io_cds_view                  TYPE REF TO zcl_sat_cds_view
        it_fields                    TYPE zdbbr_tabfield_info_ui_itab
      RETURNING
        VALUE(rt_requested_elements) TYPE stringtab.

    "! <p class="shorttext synchronized" lang="en">Determine if virtual element calculation is needed</p>
    "!
    "! @parameter iv_entity_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter it_fields | <p class="shorttext synchronized" lang="en">Field list</p>
    "! @raising zcx_dbbr_application_exc | <p class="shorttext synchronized" lang="en">DB browser exception</p>
    METHODS adjust_requested
      IMPORTING
        iv_entity_name TYPE zsat_cds_view_name
        it_fields      TYPE zdbbr_tabfield_info_ui_itab
      RAISING
        zcx_dbbr_application_exc.

    "! <p class="shorttext synchronized" lang="en">Calculate virtual elements</p>
    "!
    "! @parameter iv_entity_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter ct_data | <p class="shorttext synchronized" lang="en">List of data </p>
    "! @raising zcx_dbbr_application_exc | <p class="shorttext synchronized" lang="en">DB browser exception</p>
    METHODS calculate_elements
      IMPORTING
        iv_entity_name TYPE zsat_cds_view_name
      CHANGING
        ct_data        TYPE REF TO data
      RAISING
        zcx_dbbr_application_exc.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_t_sorted_string TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_annotation_objectmodel,
        virtual_elem_calc_by TYPE string VALUE 'OBJECTMODEL.VIRTUALELEMENTCALCULATEDBY',
      END OF c_annotation_objectmodel,
      c_meth_get_calculation_info TYPE string VALUE 'IF_SADL_EXIT_CALC_ELEMENT_READ~GET_CALCULATION_INFO'.

    DATA mo_sadl_exit_handler TYPE REF TO lcl_sadl_exit_handler.

    "! <p class="shorttext synchronized" lang="en">Get sadl exit handler instance</p>
    "!
    "! @parameter iv_entity_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter ro_sadl_exit_handler | <p class="shorttext synchronized" lang="en">Reference to SADL exit handler</p>
    METHODS get_sadl_exit_handler
      IMPORTING
        iv_entity_name              TYPE zsat_cds_view_name
      RETURNING
        VALUE(ro_sadl_exit_handler) TYPE REF TO lcl_sadl_exit_handler.

ENDCLASS.

CLASS zcl_dbbr_virtual_elem_handler IMPLEMENTATION.

  METHOD get_sadl_exit_handler.
    IF mo_sadl_exit_handler IS INITIAL.
      mo_sadl_exit_handler = NEW #( iv_entity_name ).
    ENDIF.
    ro_sadl_exit_handler = mo_sadl_exit_handler.
  ENDMETHOD.

  METHOD determine_requested_elements.

    DATA: lt_exit_class         TYPE TABLE OF classname,
          lo_exit_class         TYPE REF TO object,
          lt_requested_elements TYPE ty_t_sorted_string.

    DATA(lt_annotation) = io_cds_view->get_annotations(
      it_annotation_name = VALUE #( ( sign = 'I' option = 'EQ' low = c_annotation_objectmodel-virtual_elem_calc_by ) ) ).

    LOOP AT lt_annotation ASSIGNING FIELD-SYMBOL(<ls_annotation>).
      IF line_exists( it_fields[ fieldname = <ls_annotation>-fieldname ] ).
        lt_exit_class = VALUE #( BASE lt_exit_class ( CONV #( <ls_annotation>-value+5 ) ) ).
      ENDIF.
    ENDLOOP.

    SORT lt_exit_class.
    DELETE ADJACENT DUPLICATES FROM lt_exit_class.

    DATA(lt_calc_elements) = VALUE ty_t_sorted_string(
      FOR ls_field IN it_fields WHERE ( is_text_field = abap_false ) ( CONV #( ls_field-fieldname ) ) ).

    LOOP AT lt_exit_class ASSIGNING FIELD-SYMBOL(<lv_exit_class>).

      TRY.
          CREATE OBJECT lo_exit_class TYPE (<lv_exit_class>).

          CALL METHOD lo_exit_class->(c_meth_get_calculation_info)
            EXPORTING
              it_requested_calc_elements = lt_calc_elements
              iv_entity                  = CONV string( io_cds_view->mv_view_name )
            IMPORTING
              et_requested_orig_elements = lt_requested_elements.
        CATCH cx_sy_create_object_error
              cx_sy_ref_is_initial
              cx_sy_dyn_call_error.
      ENDTRY.

      rt_requested_elements = VALUE #( BASE rt_requested_elements ( LINES OF lt_requested_elements ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD calculate_elements.

    get_sadl_exit_handler( iv_entity_name )->calculate_elements( CHANGING ct_data = ct_data ).

  ENDMETHOD.

  METHOD adjust_requested.

    DATA(lt_requested) = VALUE stringtab( FOR ls_field IN it_fields ( CONV #( ls_field-fieldname ) ) ).
    get_sadl_exit_handler( iv_entity_name )->adjust_requested( CHANGING ct_requested_element = lt_requested ).

  ENDMETHOD.

ENDCLASS.
