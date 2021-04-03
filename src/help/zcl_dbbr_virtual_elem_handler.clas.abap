CLASS zcl_dbbr_virtual_elem_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter iv_entity_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    METHODS constructor
      IMPORTING
        iv_entity_name TYPE zsat_cds_view_name.

    "! <p class="shorttext synchronized" lang="en">Determine fields needed for virtual element</p>
    "!
    "! @parameter it_fields | <p class="shorttext synchronized" lang="en">Field list</p>
    "! @parameter iv_entity_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter et_requested_elements | <p class="shorttext synchronized" lang="en">List of requested elements</p>
    "! @parameter et_virtual_elements | <p class="shorttext synchronized" lang="en">List of virtual elements</p>
    METHODS determine_relevant_elements
      IMPORTING
        it_fields             TYPE zdbbr_tabfield_info_ui_itab
        iv_entity_name        TYPE zsat_cds_view_name
      EXPORTING
        et_requested_elements TYPE stringtab
        et_virtual_elements   TYPE stringtab.

    "! <p class="shorttext synchronized" lang="en">Determine if virtual element calculation is needed</p>
    "!
    "! @parameter it_fields | <p class="shorttext synchronized" lang="en">Field list</p>
    "! @parameter rf_needs_calculation | <p class="shorttext synchronized" lang="en">Flag if calculation needed</p>
    METHODS needs_calculation
      IMPORTING
        it_fields                   TYPE zdbbr_tabfield_info_ui_itab
      RETURNING
        VALUE(rf_needs_calculation) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Calculate virtual elements</p>
    "!
    "! @parameter ct_data | <p class="shorttext synchronized" lang="en">List of data </p>
    "! @raising zcx_dbbr_application_exc | <p class="shorttext synchronized" lang="en">DB browser exception</p>
    METHODS calculate_elements
      CHANGING
        ct_data TYPE REF TO data
      RAISING
        zcx_dbbr_application_exc.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_t_sorted_string TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY.

    DATA mo_sadl_exit_handler TYPE REF TO object.
    DATA mr_entity_load TYPE REF TO data.

ENDCLASS.



CLASS zcl_dbbr_virtual_elem_handler IMPLEMENTATION.
  METHOD constructor.

    DATA: lv_uuid TYPE char255,
          lr_mdp  TYPE REF TO data.

    TRY.
        CALL METHOD ('CL_SADL_MDP_EXPOSURE')=>('GET_EXPOSURE_LOAD_ID')
          EXPORTING
            iv_entity_type = 'CDS'
            iv_entity_id   = CONV char255( iv_entity_name )
          RECEIVING
            rv_id          = lv_uuid.

        DATA(lo_mdp_type) = cl_abap_refdescr=>get_by_name( 'IF_SADL_METADATA_PROVIDER' ).
        CREATE DATA lr_mdp TYPE HANDLE lo_mdp_type.
        ASSIGN lr_mdp->* TO FIELD-SYMBOL(<lo_mdp>).

        CALL METHOD ('CL_SADL_MDP_FACTORY')=>('GET_MDP_FOR_ID')
          EXPORTING
            iv_sadl_id = lv_uuid
          RECEIVING
            ro_mdp     = <lo_mdp>.

        CALL METHOD ('CL_SADL_MDP_FACTORY')=>('GET_ENTITY_LOAD_BY_ID')
          EXPORTING
            iv_entity_id   = lv_uuid
          RECEIVING
            rr_entity_load = mr_entity_load.

        ASSIGN mr_entity_load->* TO FIELD-SYMBOL(<ls_entity_load>).
        ASSIGN COMPONENT 'ENTITY_ID' OF STRUCTURE <ls_entity_load> TO FIELD-SYMBOL(<lv_entity_id>).
        IF sy-subrc = 0.
          CREATE OBJECT mo_sadl_exit_handler TYPE ('CL_SADL_EXIT_HANDLER')
            EXPORTING
              io_mdp       = <lo_mdp>
              iv_entity_id = CONV char255( <lv_entity_id> ).
        ENDIF.
      CATCH cx_sy_create_object_error
          cx_sy_ref_is_initial
          cx_sy_dyn_call_error.
        " no SADL classes available

      CATCH BEFORE UNWIND cx_root INTO DATA(lx_sadl_error) . "SADL exception raised
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc
          EXPORTING
            previous = lx_sadl_error.

    ENDTRY.
  ENDMETHOD.

  METHOD determine_relevant_elements.

    DATA: lo_exit_class         TYPE REF TO object,
          lt_sort_exit_class    TYPE stringtab,
          lt_requested_elements TYPE ty_t_sorted_string.

    FIELD-SYMBOLS <lt_exits> TYPE SORTED TABLE.

    DATA(lt_calc_elements) = VALUE ty_t_sorted_string(
      FOR ls_field IN it_fields WHERE ( is_text_field = abap_false ) ( CONV #( ls_field-fieldname ) ) ).

    ASSIGN mr_entity_load->* TO FIELD-SYMBOL(<ls_entity_load>).
    ASSIGN COMPONENT 'ELEMENT_EXITS' OF STRUCTURE <ls_entity_load> TO <lt_exits>.

*    DATA(lv_where) = |EXIT_TYPE = { if_sadl_load=>cs_element_exit_type-calculation }|.

    LOOP AT <lt_exits> ASSIGNING FIELD-SYMBOL(<ls_exit>)." WHERE (lv_where).
      ASSIGN COMPONENT 'EXIT_TYPE' OF STRUCTURE <ls_exit> TO FIELD-SYMBOL(<lv_exit_type>).
      IF sy-subrc = 0.
        IF <lv_exit_type> = if_sadl_load=>cs_element_exit_type-calculation.
          ASSIGN COMPONENT 'EXIT_CLASS_NAME' OF STRUCTURE <ls_exit> TO FIELD-SYMBOL(<lv_exit_class>).
          IF sy-subrc = 0.
            lt_sort_exit_class = VALUE #( BASE lt_sort_exit_class ( <lv_exit_class> ) ).
          ENDIF.
          ASSIGN COMPONENT 'ELEMENT' OF STRUCTURE <ls_exit> TO FIELD-SYMBOL(<lv_element>).
          IF sy-subrc = 0.
            et_virtual_elements = VALUE #( BASE et_virtual_elements ( <lv_element> ) ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT lt_sort_exit_class.
    DELETE ADJACENT DUPLICATES FROM lt_sort_exit_class.

    LOOP AT lt_sort_exit_class ASSIGNING <lv_exit_class>.

      TRY.
          CREATE OBJECT lo_exit_class TYPE (<lv_exit_class>).

          CALL METHOD lo_exit_class->('IF_SADL_EXIT_CALC_ELEMENT_READ~GET_CALCULATION_INFO')
            EXPORTING
              it_requested_calc_elements = lt_calc_elements
              iv_entity                  = CONV string( iv_entity_name )
            IMPORTING
              et_requested_orig_elements = lt_requested_elements.
        CATCH cx_sy_create_object_error
        cx_sy_ref_is_initial
        cx_sy_dyn_call_error.
      ENDTRY.

*      et_requested_fields = value #( ( lines of lt_requested_fields ) ).
      APPEND LINES OF lt_requested_elements TO et_requested_elements.
    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_elements.

    ASSIGN ct_data->* TO FIELD-SYMBOL(<lt_data>).
    TRY.
        CALL METHOD mo_sadl_exit_handler->('EXECUTE_CALCULATION')
          CHANGING
            ct_data_rows = <lt_data>.

      CATCH cx_sy_create_object_error
            cx_sy_ref_is_initial
            cx_sy_dyn_call_error.
        " no SADL classes available

      CATCH BEFORE UNWIND cx_root INTO DATA(lx_sadl_error) . "SADL exception raised
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc
          EXPORTING
            previous = lx_sadl_error.
    ENDTRY.

  ENDMETHOD.

  METHOD needs_calculation.

    DATA(lt_requested) = VALUE stringtab( FOR ls_field IN it_fields ( CONV #( ls_field-fieldname ) ) ).
    TRY.
        CALL METHOD mo_sadl_exit_handler->('ADJUST_REQUESTED')
          CHANGING
            ct_requested_element = lt_requested.

        CALL METHOD mo_sadl_exit_handler->('NEEDS_CALCULATION')
          RECEIVING
            rv_needs_calculation = rf_needs_calculation.

      CATCH cx_sy_create_object_error
            cx_sy_ref_is_initial
            cx_sy_dyn_call_error.
        " no SADL classes available
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
