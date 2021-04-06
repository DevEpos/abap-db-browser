CLASS zcl_dbbr_virtual_elem_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Determine fields needed for virtual element</p>
    "!
    "! @parameter it_fields | <p class="shorttext synchronized" lang="en">Field list</p>
    "! @parameter io_cds_view | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter et_requested_elements | <p class="shorttext synchronized" lang="en">List of requested elements</p>
    "! @parameter et_virtual_elements | <p class="shorttext synchronized" lang="en">List of virtual elements</p>
    METHODS determine_relevant_elements
      IMPORTING
        io_cds_view           TYPE REF TO zcl_sat_cds_view
        it_fields             TYPE zdbbr_tabfield_info_ui_itab
      EXPORTING
        et_requested_elements TYPE fieldname_tab "stringtab
        et_virtual_elements   TYPE fieldname_tab. "stringtab.

    "! <p class="shorttext synchronized" lang="en">Determine if virtual element calculation is needed</p>
    "!
    "! @parameter iv_entity_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    "! @parameter it_fields | <p class="shorttext synchronized" lang="en">Field list</p>
    METHODS adjust_requested
      IMPORTING
        iv_entity_name TYPE zsat_cds_view_name
        it_fields      TYPE fieldname_tab
      RAISING
        zcx_dbbr_application_exc.

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
    DATA mr_entity_load       TYPE REF TO data.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter iv_entity_name | <p class="shorttext synchronized" lang="en">CDS view name</p>
    METHODS instantiate_sadl_exit_handler
      IMPORTING
        iv_entity_name TYPE zsat_cds_view_name
      RAISING
        zcx_dbbr_application_exc.

ENDCLASS.



CLASS zcl_dbbr_virtual_elem_handler IMPLEMENTATION.

  METHOD instantiate_sadl_exit_handler.
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

    DATA: lt_exit_class         TYPE TABLE OF classname,
          lo_exit_class         TYPE REF TO object,
          lt_requested_elements TYPE ty_t_sorted_string.

    DATA(lt_annotation) = io_cds_view->get_annotations(
      it_annotation_name = VALUE #( ( sign = 'I' option = 'CP' low = '*VIRTUALELEMENTCALCULATEDBY*' ) ) ).

    LOOP AT lt_annotation ASSIGNING FIELD-SYMBOL(<ls_annotation>).
      IF line_exists( it_fields[ fieldname = <ls_annotation>-fieldname ] ).
        et_virtual_elements = VALUE #( BASE et_virtual_elements ( <ls_annotation>-fieldname ) ).
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

          CALL METHOD lo_exit_class->('IF_SADL_EXIT_CALC_ELEMENT_READ~GET_CALCULATION_INFO')
            EXPORTING
              it_requested_calc_elements = lt_calc_elements
              iv_entity                  = CONV string( io_cds_view->mv_view_name )
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

    CHECK mo_sadl_exit_handler IS NOT INITIAL.

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

  METHOD adjust_requested.

    instantiate_sadl_exit_handler( iv_entity_name ).

    DATA(lt_requested) = VALUE stringtab( FOR ls_field IN it_fields ( CONV #( ls_field ) ) ).
    TRY.
        CALL METHOD mo_sadl_exit_handler->('ADJUST_REQUESTED')
          CHANGING
            ct_requested_element = lt_requested.

      CATCH cx_sy_create_object_error
            cx_sy_ref_is_initial
            cx_sy_dyn_call_error.
        " no SADL classes available
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
