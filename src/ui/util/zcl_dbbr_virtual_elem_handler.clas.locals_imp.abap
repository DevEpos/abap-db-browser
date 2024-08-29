*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_sadl_mdp_exposure IMPLEMENTATION.
  METHOD get_exposure_load_id.
    DATA lo_class_descr TYPE REF TO cl_abap_classdescr.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = c_cls-sadl_mpd_exposure-name
                                         RECEIVING  p_descr_ref    = DATA(lo_type_descr)
                                         EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0 AND lo_type_descr->kind = cl_abap_typedescr=>kind_class.
      lo_class_descr ?= lo_type_descr.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_excp_not_found.
    ENDIF.

    DATA(lr_param_entity_id) = create_method_param_data(
                                   io_class_descr = lo_class_descr
                                   iv_method_name = c_cls-sadl_mpd_exposure-meth-get_exposure-name
                                   iv_param_name  = c_cls-sadl_mpd_exposure-meth-get_exposure-param-iv_entity_id ).
    ASSIGN lr_param_entity_id->* TO FIELD-SYMBOL(<lv_param_entity_id>).
    <lv_param_entity_id> = CONV #( iv_entity_name ).

    DATA(lr_param_id) = create_method_param_data(
                            io_class_descr = lo_class_descr
                            iv_method_name = c_cls-sadl_mpd_exposure-meth-get_exposure-name
                            iv_param_name  = c_cls-sadl_mpd_exposure-meth-get_exposure-param-rv_id ).
    ASSIGN lr_param_id->* TO FIELD-SYMBOL(<lv_param_id>).

    CALL METHOD (c_cls-sadl_mpd_exposure-name)=>(c_cls-sadl_mpd_exposure-meth-get_exposure-name)
      EXPORTING iv_entity_type = 'CDS'
                iv_entity_id   = <lv_param_entity_id>
      RECEIVING rv_id          = <lv_param_id>.

    rr_load_id = REF #( <lv_param_id> ).
  ENDMETHOD.

  METHOD create_method_param_data.
    io_class_descr->get_method_parameter_type( EXPORTING  p_method_name       = iv_method_name
                                                          p_parameter_name    = iv_param_name
                                               RECEIVING  p_descr_ref         = DATA(lo_param_entity_id_type)
                                               EXCEPTIONS parameter_not_found = 1
                                                          method_not_found    = 2 ).
    IF sy-subrc = 0.
      CREATE DATA rr_param_data TYPE HANDLE lo_param_entity_id_type.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_param_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_sadl_mdp_factory IMPLEMENTATION.
  METHOD constructor.
    mr_load_id = NEW lcl_sadl_mdp_exposure( )->get_exposure_load_id( iv_entity_name ).
  ENDMETHOD.

  METHOD create_metadata_provider_ref.
    DATA(lo_mdp_type) = cl_abap_refdescr=>get_by_name( 'IF_SADL_METADATA_PROVIDER' ).
    CREATE DATA rr_mdp TYPE HANDLE lo_mdp_type.
  ENDMETHOD.

  METHOD get_mdp_for_id.
    rr_mdp = create_metadata_provider_ref( ).

    ASSIGN mr_load_id->* TO FIELD-SYMBOL(<lv_sadl_id>).
    ASSIGN rr_mdp->* TO FIELD-SYMBOL(<lo_mdp>).

    TRY.
        CALL METHOD (c_cls-sadl_mdp_factory-name)=>(c_cls-sadl_mdp_factory-meth-get_mdp_for_id-name)
          EXPORTING iv_sadl_id = <lv_sadl_id>
          RECEIVING ro_mdp     = <lo_mdp>.
      CATCH cx_sy_dyn_call_error.
        " no SADL classes available
    ENDTRY.
  ENDMETHOD.

  METHOD get_entity_load_by_id.
    ASSIGN mr_load_id->* TO FIELD-SYMBOL(<lv_sadl_id>).
    TRY.
        CALL METHOD (c_cls-sadl_mdp_factory-name)=>(c_cls-sadl_mdp_factory-meth-get_entity_load_by_id-name)
          EXPORTING iv_entity_id   = <lv_sadl_id>
          RECEIVING rr_entity_load = rr_entity_load.
      CATCH cx_sy_dyn_call_error.
        " no SADL classes available
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_sadl_exit_handler IMPLEMENTATION.
  METHOD constructor.
    DATA lr_path_element_targets TYPE REF TO data.
    DATA lr_function_aliases TYPE REF TO data.

    FIELD-SYMBOLS <lt_path_element_targets> TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_function_aliases> TYPE ANY TABLE.

    CREATE DATA lr_path_element_targets TYPE ('CL_SADL_FILTER_PATH_UTILS=>TT_PATH_ELEMENT_TARGETS').
    ASSIGN lr_path_element_targets->* TO <lt_path_element_targets>.

    CREATE DATA lr_function_aliases TYPE ('IF_SADL_TYPES=>TT_SORTED_STRINGS').
    ASSIGN lr_function_aliases->* TO <lt_function_aliases>.

    TRY.
        mv_entity_name = iv_entity_name.
        DATA(lo_sadl_mdp_factory) = NEW lcl_sadl_mdp_factory( iv_entity_name ).
        DATA(lr_entity_load) = lo_sadl_mdp_factory->get_entity_load_by_id( ).
        ASSIGN lr_entity_load->* TO FIELD-SYMBOL(<ls_entity_load>).
        ASSIGN COMPONENT 'ENTITY_ID' OF STRUCTURE <ls_entity_load> TO FIELD-SYMBOL(<lv_entity_id>).
        IF sy-subrc = 0.
          TRY.

              CALL METHOD (c_cls-sadl_exit_handler-name)=>(c_cls-sadl_exit_handler-meth-create-name)
                EXPORTING iv_entity_id                 = <lv_entity_id>
                          it_path_element_targets      = <lt_path_element_targets>
                          it_function_aliases          = <lt_function_aliases>
                          iv_is_query_with_aggregation = abap_false
                RECEIVING ro_handler                   = mo_sadl_exit_handler.

            CATCH cx_sy_dyn_call_error.
              TRY.
                  DATA(lr_mdp) = lo_sadl_mdp_factory->get_mdp_for_id( ).
                  ASSIGN lr_mdp->* TO FIELD-SYMBOL(<lo_mdp>).

                  CREATE OBJECT mo_sadl_exit_handler TYPE (c_cls-sadl_exit_handler-name)
                    EXPORTING io_mdp       = <lo_mdp>
                              iv_entity_id = <lv_entity_id>.
                CATCH cx_sy_create_object_error.
                  " init of sadl classes failed
              ENDTRY.
          ENDTRY.
        ENDIF.
      CATCH cx_sy_dyn_call_error.
    ENDTRY.
  ENDMETHOD.

  METHOD calculate_elements.
    ASSIGN ct_data->* TO FIELD-SYMBOL(<lt_data>).
    TRY.
        CALL METHOD mo_sadl_exit_handler->(c_cls-sadl_exit_handler-meth-execute_calculation-name)
          CHANGING ct_data_rows = <lt_data>.
      CATCH cx_sy_ref_is_initial
            cx_sy_dyn_call_error ##NEEDED.
        " no SADL classes available
      CATCH cx_root INTO DATA(lx_sadl_error). " SADL exception raised " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE e072(zdbbr_exception) WITH mv_entity_name INTO DATA(lv_err_msg) ##NEEDED.
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc.
    ENDTRY.
  ENDMETHOD.

  METHOD adjust_requested.
    TRY.
        CALL METHOD mo_sadl_exit_handler->(c_cls-sadl_exit_handler-meth-adjust_requested-name)
          CHANGING ct_requested_element = ct_requested_element.
      CATCH cx_sy_ref_is_initial
            cx_sy_dyn_call_error ##NEEDED.
        " no SADL classes available
      CATCH cx_root INTO DATA(lx_sadl_error). " SADL exception raised " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE e072(zdbbr_exception) WITH mv_entity_name INTO DATA(lv_err_msg) ##NEEDED.
        RAISE EXCEPTION TYPE zcx_dbbr_application_exc.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
