*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_sadl_mdp_exposure IMPLEMENTATION.

  METHOD get_exposure_load_id.
    DATA: lr_param_id        TYPE REF TO data,
          lr_param_entity_id TYPE REF TO data.

    TRY.
        DATA(lo_class_descr) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( c_class ) ).

        DATA(lo_param_entity_id_type) = lo_class_descr->get_method_parameter_type(
          p_method_name       = c_methods-get_exposure_load_id
          p_parameter_name    = c_params-iv_entity_id ).
        CREATE DATA lr_param_entity_id TYPE HANDLE lo_param_entity_id_type.
        ASSIGN lr_param_entity_id->* TO FIELD-SYMBOL(<lv_param_entity_id>).

        <lv_param_entity_id> = CONV #( iv_entity_name ).

        DATA(lo_param_id_type) = lo_class_descr->get_method_parameter_type(
          p_method_name       = c_cls-sadl_mpd_exposure-meth-get_exposure-name
          p_parameter_name    = c_cls-sadl_mpd_exposure-meth-get_exposure-param-rv_id ).
        CREATE DATA lr_param_id TYPE HANDLE lo_param_id_type.
        ASSIGN lr_param_id->* TO FIELD-SYMBOL(<lv_param_id>).

        CALL METHOD (c_cls-sadl_mpd_exposure-name)=>(c_cls-sadl_mpd_exposure-meth-get_exposure-name)
          EXPORTING
            iv_entity_type = 'CDS'
            iv_entity_id   = <lv_param_entity_id>
          RECEIVING
            rv_id          = <lv_param_id>.

      CATCH cx_sy_create_object_error
            cx_sy_ref_is_initial
            cx_sy_dyn_call_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sadl_mdp_factory IMPLEMENTATION.

  METHOD create_metadata_provider_ref.
*    DATA(lo_mdp_type) = cl_abap_refdescr=>get_by_name( 'IF_SADL_METADATA_PROVIDER' ).
*    CREATE DATA lr_mdp TYPE HANDLE lo_mdp_type.
  ENDMETHOD.

  METHOD get_mdp_for_id.
*    ASSIGN lr_mdp->* TO FIELD-SYMBOL(<lo_mdp>).
*
*    CALL METHOD ('CL_SADL_MDP_FACTORY')=>('GET_MDP_FOR_ID')
*      EXPORTING
*        iv_sadl_id = <lv_sadl_id>
*      RECEIVING
*        ro_mdp     = <lo_mdp>.

  ENDMETHOD.

  METHOD get_entity_load_by_id.
*    CALL METHOD ('CL_SADL_MDP_FACTORY')=>('GET_ENTITY_LOAD_BY_ID')
*      EXPORTING
*        iv_entity_id   = <lv_sadl_id>
*      RECEIVING
*        rr_entity_load = lr_entity_load.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sadl_exit_handler IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

  METHOD calculate_elements.

  ENDMETHOD.

ENDCLASS.
