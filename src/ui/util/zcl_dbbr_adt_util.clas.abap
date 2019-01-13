class ZCL_DBBR_ADT_UTIL definition
  public
  final
  create public .

public section.

  class-methods JUMP_ADT
    importing
      !IV_OBJ_NAME type TADIR-OBJ_NAME
      !IV_OBJ_TYPE type TADIR-OBJECT
      !IV_SUB_OBJ_NAME type TADIR-OBJ_NAME optional
      !IV_SUB_OBJ_TYPE type TADIR-OBJECT optional
      !IV_LINE_NUMBER type I optional
    raising
      ZCX_DBBR_ADT_ERROR .
  PROTECTED SECTION.
private section.

  class-methods IS_ADT_JUMP_POSSIBLE
    importing
      !IR_WB_OBJECT type ref to CL_WB_OBJECT
      !IR_ADT type ref to IF_ADT_TOOLS_CORE_FACTORY
    returning
      value(RF_IS_ADT_JUMP_POSSIBLE) type ABAP_BOOL
    raising
      ZCX_DBBR_ADT_ERROR .
  class-methods GET_ADT_OBJECTS_AND_NAMES
    importing
      !IV_OBJ_NAME type TADIR-OBJ_NAME
      !IV_OBJ_TYPE type TADIR-OBJECT
    exporting
      !ER_ADT_URI_MAPPER type ref to IF_ADT_URI_MAPPER
      !ER_ADT_OBJECTREF type ref to CL_ADT_OBJECT_REFERENCE
      !EV_PROGRAM type PROGNAME
      !EV_INCLUDE type PROGNAME
    raising
      ZCX_DBBR_ADT_ERROR .
ENDCLASS.



CLASS ZCL_DBBR_ADT_UTIL IMPLEMENTATION.


  METHOD get_adt_objects_and_names.
    DATA lv_obj_type       TYPE trobjtype.
    DATA lv_obj_name       TYPE trobj_name.
    FIELD-SYMBOLS <lv_uri> TYPE string.

    lv_obj_name = iv_obj_name.
    lv_obj_type = iv_obj_type.

    cl_wb_object=>create_from_transport_key(
      EXPORTING
        p_object    = lv_obj_type
        p_obj_name  = lv_obj_name
      RECEIVING
        p_wb_object = DATA(lr_wb_object)
      EXCEPTIONS
        OTHERS      = 1 ).
    IF sy-subrc <> 0.
      zcx_dbbr_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDIF.

    DATA(lr_adt_tools) = cl_adt_tools_core_factory=>get_instance( ).

    IF is_adt_jump_possible( ir_wb_object = lr_wb_object
                             ir_adt       = lr_adt_tools ) = abap_false.
      zcx_dbbr_adt_error=>raise_adt_error_with_text( |ADT Jump is not possible for { iv_obj_type } - { iv_obj_name }| ).
    ENDIF.

    er_adt_uri_mapper = lr_adt_tools->get_uri_mapper( ).

    er_adt_objectref = er_adt_uri_mapper->map_wb_object_to_objref(
        wb_object          = lr_wb_object
    ).

    er_adt_uri_mapper->map_objref_to_include(
      EXPORTING
        uri                = er_adt_objectref->ref_data-uri
      IMPORTING
        program            = ev_program
        include            = ev_include
    ).
  ENDMETHOD.


  METHOD is_adt_jump_possible.
    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = ir_wb_object
      RECEIVING
        p_wb_request      = DATA(lr_wb_request)
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_dbbr_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDIF.

    TRY.
        DATA(lr_adt_uri_mapper_vit) = ir_adt->get_uri_mapper_vit( ).
        rf_is_adt_jump_possible = xsdbool( NOT lr_adt_uri_mapper_vit->is_vit_wb_request( wb_request = lr_wb_request ) ).
      CATCH cx_root.
        zcx_dbbr_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDTRY.
  ENDMETHOD.


  METHOD jump_adt.
    get_adt_objects_and_names(
      EXPORTING
        iv_obj_name        = iv_obj_name
        iv_obj_type        = iv_obj_type
      IMPORTING
        er_adt_uri_mapper = DATA(lr_adt_uri_mapper)
        er_adt_objectref  = DATA(lr_adt_objref)
        ev_program        = DATA(lv_program)
        ev_include        = DATA(lv_include)
    ).

    TRY.
        IF iv_sub_obj_name IS NOT INITIAL.

          IF ( lv_program <> iv_obj_name AND lv_include IS INITIAL ) OR
             ( lv_program = lv_include AND iv_sub_obj_name IS NOT INITIAL ).
            lv_include = iv_sub_obj_name.
          ENDIF.

          DATA(lr_adt_sub_objref) = lr_adt_uri_mapper->map_include_to_objref(
              program            = lv_program
              include            = lv_include
              line               = iv_line_number
              line_offset        = 0
              end_line           = iv_line_number
              end_offset         = 1
          ).

          IF lr_adt_sub_objref IS NOT INITIAL.
            lr_adt_objref = lr_adt_sub_objref.
          ENDIF.

        ENDIF.

        DATA(lv_adt_link) = |adt://{ sy-sysid }{ lr_adt_objref->ref_data-uri }|.

        cl_gui_frontend_services=>execute( EXPORTING  document = lv_adt_link
                                           EXCEPTIONS OTHERS   = 1 ).

        IF sy-subrc <> 0.
          zcx_dbbr_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
        ENDIF.

      CATCH cx_root.
        zcx_dbbr_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
