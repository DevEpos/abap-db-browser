*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_grid_grabber IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mr_salv_model = ir_alv_model.
  ENDMETHOD.

  METHOD get_grid.
    IF mr_grid IS INITIAL.
      " grab full screen adapter
      DATA(lr_controller) = mr_salv_model->r_controller.

      " in this case it is the fullscreen adapter
      DATA(lr_adapter) = CAST cl_salv_fullscreen_adapter( lr_controller->r_adapter ).
      mr_grid = lr_adapter->get_grid( ).
    ENDIF.

    rr_grid = mr_grid.
  ENDMETHOD.

ENDCLASS.
