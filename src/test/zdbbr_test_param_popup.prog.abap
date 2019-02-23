*&---------------------------------------------------------------------*
*& Report  zdbbr_test_param_popup
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdbbr_test_param_popup.

PARAMETERS: p_cds TYPE zdbbr_cds_view_name MATCHCODE OBJECT zdbbr_cds_view_sh OBLIGATORY.

START-OF-SELECTION.
  DATA(lr_cds_view) = zcl_dbbr_cds_view_factory=>read_cds_view( p_cds ).
  CHECK lr_cds_view->has_parameters( ).
  DATA(lr_tabfields) = NEW zcl_dbbr_tabfield_list( ).

  zcl_dbbr_cds_tabfield_util=>add_parameters(
      ir_tabfield_list = lr_tabfields
      it_parameters    = lr_cds_view->get_parameters( )
  ).

  data(lo_param_popup) = NEW zcl_dbbr_cds_param_popup(
      io_tabfields = lr_tabfields
  ).
  lo_param_popup->show( ).
  data(lt_entered_params) = lo_param_popup->get_param_values( ).

  IF lt_entered_params IS INITIAL.
    WRITE: / |No Parameters were entered|.
  ELSE.
    WRITE: / 'Parameters'.
    ULINE.
    LOOP AT lt_entered_params ASSIGNING FIELD-SYMBOL(<ls_param_value>).
      WRITE: / |{ <ls_param_value>-name WIDTH = 20 }: { <ls_param_value>-value WIDTH = 30 ALIGN = RIGHT }|.
    ENDLOOP.
  ENDIF.
