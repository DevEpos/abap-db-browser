CLASS ZCL_DBBR_formula_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !ir_formula                  TYPE REF TO ZCL_DBBR_formula
        ir_tabfields                 TYPE REF TO ZCL_DBBR_tabfield_list
        it_tab_components            TYPE ZDBBR_abap_comp_type_itab
      RETURNING
        VALUE(rr_formula_calculator) TYPE REF TO ZCL_DBBR_formula_calculator.
    METHODS calculate_row
      CHANGING
        !cr_row TYPE REF TO data .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_formula TYPE REF TO ZCL_DBBR_formula.
    DATA mv_subroutine_pool TYPE progname.
    DATA mr_tabfields       TYPE REF TO ZCL_DBBR_tabfield_list.
    DATA mt_tab_components  TYPE ZDBBR_abap_comp_type_itab.

    METHODS constructor
      IMPORTING
        ir_formula        TYPE REF TO ZCL_DBBR_formula
        ir_tabfields      TYPE REF TO ZCL_DBBR_tabfield_List
        it_tab_components TYPE ZDBBR_abap_comp_type_itab .
    METHODS create_subroutine_pool .
ENDCLASS.



CLASS ZCL_DBBR_FORMULA_CALCULATOR IMPLEMENTATION.


  METHOD calculate_row.
    PERFORM (ZIF_DBBR_fe_constants=>c_formula_subroutine_form) IN PROGRAM (mv_subroutine_pool) CHANGING cr_row.
  ENDMETHOD.


  METHOD constructor.
    mr_formula = ir_formula.
    mr_tabfields = ir_tabfields.
    mt_tab_components = it_tab_components.
  ENDMETHOD.


  METHOD create.
    rr_formula_calculator = new ZCL_DBBR_formula_calculator(
        ir_formula        = ir_formula
        ir_tabfields      = ir_tabfields
        it_tab_components = it_tab_components
    ).

    rr_formula_calculator->create_subroutine_pool( ).
  ENDMETHOD.


  METHOD create_subroutine_pool.
    data(lr_builder) = ZCL_DBBR_fe_form_builder=>get_builder_for_subroutine_gen(
        ir_formula        = mr_formula
        ir_tabfields      = mr_tabfields
        it_tab_components = mt_tab_components
    ).

    lr_builder->build_formula( importing et_lines = data(lt_lines) ).

    GENERATE SUBROUTINE POOL lt_lines NAME mv_subroutine_pool MESSAGE DATA(lv_message).

    IF lv_message IS NOT INITIAL.
      ZCL_DBBR_appl_util=>split_string_for_message(
        EXPORTING iv_string = lv_message
        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                  ev_msgv2  = DATA(lv_msgv2)
                  ev_msgv3  = DATA(lv_msgv3)
                  ev_msgv4  = DATA(lv_msgv4)
      ).

      RAISE EXCEPTION TYPE ZCX_DBBR_exception
        EXPORTING
          textid = ZCX_DBBR_exception=>general_error
          msgv1  = lv_msgv1
          msgv2  = lv_msgv2
          msgv3  = lv_msgv3
          msgv4  = lv_msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
