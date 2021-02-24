"! <p class="shorttext synchronized" lang="en">Calculation of formula fields</p>
CLASS zcl_dbbr_formula_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !ir_formula                  TYPE REF TO zcl_dbbr_formula
        ir_tabfields                 TYPE REF TO zcl_dbbr_tabfield_list
        it_tab_components            TYPE zdbbr_abap_comp_type_itab
      RETURNING
        VALUE(rr_formula_calculator) TYPE REF TO zcl_dbbr_formula_calculator.
    METHODS calculate_row
      CHANGING
        !cr_row TYPE REF TO data .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_formula TYPE REF TO zcl_dbbr_formula.
    DATA mv_subroutine_pool TYPE progname.
    DATA mr_tabfields       TYPE REF TO zcl_dbbr_tabfield_list.
    DATA mt_tab_components  TYPE zdbbr_abap_comp_type_itab.

    METHODS constructor
      IMPORTING
        ir_formula        TYPE REF TO zcl_dbbr_formula
        ir_tabfields      TYPE REF TO zcl_dbbr_tabfield_list
        it_tab_components TYPE zdbbr_abap_comp_type_itab .
    METHODS create_subroutine_pool .
ENDCLASS.



CLASS zcl_dbbr_formula_calculator IMPLEMENTATION.


  METHOD calculate_row.
    PERFORM (zif_dbbr_c_fe_global=>c_formula_subroutine_form) IN PROGRAM (mv_subroutine_pool) CHANGING cr_row.
  ENDMETHOD.


  METHOD constructor.
    mr_formula = ir_formula.
    mr_tabfields = ir_tabfields.
    mt_tab_components = it_tab_components.
  ENDMETHOD.


  METHOD create.
    rr_formula_calculator = NEW zcl_dbbr_formula_calculator(
        ir_formula        = ir_formula
        ir_tabfields      = ir_tabfields
        it_tab_components = it_tab_components
    ).

    rr_formula_calculator->create_subroutine_pool( ).
  ENDMETHOD.


  METHOD create_subroutine_pool.
    DATA(lr_builder) = zcl_dbbr_fe_form_builder=>get_builder_for_subroutine_gen(
        ir_formula        = mr_formula
        ir_tabfields      = mr_tabfields
        it_tab_components = mt_tab_components
    ).

    lr_builder->build_formula( IMPORTING et_lines = DATA(lt_lines) ).

    GENERATE SUBROUTINE POOL lt_lines NAME mv_subroutine_pool MESSAGE DATA(lv_message).

    IF lv_message IS NOT INITIAL.
      zcl_sat_message_helper=>split_string_for_message(
        EXPORTING iv_string = lv_message
        IMPORTING ev_msgv1  = DATA(lv_msgv1)
                  ev_msgv2  = DATA(lv_msgv2)
                  ev_msgv3  = DATA(lv_msgv3)
                  ev_msgv4  = DATA(lv_msgv4)
      ).

      RAISE EXCEPTION TYPE zcx_dbbr_exception
        EXPORTING
          textid = zcx_dbbr_exception=>general_error
          msgv1  = lv_msgv1
          msgv2  = lv_msgv2
          msgv3  = lv_msgv3
          msgv4  = lv_msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
