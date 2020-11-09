*"* use this source file for your ABAP unit test classes
CLASS ltcl_fe_validator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_all_keywords FOR TESTING,
      test_val_correct_form FOR TESTING,
      test_undefined_formfield FOR TESTING,
      test_invalid_keyword FOR TESTING,
      test_unknown_keyword FOR TESTING,
      test_no_executable_code FOR TESTING,
      test_tokenization_error FOR TESTING,
      test_def_keyword_template FOR TESTING,
      test_text_keyword_template FOR TESTING,
      test_icon_keyword_template FOR TESTING,
      test_icon_tt_keyowrd_tmplt FOR TESTING,
      test_set_icon_tt_keyword_tmplt FOR TESTING,
      test_set_row_color_tmplt FOR TESTING,
      test_set_cell_color_tmplt FOR TESTING,
      create_test_tabfields
        RETURNING
          VALUE(rr_tabfields) TYPE REF TO ZCL_DBBR_tabfield_list.

    CLASS-METHODS class_setup.
ENDCLASS.


CLASS ltcl_fe_validator IMPLEMENTATION.

  METHOD test_val_correct_form.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$def my_form TYPE coep-wtgbtr.` && cl_abap_char_utilities=>cr_lf &&
                                 `$text my_form 'propKost'.`  && cl_abap_char_utilities=>cr_lf &&
                                 `my_form = row-wtgbtr + row-wogbtr.`
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>fail(  msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.

  ENDMETHOD.

  METHOD class_setup.

  ENDMETHOD.


  METHOD test_undefined_formfield.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$def my_form TYPE coep-wtgbtr.` && cl_abap_char_utilities=>cr_lf &&
                                 `my_form2 = row-wtgbtr + row-wogbtr + row-wkgbtr + row-wkfbtr.`
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.


  ENDMETHOD.


  METHOD test_invalid_keyword.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `DATA: lv_form TYPE WTGXXX.` && cl_abap_char_utilities=>cr_lf &&
                                 `SELECT SINGLE matnr into @data(lv_matnr).`
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_unknown_keyword.
    DATA(lr_tabfields) = create_test_tabfields( ).
    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$defs my_form TYPE WTGXXX.` && cl_abap_char_utilities=>cr_lf &&
                                 `my_form = row-wtgbtr + row-wogbtr + row-wkgbtr + row-wkfbtr.`
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_no_executable_code.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `* My formula, all correct but no executable code` && cl_abap_char_utilities=>cr_lf &&
                                 `DATA: lv_var1 type char1.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_all_keywords.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `* Formel, die alle existierenden Schlüsselwörter verwendet` && cl_abap_char_utilities=>cr_lf &&
                                 " Definition key words
                                 `$DEF cost_sum TYPE WTGXXX.` && cl_abap_char_utilities=>cr_lf &&
                                 `$ICON plan_actual_icon.` && cl_abap_char_utilities=>cr_lf &&
                                 `$ICON_TT proc_indicator_icon.` && cl_abap_char_utilities=>cr_lf &&
                                 " defining column texts
                                 `$TEXT cost_sum 'Summe' 'Summe lang'.` && cl_abap_char_utilities=>cr_lf &&
                                 `$TEXT plan_actual_icon 'Ist/Plan?'.` && cl_abap_char_utilities=>cr_lf &&
                                 `$TEXT proc_indicator_icon 'Besch.Kennz'.` && cl_abap_char_utilities=>cr_lf &&
                                 " doing some calculations
                                 `cost_sum = row-wtgbtr + row-wogbtr + row-wkgbtr.` && cl_abap_char_utilities=>cr_lf  &&
                                 " fill simple icon value
                                 `IF row-wrttp = '1'.` && cl_abap_char_utilities=>cr_lf &&
                                 `plan_actual_icon = icon_space.` && cl_abap_char_utilities=>cr_lf &&
                                 `else.` && cl_abap_char_utilities=>cr_lf &&
                                 `plan_actual_icon = icon_calculation.` && cl_abap_char_utilities=>cr_lf &&
                                 `endif.` && cl_abap_char_utilities=>cr_lf  &&
                                 " fill icon with tooltip
                                 `if row-vrgng = 'KAMV'.` && cl_abap_char_utilities=>cr_lf &&
                                 `   $set_icon_tt proc_indicator_icon icon_transport 'Externe Lieferung'.` && cl_abap_char_utilities=>cr_lf &&
                                 `endif.` && cl_abap_char_utilities=>cr_lf &&
                                 " test row coloring
                                 `if row-versn = '000'.` && cl_abap_char_utilities=>cr_lf &&
                                 `  $set_row_color 'c500'.` && cl_abap_char_utilities=>cr_lf &&
                                 `endif.` && cl_abap_char_utilities=>cr_lf &&
                                 " test cell coloring
                                 `if row-wrttp = '4'.` && cl_abap_char_utilities=>cr_lf &&
                                 `  $set_cell_color row-perio  'C500'.` && cl_abap_char_utilities=>cr_lf &&
                                 `endif.` && cl_abap_char_utilities=>cr_lf

        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>fail(
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_tokenization_error.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `* My formula, all correct but no executable code` && cl_abap_char_utilities=>cr_lf &&
                                 `DATA(lv_var1) = 'String over several lines.` && cl_abap_char_utilities=>cr_lf &&
                                 `lv_var1 = row-wtgbtr + row-wogbtr.` && cl_abap_char_utilities=>cr_lf &&
                                 `IF 1 = 2.` && cl_abap_char_utilities=>cr_lf &&
                                 `ENDIF.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_def_keyword_template.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$DEF prop_cost_sum.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_text_keyword_template.
    DATA(lr_tabfields) = create_test_tabfields( ).
    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$TEXT prop_cost_sum 'Kurz.Text' 'Langtext' 'Text zu viel'.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_icon_keyword_template.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$ICON ffIcon icon_plant.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_icon_tt_keyowrd_tmplt.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$ICON_TT field_name '500'.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_set_icon_tt_keyword_tmplt.
    DATA(lr_tabfields) = create_test_tabfields( ).
    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$SET_ICON_TT icon_field 'Tooltip'.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_set_row_color_tmplt.
    DATA(lr_tabfields) = create_test_tabfields( ).
    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$SET_ROW_COLOR row-wogbtr 'C500'.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_set_cell_color_tmplt.
    DATA(lr_tabfields) = create_test_tabfields( ).

    DATA(lr_validator) = NEW ZCL_DBBR_fe_validator(
        iv_formula             = `$SET_CELL_COLOR 'C500'.` && cl_abap_char_utilities=>cr_lf
        io_tabfields           = lr_tabfields
    ).

    TRY.
        lr_validator->validate( ).
      CATCH ZCX_DBBR_formula_exception INTO DATA(lr_exception).
        lr_exception->get_source_position( IMPORTING source_line  = DATA(lv_source_line)
                                                     include_name = DATA(lv_include) ).
        cl_aunit_assert=>assert_not_initial(
            act = lr_exception
            msg = |{ lr_exception->get_text( ) } in { lv_include } at { lv_source_line }|  ).
    ENDTRY.
  ENDMETHOD.


  METHOD create_test_tabfields.

    rr_tabfields  = NEW ZCL_DBBR_tabfield_list( ).

    ZCL_DBBR_tabfield_builder=>create_tabfields(
        iv_tablename        = 'COEP'
        ir_tabfield_list    = rr_tabfields
        if_output_active    = abap_true
        if_is_primary       = abap_true
    ).

  ENDMETHOD.

ENDCLASS.
