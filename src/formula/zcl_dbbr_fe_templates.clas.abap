CLASS zcl_dbbr_fe_templates DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-DATA gv_introduction_template TYPE string.
    CLASS-DATA gv_calc_difference_exmple TYPE string.
    CLASS-DATA gv_icon_example TYPE string.
    CLASS-DATA gv_icon_quicktip_example TYPE string.
    CLASS-DATA gv_form_field_tmplt TYPE string.
    CLASS-DATA gv_form_field_rllnam_tmplt TYPE string.
    CLASS-DATA gv_text_for_field_tmplt TYPE string.
    CLASS-DATA gv_icon_field_tmplt TYPE string.
    CLASS-DATA gv_icon_tt_field_tmplt TYPE string.
    CLASS-DATA gv_set_icon_tmplt TYPE string.
    CLASS-DATA gv_set_row_color_template TYPE string.
    CLASS-DATA gv_set_cell_color_template TYPE string.
    CLASS-DATA gv_form_unit_tmplt TYPE string.
    CLASS-DATA gt_valid_keywords_range TYPE RANGE OF char20.
ENDCLASS.


CLASS zcl_dbbr_fe_templates IMPLEMENTATION.
  METHOD class_constructor.
    DATA lt_keywords TYPE TABLE OF char20.

    gv_form_field_tmplt        = `$DEF      ffname   TYPE Table-Field`.
    gv_form_field_rllnam_tmplt = `$DEF      ffname   TYPE Data Element`.
    gv_text_for_field_tmplt    = `$TEXT     ffname   'Short Text'  'Long Text'.`.
    gv_form_unit_tmplt         = `$UNIT     ffname   ROW-COLUMN_NAME`.
    gv_icon_field_tmplt        = `$ICON     ffname.`.
    gv_icon_tt_field_tmplt     = `$ICON_TT  ffname.`.
    gv_set_icon_tmplt          = `$SET_ICON_TT   ffname   ICON_PLANT  'Long text (up to 30 Characters)'.`.
    gv_set_row_color_template  = `$SET_ROW_COLOR  'C500'.`.
    gv_set_cell_color_template = `$SET_CELL_COLOR fieldname 'C500'.`.

    DEFINE concat.
      &1 = &1 && &2 && cl_abap_char_utilities=>newline.
    END-OF-DEFINITION.

    concat gv_introduction_template
      `* A formula is a series of statements, that are executed for every`.
    concat gv_introduction_template
      `* line in the data output to calculate the value of one or several`.
    concat gv_introduction_template
      `* formula fields. Subsequently the defined formula fields are shown`.
    concat gv_introduction_template
      `* as new columns at the right end of the list. They are prefixed with`.
    concat gv_introduction_template
      `* 'X~' .`.
    concat gv_introduction_template
      `*`.
    concat gv_introduction_template
      `* To define a formula field the following commands exist:`.
    concat gv_introduction_template
      `* - $DEF - To define a calculated field`.
    concat gv_introduction_template
      `* - $TEXT - To define a column label for a formula field`.
    concat gv_introduction_template
      `*`.
    concat gv_introduction_template
      `* The formula itself can be programmed in a subset of the ABAP language.`.
    concat gv_introduction_template
      `* You can define local or even static variables. The static variables`.
    concat gv_introduction_template
      `* are useful if you want to build a formula with global consideration.`.
    concat gv_introduction_template
      `* The are initialized during the first formula execution of the first`.
    concat gv_introduction_template
      `* row in the output list.`.
    concat gv_introduction_template
      `*`.
    concat gv_introduction_template
      `* You can access the fields in the current row via ROW-fieldname respectively`.
    concat gv_introduction_template
      `* ROW-a-fieldname where 'a' is a table alias.`.
    concat gv_introduction_template
      `*`.
    concat gv_introduction_template
      `* The statements of the formula are automatically wrapped inside a `.
    concat gv_introduction_template
      `* TRY / CATCH block. If an error should occur during the execution`.
    concat gv_introduction_template
      `* the values of the formula fields will be set to their initial value.`.

    concat gv_calc_difference_exmple
      `* In this example the arithmetic difference between requested and`.
    concat gv_calc_difference_exmple
      `* confirmed quantity will be calculated.`.
    concat gv_calc_difference_exmple
      `* It is assumed that the list contains entries from table VBAP.`.
    concat gv_calc_difference_exmple
      `*`.
    concat gv_calc_difference_exmple
      `* Firstly the formula field for the difference is defined`.
    concat gv_calc_difference_exmple
      `$DEF   DIFF    TYPE VBAP-KWMENG.`.
    concat gv_calc_difference_exmple
      ***      `* Afterwards the unit field will be assigned to the field. The field`,
***      `* will get the same unit as the fields KWMENG and KBMENG.`,
***      `* This step is optional. The ALV uses the unit during the building`,
***      `* of totals and sub totals.`,
***      `$UNIT  DIFF    ROW-VRKME.`,
            `* You can also define an optional column header.`.
    concat gv_calc_difference_exmple
      `$TEXT  DIFF  'unConfQu.'   'unconfirmed Quantity'.`.
    concat gv_calc_difference_exmple
      `* Finally the actual calculation of the formula field`.
    concat gv_calc_difference_exmple
      `DIFF = ROW-KWMENG - ROW-KBMENG.`.

    concat gv_icon_example
      `* In this example an Icon field is added to the output list.`.
    concat gv_icon_example
      `* It is assumed that the list contains entries from table VBAP.`.
    concat gv_icon_example
      `*`.
    concat gv_icon_example
      `* If there is an unconfirmed quantity, i.e. if the confirmed `.
    concat gv_icon_example
      `* quantity is less than the requested quantity KWMENG, the current`.
    concat gv_icon_example
      `* row should get a red flag.`.
    concat gv_icon_example
      `*`.
    concat gv_icon_example
      `* Firstly, the icon field is defined. To do this the commands`.
    concat gv_icon_example
      `* $ICON (icon only) and $ICON_TT (icon with tooltip) exist.`.
    concat gv_icon_example
      `$ICON icon_field.`.
    concat gv_icon_example
      `* You can also define an optional column header.`.
    concat gv_icon_example
      `$TEXT  icon_field  'unconf.'   'unconfirmed Quantity'.`.
    concat gv_icon_example
      `* Now the icon field is ready to be used. `.
    concat gv_icon_example
      `* You can use the 'Insert Icon' Button to conveniently insert`.
    concat gv_icon_example
      `* any existing icon in the system.`.
    concat gv_icon_example
      `*`.
    concat gv_icon_example
      `IF ROW-KWMENG > ROW-KBMENG.`.
    concat gv_icon_example
      `  icon_field = ICON_DEFECT.`.
    concat gv_icon_example
      `ELSE.`.
    concat gv_icon_example
      `  icon_field = ICON_SPACE.`.
    concat gv_icon_example
      `ENDIF.`.

    concat gv_icon_quicktip_example
      `* In this example an Icon field with a tooltip will be added.`.
    concat gv_icon_quicktip_example
      `* It is assumed that the list contains entries from table MARC.`.
    concat gv_icon_quicktip_example
      `*`.
    concat gv_icon_quicktip_example
      `* The icon is used to mark the procurement indicator of the material`.
    concat gv_icon_quicktip_example
      `* i.e. MARC-BESKZ.`.
    concat gv_icon_quicktip_example
      `*`.
    concat gv_icon_quicktip_example
      `* Firstly an icon field with a tooltip is defined. This is done with`.
    concat gv_icon_quicktip_example
      `* the command $ICON_TT field`.
    concat gv_icon_quicktip_example
      `$ICON_TT  icon_field.`.
    concat gv_icon_quicktip_example
      `* You can also define an optional column header.`.
    concat gv_icon_quicktip_example
      `$TEXT icon_field    'ProcInd.'  'Procuremnt Ind.'.`.
    concat gv_icon_quicktip_example
      `* Now the field can be used.`.
    concat gv_icon_quicktip_example
      `IF ROW-BESKZ = 'E'.`.
    concat gv_icon_quicktip_example
      `  $SET_ICON_TT  icon_field   ICON_PLANT        'In house production'.`.
    concat gv_icon_quicktip_example
      `ELSEIF ROW-BESKZ = 'F'.`.
    concat gv_icon_quicktip_example
      `  $SET_ICON_TT  icon_field   ICON_ARROW_LEFT   'External Procurement'.`.
    concat gv_icon_quicktip_example
      `ELSEIF ROW-BESKZ = 'X'.`.
    concat gv_icon_quicktip_example
      `  $SET_ICON_TT  icon_field   ICON_SYSTEMTYPE   'Both types'.`.
    concat gv_icon_quicktip_example
      `ELSE.`.
    concat gv_icon_quicktip_example
      `  $SET_ICON_TT  icon_field   ICON_SPACE         space.`.
    concat gv_icon_quicktip_example
      `ENDIF.`.

    lt_keywords = VALUE #( ( 'DATA' )
                           ( 'IF' )
                           ( 'ELSE' )
                           ( 'ELSEIF' )
                           ( 'ENDIF' )
                           ( 'ASSIGN' )
                           ( 'CASE' )
                           ( 'WHEN' )
                           ( 'ENDCASE' )
                           ( 'CHECK' )
                           ( 'CLEAR' )
                           ( 'COMPUTE' )
                           ( 'WHILE' )
                           ( 'ENDWHILE' )
                           ( 'CONTINUE' )
                           ( 'EXIT' )
                           ( 'DO' )
                           ( 'ENDDO' )
                           ( 'CONCATENATE' )
                           ( 'CONDENSE' )
                           ( 'CONSTANTS' )
                           ( 'CONVERT' )
                           ( 'FIELD-SYMBOLS' )
                           ( 'MOVE' )
                           ( 'REPLACE' )
                           ( 'SHIFT' )
                           ( 'SPLIT' )
                           ( 'STATICS' )
                           ( 'TRANSLATE' )
                           ( 'TYPES' )
                           ( 'WHEN' )
                           ( 'WRITE' ) ).

    gt_valid_keywords_range = VALUE #( LET i  = 'I'
                                           eq = 'EQ' IN
                                       FOR keyword IN lt_keywords
                                       ( sign = i option = eq low = keyword ) ).
  ENDMETHOD.
ENDCLASS.
