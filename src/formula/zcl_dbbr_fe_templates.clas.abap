CLASS zcl_dbbr_fe_templates DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_fe_templates IMPLEMENTATION.


  METHOD class_constructor.
    DATA: lt_keywords TYPE TABLE OF char20.

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

    concat gv_introduction_template:
      `* A formula is a series of statements, that are executed for every`,
      `* line in the data output to calculate the value of one or several`,
      `* formula fields. Subsequently the defined formula fields are shown`,
      `* as new columns at the right end of the list. They are prefixed with`,
      `* 'X~' .`,
      `*`,
      `* To define a formula field the following commands exist:`,
      `* - $DEF - To define a calculated field`,
      `* - $TEXT - To define a column label for a formula field`,
      `*`,
      `* The formula itself can be programmed in a subset of the ABAP language.`,
      `* You can define local or even static variables. The static variables`,
      `* are useful if you want to build a formula with global consideration.`,
      `* The are initialized during the first formula execution of the first`,
      `* row in the output list.`,
      `*`,
      `* You can access the fields in the current row via ROW-fieldname respectively`,
      `* ROW-a-fieldname where 'a' is a table alias.`,
      `*`,
      `* The statements of the formula are automatically wrapped inside a `,
      `* TRY / CATCH block. If an error should occur during the execution`,
      `* the values of the formula fields will be set to their initial value.`.

    concat gv_calc_difference_exmple:
      `* In this example the arithmetic difference between requested and`,
      `* confirmed quantity will be calculated.`,
      `* It is assumed that the list contains entries from table VBAP.`,
      `*`,
      `* Firstly the formula field for the difference is defined`,
      `$DEF   DIFF    TYPE VBAP-KWMENG.`,
***      `* Afterwards the unit field will be assigned to the field. The field`,
***      `* will get the same unit as the fields KWMENG and KBMENG.`,
***      `* This step is optional. The ALV uses the unit during the building`,
***      `* of totals and sub totals.`,
***      `$UNIT  DIFF    ROW-VRKME.`,
      `* You can also define an optional column header.`,
      `$TEXT  DIFF  'unConfQu.'   'unconfirmed Quantity'.`,
      `* Finally the actual calculation of the formula field`,
      `DIFF = ROW-KWMENG - ROW-KBMENG.`.

    concat gv_icon_example:
      `* In this example an Icon field is added to the output list.`,
      `* It is assumed that the list contains entries from table VBAP.`,
      `*`,
      `* If there is an unconfirmed quantity, i.e. if the confirmed `,
      `* quantity is less than the requested quantity KWMENG, the current`,
      `* row should get a red flag.`,
      `*`,
      `* Firstly, the icon field is defined. To do this the commands`,
      `* $ICON (icon only) and $ICON_TT (icon with tooltip) exist.`,
      `$ICON icon_field.`,
      `* You can also define an optional column header.`,
      `$TEXT  icon_field  'unconf.'   'unconfirmed Quantity'.`,
      `* Now the icon field is ready to be used. `,
      `* You can use the 'Insert Icon' Button to conveniently insert`,
      `* any existing icon in the system.`,
      `*`,
      `IF ROW-KWMENG > ROW-KBMENG.`,
      `  icon_field = ICON_DEFECT.`,
      `ELSE.`,
      `  icon_field = ICON_SPACE.`,
      `ENDIF.`.

    concat gv_icon_quicktip_example:
      `* In this example an Icon field with a tooltip will be added.`,
      `* It is assumed that the list contains entries from table MARC.`,
      `*`,
      `* The icon is used to mark the procurement indicator of the material`,
      `* i.e. MARC-BESKZ.`,
      `*`,
      `* Firstly an icon field with a tooltip is defined. This is done with`,
      `* the command $ICON_TT field`,
      `$ICON_TT  icon_field.`,
      `* You can also define an optional column header.`,
      `$TEXT icon_field    'ProcInd.'  'Procuremnt Ind.'.`,
      `* Now the field can be used.`,
      `IF ROW-BESKZ = 'E'.`,
      `  $SET_ICON_TT  icon_field   ICON_PLANT        'In house production'.`,
      `ELSEIF ROW-BESKZ = 'F'.`,
      `  $SET_ICON_TT  icon_field   ICON_ARROW_LEFT   'External Procurement'.`,
      `ELSEIF ROW-BESKZ = 'X'.`,
      `  $SET_ICON_TT  icon_field   ICON_SYSTEMTYPE   'Both types'.`,
      `ELSE.`,
      `  $SET_ICON_TT  icon_field   ICON_SPACE         space.`,
      `ENDIF.`.

    lt_keywords = VALUE #(
      ( 'DATA' ) ( 'IF' ) ( 'ELSE' ) ( 'ELSEIF' ) ( 'ENDIF' ) ( 'ASSIGN' ) ( 'CASE' ) ( 'WHEN' ) ( 'ENDCASE' ) ( 'CHECK' ) ( 'CLEAR' ) ( 'COMPUTE' )
      ( 'WHILE' ) ( 'ENDWHILE' ) ( 'CONTINUE' ) ( 'EXIT' ) ( 'DO' ) ( 'ENDDO' )
      ( 'CONCATENATE' ) ( 'CONDENSE' ) ( 'CONSTANTS' )  ( 'CONVERT' )
      ( 'FIELD-SYMBOLS' ) ( 'MOVE' ) ( 'REPLACE' ) ( 'SHIFT' ) ( 'SPLIT' ) ( 'STATICS' ) ( 'TRANSLATE' ) ( 'TYPES' )
      ( 'WHEN' ) ( 'WRITE' )
    ).

    gt_valid_keywords_range = VALUE #(
      LET i = 'I' eq = 'EQ' IN
      FOR keyword IN lt_keywords
      ( sign = i option = eq low = keyword )
    ).
  ENDMETHOD.
ENDCLASS.
