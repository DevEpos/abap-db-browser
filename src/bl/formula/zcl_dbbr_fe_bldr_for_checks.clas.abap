CLASS zcl_dbbr_fe_bldr_for_checks DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_fe_generic_form_bldr
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_fe_formula_builder.

    METHODS constructor
      IMPORTING
        ir_formula   TYPE REF TO zcl_dbbr_formula
        ir_tabfields TYPE REF TO zcl_dbbr_tabfield_list.
  PRIVATE SECTION.
    DATA mr_formula   TYPE REF TO zcl_dbbr_formula.
    DATA mr_tabfields TYPE REF TO zcl_dbbr_tabfield_list.
    METHODS include_form_definitions
      IMPORTING
        it_statements TYPE zif_dbbr_fe_types=>tt_statement
      EXPORTING
        et_form       TYPE string_table.
ENDCLASS.



CLASS zcl_dbbr_fe_bldr_for_checks IMPLEMENTATION.

  METHOD zif_dbbr_fe_formula_builder~build_formula.

    et_lines = VALUE #( ( |REPORT Z_FORMULA.| ) ).


    " insert data definition for color column (if needed)
    IF mr_formula->is_color_column_needed( ).
      et_lines = VALUE #( BASE et_lines
        ( |DATA: | &&
          zif_dbbr_c_special_out_columns=>cell_col_row_color &&
          ` TYPE ` &&
          zif_dbbr_c_special_out_columns=>alv_col_color_type && '.' ) ).
    ENDIF.

    " insert all tables
    DATA(lt_tables) = mr_tabfields->get_table_list( ).
    DELETE lt_tables WHERE tabname = zif_dbbr_global_consts=>gc_formula_dummy_table.
    IF lines( lt_tables ) > 1.
      et_lines = VALUE #( BASE et_lines ( |data: begin of row,| ) ).
      LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
        et_lines = VALUE #( BASE et_lines ( |{ <ls_table>-alias } type { <ls_table>-tabname },| ) ).
      ENDLOOP.
      et_lines = VALUE #( BASE et_lines ( |end of row.| ) ).
    ELSE.
      et_lines = VALUE #( BASE et_lines ( |DATA row type { lt_tables[ 1 ]-tabname }.| ) ).
    ENDIF.

    et_lines = VALUE #( BASE et_lines ( |START-OF-SELECTION.| ) ).


    mr_formula->get_statements( IMPORTING et_statements = DATA(lt_statements) ).

    include_form_definitions( EXPORTING it_statements = lt_statements
                              IMPORTING et_form       = et_lines       ).

    ev_starting_line = lines( et_lines ) + 3.

    include_normal_statements( EXPORTING it_statements = lt_statements
                               IMPORTING et_form       = et_lines       ).
  ENDMETHOD.

  METHOD include_form_definitions.
    LOOP AT it_statements ASSIGNING FIELD-SYMBOL(<ls_statement>) WHERE is_form_stmnt = abap_true.
      et_form = VALUE #( BASE et_form ( <ls_statement>-stringform ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    mr_formula = ir_formula.
    mr_tabfields = ir_tabfields.
  ENDMETHOD.

ENDCLASS.
