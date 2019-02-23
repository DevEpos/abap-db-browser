CLASS zcl_dbbr_start DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS start .
    METHODS start_formula_editor_test .
    METHODS start_search .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_start IMPLEMENTATION.


  METHOD constructor.
    zcl_dbbr_system_helper=>set_locale_language( ).
  ENDMETHOD.


  METHOD start.
    CALL FUNCTION 'ZDBBR_START'.
  ENDMETHOD.


  METHOD start_formula_editor_test.
    DATA(lr_tabfields) = NEW zcl_dbbr_tabfield_list( ).

    zcl_dbbr_tabfield_builder=>create_tabfields(
        iv_tablename        = 'COEP'
        ir_tabfield_list    = lr_tabfields
        if_output_active    = abap_true
        if_is_primary       = abap_true
    ).
    DATA(lr_formula_editor) = NEW zcl_dbbr_formula_editor(
      io_tabfield_list = lr_tabfields
*      ir_join_def_stru = NEW zdbbr_join_data( )
      is_join_def      = VALUE zdbbr_join_def( )
      iv_display_mode  = zif_dbbr_global_consts=>gc_display_modes-edit
      iv_formula       = `* Meine Formel` && cl_abap_char_utilities=>cr_lf &&
                         `$DEF my_form type wtgxxx.`  && cl_abap_char_utilities=>cr_lf &&
                         `my_form = row-wtgbtr + 4.`
    ).

    lr_formula_editor->show( ).
  ENDMETHOD.


  METHOD start_search.
    NEW zcl_dbbr_object_central_search( if_new_transaction_mode = abap_true )->show( ).
  ENDMETHOD.

ENDCLASS.
