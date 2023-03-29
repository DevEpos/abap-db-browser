"! <p class="shorttext synchronized" lang="en">Starts the DB Browser</p>
"! *******************************************************************************
"!  The MIT License (MIT)
"!
"!  Copyright (c) 2019 Ludwig Stockbauer-Muhr
"!
"!  Permission is hereby granted, free of charge, to any person obtaining a copy
"!  of this software and associated documentation files (the "Software"), to deal
"!  in the Software without restriction, including without limitation the rights
"!  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
"!  copies of the Software, and to permit persons to whom the Software is
"!  furnished to do so, subject to the following conditions:
"!
"!  The above copyright notice and this permission notice shall be included in all
"!  copies or substantial portions of the Software.
"!
"!  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
"!  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
"!  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
"!  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
"!  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
"!  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
"!  SOFTWARE.
"!
"! *******************************************************************************
CLASS zcl_dbbr_start DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor .
    "! <p class="shorttext synchronized" lang="en">Starts DB Browser</p>
    METHODS start .
    "! <p class="shorttext synchronized" lang="en">Starts Test of DB Browser Formula Editor</p>
    METHODS start_formula_editor_test .
    "! <p class="shorttext synchronized" lang="en">Start Central Search Function</p>
    METHODS start_search .
    "! <p class="shorttext synchronized" lang="en">Starts the SQL Query Console</p>
    METHODS start_sql_console.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_start IMPLEMENTATION.


  METHOD constructor.
    zcl_sat_system_helper=>set_locale_language( ).
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
      iv_display_mode  = zif_dbbr_c_global=>c_display_modes-edit
      iv_formula       = `* Meine Formel` && cl_abap_char_utilities=>cr_lf &&
                         `$DEF my_form type wtgxxx.`  && cl_abap_char_utilities=>cr_lf &&
                         `my_form = row-wtgbtr + 4.`
    ).

    lr_formula_editor->show( ).
  ENDMETHOD.


  METHOD start_search.
    NEW zcl_dbbr_object_central_search( if_new_transaction_mode = abap_true )->show( ).
  ENDMETHOD.

  METHOD start_sql_console.
    DATA: lv_query TYPE string.
    IMPORT
      query = lv_query
    FROM MEMORY ID zcl_dbbr_sql_console=>c_sqlquery_export_mem_id.
    FREE MEMORY ID zcl_dbbr_sql_console=>c_sqlquery_export_mem_id.

    NEW zcl_dbbr_sql_console( iv_query = lv_query )->show( ).
  ENDMETHOD.

ENDCLASS.
