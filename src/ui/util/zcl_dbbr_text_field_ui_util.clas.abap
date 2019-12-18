"! <p class="shorttext synchronized" lang="en">Text Field Util for UI</p>
CLASS zcl_dbbr_text_field_ui_util DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_selection_util TYPE REF TO zcl_dbbr_selection_util.
    "! <p class="shorttext synchronized" lang="en">Adds text field columns for selected columns</p>
    METHODS add_text_field_columns.

    "! <p class="shorttext synchronized" lang="en">Manages text field columns</p>
    METHODS manage_text_field_columns.
    "! <p class="shorttext synchronized" lang="en">Adds 'Add Text Field' function to context menu</p>
    "!
    "! @parameter it_selected_cols | <p class="shorttext synchronized" lang="en">List of selected columns</p>
    "! @parameter it_fieldcat      | <p class="shorttext synchronized" lang="en">ALV Field Catalog</p>
    "! @parameter ct_menu_entries  | <p class="shorttext synchronized" lang="en">List of context menu entries</p>
    METHODS add_text_field_column_func
      IMPORTING
        !it_selected_cols TYPE lvc_t_col
        !it_fieldcat      TYPE lvc_t_fcat
      CHANGING
        !ct_menu_entries  TYPE sctx_entrytab .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_selection_util TYPE REF TO zcl_dbbr_selection_util.

    "! <p class="shorttext synchronized" lang="en">Updates text fields in ALV</p>
    "!
    "! @parameter it_fields | <p class="shorttext synchronized" lang="en"></p>
    METHODS update_field_status
      IMPORTING
        it_fields TYPE zif_dbbr_ty_global=>ty_t_text_field.
ENDCLASS.



CLASS ZCL_DBBR_TEXT_FIELD_UI_UTIL IMPLEMENTATION.


  METHOD add_text_field_columns.
    DATA: lf_new_text_field TYPE abap_bool.
    DATA: lt_new_fields TYPE TABLE OF REF TO zdbbr_tabfield_info_ui.

    DATA(lo_alv_grid) = mo_selection_util->mo_alv_grid.
    DATA(lo_tabfields) = mo_selection_util->mo_tabfields.

*... Get columns for which text columns should be added
    lo_alv_grid->get_selected_columns( IMPORTING et_index_columns = DATA(lt_cols) ).
    lo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    DATA(lt_text_field_config) = VALUE zif_dbbr_ty_global=>ty_t_text_field( ).

    LOOP AT lt_cols ASSIGNING FIELD-SYMBOL(<ls_col>).
      DATA(lr_field) = mo_selection_util->mo_tabfields->get_field_ref_by_alv_name( iv_alv_fieldname = <ls_col>-fieldname ).
      CHECK lr_field->has_text_field = abap_true.

      TRY.
          DATA(lr_text_field) = lo_tabfields->get_field_ref(
              iv_tabname_alias = lr_field->tabname_alias
              iv_fieldname     = lr_field->fieldname
              if_is_text_field = abap_true
          ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      ASSIGN lt_fieldcat[ fieldname = <ls_col>-fieldname ] TO FIELD-SYMBOL(<ls_field>).
      ASSIGN lt_fieldcat[ fieldname = lr_text_field->alv_fieldname ] TO FIELD-SYMBOL(<ls_existing_text_field>).

*.... check if it is already existing in the field catalog and only needs to be made visible
      IF sy-subrc <> 0.
        lf_new_text_field = abap_true.
        lt_new_fields = VALUE #( BASE lt_new_fields ( lr_text_field ) ).
*...... Perform steps to make field visible and fill it with data
      ENDIF.

      lr_text_field->output_active = abap_true.
      IF lr_text_field->output_order IS INITIAL.
        lr_text_field->output_order = lr_field->output_order.
      ENDIF.

      lt_text_field_config = VALUE #( BASE lt_text_field_config
       ( alv_fieldname = lr_text_field->alv_fieldname
         visible       = abap_true
         new_field     = lf_new_text_field )
      ).
    ENDLOOP.

    update_field_status( it_fields = lt_text_field_config ).
  ENDMETHOD.


  METHOD add_text_field_column_func.
    DATA: lf_add_text_fields TYPE abap_bool.

    DATA(lo_tabfields) = mo_selection_util->mo_tabfields.
*.. Check if selected columns have associated text field

    LOOP AT it_selected_cols ASSIGNING FIELD-SYMBOL(<ls_col>).
      TRY.
          DATA(lr_field) = lo_tabfields->get_field_ref_by_alv_name( iv_alv_fieldname = <ls_col>-fieldname ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
      CHECK lr_field->has_text_field = abap_true.

*.... Check if the text field is invisible
      TRY.
          DATA(lr_text_field) = lo_tabfields->get_field_ref(
              iv_tabname_alias = lr_field->tabname_alias
              iv_fieldname     = lr_field->fieldname
              if_is_text_field = abap_true
          ).
          ASSIGN it_fieldcat[ fieldname = lr_text_field->alv_fieldname ] TO FIELD-SYMBOL(<ls_text_field>).
          IF sy-subrc = 0.
            CHECK <ls_text_field>-no_out = abap_true.
          ENDIF.
          lf_add_text_fields = abap_true.
          EXIT.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDLOOP.

    IF lf_add_text_fields = abap_true.
      INSERT VALUE #(
        type  = sctx_c_type_function
        fcode = zif_dbbr_c_selection_functions=>add_text_field
        text  = 'Add Text Field'(010)
      ) INTO ct_menu_entries INDEX 1.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mo_selection_util = io_selection_util.
  ENDMETHOD.


  METHOD manage_text_field_columns.
    mo_selection_util->mo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    DATA(lo_text_field_col_selector) = NEW zcl_dbbr_text_field_selector(
      io_tabfields = mo_selection_util->mo_tabfields
      it_fieldcat  = lt_fieldcat
    ).

    DATA(lt_text_field_status) = lo_text_field_col_selector->get_text_fields( ).
    CHECK lt_text_field_status IS NOT INITIAL.

*.. Check if fields to be made visible are
    LOOP AT lt_text_field_status ASSIGNING FIELD-SYMBOL(<ls_text_field>) WHERE visible = abap_true.
      <ls_text_field>-new_field = xsdbool( NOT line_exists( lt_fieldcat[ fieldname = <ls_text_field>-alv_fieldname ] ) ).
    ENDLOOP.

    update_field_status( lt_text_field_status ).
  ENDMETHOD.


  METHOD update_field_status.
    DATA(lo_alv_grid) = mo_selection_util->mo_alv_grid.
    DATA(lo_tabfields) = mo_selection_util->mo_tabfields.
    lo_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    IF line_exists( it_fields[ new_field = abap_true ] ).
      DATA(lf_reset_alv_table) = abap_true.
*      CLEAR mo_selection_util->mt_add_texts.
      zcl_dbbr_addtext_helper=>prepare_text_fields(
        EXPORTING ir_fields    = lo_tabfields
        CHANGING  ct_add_texts = mo_selection_util->mt_add_texts
      ).
    ENDIF.

*.. Update field catalog with new text fields
    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF <ls_field>-new_field = abap_true.
        DATA(lr_new_field) = lo_tabfields->get_field_ref_by_alv_name( <ls_field>-alv_fieldname ).
        DATA(ls_text_fieldcat) = mo_selection_util->create_text_fieldcat_entry( lr_new_field ).
        DATA(lv_ref_field_index) = line_index( lt_fieldcat[ fieldname = lr_new_field->reference_alv_fieldname ] ) + 1.
        INSERT ls_text_fieldcat INTO lt_fieldcat INDEX lv_ref_field_index.
      ELSE.
        lt_fieldcat[
            fieldname = <ls_field>-alv_fieldname
        ]-no_out = COND #( WHEN <ls_field>-visible = abap_true THEN abap_false ELSE abap_true ).
      ENDIF.
    ENDLOOP.

    IF lf_reset_alv_table = abap_true.
      mo_selection_util->create_dynamic_table( ).
      mo_selection_util->get_alv_util( )->update_result_table_ref( exporting ir_t_data = mo_selection_util->mr_t_data ).

      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
        CLEAR: <ls_fcat>-col_pos.
      ENDLOOP.
      mo_selection_util->mt_temp_fieldcat = lt_fieldcat.

      mo_selection_util->refresh_selection( if_reset_table_in_alv = lf_reset_alv_table ).

    ELSE.
      lo_alv_grid->set_frontend_fieldcatalog( lt_fieldcat ).
      lo_alv_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
