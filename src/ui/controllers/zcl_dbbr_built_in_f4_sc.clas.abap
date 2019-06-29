CLASS zcl_dbbr_built_in_f4_sc DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_generic_f4_sc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_uitb_screen_controller~call_screen REDEFINITION.

    METHODS constructor
      IMPORTING
        iv_display_mode   TYPE zdbbr_display_mode
        !iv_current_table TYPE tabname OPTIONAL
        !iv_current_field TYPE fieldname OPTIONAL.
  PROTECTED SECTION.
    METHODS save_search_help REDEFINITION.
    METHODS should_save REDEFINITION.
  PRIVATE SECTION.
    DATA mr_ui_built_in_f4 TYPE REF TO zdbbr_build_in_f4_ui_data.
    DATA mf_tabfield_provided TYPE abap_bool.

    METHODS built_in_search_help_exists
      RETURNING
        VALUE(rf_exists) TYPE boolean .
ENDCLASS.



CLASS zcl_dbbr_built_in_f4_sc IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_display_mode = iv_display_mode ).
    mt_function_exclude = VALUE #( BASE mt_function_exclude ( c_test_f4_func ) ).

    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).
    mr_ui_built_in_f4 = CAST zdbbr_build_in_f4_ui_data( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_built_in_f4 ) ).

    CLEAR: mr_ui_built_in_f4->*.

    mr_ui_built_in_f4->tabname = iv_current_table.
    mr_ui_built_in_f4->fieldname = iv_current_field.
    mr_ui_built_in_f4->perform_alpha_conversion = abap_true.

    mf_tabfield_provided = xsdbool( iv_current_table IS NOT INITIAL AND iv_current_field IS NOT INITIAL ).
  ENDMETHOD.


  METHOD built_in_search_help_exists.
*& Description: Is there an existing built in search help
*&---------------------------------------------------------------------*
    rf_exists = abap_true.
    CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
      EXPORTING
        tabname           = mr_ui_built_in_f4->search_table
        fieldname         = mr_ui_built_in_f4->search_field
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      rf_exists = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD save_search_help.
    DATA: lv_built_in_f4_id TYPE zdbbr_f4_id.

    IF NOT built_in_search_help_exists( ).
      zcx_dbbr_validation_exception=>raise_with_text( |Theres is no Value Help for the Field { mr_ui_built_in_f4->search_field }| &&
                                                      | in Table { mr_ui_built_in_f4->search_table }| ).
    ENDIF.

    IF NOT should_save( ).
      RETURN.
    ENDIF.

    IF mf_delete_existing = abap_true.
      zcl_dbbr_custom_f4_factory=>delete_f4_assignments(
          iv_tabname   = mr_ui_built_in_f4->tabname
          iv_fieldname = mr_ui_built_in_f4->fieldname
      ).
    ENDIF.

    IF zcl_dbbr_custom_f4_factory=>exists_built_in_f4(
         EXPORTING iv_tabname   = mr_ui_built_in_f4->search_table
                   iv_fieldname = mr_ui_built_in_f4->search_field
         IMPORTING ev_f4_id     = lv_built_in_f4_id ).


*.... create new assignment for the built in search help
      IF mf_tabfield_provided = abap_true.
        mf_saved = zcl_dbbr_custom_f4_factory=>update_f4_assignments(
            it_f4_assignments = VALUE #( ( entity_id = mr_ui_built_in_f4->tabname
                                           fieldname = mr_ui_built_in_f4->fieldname
                                           ref_f4_id = lv_built_in_f4_id ) )
        ).
      ENDIF.
    ELSE.
*.... Determine data type of field
      DATA(ls_search_table_field) = zcl_dbbr_dictionary_helper=>get_table_field_info(
        iv_tablename = mr_ui_built_in_f4->search_table
        iv_fieldname = mr_ui_built_in_f4->search_field
      ).
      DATA(lv_rollname) = ls_search_table_field-rollname.
      DATA(ls_built_in_type_info) = VALUE zdbbr_built_in_data_type(
          datatype = ls_search_table_field-datatype
          inttype  = ls_search_table_field-inttype
          leng     = ls_search_table_field-outputlen
      ).
      ms_f4_def = VALUE zdbbr_f4_data(
          description              = mr_ui_built_in_f4->description
          created_by               = sy-uname
          is_built_in              = abap_true
          perform_alpha_conversion = mr_ui_built_in_f4->perform_alpha_conversion
          apply_to_same_type       = mr_ui_built_in_f4->apply_to_same_type
          fields                   = VALUE #(
            ( counter                  = 1
              search_table             = mr_ui_built_in_f4->search_table
              search_field             = mr_ui_built_in_f4->search_field
              rollname                 = lv_rollname
              built_in_info            = ls_built_in_type_info
              is_search_key            = abap_true )
          )
      ).

      DATA(lv_new_f4_id) = zcl_dbbr_custom_f4_factory=>save_custom( is_f4_data = ms_f4_def ).
      IF mf_tabfield_provided = abap_true.
        zcl_dbbr_custom_f4_factory=>update_f4_assignments(
          it_f4_assignments = VALUE #( ( ref_f4_id = lv_new_f4_id
                                         entity_id = mr_ui_built_in_f4->tabname
                                         fieldname = mr_ui_built_in_f4->fieldname ) )
        ).
      ENDIF.
      mf_saved = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD should_save.
*& Description: Determination if the save action should be performed
*&---------------------------------------------------------------------*
    mf_delete_existing = abap_false.
    rf_save = abap_true.

    CHECK mf_tabfield_provided = abap_true.

    " --- check if there is already a search help for this table/fieldname
    IF zcl_dbbr_custom_f4_factory=>exists_assignment_for_tabfield(
          iv_tabname   = mr_ui_built_in_f4->tabname
          iv_fieldname = mr_ui_built_in_f4->fieldname ).

      DATA(lv_message_result) = zcl_dbbr_appl_util=>popup_to_confirm(
           iv_title        = 'Save'
           iv_query        = |There already is one or several Value Helps for this field. | &&
                             |Do you want to replace the existing Value Helps or do you want to amend them?|
           iv_text_button1 = |{ TEXT-001 }|
           iv_text_button2 = |{ TEXT-002 }|
           iv_icon_type    = 'ICON_MESSAGE_WARNING'
      ).

      CASE lv_message_result.
        WHEN '1'.
          mf_delete_existing = abap_true.
        WHEN 'A'.
          CLEAR rf_save.
        WHEN '2'.
****        " check if there is another search help which has the same search key field
****        " TODO:
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = zif_dbbr_screen_ids=>c_define_built_in_f4
        iv_report_id    = zif_dbbr_c_report_id=>main
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_f4_screen_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).
  ENDMETHOD.
ENDCLASS.
