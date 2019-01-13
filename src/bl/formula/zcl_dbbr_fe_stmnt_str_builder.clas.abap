CLASS ZCL_DBBR_fe_stmnt_str_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_string_builder
      IMPORTING
        is_statement      TYPE ZIF_DBBR_fe_types=>ty_statement
      RETURNING
        VALUE(rr_builder) TYPE REF TO ZIF_DBBR_stmnt_string_builder.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_FE_STMNT_STR_BUILDER IMPLEMENTATION.


  METHOD get_string_builder.
    IF is_statement-is_form_stmnt = abap_true.

      CASE is_statement-first_token_str.

        WHEN zif_dbbr_c_fe_keywords=>define_field.
          rr_builder = NEW ZCL_DBBR_fe_form_field_stb( ).

        WHEN zif_dbbr_c_fe_keywords=>define_icon.
          rr_builder = NEW ZCL_DBBR_fe_icon_stb( ).

        WHEN zif_dbbr_c_fe_keywords=>define_icon_quick.
          rr_builder = NEW ZCL_DBBR_fe_icon_tt_stb( ).

          " will be included in existing formula field definition
        WHEN zif_dbbr_c_fe_keywords=>define_description.
          rr_builder = NEW ZCL_DBBR_fe_dummy_stb( ).
      ENDCASE.

    ELSEIF is_statement-is_function_call = abap_true.

      CASE is_statement-first_token_str.

        WHEN zif_dbbr_c_fe_keywords=>set_icon_value.
          rr_builder = NEW ZCL_DBBR_fe_set_icon_stb( ).

        WHEN zif_dbbr_c_fe_keywords=>set_cell_color.
          rr_builder = NEW ZCL_DBBR_fe_set_cell_colr_stb( ).

        WHEN zif_dbbr_c_fe_keywords=>set_row_color.
          rr_builder = NEW ZCL_DBBR_fe_set_row_color_stb( ).
      ENDCASE.

    ELSE.
      rr_builder = NEW ZCL_DBBR_fe_gen_stmnt_builder( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
