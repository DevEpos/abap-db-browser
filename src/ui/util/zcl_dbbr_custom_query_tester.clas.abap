"! <p class="shorttext synchronized" lang="en">Test execution for a custom query</p>
CLASS zcl_dbbr_custom_query_tester DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Tests the given query string</p>
    CLASS-METHODS test_query
      IMPORTING
        iv_query_string TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_custom_query_tester IMPLEMENTATION.
  METHOD test_query.
    " create and start selection controller
    DATA(lr_controller) = zcl_dbbr_selection_controller=>create_controller(
      VALUE #(
         entity_type        = zif_dbbr_c_selscreen_mode=>query
         query_string       = iv_query_string
         technical_infos    = CORRESPONDING #( zcl_dbbr_usersettings_factory=>get_settings( ) )
         exclude_function   = VALUE #(
           ( zif_dbbr_c_selection_functions=>leave_screen_with_layout )
           ( zif_dbbr_c_selection_functions=>transfer_filter_values   )
         )
       )
    ).

    lr_controller->execute_selection( ).
  ENDMETHOD.

ENDCLASS.
