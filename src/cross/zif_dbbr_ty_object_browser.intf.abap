"! <p class="shorttext synchronized" lang="en">Types for object browser</p>
INTERFACE zif_dbbr_ty_object_browser
  PUBLIC .

  TYPES:
    BEGIN OF ty_search_option_values,
      option      TYPE string,
      value_range TYPE RANGE OF string,
    END OF ty_search_option_values.

  TYPES: tt_search_option_values TYPE STANDARD TABLE OF ty_search_option_values WITH KEY option.
  TYPES:
    BEGIN OF ty_search,
      options TYPE tt_search_option_values,
      query   TYPE string,
    END OF ty_search.
ENDINTERFACE.
