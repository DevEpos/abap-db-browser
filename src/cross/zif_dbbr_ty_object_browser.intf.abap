"! <p class="shorttext synchronized" lang="en">Types for object browser</p>
INTERFACE zif_dbbr_ty_object_browser
  PUBLIC .

  TYPES:
    BEGIN OF ty_s_value_range,
      sign    TYPE ddsign,
      sign2   TYPE ddsign,
      option  TYPE ddoption,
      option2 TYPE ddoption,
      low     TYPE string,
      high    TYPE string,
    END OF ty_s_value_range.
  TYPES: ty_t_value_range TYPE STANDARD TABLE OF ty_s_value_range WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_search_option_values,
      option      TYPE string,
      value_range TYPE ty_t_value_range,
    END OF ty_search_option_values.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Search engine parameters</p>
    BEGIN OF ty_s_search_engine_params,
      use_and_cond_for_options TYPE abap_bool,
      with_api_state           TYPE abap_bool,
      get_all                  TYPE abap_bool,
    END OF ty_s_search_engine_params.

  TYPES: tt_search_option_values TYPE STANDARD TABLE OF ty_search_option_values WITH KEY option.
  TYPES:
    BEGIN OF ty_search,
      options TYPE tt_search_option_values,
      query   TYPE string,
    END OF ty_search.
ENDINTERFACE.
