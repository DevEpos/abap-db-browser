CLASS zcl_dbbr_selscr_nav_events DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-EVENTS entity_chosen
      EXPORTING
        VALUE(ev_entity_id) TYPE tabname
        VALUE(ev_entity_type) TYPE zdbbr_entity_type
        VALUE(ef_force_loading) TYPE abap_bool.
    CLASS-EVENTS variant_entry_chosen
      EXPORTING
        VALUE(ev_entity_id) TYPE tabname
        VALUE(ev_entity_type) TYPE zdbbr_entity_type
        VALUE(ev_variant_id) TYPE zdbbr_variant_id
        VALUE(ef_go_to_result) TYPE abap_bool OPTIONAL .
    CLASS-EVENTS favtree_event
      EXPORTING
        VALUE(er_handler) TYPE REF TO zif_dbbr_favmenu_evt_handler .
    CLASS-EVENTS object_search
      EXPORTING
        VALUE(ev_object_type) TYPE zdbbr_obj_browser_mode
        VALUE(ev_search_query) TYPE string
        VALUE(ef_close_popup) TYPE abap_bool.
    CLASS-EVENTS request_object_search
      EXPORTING
        VALUE(ev_object_type) TYPE zdbbr_obj_browser_mode
        VALUE(ev_search_query) TYPE string
        VALUE(ef_close_popup) TYPE abap_bool.
    CLASS-EVENTS close_object_search_modal.
    CLASS-EVENTS display_object_list
      EXPORTING
        VALUE(ev_entity_id) TYPE zdbbr_entity_id
        VALUE(ev_entity_type) TYPE zdbbr_entity_type.
    "! <p class="shorttext synchronized" lang="en">Request next view in object navigator</p>
    CLASS-EVENTS goto_next_view_in_objnav.
    "! <p class="shorttext synchronized" lang="en">Raise ENTITY_CHOSEN event</p>
    CLASS-METHODS raise_entity_chosen
      IMPORTING
        iv_entity_id     TYPE tabname
        iv_entity_type   TYPE zdbbr_entity_type
        if_force_loading TYPE abap_bool OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Raise VARIANT_ENTRY_CHOSEN event</p>
    CLASS-METHODS raise_variant_entry_chosen
      IMPORTING
        iv_entity_id    TYPE tabname
        iv_entity_type  TYPE zdbbr_entity_type
        iv_variant_id   TYPE zdbbr_variant_id
        if_go_to_result TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Raise FAVTREE_EVENT event</p>
    CLASS-METHODS raise_favtree_event
      IMPORTING
        ir_handler TYPE REF TO zif_dbbr_favmenu_evt_handler.
    "! <p class="shorttext synchronized" lang="en">Raise event DISPLAY_OBJECT_LIST</p>
    "!
    "! @parameter iv_entity_id | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_entity_type | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS raise_display_object_list
      IMPORTING
        iv_entity_id   TYPE zdbbr_entity_id
        iv_entity_type TYPE zdbbr_entity_type.
    "! <p class="shorttext synchronized" lang="en">Raise event OBJECT_SEARCH</p>
    "!
    "! @parameter iv_object_type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_search_query | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS raise_object_search
      IMPORTING
        iv_object_type  TYPE zdbbr_obj_browser_mode
        iv_search_query TYPE string
        if_close_popup  TYPE abap_bool DEFAULT abap_true.
    "! <p class="shorttext synchronized" lang="en">Raise event REQUEST_OBJECT_SEARCH</p>
    "!
    "! @parameter iv_object_type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_search_query | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter if_close_on_success | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS raise_request_object_search
      IMPORTING
        iv_object_type      TYPE zdbbr_obj_browser_mode
        iv_search_query     TYPE string
        if_close_on_success TYPE abap_bool DEFAULT abap_true.
    "! <p class="shorttext synchronized" lang="en">Raise event GOTO_NEXT_VIEW_IN_OBJNAV</p>
    "!
    CLASS-METHODS raise_goto_nxt_view_in_objnav.
    "! <p class="shorttext synchronized" lang="en">Raise event CLOSE_OBJECT_SEARCH_MODAL</p>
    "!
    CLASS-METHODS raise_close_object_srch_dialog.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_dbbr_selscr_nav_events IMPLEMENTATION.


  METHOD raise_entity_chosen.
    RAISE EVENT entity_chosen
      EXPORTING
        ev_entity_id   = iv_entity_id
        ev_entity_type = iv_entity_type
        ef_force_loading = if_force_loading.
  ENDMETHOD.


  METHOD raise_favtree_event.
    RAISE EVENT favtree_event
      EXPORTING
        er_handler = ir_handler.
  ENDMETHOD.


  METHOD raise_variant_entry_chosen.
    RAISE EVENT variant_entry_chosen
      EXPORTING
        ev_entity_id    = iv_entity_id
        ev_entity_type  = iv_entity_type
        ev_variant_id   = iv_variant_id
        ef_go_to_result = if_go_to_result.
  ENDMETHOD.

  METHOD raise_display_object_list.
    RAISE EVENT display_object_list
      EXPORTING
        ev_entity_id   = iv_entity_id
        ev_entity_type = iv_entity_type.
  ENDMETHOD.

  METHOD raise_goto_nxt_view_in_objnav.
    RAISE EVENT goto_next_view_in_objnav.
  ENDMETHOD.

  METHOD raise_object_search.
    RAISE EVENT object_search
      EXPORTING
        ev_object_type  = iv_object_type
        ev_search_query = iv_search_query
        ef_close_popup  = if_close_popup.
  ENDMETHOD.

  METHOD raise_request_object_search.
    RAISE EVENT request_object_search
      EXPORTING
        ev_object_type  = iv_object_type
        ev_search_query = iv_search_query
        ef_close_popup  = if_close_on_success.
  ENDMETHOD.

  METHOD raise_close_object_srch_dialog.
    RAISE EVENT close_object_search_modal.
  ENDMETHOD.

ENDCLASS.
