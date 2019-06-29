"! <p class="shorttext synchronized" lang="en">History Manager for Selection Screen</p>
CLASS zcl_dbbr_selscreen_history DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Navigation occurred</p>
    CLASS-EVENTS navigated
      EXPORTING
        VALUE(es_history_entry) TYPE zdbbr_selscreen_history
        VALUE(ev_current_index) TYPE i OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">History was modified</p>
    CLASS-EVENTS history_modified .

    "! <p class="shorttext synchronized" lang="en">Clears navigation history</p>
    CLASS-METHODS clear_history.
    "! <p class="shorttext synchronized" lang="en">Adds new history entry</p>
    CLASS-METHODS add_new_entry
      IMPORTING
        !iv_entity      TYPE zdbbr_entity_id
        !iv_type        TYPE zdbbr_entity_type
        !iv_description TYPE ddtext .
    "! <p class="shorttext synchronized" lang="en">Deletes the given entity from the history</p>
    CLASS-METHODS delete_entity_from_history
      IMPORTING
        !iv_entity_id   TYPE zdbbr_entity_id
        !iv_entity_type TYPE zdbbr_entity_type .
    "! <p class="shorttext synchronized" lang="en">Gets the current history index</p>
    CLASS-METHODS get_current_index
      RETURNING
        VALUE(result) TYPE i .
    "! <p class="shorttext synchronized" lang="en">Retrieves all History entries</p>
    CLASS-METHODS get_history
      RETURNING
        VALUE(result) TYPE zdbbr_selscreen_history_t .
    "! <p class="shorttext synchronized" lang="en">Checks if there is a next entry</p>
    CLASS-METHODS has_next
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Checks if there is a previous entry</p>
    CLASS-METHODS has_previous
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Get Previous history entry</p>
    CLASS-METHODS navigate_back .
    "! <p class="shorttext synchronized" lang="en">Get next history entry</p>
    CLASS-METHODS navigate_forward .
    "! <p class="shorttext synchronized" lang="en">Navigate to specific history entry</p>
    CLASS-METHODS navigate_to
      IMPORTING
        !iv_index            TYPE i
        !if_force_navigation TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rf_navigated)  TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Navigates to the current history entry</p>
    CLASS-METHODS navigate_to_current
      RETURNING
        VALUE(rf_navigated) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">List of history entries of Selection screen</p>
    CLASS-DATA st_history TYPE zdbbr_selscreen_history_t .
    CLASS-DATA sv_current_index TYPE i .
    CLASS-DATA sv_history_count TYPE i .
ENDCLASS.



CLASS zcl_dbbr_selscreen_history IMPLEMENTATION.

  METHOD clear_history.
    IF st_history IS NOT INITIAL AND sv_current_index > 0 AND sv_current_index <= sv_history_count.
      DATA(ls_history) = st_history[ sv_current_index ].
    ENDIF.

    CLEAR: st_history,
           sv_current_index,
           sv_history_count.

    IF ls_history IS NOT INITIAL.
      st_history = value #( ( ls_history ) ).
      sv_current_index = 1.
      sv_history_count = 1.
    ENDIF.

    RAISE EVENT history_modified.
  ENDMETHOD.

  METHOD add_new_entry.
*.. Check if the entry to be inserted is already at the first position
    IF st_history IS NOT INITIAL.
      ASSIGN st_history[ 1 ] TO FIELD-SYMBOL(<ls_history>).
      IF <ls_history>-entity_id = iv_entity AND
         <ls_history>-entity_type = iv_type.
        RETURN.
      ENDIF.
    ENDIF.

    INSERT VALUE #(
      entity_id   = iv_entity
      entity_type = iv_type
      description = iv_description
    ) INTO st_history INDEX 1.

    sv_current_index = 1.
    sv_history_count = lines( st_history ).

    RAISE EVENT history_modified.
  ENDMETHOD.


  METHOD delete_entity_from_history.
    DELETE st_history WHERE entity_id = iv_entity_id
                        AND entity_type = iv_entity_type.

    IF st_history IS INITIAL.
      CLEAR sv_current_index.
    ELSEIF sv_current_index > lines( st_history ).
      sv_current_index = lines( st_history ).
    ENDIF.

    RAISE EVENT history_modified.
  ENDMETHOD.


  METHOD get_current_index.
    result = sv_current_index.
  ENDMETHOD.


  METHOD get_history.
    result = st_history.
  ENDMETHOD.


  METHOD has_next.
    result = abap_true.
    IF sv_history_count = 0 OR
       sv_current_index = 1.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD has_previous.
    result = abap_true.
    IF sv_history_count = 1 OR
       sv_current_index + 1 > sv_history_count.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD navigate_back.
    CHECK has_previous( ).

    sv_current_index = sv_current_index + 1.

    RAISE EVENT navigated
      EXPORTING
        es_history_entry = st_history[ sv_current_index ]
        ev_current_index = sv_current_index.
  ENDMETHOD.


  METHOD navigate_forward.
    CHECK has_next( ).

    sv_current_index = sv_current_index - 1.

    RAISE EVENT navigated
      EXPORTING
        es_history_entry = st_history[ sv_current_index ]
        ev_current_index = sv_current_index.
  ENDMETHOD.


  METHOD navigate_to.
    CHECK: sv_history_count > 0,
           iv_index > 0,
           iv_index <= sv_history_count.

    IF if_force_navigation = abap_false.
      CHECK iv_index <> sv_current_index.
    ENDIF.

    sv_current_index = iv_index.

    rf_navigated = abap_true.

    RAISE EVENT navigated
      EXPORTING
        es_history_entry = st_history[ sv_current_index ]
        ev_current_index = sv_current_index.
  ENDMETHOD.


  METHOD navigate_to_current.
    CHECK sv_current_index > 0.

    rf_navigated = navigate_to(
        iv_index            = sv_current_index
        if_force_navigation = abap_true
    ).
  ENDMETHOD.
ENDCLASS.
