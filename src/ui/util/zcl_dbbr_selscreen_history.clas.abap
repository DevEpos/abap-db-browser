class ZCL_DBBR_SELSCREEN_HISTORY definition
  public
  final
  create public .

public section.

  class-events NAVIGATED
    exporting
      value(ES_HISTORY_ENTRY) type ZDBBR_SELSCREEN_HISTORY
      value(EV_CURRENT_INDEX) type I optional .
  class-events HISTORY_MODIFIED .

  class-methods ADD_NEW_ENTRY
    importing
      !IV_ENTITY type ZDBBR_ENTITY_ID
      !IV_TYPE type ZDBBR_ENTITY_TYPE
      !IV_DESCRIPTION type DDTEXT .
  class-methods DELETE_ENTITY_FROM_HISTORY
    importing
      !IV_ENTITY_ID type ZDBBR_ENTITY_ID
      !IV_ENTITY_TYPE type ZDBBR_ENTITY_TYPE .
  class-methods GET_CURRENT_INDEX
    returning
      value(RESULT) type I .
  class-methods GET_HISTORY
    returning
      value(RESULT) type ZDBBR_SELSCREEN_HISTORY_T .
  class-methods HAS_NEXT
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods HAS_PREVIOUS
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods NAVIGATE_BACK .
  class-methods NAVIGATE_FORWARD .
  class-methods NAVIGATE_TO
    importing
      !IV_INDEX type I
      !IF_FORCE_NAVIGATION type ABAP_BOOL optional
    returning
      value(RF_NAVIGATED) type ABAP_BOOL .
  class-methods NAVIGATE_TO_CURRENT
    returning
      value(RF_NAVIGATED) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  class-data ST_HISTORY type ZDBBR_SELSCREEN_HISTORY_T .
  class-data SV_CURRENT_INDEX type I .
  class-data SV_HISTORY_COUNT type I .
ENDCLASS.



CLASS ZCL_DBBR_SELSCREEN_HISTORY IMPLEMENTATION.


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
    IF sv_history_count = 0 or
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
