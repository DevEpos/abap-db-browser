INTERFACE zif_dbbr_search_control
  PUBLIC .


  INTERFACES zif_uitb_disposable .

  ALIASES dispose
    FOR zif_uitb_disposable~dispose .

  EVENTS entry_chosen
    EXPORTING
      VALUE(ev_entity_id)   TYPE ZSAT_ENTITY_ID
      VALUE(ev_entity_type) TYPE ZSAT_ENTITY_TYPE
      VALUE(ev_action)      TYPE c OPTIONAL.

  METHODS set_max_hits
    IMPORTING
      !iv_max_hits TYPE int2 .
ENDINTERFACE.
