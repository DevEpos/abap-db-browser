INTERFACE zif_dbbr_favmenu_evt_handler
  PUBLIC .
  "! Add new Favorite to the tree
  "! @parameter iv_favorite | the id of the favorite
  "! @parameter iv_description | the description for the favorite
  "! @parameter iv_type | the type of the favorite
  METHODS add_favorite
    IMPORTING
      iv_favorite    TYPE char30
      iv_description TYPE ddtext
      iv_type        TYPE ZSAT_FAVMENU_TYPE.
ENDINTERFACE.
