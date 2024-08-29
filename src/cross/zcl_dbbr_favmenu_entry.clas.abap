CLASS zcl_dbbr_favmenu_entry DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_favmenu_data TYPE zdbbr_favmenu.

    METHODS get_favmenu_data
      RETURNING
        VALUE(rs_favmenu_entry) TYPE zdbbr_favmenu.

  PRIVATE SECTION.
    DATA ms_favmenu_data TYPE zdbbr_favmenu.
ENDCLASS.


CLASS zcl_dbbr_favmenu_entry IMPLEMENTATION.
  METHOD constructor.
    ms_favmenu_data = is_favmenu_data.
  ENDMETHOD.

  METHOD get_favmenu_data.
    rs_favmenu_entry = ms_favmenu_data.
  ENDMETHOD.
ENDCLASS.
