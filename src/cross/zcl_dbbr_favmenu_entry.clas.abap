CLASS ZCL_DBBR_favmenu_entry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_favmenu_data TYPE ZDBBR_favmenu.
    methods get_favmenu_data
      returning
        value(rs_favmenu_entry) type ZDBBR_favmenu.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ms_favmenu_data TYPE ZDBBR_favmenu.
ENDCLASS.



CLASS ZCL_DBBR_favmenu_entry IMPLEMENTATION.
  METHOD constructor.
    ms_favmenu_data = is_favmenu_data.
  ENDMETHOD.

  METHOD get_favmenu_data.
    rs_favmenu_entry = ms_favmenu_data.
  ENDMETHOD.
ENDCLASS.
