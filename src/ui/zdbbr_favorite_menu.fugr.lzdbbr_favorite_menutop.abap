FUNCTION-POOL zdbbr_favorite_menu.         "MESSAGE-ID ..

TABLES: sscrfields.

DATA: ok_code TYPE sy-ucomm.

**********************************************************************
*** Global variables
**********************************************************************
DATA: gs_favmode TYPE zdbbr_browser_mode_data.

**********************************************************************
*** controller references
**********************************************************************
DATA: gr_export_fav_controller TYPE REF TO zcl_dbbr_export_favmenu_ctrl
      .

**********************************************************************
*** Selection screen definitions
**********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 1100 TITLE TEXT-s01.
SELECTION-SCREEN BEGIN OF BLOCK options NO INTERVALS.
PARAMETERS: p_xfglb TYPE boolean AS CHECKBOX,
            p_xfprv TYPE boolean AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK options.
SELECTION-SCREEN END OF SCREEN 1100.
