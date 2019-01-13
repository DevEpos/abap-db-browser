FUNCTION-POOL ZDBBR_favorite_menu.         "MESSAGE-ID ..

tables: sscrfields.

DATA: ok_code TYPE sy-ucomm.

**********************************************************************
*** Global variables
**********************************************************************
DATA: gs_favmode TYPE ZDBBR_browser_mode_data.

**********************************************************************
*** controller references
**********************************************************************
DATA: gr_export_fav_controller type ref to ZCL_DBBR_export_favmenu_ctrl
      .


**********************************************************************
*** Selection screen definitions
**********************************************************************
selection-screen begin of screen 1100 title text-s01.
selection-screen begin of block options NO INTERVALS.
PARAMETERs: p_xfglb type boolean as checkbox,
            p_xfprv type boolean as checkbox.
selection-screen end of block options.
SELECTION-SCREEN end of screen 1100.
