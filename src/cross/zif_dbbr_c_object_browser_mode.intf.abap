"! <p class="shorttext synchronized">Mode for Object Browser</p>
INTERFACE zif_dbbr_c_object_browser_mode
  PUBLIC.
  CONSTANTS package TYPE zdbbr_obj_browser_mode VALUE 'P'.
  CONSTANTS cds_view TYPE zdbbr_obj_browser_mode VALUE 'C'.
  CONSTANTS database_table TYPE zdbbr_obj_browser_mode VALUE 'T'.
  CONSTANTS database_view TYPE zdbbr_obj_browser_mode VALUE 'V'.
  CONSTANTS query TYPE zdbbr_obj_browser_mode VALUE 'Q'.
ENDINTERFACE.
