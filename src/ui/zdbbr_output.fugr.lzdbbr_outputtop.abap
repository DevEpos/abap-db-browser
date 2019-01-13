FUNCTION-POOL zdbbr_output.                "MESSAGE-ID ..

* INCLUDE LZDBBR_SELECTD...                 " Local class definition

TABLES: sscrfields.

DATA: gr_controller TYPE REF TO zif_uitb_screen_controller,
      ok_code       TYPE sy-ucomm.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" dynamic functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
DATA: userfunc01 TYPE smp_dyntxt,
      userfunc02 TYPE smp_dyntxt,
      userfunc03 TYPE smp_dyntxt.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SELECTION SCREEN DEFINITIONS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
