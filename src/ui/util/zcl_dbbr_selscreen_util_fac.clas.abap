"! <p class="shorttext synchronized" lang="en">Factory to manage Selection Screen Util instances</p>
CLASS zcl_dbbr_selscreen_util_fac DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Create new Util instance</p>
    "!
    "! @parameter ir_data | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rr_util | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS create_util_instance
      IMPORTING
        ir_data        TYPE REF TO zcl_dbbr_selscreen_data
      RETURNING
        VALUE(rr_util) TYPE REF TO zcl_dbbr_selscreen_util.
    "! <p class="shorttext synchronized" lang="en">Get current util instance</p>
    "!
    "! @parameter rr_util | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_util_instance
      RETURNING
        VALUE(rr_util) TYPE REF TO zcl_dbbr_selscreen_util.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gr_instance TYPE REF TO zcl_dbbr_selscreen_util.
ENDCLASS.



CLASS zcl_dbbr_selscreen_util_fac IMPLEMENTATION.

  METHOD create_util_instance.
    gr_instance = COND #(
      WHEN ir_data->is_cds_mode( ) THEN
        CAST #( NEW zcl_dbbr_cds_selscreen_util( ir_data ) )
      WHEN ir_data->is_query_mode( ) THEN
        CAST #( NEW zcl_dbbr_query_selscreen_util( ir_data ) )
      ELSE
        CAST #( NEW zcl_dbbr_table_selscreen_util( ir_data ) )
    ).

    rr_util = gr_instance.
  ENDMETHOD.

  METHOD get_util_instance.
    rr_util = gr_instance.
  ENDMETHOD.

ENDCLASS.
