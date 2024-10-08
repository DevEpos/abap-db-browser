"! <p class="shorttext synchronized">Loads Variants into Selection Screen</p>
INTERFACE zif_dbbr_variant_loader
  PUBLIC.

  "! <p class="shorttext synchronized">Start loading the variant</p>
  METHODS load
    IMPORTING
      if_no_message     TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(rs_variant) TYPE zdbbr_variant_info.
ENDINTERFACE.
