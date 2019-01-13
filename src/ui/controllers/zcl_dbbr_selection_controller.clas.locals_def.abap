*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: BEGIN OF mty_salv_sort,
         columnname TYPE lvc_fname,
         position   TYPE i,
         sequence   TYPE salv_de_sort_sequence,
       END OF mty_salv_sort.

TYPES: mtt_salv_sort TYPE STANDARD TABLE OF mty_salv_sort.
