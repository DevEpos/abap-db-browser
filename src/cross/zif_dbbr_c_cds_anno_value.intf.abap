"! <p class="shorttext synchronized" lang="en">Value constants for CDS annotations</p>
INTERFACE zif_dbbr_c_cds_anno_value
  PUBLIC .
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Constants for 'Environment.systemField'</p>
    BEGIN OF c_environment_system_field,
      user     TYPE string VALUE '#USER',
      date     TYPE string VALUE '#SYSTEM_DATE',
      time     TYPE string VALUE '#SYSTEM_TIME',
      language TYPE string VALUE '#SYSTEM_LANGUAGE',
      client   TYPE string VALUE '#CLIENT',
    END OF c_environment_system_field.
ENDINTERFACE.
