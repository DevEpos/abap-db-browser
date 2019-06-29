"! <p class="shorttext synchronized" lang="en">Type for a Join condition</p>
INTERFACE zif_dbbr_c_join_cond_type
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en">Filter condition</p>
  "! Examples: <br/>
  "! <ul>
  "! <li>field = '4'</li>
  "! <li>field = SY-LANGU</li>
  "! </ul>
  CONSTANTS filter TYPE char1 VALUE '1'.
  "! <p class="shorttext synchronized" lang="en">Field condition</p>
  "! Example: <br/>
  "! <ul>
  "! <li>field1 = field2</li>
  "! </ul>
  CONSTANTS field TYPE char1 VALUE '2'.
ENDINTERFACE.
