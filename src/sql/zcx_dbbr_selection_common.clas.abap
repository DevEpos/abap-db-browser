CLASS zcx_dbbr_selection_common DEFINITION
  PUBLIC
  INHERITING FROM zcx_dbbr_application_exc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        text     TYPE string OPTIONAL
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        msgv1    TYPE sy-msgv1 OPTIONAL
        msgv2    TYPE sy-msgv2 OPTIONAL
        msgv3    TYPE sy-msgv3 OPTIONAL
        msgv4    TYPE sy-msgv4 OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_dbbr_selection_common IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        text     = text
        previous = previous
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4.
  ENDMETHOD.
ENDCLASS.
