CLASS ZCL_DBBR_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add_from_sy
      IMPORTING
        !if_newobj TYPE protnewobj
        !iv_level  TYPE protlevel .
    METHODS protocol_write .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_sprot_u TYPE sprot_u_t .
ENDCLASS.



CLASS ZCL_DBBR_log IMPLEMENTATION.


  METHOD protocol_write.
    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES
        xmsg           = mt_sprot_u
      EXCEPTIONS
        file_not_found = 1
        wrong_call     = 2
        OTHERS         = 3.                                   "#EC *

    CALL FUNCTION 'TR_FLUSH_LOG'.

  ENDMETHOD.


  METHOD add_from_sy.
* message level
*	1     do not use
*	2     errors and warnings which need action of customer
*	3     warning and success messages
*	4     additional info

    DATA ls_sprot_u TYPE sprot_u.

    APPEND VALUE sprot_u(
      level    = iv_level
      severity = sy-msgty
      langu    = sy-langu
      ag       = sy-msgid
      msgnr    = sy-msgno
      newobj   = if_newobj
      var1     = sy-msgv1
      var2     = sy-msgv2
      var3     = sy-msgv3
      var4     = sy-msgv4
    ) TO mt_sprot_u.

  ENDMETHOD.
ENDCLASS.
