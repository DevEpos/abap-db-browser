"! <p class="shorttext synchronized" lang="en">Util class for icons</p>
CLASS zcl_dbbr_icon_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Creates icon</p>
    "!
    "! @parameter iv_icon_name | <p class="shorttext synchronized" lang="en">Name of an icon</p>
    "! @parameter iv_text | <p class="shorttext synchronized" lang="en">Tooltip for the icon</p>
    "! @parameter iv_info | <p class="shorttext synchronized" lang="en">Info Text of the Icon</p>
    "! @parameter ev_info_text | <p class="shorttext synchronized" lang="en">Created info text of the icon</p>
    "! @parameter ev_push | <p class="shorttext synchronized" lang="en">The created icon</p>
    CLASS-METHODS create_icon
      IMPORTING
        !iv_icon_name TYPE iconname
        !iv_text      TYPE any DEFAULT space
        !iv_info      TYPE any DEFAULT space
      EXPORTING
        !ev_info_text TYPE iconquick
        !ev_push      TYPE any .

    "! <p class="shorttext synchronized" lang="en">Shows Icon Value Help</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en">The chosen icon</p>
    CLASS-METHODS show_icon_value_help
      RETURNING
        VALUE(result) TYPE icon-name .
    "! <p class="shorttext synchronized" lang="en">Create icon string with quicktip</p>
    "!
    "! @parameter iv_icon | <p class="shorttext synchronized" lang="en">The icon</p>
    "! @oarameter iv_quicktip | <p class="shorttext synchronized" lang="en">The tooltip for the icon</p>
    CLASS-METHODS create_icon_with_tip
      IMPORTING
        !iv_icon                     TYPE icon_d
        !iv_quicktip                 TYPE string
      RETURNING
        VALUE(rv_icon_with_quicktip) TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_icon_handler IMPLEMENTATION.


  METHOD create_icon.
*&---------------------------------------------------------------------*
*& Description: Creates Icon
*&---------------------------------------------------------------------*

    IF iv_icon_name = 'ICON_EQUAL'.
      ev_push = |@{ icon_equal+1(2) }\\Q{ 'Select: Is Null'(001) }@|.
      ev_info_text = |{ 'Select: Is Null'(003) }|.
      RETURN.
    ELSEIF iv_icon_name = 'ICON_NOT_EQUAL'.
      ev_push = |@{ icon_not_equal+1(2) }\\Q{ 'Select: Is Not Null'(002) }@|.
      ev_info_text = |{ 'Select: Is Not Null'(004) }|.
      RETURN.
    ELSEIF iv_icon_name = 'ICON_SELECTION'.
      ev_push = |@{ icon_selection+1(2) }\\Q{ 'Default' }@|.
      ev_info_text = |{ 'Select: No Modifier'(005) }|.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = iv_icon_name
        text                  = iv_text
        info                  = iv_info
      IMPORTING
        result                = ev_push
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      IF sy-subrc = 1.
        RAISE EXCEPTION TYPE zcx_dbbr_exception
          EXPORTING
            textid = zcx_dbbr_exception=>icon_not_existing
            msgv1  = |{ iv_icon_name }|.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ICON_CHECK'
      EXPORTING
        icon_name      = iv_icon_name
        language       = sy-langu
        button         = ' '
        status         = ' '
        message        = ' '
        function       = ' '
        textfield      = 'X'
        locked         = ' '
      IMPORTING
        icon_text      = ev_info_text
      EXCEPTIONS
        icon_not_found = 1
        OTHERS         = 2.

  ENDMETHOD.


  METHOD create_icon_with_tip.
    rv_icon_with_quicktip = |@{ iv_icon+1(2) }\\Q{ iv_quicktip }@|.
  ENDMETHOD.


  METHOD show_icon_value_help.
    CALL FUNCTION 'ICON_SHOW'
      IMPORTING
        icon_name        = result
      EXCEPTIONS
        no_icon_selected = 1
        no_object_found  = 2
        OTHERS           = 3.
  ENDMETHOD.

ENDCLASS.
