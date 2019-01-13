CLASS zcl_dbbr_icon_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create_icon
      IMPORTING
        !iv_icon_name TYPE iconname
        !iv_text      TYPE any DEFAULT space
        !iv_info      TYPE any DEFAULT space
      EXPORTING
        !ev_info_text TYPE iconquick
        !ev_push      TYPE any .
    CLASS-METHODS get_sign_icon_name
      IMPORTING
        !iv_sign            TYPE any
        !iv_option          TYPE any
      RETURNING
        VALUE(rv_icon_name) TYPE iconname .
    CLASS-METHODS create_sel_option_icon
      IMPORTING
        !iv_icon_name        TYPE any
        !iv_sign             TYPE any
      RETURNING
        VALUE(rs_sel_option) TYPE se16n_sel_option .
    CLASS-METHODS show_icon_value_help
      RETURNING
        VALUE(result) TYPE icon-name .
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
      ev_push = |@{ icon_equal+1(2) }\\Q{ 'Is Null'(001) }@|.
      ev_info_text = |{ 'Select: Is Null'(003) }|.
      RETURN.
    ELSEIF iv_icon_name = 'ICON_NOT_EQUAL'.
      ev_push = |@{ icon_not_equal+1(2) }\\Q{ 'Is Not Null'(002) }@|.
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


  METHOD create_sel_option_icon.
*& Description: Creates select option icon line
*&---------------------------------------------------------------------*
    rs_sel_option-option = iv_icon_name.
    rs_sel_option-sign = iv_sign.

    DATA(lv_icon_name) = get_sign_icon_name(
      iv_sign      = iv_sign
      iv_option    = iv_icon_name
    ).

    CHECK: lv_icon_name IS NOT INITIAL.

    create_icon(
      EXPORTING
        iv_icon_name = lv_icon_name
      IMPORTING
        ev_info_text = DATA(lv_icon_quickinfo)
        ev_push      = rs_sel_option-icon
    ).

    rs_sel_option-text = lv_icon_quickinfo.

  ENDMETHOD.


  METHOD get_sign_icon_name.
*&---------------------------------------------------------------------*
*& Description: Returns icon name
*&---------------------------------------------------------------------*

    CASE iv_option.
      WHEN zif_dbbr_global_consts=>gc_options-default.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-default.
      WHEN zif_dbbr_global_consts=>gc_options-bt.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-bt.
      WHEN zif_dbbr_global_consts=>gc_options-nb.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-nb.
      WHEN zif_dbbr_global_consts=>gc_options-eq.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-eq.
      WHEN zif_dbbr_global_consts=>gc_options-ne.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-ne.
      WHEN zif_dbbr_global_consts=>gc_options-gt.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-gt.
      WHEN zif_dbbr_global_consts=>gc_options-lt.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-lt.
      WHEN zif_dbbr_global_consts=>gc_options-ge.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-ge.
      WHEN zif_dbbr_global_consts=>gc_options-le.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-le.
      WHEN zif_dbbr_global_consts=>gc_options-cp.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-cp.
      WHEN zif_dbbr_global_consts=>gc_options-np.
        rv_icon_name = zif_dbbr_global_consts=>gc_icons-np.
      WHEN zif_dbbr_global_consts=>gc_options-is_null.
        rv_icon_name = 'ICON_EQUAL'.
      WHEN zif_dbbr_global_consts=>gc_options-is_not_null.
        rv_icon_name = 'ICON_NOT_EQUAL'.
    ENDCASE.

    IF iv_sign = zif_dbbr_global_consts=>gc_options-i OR
       iv_sign = space.
      REPLACE '#' WITH zif_dbbr_global_consts=>gc_icons-green INTO rv_icon_name.
    ELSE.
      REPLACE '#' WITH zif_dbbr_global_consts=>gc_icons-red INTO rv_icon_name.
    ENDIF.


  ENDMETHOD.


  METHOD show_icon_value_help.
    CALL FUNCTION 'ICON_SHOW'
      IMPORTING
        icon_name        = result
      EXCEPTIONS
        no_icon_selected = 1
        no_object_found  = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
