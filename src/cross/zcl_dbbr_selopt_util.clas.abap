"! <p class="shorttext synchronized" lang="en">Utility regarding Select Options</p>
CLASS zcl_dbbr_selopt_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-METHODS get_option_icon
      IMPORTING
        !iv_sign            TYPE ddsign
        !iv_option          TYPE ddoption
      RETURNING
        VALUE(rv_icon_name) TYPE iconname .
    "! <p class="shorttext synchronized" lang="en">Choose select option</p>
    CLASS-METHODS choose_sel_option
      IMPORTING
        if_allow_null           TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rs_chosen_option) TYPE zif_dbbr_ty_global=>ty_s_sel_option.
    CLASS-METHODS create_option_icon
      IMPORTING
        iv_option      TYPE ddoption
        iv_sign        TYPE ddsign
      RETURNING
        VALUE(rv_icon) TYPE zdbbr_button.

    "! <p class="shorttext synchronized" lang="en">Returns controlling params for option</p>
    "!
    "! @parameter iv_option | <p class="shorttext synchronized" lang="en">Option of Select-Option</p>
    "! @parameter rs_control | <p class="shorttext synchronized" lang="en">Controlling parameters for option</p>
    CLASS-METHODS get_selopt_control
      IMPORTING
        iv_option         TYPE ddoption
      RETURNING
        VALUE(rs_control) TYPE zdbbr_selopt_control.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_option_icons,
        default(50) VALUE 'ICON_SELECTION' ##no_text,
        eq(50)      VALUE 'ICON_EQUAL_#' ##no_text,
        ne(50)      VALUE 'ICON_NOT_EQUAL_#' ##no_text,
        gt(50)      VALUE 'ICON_GREATER_#' ##no_text,
        lt(50)      VALUE 'ICON_LESS_#' ##no_text,
        ge(50)      VALUE 'ICON_GREATER_EQUAL_#' ##no_text,
        le(50)      VALUE 'ICON_LESS_EQUAL_#' ##no_text,
        bt(50)      VALUE 'ICON_INTERVAL_INCLUDE_#' ##no_text,
        nb(50)      VALUE 'ICON_INTERVAL_EXCLUDE_#' ##no_text,
        cp(50)      VALUE 'ICON_PATTERN_INCLUDE_#' ##no_text,
        np(50)      VALUE 'ICON_PATTERN_EXCLUDE_#' ##no_text,
        green(5)    VALUE 'GREEN' ##no_text,
        red(3)      VALUE 'RED' ##no_text,
      END OF c_option_icons .
    CLASS-DATA gt_i_options TYPE zif_dbbr_ty_global=>ty_t_sel_option.
    CLASS-DATA gt_e_options TYPE zif_dbbr_ty_global=>ty_t_sel_option.
    CLASS-DATA gt_selopt_control TYPE zdbbr_selopt_control_itab.

    CLASS-METHODS get_opt_icon_info
      IMPORTING
        iv_option      TYPE ddoption
        iv_sign        TYPE ddsign
      RETURNING
        VALUE(rs_icon) TYPE zif_dbbr_ty_global=>ty_s_sel_option.
ENDCLASS.



CLASS zcl_dbbr_selopt_util IMPLEMENTATION.

  METHOD class_constructor.
    gt_i_options = VALUE #(
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>default
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>between
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>contains_pattern
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>not_contains_pattern
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>equals
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>not_between
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>not_equals
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>greater_than
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>lesser_than
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>greater_equal
                           iv_sign   = zif_sat_c_options=>including ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>lesser_equal
                           iv_sign   = zif_sat_c_options=>including ) )
    ).
    gt_e_options = VALUE #(
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>between
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>contains_pattern
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>not_contains_pattern
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>equals
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>not_between
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>not_equals
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>greater_than
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>lesser_than
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>greater_equal
                           iv_sign   = zif_sat_c_options=>excluding ) )
      ( get_opt_icon_info( iv_option = zif_sat_c_options=>lesser_equal
                           iv_sign   = zif_sat_c_options=>excluding ) )
    ).

    gt_selopt_control = VALUE #(
        ( option = zif_sat_c_options=>equals               low = abap_true )
        ( option = zif_sat_c_options=>not_equals           low = abap_true )
        ( option = zif_sat_c_options=>between              low = abap_true high = abap_true )
        ( option = zif_sat_c_options=>not_between          low = abap_true high = abap_true )
        ( option = zif_sat_c_options=>contains_pattern     low = abap_true )
        ( option = zif_sat_c_options=>not_contains_pattern low = abap_true )
        ( option = zif_sat_c_options=>greater_than         low = abap_true )
        ( option = zif_sat_c_options=>greater_than         low = abap_true )
        ( option = zif_sat_c_options=>lesser_than          low = abap_true )
        ( option = zif_sat_c_options=>greater_equal        low = abap_true )
        ( option = zif_sat_c_options=>lesser_equal         low = abap_true )
        ( option = zif_sat_c_options=>is_null              no_multi = abap_true )
        ( option = zif_sat_c_options=>is_not_null          no_multi = abap_true )
    ).
  ENDMETHOD.




  METHOD get_option_icon.
*&---------------------------------------------------------------------*
*& Description: Returns icon name
*&---------------------------------------------------------------------*

    CASE iv_option.
      WHEN zif_sat_c_options=>default.
        rv_icon_name = c_option_icons-default.
      WHEN zif_sat_c_options=>between.
        rv_icon_name = c_option_icons-bt.
      WHEN zif_sat_c_options=>not_between.
        rv_icon_name = c_option_icons-nb.
      WHEN zif_sat_c_options=>equals.
        rv_icon_name = c_option_icons-eq.
      WHEN zif_sat_c_options=>not_equals.
        rv_icon_name = c_option_icons-ne.
      WHEN zif_sat_c_options=>greater_than.
        rv_icon_name = c_option_icons-gt.
      WHEN zif_sat_c_options=>lesser_than.
        rv_icon_name = c_option_icons-lt.
      WHEN zif_sat_c_options=>greater_equal.
        rv_icon_name = c_option_icons-ge.
      WHEN zif_sat_c_options=>lesser_equal.
        rv_icon_name = c_option_icons-le.
      WHEN zif_sat_c_options=>contains_pattern.
        rv_icon_name = c_option_icons-cp.
      WHEN zif_sat_c_options=>not_contains_pattern.
        rv_icon_name = c_option_icons-np.
      WHEN zif_sat_c_options=>is_null.
        rv_icon_name = 'ICON_EQUAL'.
      WHEN zif_sat_c_options=>is_not_null.
        rv_icon_name = 'ICON_NOT_EQUAL'.
    ENDCASE.

    IF iv_sign = zif_sat_c_options=>including OR
       iv_sign = space.
      REPLACE '#' WITH c_option_icons-green INTO rv_icon_name.
    ELSE.
      REPLACE '#' WITH c_option_icons-red INTO rv_icon_name.
    ENDIF.

  ENDMETHOD.

  METHOD choose_sel_option.
    DATA: lt_fieldcat  TYPE lvc_t_fcat.

    DATA(lt_sel_option) = gt_i_options.
    IF if_allow_null = abap_true.
      lt_sel_option = VALUE #( BASE lt_sel_option
        ( get_opt_icon_info( iv_option = zif_sat_c_options=>is_null
                             iv_sign   = zif_sat_c_options=>including ) )
        ( get_opt_icon_info( iv_option = zif_sat_c_options=>is_not_null
                             iv_sign   = zif_sat_c_options=>including ) )
      ).
    ENDIF.
    lt_sel_option = VALUE #( BASE lt_sel_option ( LINES OF gt_e_options ) ).

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_buffer_active        = space
        i_structure_name       = 'ZDBBR_SEL_OPTION'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      CASE <ls_fieldcat>-fieldname.
        WHEN 'SIGN'.
          <ls_fieldcat>-no_out = abap_true.
        WHEN 'OPTION'.
          <ls_fieldcat>-no_out = abap_true.
        WHEN 'ICON'.
          <ls_fieldcat>-outputlen = 2.
      ENDCASE.
    ENDLOOP.

    DATA: ls_selfield TYPE slis_selfield,
          lf_exit     TYPE abap_bool.

*.. Show popup with the options and give one back
    DATA(lt_status_exclude) = VALUE lvc_t_excl(
      ( func = '&OL0' ) ( func = '&ELP' ) ( func = '&OAD' ) ( func = '&RNT' ) ( func = '&AVE' )
    ).
    CALL FUNCTION 'LVC_SINGLE_ITEM_SELECTION'
      EXPORTING
        i_title         = |{ 'Choose Option'(001) }|
        it_fieldcatalog = lt_fieldcat
        it_status_excl  = lt_status_exclude
      IMPORTING
        es_selfield     = ls_selfield
        e_exit          = lf_exit
      TABLES
        t_outtab        = lt_sel_option.

    IF lf_exit <> abap_true.
      TRY .
          rs_chosen_option = lt_sel_option[ ls_selfield-tabindex ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.



  METHOD create_option_icon.
    DATA(lv_icon_name) = get_option_icon(
        iv_sign   = COND #( WHEN iv_sign IS INITIAL THEN zif_sat_c_options=>including ELSE iv_sign )
        iv_option = COND #( WHEN iv_option IS INITIAL THEN zif_dbbr_c_global=>c_options-default ELSE iv_option )
    ).

    zcl_dbbr_icon_handler=>create_icon(
      EXPORTING iv_icon_name = lv_icon_name
      IMPORTING ev_push      = rv_icon
    ).
  ENDMETHOD.

  METHOD get_opt_icon_info.
    rs_icon-sign = COND #( WHEN iv_sign IS INITIAL THEN zif_sat_c_options=>including ELSE iv_sign ).
    rs_icon-option = COND #( WHEN iv_option IS INITIAL THEN zif_dbbr_c_global=>c_options-default ELSE iv_option ).
    DATA(lv_icon_name) = get_option_icon(
      iv_sign   = rs_icon-sign
      iv_option = rs_icon-option
    ).

    zcl_dbbr_icon_handler=>create_icon(
      EXPORTING iv_icon_name = lv_icon_name
      IMPORTING ev_push      = rs_icon-icon
                ev_info_text = rs_icon-text
    ).
  ENDMETHOD.

  METHOD get_selopt_control.
    rs_control = VALUE #( gt_selopt_control[ option = iv_option ]
                          DEFAULT VALUE #( option = iv_option low = abap_true high = abap_true  ) ).
  ENDMETHOD.

ENDCLASS.
