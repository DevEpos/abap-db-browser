"! <p class="shorttext synchronized" lang="en">Controller for Object Browser Search input</p>
CLASS zcl_dbbr_obj_brws_search_sc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_screen_controller.

    ALIASES pbo
      FOR zif_uitb_screen_controller~pbo.
    ALIASES pai
      FOR zif_uitb_screen_controller~handle_user_command.
    ALIASES cancel
      FOR zif_uitb_screen_controller~cancel.
    ALIASES show
      FOR zif_uitb_screen_controller~call_screen.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id.
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id.
    ALIASES set_status
      FOR zif_uitb_screen_controller~set_status.
    ALIASES mf_first_call
      FOR zif_uitb_screen_controller~mf_first_call.

    CONSTANTS c_r_controller_name TYPE dynfnam VALUE 'GR_OBJ_BRWS_SEARCH_ENTER_CTRL'.
    CONSTANTS c_s_object_search TYPE dynfnam VALUE 'GS_OBJECT_SEARCH'.
    CONSTANTS c_v_search_type  TYPE dynfnam VALUE 'SEARCH_TYPE'.
    CONSTANTS c_v_search_input TYPE dynfnam VALUE 'SEARCH_INPUT'.

    DATA: mr_v_search_input  TYPE REF TO string,
          mr_s_object_search TYPE REF TO data,
          mr_v_search_type   TYPE REF TO zdbbr_obj_browser_mode.

    "! <p class="shorttext synchronized" lang="en">Handler for closing this dialog</p>
    "!
    METHODS on_close_object_search
        FOR EVENT close_object_search_modal OF zcl_dbbr_selscr_nav_events.
ENDCLASS.



CLASS zcl_dbbr_obj_brws_search_sc IMPLEMENTATION.

  METHOD constructor.
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).
    mr_v_search_input = CAST #( lr_data_cache->get_data_ref( |{ c_s_object_search }-{ c_v_search_input }| ) ).
    mr_v_search_type = CAST #( lr_data_cache->get_data_ref( |{ c_s_object_search }-{ c_v_search_type }| ) ).

    DATA(lr_s_global_data) = CAST zdbbr_global_data( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>c_s_data ) ).

*    CLEAR: mr_v_search_input->*.
    IF mr_v_search_type->* IS INITIAL.
      mr_v_search_type->* = lr_s_global_data->settings-initial_obj_brws_mode.
    ENDIF.

    SET HANDLER:
      on_close_object_search.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~call_screen.
    mf_first_call = abap_true.
    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        it_object_map   = VALUE #(
          (  global_ref = me variable_name = c_r_controller_name )
        )
        iv_start_column = 50
        iv_start_line   = 10
    ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~handle_user_command.
    CASE cv_function_code.

      WHEN 'SEARCH' OR 'SEARCHMORE'.
        IF mr_v_search_input->* IS INITIAL.
          MESSAGE i016(zdbbr_exception) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        zcl_dbbr_selscr_nav_events=>raise_request_object_search(
            iv_object_type      = mr_v_search_type->*
            iv_search_query     = mr_v_search_input->*
            if_close_on_success = xsdbool( cv_function_code = 'SEARCH' )
        ).
      WHEN 'APPHELP'.
        zcl_dbbr_help_repository=>show_help( zcl_dbbr_help_repository=>c_help_id-object_search ).

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~pbo.
    set_status( ).

    IF mf_first_call = abap_false.
      zcl_uitb_cursor=>set_cursor( iv_field = |{ c_s_object_search }-{ c_v_search_input }| ).
    ENDIF.

    CLEAR mf_first_call.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~cancel.
    zcl_uitb_screen_util=>leave_screen( ).
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.

  METHOD zif_uitb_screen_controller~get_screen_id.
    result = '0300'.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.
    DATA(lv_report_id) = get_report_id( ).
    SET PF-STATUS '0301' OF PROGRAM lv_report_id.
    SET TITLEBAR 'PROGTITLE' OF PROGRAM lv_report_id WITH 'Search Objects'(001).
  ENDMETHOD.

  METHOD on_close_object_search.
    SET HANDLER on_close_object_search ACTIVATION abap_false.
    cancel( ).
  ENDMETHOD.

ENDCLASS.
