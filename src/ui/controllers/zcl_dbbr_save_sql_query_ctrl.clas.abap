"! <p class="shorttext synchronized" lang="en">Controller for saving a sql query</p>
CLASS zcl_dbbr_save_sql_query_ctrl DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_screen_controller .

    ALIASES show
      FOR zif_uitb_screen_controller~call_screen .
    ALIASES was_saved
      FOR zif_uitb_screen_controller~was_not_cancelled .

    METHODS constructor
      IMPORTING
        !is_query_info      TYPE zdbbr_query_info
        it_query_parameters TYPE zdbbr_query_parameter_t OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Returns the name of the saved query</p>
    METHODS get_query_name
      RETURNING
        VALUE(rv_query_name) TYPE zdbbr_query_name .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES get_report_id
      FOR zif_uitb_screen_controller~get_report_id .
    ALIASES get_screen_id
      FOR zif_uitb_screen_controller~get_screen_id .

    DATA mt_query_parameters TYPE zdbbr_query_parameter_t.
    DATA ms_query_info TYPE zdbbr_query_info .
    DATA mr_ui_global_data TYPE REF TO zdbbr_global_data .
    DATA mr_ui_query_name TYPE REF TO zdbbr_query_name .
    DATA mr_ui_query_desc TYPE REF TO ddtext .
    DATA mr_ui_is_global TYPE REF TO boolean .
    "! <p class="shorttext synchronized" lang="en">Definition of a Join</p>
    DATA ms_join_def TYPE zdbbr_join_def .
    DATA mf_saved TYPE abap_bool .
    DATA mo_query_f TYPE REF TO zcl_dbbr_query_factory.

    METHODS save_query .
ENDCLASS.



CLASS zcl_dbbr_save_sql_query_ctrl IMPLEMENTATION.


  METHOD constructor.
    DEFINE read_parameter_reference.
      &1 = CAST #( lr_data_cache->get_data_ref( zif_dbbr_main_report_var_ids=>&2 ) ).
    END-OF-DEFINITION.

    mt_query_parameters = it_query_parameters.
    mo_query_f = NEW #( ).
    ms_query_info = is_query_info.

    " init some global data references from ui
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( zif_dbbr_c_report_id=>main ).
    read_parameter_reference:
      mr_ui_global_data           c_s_data,
      mr_ui_query_name            c_p_scrnam,
      mr_ui_query_desc            c_p_scrdec,
      mr_ui_is_global             c_p_xglob.
  ENDMETHOD.


  METHOD get_query_name.
    rv_query_name = mr_ui_query_name->*.
  ENDMETHOD.


  METHOD save_query.
    DATA: lv_primary_entity_type TYPE zdbbr_entity_type.

*.. validate query name
    TRY .
        zcl_dbbr_query_helper=>check_query_name( iv_query_name = mr_ui_query_name->*
                                                 if_global     = mr_ui_is_global->* ).
      CATCH zcx_dbbr_exception INTO DATA(lr_error).
        lr_error->show_message( iv_message_type = 'S' ).
        RETURN.
    ENDTRY.

    " 2) does this query already exists?

    DATA(ls_query_existing) = mo_query_f->get_query( mr_ui_query_name->* ).
    IF ls_query_existing IS NOT INITIAL.

      """ check if overwrite is possible
      IF ls_query_existing-is_global = abap_false AND ls_query_existing-created_by <> sy-uname.
        MESSAGE e037(zdbbr_exception) WITH ls_query_existing-query_name ls_query_existing-created_by.
        RETURN.
      ENDIF.

      IF zcl_dbbr_appl_util=>popup_to_confirm(
            iv_title                 = 'Save?'
            iv_query                 = |{ 'There already is a query with the name'(011) } '{ mr_ui_query_name->* }'. | &&
                                       |{ 'Do you want to override the existing query?'(012) }|
            iv_icon_type             = 'ICON_MESSSAGE_WARNING' ) <> '1'.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(ls_query_data) = VALUE zdbbr_query_data(
        query_id         = ls_query_existing-query_id
        is_global        = mr_ui_is_global->*
        created_by       = COND #( WHEN ls_query_existing-created_by IS NOT INITIAL THEN ls_query_existing-created_by ELSE sy-uname )
        created_date     = COND #( WHEN ls_query_existing IS NOT INITIAL THEN ls_query_existing-created_date ELSE sy-datum )
        changed_date     = COND #( WHEN ls_query_existing IS NOT INITIAL THEN sy-datum )
        query_name       = mr_ui_query_name->*
        description      = mr_ui_query_desc->*
**        formula          = ms_query_info-formula
        is_sql_query     = abap_true
        source           = ms_query_info-source
        parameters       = mt_query_parameters
    ).

    DATA(lv_new_query_id) = mo_query_f->save_query( ls_query_data ).

    MESSAGE s022(zdbbr_info) WITH mr_ui_query_name->*.
    mf_saved = abap_true.

    zcl_dbbr_screen_helper=>leave_screen( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~call_screen.
    mr_ui_query_name->* = ms_query_info-query_name.
    mr_ui_query_desc->* = ms_query_info-description.
    mr_ui_is_global->* = ms_query_info-is_global.

    zcl_uitb_screen_util=>call_screen(
        iv_screen_id    = get_screen_id( )
        iv_report_id    = get_report_id( )
        if_selscreen    = abap_true
        it_object_map   = VALUE #(
          ( variable_name = zif_dbbr_main_report_var_ids=>c_r_save_sql_query_controller
            global_ref    = me )
        )
        iv_start_column = 10
        iv_start_line   = 2
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~cancel.

    zcl_dbbr_screen_helper=>leave_screen( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~free_screen_resources.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_report_id.
    result = zif_dbbr_c_report_id=>main.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~get_screen_id.
    result = zif_dbbr_screen_ids=>c_save_sql_query.
  ENDMETHOD.


  METHOD zif_uitb_screen_controller~handle_user_command.

    CASE cv_function_code.

      WHEN 'SAVE'.
        save_query( ).

    ENDCASE.

    CLEAR cv_function_code.

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~pbo.

    zif_uitb_screen_controller~set_status( ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~set_status.

    zcl_uitb_screen_util=>set_selscreen_status(
        iv_status              = '0710'
        iv_repid               = zif_dbbr_c_report_id=>main
        it_excluding_functions = VALUE #( ( 'SAVE_LOAD' ) )
    ).

  ENDMETHOD.


  METHOD zif_uitb_screen_controller~was_not_cancelled.
    rf_not_cancelled = mf_saved.
  ENDMETHOD.
ENDCLASS.
