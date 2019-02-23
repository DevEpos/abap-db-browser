CLASS zcl_dbbr_table_var_starter DEFINITION
  PUBLIC
  INHERITING FROM zcl_dbbr_variant_starter
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_variant_starter.
    METHODS constructor
      IMPORTING
        iv_tabname    TYPE tabname
        iv_variant_id TYPE zdbbr_variant_id.
  PROTECTED SECTION.
    METHODS fill_primary_entity
        REDEFINITION.
  PRIVATE SECTION.
    DATA mv_tabname TYPE tabname.
ENDCLASS.



CLASS zcl_dbbr_table_var_starter IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_variant_id ).

    mv_tabname = iv_tabname.
  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~execute_variant.
    show_start_progress_text( ).

    fill_data_from_variant( ).

    get_tabfields(
      IMPORTING
        er_tabfields     = DATA(lr_tabfields)
        er_tabfields_all = DATA(lr_tabfields_all)
    ).

    " create and start selection controller
    DATA(lr_controller) = zcl_dbbr_selection_controller=>create_controller(
       iv_entity_type        = zif_dbbr_c_selscreen_mode=>table
       iv_entity_id          = ms_global_data-primary_table
       it_selection_fields   = mt_selfields
       it_multi_or           = mt_selfields_or
       is_technical_infos    = CORRESPONDING #( ms_global_data )
       it_selfields_multi    = mt_selfields_multi
       ir_tabfields          = lr_tabfields
       ir_tabfields_all      = lr_tabfields_all
       it_table_to_alias_map = mt_table_to_alias_map
       it_exclude_function   = VALUE #(
         ( zif_dbbr_c_selection_functions=>leave_screen_with_layout )
         ( zif_dbbr_c_selection_functions=>transfer_filter_values   )
       )
    ).

    lr_controller->execute_selection( ).
  ENDMETHOD.


  METHOD zif_dbbr_variant_starter~initialize.
    ms_global_data-primary_table = mv_tabname.

    fill_primary_entity( ).
    load_variant( ).

    CHECK mv_variant_id <> zif_dbbr_global_consts=>c_dummy_variant.

    NEW zcl_dbbr_favmenu_factory( )->refresh_most_used(
        iv_entry     = mv_tabname
        iv_entry_raw = mv_tabname
        iv_type      = zif_dbbr_c_favmenu_type=>table
        iv_text      = zcl_dbbr_dictionary_helper=>get_table_info( mv_tabname )-ddtext
    ).
  ENDMETHOD.

  METHOD fill_primary_entity.
    DATA: ls_table_info TYPE dd02v.

    ls_table_info = zcl_dbbr_dictionary_helper=>get_table_info( ms_global_data-primary_table ).
    IF ls_table_info IS INITIAL.
      RETURN.
    ENDIF.

    ms_global_data-client_dependent = ls_table_info-clidep.

    create_table_fields( VALUE #(
      tabname          = mv_tabname
      tabname_alias    = mv_tabname
      is_primary       = abap_true
      active_selection = abap_true
    ) ).

  ENDMETHOD.

ENDCLASS.
