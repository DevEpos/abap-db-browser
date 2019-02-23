CLASS zcl_dbbr_cds_sub_entity_sel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view .
    INTERFACES zif_uitb_gui_control.
    INTERFACES zif_uitb_disposable .

    ALIASES focus
      FOR zif_uitb_gui_control~focus.
    ALIASES dispose
      FOR zif_uitb_disposable~dispose .
    ALIASES is_visible
      FOR zif_uitb_view~is_visible .
    ALIASES show
      FOR zif_uitb_view~show .

    EVENTS entity_chosen
      EXPORTING
        VALUE(ev_chosen_entity_id) TYPE zdbbr_entity_id OPTIONAL
        VALUE(ev_chosen_entity_type) TYPE ddtargetkind OPTIONAL
        VALUE(es_chosen_association) TYPE zdbbr_cds_association OPTIONAL .

    METHODS constructor
      IMPORTING
        !ir_cds_view          TYPE REF TO zcl_dbbr_cds_view
        !if_only_associations TYPE abap_bool OPTIONAL
        !if_as_dock           TYPE abap_bool OPTIONAL .
    METHODS get_chosen_sub_entity
      EXPORTING
        !ev_enttiy_id   TYPE zdbbr_entity_id
        !ev_entity_type TYPE ddtargetkind .
    METHODS get_chosen_association
      RETURNING
        VALUE(result) TYPE zdbbr_cds_association .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mf_visible
      FOR zif_uitb_view~mf_visible .

    TYPES:
      BEGIN OF ty_association.
        INCLUDE TYPE zdbbr_cds_association.
    TYPES: kind_description TYPE ddtext.
    TYPES: END OF ty_association .

    DATA mr_tmplt_prog TYPE REF TO zif_uitb_template_prog .
    DATA:
      mt_associations TYPE STANDARD TABLE OF ty_association WITH EMPTY KEY .
    DATA mr_cds_view TYPE REF TO zcl_dbbr_cds_view .
    DATA mr_assoc_alv TYPE REF TO zcl_uitb_alv .
    DATA mr_cds_subentity_tree TYPE REF TO zcl_uitb_column_tree_model .
    CONSTANTS c_association_root TYPE string VALUE 'ASSOC' ##NO_TEXT.
    CONSTANTS c_select_root TYPE string VALUE 'SELECT' ##NO_TEXT.
    CONSTANTS c_description_col TYPE tv_itmname VALUE 'DESC' ##NO_TEXT.
    CONSTANTS c_type_col TYPE tv_itmname VALUE 'TYPE' ##NO_TEXT.
    DATA mf_only_associations TYPE abap_bool .
    DATA mv_chosen_entity_id TYPE zdbbr_entity_id .
    DATA mv_chosen_entity_type TYPE ddtargetkind .
    DATA mv_subentity_count TYPE i .
    DATA ms_chosen_association TYPE zdbbr_cds_association .
    DATA mr_dock TYPE REF TO cl_gui_docking_container .
    DATA mf_as_dock TYPE abap_bool .
    DATA mf_all TYPE abap_bool VALUE abap_true ##NO_TEXT.

    METHODS choose_association .
    METHODS create_association_alv .
    METHODS create_tree .
    METHODS fill_association_alv .
    METHODS fill_tree .
    METHODS do_on_first_screen_call .
    METHODS fill_status_functions .
    METHODS on_before_output
          FOR EVENT before_output OF zif_uitb_view_callback
      IMPORTING
          !er_callback .
    METHODS on_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          !ev_row
          !ev_column .
    METHODS on_node_double_click
          FOR EVENT node_double_click OF zcl_uitb_ctm_events
      IMPORTING
          !ev_node_key .
    METHODS on_user_command
          FOR EVENT user_command OF zif_uitb_view_callback
      IMPORTING
          !ev_function_id
          !er_callback .
    METHODS on_alv_user_command
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          !ev_function
          !ev_tag .
    METHODS get_container
      RETURNING
        VALUE(result) TYPE REF TO cl_gui_container .
    METHODS change_alv_fields .
ENDCLASS.



CLASS zcl_dbbr_cds_sub_entity_sel IMPLEMENTATION.


  METHOD change_alv_fields.
    DATA(lr_cols) = mr_assoc_alv->get_columns( ).

    lr_cols->get_column( 'KIND_DESCRIPTION' )->set_visible( mf_all ).
    lr_cols->get_column( 'REF_CDS_VIEW_RAW' )->set_visible( mf_all ).

    IF mf_all = abap_true.
      lr_cols->set_column_position( iv_columnname = 'CARDINALITY_TEXT' iv_position = 1 ).
      lr_cols->set_column_position( iv_columnname = 'RAW_NAME' iv_position = 2 ).
      lr_cols->set_column_position( iv_columnname = 'KIND_DESCRIPTION' iv_position = 3 ).
      lr_cols->set_column_position( iv_columnname = 'REF_CDS_VIEW_RAW' iv_position = 4 ).
      lr_cols->set_column_position( iv_columnname = 'DDTEXT' iv_position = 5 ).
    ENDIF.
    lr_cols->set_optimized( ).

    mr_assoc_alv->get_functions( )->set_function(
        iv_name    = 'ALLFIELDS'
        if_checked = mf_all
    ).
  ENDMETHOD.


  METHOD choose_association.
*... retrieve current row
    mr_assoc_alv->get_metadata( ).

    DATA(ls_cell) = mr_assoc_alv->get_selections( )->get_current_cell( ).
    on_link_click( ev_row = ls_cell-row ev_column = space ).

  ENDMETHOD.


  METHOD constructor.
    mf_only_associations = if_only_associations.
    mf_as_dock = if_as_dock.
    mr_cds_view = ir_cds_view.
*... get count of sub entities
    mv_subentity_count = ir_cds_view->mv_association_count + ir_cds_view->mv_select_table_count.

*... create template program
    IF mf_as_dock = abap_false.

      mr_tmplt_prog = zcl_uitb_templt_prog_callback=>create_template_program(
        iv_title = COND #( WHEN mf_only_associations = abap_true THEN TEXT-001 ELSE TEXT-002 )
      ).
      fill_status_functions( ).

      SET HANDLER:
        on_before_output FOR mr_tmplt_prog,
        on_user_command FOR mr_tmplt_prog.
    ENDIF.

  ENDMETHOD.


  METHOD create_association_alv.
    DATA: lr_col TYPE REF TO zcl_uitb_alv_column.

    TRY.
        mr_assoc_alv = zcl_uitb_alv=>create_alv(
            ir_data                 = REF #( mt_associations )
            iv_description_language = 'E'
            ir_container            = get_container( )
            if_editable             = abap_false
        ).

        SET HANDLER:
          on_link_click FOR mr_assoc_alv->get_events( ).

        DATA(lr_cols) = mr_assoc_alv->get_columns( ).

        lr_cols->get_column( 'NAME' )->set_technical( ).
        lr_cols->get_column( 'REF_CDS_VIEW' )->set_technical( ).
        lr_cols->get_column( 'CARD_MIN' )->set_technical( ).
        lr_cols->get_column( 'CARDINALITY' )->set_technical( ).
        lr_cols->get_column( 'CARD_MAX' )->set_technical( ).
        lr_cols->get_column( 'FIELDS' )->set_technical( ).
        lr_cols->get_column( 'ENTITY_TYPE' )->set_technical( ).

        lr_col = lr_cols->get_column( 'CARDINALITY_TEXT' ).
        lr_cols->set_column_position( iv_columnname = 'CARDINALITY_TEXT'
                                      iv_position   = 1 ).
        lr_col->set_descriptions( iv_short = 'Card.' iv_long = 'Cardinality' ).
        lr_col->set_tooltip( 'Cardinality' ).

        lr_cols->set_column_position( iv_columnname = 'KIND_DESCRIPTION'
                                      iv_position   = 5 ).
        lr_cols->get_column( 'KIND' )->set_technical( ).

        lr_col = lr_cols->get_column( 'RAW_NAME' ).
        lr_col->set_descriptions( iv_long = 'Association' ).
        lr_col->set_key( ).
        lr_col->set_hotspot( ).

        lr_col = lr_cols->get_column( 'KIND_DESCRIPTION' ).
        lr_col->set_descriptions( iv_long = 'Kind' ).

        lr_col = lr_cols->get_column( 'REF_CDS_VIEW_RAW' ).
        lr_col->set_descriptions( iv_long = 'Entity Name' ).

        DATA(lr_func) = mr_assoc_alv->get_functions( ).
        lr_func->set_all( abap_false ).
        lr_func->set_function( zif_uitb_c_alv_functions=>sort_asc ).
        lr_func->set_function( zif_uitb_c_alv_functions=>sort_desc ).
        lr_func->set_function( zif_uitb_c_alv_functions=>find ).
        lr_func->set_function( zif_uitb_c_alv_functions=>find_more ).

        mr_assoc_alv->get_display_settings( )->set_title( 'Association Navigator' ).
        mr_assoc_alv->get_display_settings( )->set_small_title( ).

        mr_assoc_alv->get_display_settings( )->set_row_marks( abap_false ).
        mr_assoc_alv->get_selections( )->set_mode( value = zif_uitb_c_alv_selection=>row_column ).

*... create function for closing the association browser
        IF mf_as_dock = abap_true.
          mr_assoc_alv->get_functions( )->add_function(
              iv_name     = 'ALLFIELDS'
              iv_text     = 'All Fields'
              iv_tooltip  = 'Toggle All Fields'
          ).
          mr_assoc_alv->get_functions( )->add_function(
              iv_name     = 'CLOSE'
              iv_icon     = |{ icon_close }|
              iv_tooltip  = 'Close association Browser'
          ).
          mf_all = abap_false.
          change_alv_fields( ).
          SET HANDLER: on_alv_user_command FOR mr_assoc_alv->get_events( ).
        ENDIF.

        mr_assoc_alv->display( ).

      CATCH zcx_uitb_alv_error.
    ENDTRY.
  ENDMETHOD.


  METHOD create_tree.
    " create the tree model for the association tree
    mr_cds_subentity_tree = NEW #(
        ir_parent           = get_container( )
        is_hierarchy_header = VALUE treemhhdr(
            heading = 'Entity Name'(003)
            width   = 60
        )
    ).

    SET HANDLER:
      on_node_double_click FOR mr_cds_subentity_tree->get_events( ).

    DATA(lr_toolbar) = mr_cds_subentity_tree->get_toolbar( ).

    TRY.
        mr_cds_subentity_tree->get_columns( )->add_column(
            iv_colname           = c_type_col
            iv_width             = 40
            iv_header_text       = 'Type'(004)
        ).
        mr_cds_subentity_tree->get_columns( )->add_column(
            iv_colname           = c_description_col
            iv_width             = 60
            iv_header_text       = 'Description'(005)
        ).

        mr_cds_subentity_tree->get_nodes( )->add_node(
            iv_node_key          = c_select_root
            if_folder            = abap_true
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = 'Select From'(006) )
              ( item_name = c_description_col
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = 'Select Part of CDS View'(007) )
            )
        ).
        mr_cds_subentity_tree->get_nodes( )->add_node(
            iv_node_key          = c_association_root
            if_folder            = abap_true
            it_item_table        = VALUE #(
              ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = 'Associations' )
              ( item_name = c_description_col
                class     = cl_column_tree_model=>item_class_text
                font      = cl_item_tree_model=>item_font_prop
                text      = 'Associations of CDS'(008) )
            )
        ).

        mr_cds_subentity_tree->create_tree_control( ).

      CATCH zcx_uitb_tree_error.    "
    ENDTRY.

  ENDMETHOD.


  METHOD do_on_first_screen_call.
*... create correct control depending on association flag
    IF mf_only_associations = abap_true.
      create_association_alv( ).
      fill_association_alv( ).
    ELSE.
      create_tree( ).
      fill_tree( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_association_alv.
    mt_associations = CORRESPONDING #( mr_cds_view->get_associations( ) ).

    DATA(lr_type_descr) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_data( VALUE ddtargetkind( ) )
    ).

    DATA(lt_fix_entity_type_vals) = lr_type_descr->get_ddic_fixed_values( p_langu = 'E' ).

    LOOP AT mt_associations ASSIGNING FIELD-SYMBOL(<ls_assoc>).
      <ls_assoc>-kind_description = VALUE #( lt_fix_entity_type_vals[ low = <ls_assoc>-kind ]-ddtext OPTIONAL ).
    ENDLOOP.

    mr_assoc_alv->get_columns( )->set_optimized( ).

    mr_assoc_alv->get_selections( )->set_current_cell(
      VALUE #( row = 1 column = 'RAW_NAME' )
    ).
    mr_assoc_alv->refresh(  ).

    mr_assoc_alv->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD fill_status_functions.
    CHECK mf_only_associations = abap_true.

    mr_tmplt_prog->add_function(
        iv_function_id = zif_uitb_template_prog=>c_func_f2
        iv_text        = |{ 'Navigate to Association'(009) }|
        iv_icon_text   = |{ 'Choose'(010) }|
        iv_icon        = icon_select_detail
    ).
  ENDMETHOD.


  METHOD fill_tree.
    DATA: lv_icon TYPE tv_image.

*... fill select part tree node
    DATA(lt_base_tables) = mr_cds_view->get_base_tables( ).

    DATA(lr_nodes) = mr_cds_subentity_tree->get_nodes( ).
**
    LOOP AT lt_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_table>).
      lv_icon = SWITCH #(
        <ls_base_table>-table_kind
        WHEN zif_dbbr_c_entity_type=>cds_view THEN zif_dbbr_c_icon=>cds_view
        WHEN zif_dbbr_c_entity_type=>table THEN zif_dbbr_c_icon=>database_table
      ).
      lr_nodes->add_node(
          iv_node_key          = CONV #( <ls_base_table>-entityname )
          iv_relative_node_key = c_select_root
          if_folder            = abap_false
          iv_image             = lv_icon
          ir_user_object       = NEW zcl_dbbr_entity_uo( iv_entity_id   = <ls_base_table>-entityname
                                                         iv_entity_type = <ls_base_table>-table_kind
                                                         iv_source_type = <ls_base_table>-table_kind )
          it_item_table        = VALUE #(
            ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
              class     = cl_column_tree_model=>item_class_text
              font      = cl_item_tree_model=>item_font_prop
              text      = <ls_base_table>-entityname_raw )
            ( item_name = c_description_col
              class     = cl_column_tree_model=>item_class_text
              font      = cl_item_tree_model=>item_font_prop
              text      = <ls_base_table>-description )
          )
      ).

    ENDLOOP.

*... fill association tree node
    DATA(lt_associations) = mr_cds_view->get_associations( ).

    LOOP AT lt_associations ASSIGNING FIELD-SYMBOL(<ls_assoc>).
      lv_icon = SWITCH #(
        <ls_assoc>-entity_type
        WHEN zif_dbbr_c_entity_type=>cds_view THEN zif_dbbr_c_icon=>cds_view
        WHEN zif_dbbr_c_entity_type=>table THEN zif_dbbr_c_icon=>database_table
      ).
      lr_nodes->add_node(
          iv_node_key          = CONV #( <ls_assoc>-name )
          iv_relative_node_key = c_association_root
          if_folder            = abap_false
          iv_image             = lv_icon
          ir_user_object       = NEW zcl_dbbr_entity_uo( iv_entity_id   = <ls_assoc>-ref_cds_view
                                                         iv_entity_type = <ls_assoc>-entity_type
                                                         iv_source_type = <ls_assoc>-kind )
          it_item_table        = VALUE #(
            ( item_name = zcl_uitb_column_tree_model=>c_hierarchy_column
              class     = cl_column_tree_model=>item_class_text
              font      = cl_item_tree_model=>item_font_prop
              text      = |{ <ls_assoc>-cardinality_text } { <ls_assoc>-raw_name }| )
            ( item_name = c_type_col
              class     = cl_column_tree_model=>item_class_text
              font      = cl_item_tree_model=>item_font_prop
              text      = <ls_assoc>-ref_cds_view_raw )
            ( item_name = c_description_col
              class     = cl_column_tree_model=>item_class_text
              font      = cl_item_tree_model=>item_font_prop
              text      = <ls_assoc>-ddtext )
          )
      ).
    ENDLOOP.

    lr_nodes->expand_root_nodes( ).

    lr_nodes->set_top_node( iv_node_key = c_select_root ).

    mr_cds_subentity_tree->zif_uitb_gui_control~focus( ).
  ENDMETHOD.


  METHOD get_chosen_association.
    result = ms_chosen_association.
  ENDMETHOD.


  METHOD get_chosen_sub_entity.
    ev_entity_type = mv_chosen_entity_type.
    ev_enttiy_id = mv_chosen_entity_id.
  ENDMETHOD.


  METHOD get_container.
    result = COND #(
      WHEN mf_as_dock = abap_true THEN
        mr_dock
      ELSE
        mr_tmplt_prog->get_container( )
    ).
  ENDMETHOD.


  METHOD on_alv_user_command.

    CASE ev_function.

      WHEN 'CLOSE'.
        dispose( ).

      WHEN 'ALLFIELDS'.
        zcl_uitb_appl_util=>toggle( CHANGING value = mf_all ).
        change_alv_fields( ).
        mr_assoc_alv->refresh( ).
    ENDCASE.
  ENDMETHOD.


  METHOD on_before_output.
    IF er_callback->is_first_screen_call( ).
      do_on_first_screen_call( ).
    ENDIF.

    er_callback->deactivate_function( zif_uitb_template_prog=>c_save ).
  ENDMETHOD.


  METHOD on_link_click.
    DATA(lr_assoc) = REF #( mt_associations[ ev_row ] OPTIONAL ).

    CHECK lr_assoc IS BOUND.

    mv_chosen_entity_id = lr_assoc->ref_cds_view.
    mv_chosen_entity_type = lr_assoc->entity_type.

    ms_chosen_association = CORRESPONDING #( lr_assoc->* ).

    IF mf_as_dock = abap_false.
      mr_tmplt_prog->leave_program( ).
    ELSE.
      RAISE EVENT entity_chosen
        EXPORTING
          es_chosen_association = ms_chosen_association.
    ENDIF.
  ENDMETHOD.


  METHOD on_node_double_click.
    CHECK ev_node_key IS NOT INITIAL.

*... retrieve association/select part
    DATA(lr_user_object) = CAST zcl_dbbr_entity_uo( mr_cds_subentity_tree->get_nodes( )->get_node( ev_node_key )->get_user_object( ) ).
    IF lr_user_object IS BOUND.
      mv_chosen_entity_id = lr_user_object->mv_entity_id.
      mv_chosen_entity_type = lr_user_object->mv_entity_type.

      mr_tmplt_prog->leave_program( ).
    ENDIF.
  ENDMETHOD.


  METHOD on_user_command.

    CASE ev_function_id.

      WHEN zif_uitb_template_prog=>c_func_f2.
        choose_association( ).

      WHEN 'OK'.
        er_callback->exit_screen( ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_uitb_disposable~dispose.
    IF mr_dock IS BOUND.
      mr_dock->free( ).
    ENDIF.

    CLEAR: mr_dock,
           mr_cds_subentity_tree,
           mr_tmplt_prog,
           mr_assoc_alv.

    mf_visible = abap_false.
  ENDMETHOD.


  METHOD zif_uitb_view~is_visible.
    result = zif_uitb_view~mf_visible.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    IF mf_as_dock = abap_true.
      CHECK NOT is_visible( ).
      mr_dock = NEW cl_gui_docking_container(
*          side        = cl_gui_docking_container=>dock_at_left
          lifetime    = cl_gui_control=>lifetime_dynpro
          extension   = 400
          caption     = 'Associations'
*          ratio       =
      ).
      do_on_first_screen_call( ).
      mf_visible = abap_true.
    ELSE.
      mr_tmplt_prog->zif_uitb_view~show(
          iv_start_column = 10
          iv_start_line   = 2
          iv_end_column   = COND #( WHEN mf_only_associations = abap_true THEN 110 ELSE 140 )
          iv_end_line     = COND #(
            WHEN mv_subentity_count < 10 THEN 15 ELSE 25
          )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    IF mr_dock IS BOUND.
      mr_assoc_alv->zif_uitb_gui_control~focus( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.

  ENDMETHOD.

ENDCLASS.
