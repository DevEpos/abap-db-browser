CLASS ZCL_DBBR_fe_text_editor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_control.
    EVENTS f4
      EXPORTING
        VALUE(ev_selected_text) TYPE string.
    METHODS constructor
      IMPORTING
        ir_parent    TYPE REF TO cl_gui_container
        ir_tabfields TYPE REF TO ZCL_DBBR_tabfield_list.
    METHODS set_text
      IMPORTING
        iv_text TYPE string OPTIONAL
        it_text TYPE string_table OPTIONAL.
    METHODS get_selected_text
      RETURNING
        VALUE(rv_selected) TYPE string.
    METHODS set_selected_text
      IMPORTING
        value TYPE string.
    METHODS get_raw_text_as_string
      RETURNING
        VALUE(rv_string) TYPE string.
    METHODS get_raw_text_as_table
      RETURNING
        VALUE(rt_lines) TYPE string_table.
    METHODS get_text_as_table
      IMPORTING
        if_remove_line_break_chars TYPE sap_bool
      RETURNING
        VALUE(rt_lines)            TYPE string_table.
    METHODS toggle_view_edit_mode.
    METHODS select_row
      IMPORTING
        iv_row TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_read_only TYPE i.
    DATA mr_editor TYPE REF TO cl_gui_textedit.
    DATA mr_tabfield_list TYPE REF TO ZCL_DBBR_tabfield_list.
    DATA mr_container TYPE REF TO cl_gui_container.
    DATA mv_editor_string TYPE string.
    DATA mv_selected_text TYPE string.
    DATA mt_editor_string TYPE string_table.
    DATA mr_dnd_object TYPE REF TO cl_dragdrop.

    METHODS create_editor.

    METHODS handle_f4
         FOR EVENT f4 OF cl_gui_textedit.
    METHODS handle_on_textedit_drop
      FOR EVENT on_drop
          OF cl_gui_textedit
      IMPORTING
          index
          line
          pos
          dragdrop_object.
ENDCLASS.


CLASS ZCL_DBBR_fe_text_editor IMPLEMENTATION.
  METHOD constructor.
    mr_container = ir_parent.
    mr_tabfield_list = ir_tabfields.

    create_editor( ).
  ENDMETHOD.

  METHOD create_editor.
    CHECK mr_editor IS INITIAL.

    mr_editor = NEW #( parent = mr_container ).

    IF mr_editor IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    mr_editor->register_event_f4( ).

    mr_editor->set_comments_string( ).
    mr_editor->set_highlight_comments_mode( ).
    mr_editor->set_font_fixed( ).
    mr_editor->set_selection_pos_in_line( line = 2 ).

    mr_dnd_object = NEW cl_dragdrop( ).

    mr_dnd_object->add(
        flavor     = zif_dbbr_c_fe_tree_flavor=>element
        dragsrc    = abap_false
        droptarget = abap_true
    ).

    mr_dnd_object->add(
        flavor     = zif_dbbr_c_fe_tree_flavor=>example
        dragsrc    = abap_false
        droptarget = abap_true
    ).

    mr_editor->set_dragdrop( mr_dnd_object ).

    SET HANDLER:
       handle_on_textedit_drop FOR mr_editor,
       handle_f4 FOR mr_editor.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    cl_gui_control=>set_focus( mr_editor ).
  ENDMETHOD.

  METHOD set_text.
    IF iv_text IS SUPPLIED.
      mr_editor->set_textstream( iv_text ).
    ELSEIF it_text IS SUPPLIED.
      mr_editor->set_text_as_stream( it_text ).
    ENDIF.
  ENDMETHOD.

  METHOD get_raw_text_as_string.
    mr_editor->get_textstream( IMPORTING text = mv_editor_string ).
    cl_gui_cfw=>flush( ).
    rv_string = mv_editor_string.
  ENDMETHOD.

  METHOD get_raw_text_as_table.
    mr_editor->get_text_as_stream( IMPORTING text = mt_editor_string ).
    cl_gui_cfw=>flush( ).
    rt_lines = mt_editor_string.
  ENDMETHOD.

  METHOD get_text_as_table.
    IF if_remove_line_break_chars = abap_true.
      DATA(lv_text) = get_raw_text_as_string( ).
      SPLIT lv_text AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      rt_lines = get_raw_text_as_table( ).
    ENDIF.

  ENDMETHOD.

  METHOD toggle_view_edit_mode.
    IF mv_read_only = cl_gui_textedit=>true.
      mv_read_only = cl_gui_textedit=>false.
    ELSE.
      mv_read_only = cl_gui_textedit=>true.
    ENDIF.

    mr_editor->set_readonly_mode( mv_read_only ).
  ENDMETHOD.

  METHOD handle_on_textedit_drop.
    " get dragdrop object
    DATA(lr_dnd_object) = CAST ZCL_DBBR_fe_dnd_object( dragdrop_object->object ).

    CASE dragdrop_object->flavor.

      WHEN zif_dbbr_c_fe_tree_flavor=>element.
        mr_editor->set_selected_textstream( selected_text = lr_dnd_object->get_text( ) ).

      WHEN zif_dbbr_c_fe_tree_flavor=>example.
        set_text( iv_text = lr_dnd_object->get_text( ) ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_f4.
    " retrieve selected text
    mr_editor->get_selected_textstream( IMPORTING selected_text = mv_selected_text ).
    cl_gui_cfw=>flush( ).

    RAISE EVENT f4
      EXPORTING
        ev_selected_text = mv_selected_text.
  ENDMETHOD.

  METHOD get_selected_text.
    mr_editor->get_selected_textstream( IMPORTING selected_text = mv_selected_text ).
    cl_gui_cfw=>flush( ).

    rv_selected = mv_selected_text.
  ENDMETHOD.

  METHOD set_selected_text.
    mr_editor->set_selected_textstream( selected_text = value ).
  ENDMETHOD.


  METHOD select_row.
    mr_editor->select_lines( from_line = iv_row to_line = iv_row ).
  ENDMETHOD.

ENDCLASS.
