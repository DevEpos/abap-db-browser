CLASS ZCL_DBBR_list_tree_wrapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_tree_node_wrapper TYPE STANDARD TABLE OF REF TO ZCL_DBBR_tree_node_wrapper WITH DEFAULT KEY.
    METHODS constructor
      IMPORTING
        !ir_list_tree_model   TYPE REF TO cl_list_tree_model
        if_multiple_selection TYPE abap_bool OPTIONAL.
    METHODS get_node
      IMPORTING
        !iv_node_key   TYPE tm_nodekey
        !if_recursive  TYPE boolean OPTIONAL
      RETURNING
        VALUE(rr_node) TYPE REF TO ZCL_DBBR_tree_node_wrapper .
    METHODS add_node
      IMPORTING
        !ir_node              TYPE REF TO ZCL_DBBR_tree_node_wrapper
        !if_recursive         TYPE boolean OPTIONAL
        !iv_relative_node_key TYPE tm_nodekey OPTIONAL
        !iv_relationship      TYPE i OPTIONAL .
    METHODS get_selected_node
      IMPORTING
        !if_recursive  TYPE boolean OPTIONAL
      RETURNING
        VALUE(rr_node) TYPE REF TO ZCL_DBBR_tree_node_wrapper .
    METHODS get_selected_nodes
          IMPORTING
        !if_recursive  TYPE boolean OPTIONAL
      RETURNING
        VALUE(rt_nodes) TYPE tt_tree_node_wrapper.
    METHODS is_node_expanded
      IMPORTING
        !iv_node_key          TYPE tm_nodekey
      RETURNING
        VALUE(rf_is_expanded) TYPE boolean .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_list_tree_model TYPE REF TO cl_list_tree_model .
    DATA mf_multiple_selection TYPE abap_bool.

    METHODS get_node_internal
      IMPORTING
        !iv_node_key   TYPE tm_nodekey
      RETURNING
        VALUE(rr_node) TYPE REF TO ZCL_DBBR_tree_node_wrapper .
    METHODS get_node_parent
      IMPORTING
        !iv_node_key   TYPE tm_nodekey
      RETURNING
        VALUE(rr_node) TYPE REF TO ZCL_DBBR_tree_node_wrapper .
    METHODS get_node_children
      IMPORTING
        !iv_node_key            TYPE tm_nodekey
        !if_recursive           TYPE boolean OPTIONAL
      RETURNING
        VALUE(rt_node_children) TYPE ZCL_DBBR_tree_node_wrapper=>node_itab .
    METHODS get_node_prev_sibling
      IMPORTING
        !iv_node_key                TYPE tm_nodekey
      RETURNING
        VALUE(rr_node_prev_sibling) TYPE REF TO ZCL_DBBR_tree_node_wrapper .
    METHODS get_node_next_sibling
      IMPORTING
        !iv_node_key                TYPE tm_nodekey
      RETURNING
        VALUE(rr_node_next_sibling) TYPE REF TO ZCL_DBBR_tree_node_wrapper .
ENDCLASS.



CLASS ZCL_DBBR_list_tree_wrapper IMPLEMENTATION.


  METHOD add_node.
    DATA(ls_properties) = ir_node->get_node_properties( ).

    mr_list_tree_model->add_node(
        node_key                = ir_node->get_node_key( )
        relative_node_key       = iv_relative_node_key
        relationship            = iv_relationship
        isfolder                = ls_properties-isfolder
        image                   = ls_properties-n_image
*        drag_drop_id            =     " "
        user_object             = ls_properties-userobject
        item_table              = ir_node->get_items( )
    ).

    IF if_recursive = abap_true.
      " add children nodes
      LOOP AT ir_node->get_children( ) ASSIGNING FIELD-SYMBOL(<lr_child_node>).
        add_node(
            ir_node              = <lr_child_node>
            if_recursive         = if_recursive
            iv_relative_node_key = ir_node->get_node_key( )
            iv_relationship      = cl_list_tree_model=>relat_last_child
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mr_list_tree_model = ir_list_tree_model.
    mf_multiple_selection = if_multiple_selection.
  ENDMETHOD.


  METHOD get_node.
    rr_node = get_node_internal( iv_node_key ).
    rr_node->set_children( get_node_children( iv_node_key  = iv_node_key
                                              if_recursive = if_recursive ) ).
    rr_node->set_parent( get_node_parent( iv_node_key ) ).
    rr_node->set_next_sibling( get_node_next_sibling( iv_node_key ) ).
    rr_node->set_prev_sibling( get_node_prev_sibling( iv_node_key ) ).

  ENDMETHOD.


  METHOD get_node_children.
    mr_list_tree_model->node_get_children( EXPORTING node_key       = iv_node_key
                                           IMPORTING node_key_table = DATA(lt_children_node_key) ).

    LOOP AT lt_children_node_key ASSIGNING FIELD-SYMBOL(<lv_child_node>).
      APPEND get_node( iv_node_key  = <lv_child_node>
                       if_recursive = if_recursive ) TO rt_node_children.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_node_internal.
    DATA: lf_is_folder TYPE abap_bool.

    CHECK iv_node_key IS NOT INITIAL.

    mr_list_tree_model->node_get_properties( EXPORTING node_key   = iv_node_key
                                             IMPORTING properties = DATA(ls_properties) ).

    mr_list_tree_model->node_get_user_object(
      EXPORTING node_key    = iv_node_key
      IMPORTING user_object = DATA(lr_user_object)
    ).

    mr_list_tree_model->node_get_items( EXPORTING node_key   = iv_node_key
                                        IMPORTING item_table = DATA(lt_items) ).

    IF lr_user_object IS NOT INITIAL.
      TRY.
          lf_is_folder = xsdbool( CAST ZCL_DBBR_favmenu_entry( lr_user_object )->get_favmenu_data( )-favtype = zif_dbbr_c_favmenu_type=>folder ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.
    ELSE.
      lf_is_folder = abap_true.
    ENDIF.

    rr_node = NEW ZCL_DBBR_tree_node_wrapper(
        iv_node_key        = iv_node_key
        it_items           = lt_items
        is_node_properties = ls_properties
        if_is_folder       = lf_is_folder
    ).
  ENDMETHOD.


  METHOD get_node_next_sibling.
    mr_list_tree_model->node_get_next_sibling( EXPORTING node_key         = iv_node_key
                                               IMPORTING sibling_node_key = DATA(lv_sibling) ).
    rr_node_next_sibling = get_node_internal( lv_sibling ).
  ENDMETHOD.


  METHOD get_node_parent.
    mr_list_tree_model->node_get_parent( EXPORTING node_key        = iv_node_key
                                         IMPORTING parent_node_key = DATA(lv_parent) ).

    rr_node = get_node_internal( lv_parent ).
  ENDMETHOD.


  METHOD get_node_prev_sibling.
    mr_list_tree_model->node_get_prev_sibling( EXPORTING node_key         = iv_node_key
                                               IMPORTING sibling_node_key = DATA(lv_sibling) ).
    rr_node_prev_sibling = get_node_internal( lv_sibling ).
  ENDMETHOD.


  METHOD get_selected_node.
    IF mf_multiple_selection = abap_true.
      DATA(lt_node_refs) = get_selected_nodes( ).
      IF lt_node_refs IS NOT INITIAL.
        rr_node = lt_node_refs[ 1 ].
      ENDIF.
    ELSE.
      mr_list_tree_model->get_selected_node( IMPORTING node_key = DATA(lv_node) ).

      IF lv_node IS NOT INITIAL.
        rr_node = get_node(
          iv_node_key  = lv_node
          if_recursive = if_recursive
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_selected_nodes.
    mr_list_tree_model->get_selected_nodes( IMPORTING node_key_table = DATA(lt_nodes) ).

    IF lt_nodes IS NOT INITIAL.
      rt_nodes = VALUE #(
        FOR <lv_node_key> IN lt_nodes
        ( get_node( iv_node_key  = <lv_node_key>
                    if_recursive = if_recursive ) )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD is_node_expanded.
    mr_list_tree_model->get_expanded_nodes( IMPORTING node_key_table = DATA(lt_expanded_nodes) ).

    IF line_exists( lt_expanded_nodes[ table_line = iv_node_key ] ).
      rf_is_expanded = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
