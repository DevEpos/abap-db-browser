"! <p class="shorttext synchronized" lang="en">Creates subroutine pool program for selection data from db</p>
CLASS zcl_dbbr_select_prog_creator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Creates new instance of select program</p>
    "!
    CLASS-METHODS create_program
      IMPORTING
        !if_only_create_count_logic TYPE abap_bool OPTIONAL
        !if_create_for_all          TYPE abap_bool OPTIONAL
        !is_association_target      TYPE zdbbr_cds_association OPTIONAL
        !it_select                  TYPE zdbbr_string_t
        !it_from                    TYPE zdbbr_string_t
        !it_where                   TYPE zdbbr_string_t
        !it_order_by                TYPE zdbbr_string_t
        !it_group_by                TYPE zdbbr_string_t
        !iv_max_size                TYPE i
      RETURNING
        VALUE(rr_instance)          TYPE REF TO zcl_dbbr_select_prog_creator
      RAISING
        zcx_dbbr_dyn_prog_generation .
    "! <p class="shorttext synchronized" lang="en">Determine during active aggregation</p>
    METHODS determine_size_for_group_by
      IMPORTING
        ir_t_data      TYPE REF TO data
      RETURNING
        VALUE(rv_size) TYPE zdbbr_no_of_lines
      RAISING
        zcx_dbbr_selection_common.
    "! <p class="shorttext synchronized" lang="en">Determines the size of existing entries</p>
    "!
    METHODS determine_size
      IMPORTING
        !ir_t_for_all  TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(rv_size) TYPE zdbbr_no_of_lines
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Returns SQL String for current select</p>
    "!
    METHODS get_select_sql
      RETURNING
        VALUE(rv_select_sql) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Selects data</p>
    "!
    METHODS select_data
      IMPORTING
        !ir_t_for_all  TYPE REF TO data OPTIONAL
      EXPORTING
        VALUE(et_data) TYPE table
      RAISING
        zcx_dbbr_selection_common .
    "! <p class="shorttext synchronized" lang="en">Set maximum number of rows</p>
    "!
    METHODS set_max_rows
      IMPORTING
        !iv_max_rows TYPE i .
    "! <p class="shorttext synchronized" lang="en">Update from clause</p>
    "!
    METHODS update_from
      IMPORTING
        it_from TYPE zdbbr_string_t.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! List of Strings
    DATA mt_select TYPE zdbbr_string_t .
    "! List of Strings
    DATA mt_from TYPE zdbbr_string_t .
    "! List of Strings
    DATA mt_where TYPE zdbbr_string_t .
    "! List of Strings
    DATA mt_order_by TYPE zdbbr_string_t .
    "! List of Strings
    DATA mt_group_by TYPE zdbbr_string_t .
    DATA mv_max_size TYPE i .
    DATA mv_class TYPE string .
    DATA mf_only_create_count_logic TYPE abap_bool .
    DATA mf_create_for_all TYPE abap_bool .
    "! Association Information for CDS View
    DATA ms_assocication_target TYPE zdbbr_cds_association .

    "! <p class="shorttext synchronized" lang="en">Fill coding lines with from clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_from
      CHANGING
        !ct_lines TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with group by clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_group_by
      CHANGING
        !ct_lines TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with order by clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_order_by
      CHANGING
        !ct_lines TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with select clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_select
      CHANGING
        !ct_lines TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Fill coding lines with where clause</p>
    "! @parameter ct_lines | <p class="shorttext synchronized" lang="en"></p>
    METHODS fill_where
      CHANGING
        !ct_lines TYPE zdbbr_string_t .
    "! <p class="shorttext synchronized" lang="en">Generate subroutine pool for selection</p>
    "!
    METHODS generate_subroutine
      EXPORTING
        VALUE(ev_prog)          TYPE string
        VALUE(ev_error_message) TYPE string
        VALUE(ev_error_line)    TYPE i
        VALUE(ev_error_offset)  TYPE i
        VALUE(ev_error_word)    TYPE string .
ENDCLASS.



CLASS zcl_dbbr_select_prog_creator IMPLEMENTATION.


  METHOD create_program.
    rr_instance = NEW zcl_dbbr_select_prog_creator( ).

    rr_instance->mf_only_create_count_logic = if_only_create_count_logic.
    rr_instance->mf_create_for_all = if_create_for_all.
    rr_instance->ms_assocication_target = is_association_target.
    rr_instance->mt_select   = it_select.
    rr_instance->mt_from     = it_from.
    rr_instance->mt_where    = it_where.
    rr_instance->mt_order_by = it_order_by.
    rr_instance->mt_group_by = it_group_by.
    rr_instance->mv_max_size = iv_max_size.

    IF if_create_for_all = abap_true.
      rr_instance->generate_subroutine(
        IMPORTING
          ev_prog          = DATA(lv_prog)
          ev_error_message = DATA(lv_error_message)
          ev_error_line    = DATA(lv_error_line)
          ev_error_offset  = DATA(lv_error_offset)
          ev_error_word    = DATA(lv_error_word)
      ).

      IF lv_error_message IS NOT INITIAL.
        zcx_dbbr_dyn_prog_generation=>raise_dyn_prog_generation(
            iv_text = lv_error_message
        ).
      ENDIF.

      rr_instance->mv_class = |\\PROGRAM={ lv_prog }\\CLASS=MAIN|.
    ENDIF.

  ENDMETHOD.

  METHOD determine_size_for_group_by.
    DATA: lx_root TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_data> TYPE table.

    ASSIGN ir_t_data->* TO <lt_data>.

    TRY.
        SELECT (mt_select)
          FROM (mt_from)
          WHERE (mt_where)
          GROUP BY (mt_group_by)
          ORDER BY (mt_order_by)
        INTO CORRESPONDING FIELDS OF TABLE @<lt_data>.

        rv_size = lines( <lt_data> ).
        CLEAR <lt_data>.
      CATCH cx_root INTO lx_root.
        RAISE EXCEPTION TYPE zcx_dbbr_selection_common
          EXPORTING
            previous = lx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD determine_size.
    DATA: lx_root TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_for_all_data> TYPE table.

    IF mf_create_for_all = abap_true.
      IF ir_t_for_all IS NOT BOUND.
        RETURN.
      ENDIF.

      ASSIGN ir_t_for_all->* TO <lt_for_all_data>.

      IF <lt_for_all_data> IS INITIAL.
        RETURN.
      ENDIF.

      CALL METHOD (mv_class)=>size
        EXPORTING
          it_for_all_data = <lt_for_all_data>
        IMPORTING
          ev_size         = rv_size.
    ELSE.
      TRY.
          SELECT COUNT( * )
            FROM (mt_from)
            WHERE (mt_where)
          INTO @rv_size.
        CATCH cx_root INTO lx_root.
          RAISE EXCEPTION TYPE zcx_dbbr_selection_common
            EXPORTING
              previous = lx_root.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD fill_from.
    DATA: lv_from TYPE string.

    LOOP AT mt_from ASSIGNING FIELD-SYMBOL(<lv_from>).
      CLEAR: lv_from.
      IF sy-tabix = 1.
        lv_from = |  FROM { <lv_from> }|.
      ELSE.
        lv_from = |       { <lv_from> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_from ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_group_by.
    DATA: lv_group_by TYPE string.

    LOOP AT mt_group_by ASSIGNING FIELD-SYMBOL(<lv_group_by>).
      CLEAR: lv_group_by.
      IF sy-tabix = 1.
        lv_group_by = |  GROUP BY { <lv_group_by> }|.
      ELSE.
        lv_group_by = |           { <lv_group_by> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_group_by ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_order_by.
    DATA: lv_order_by TYPE string.

    LOOP AT mt_order_by ASSIGNING FIELD-SYMBOL(<lv_order_by>).
      CLEAR: lv_order_by.
      IF sy-tabix = 1.
        lv_order_by = |  ORDER BY { <lv_order_by> }|.
      ELSE.
        lv_order_by = |           { <lv_order_by> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_order_by ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_select.
    DATA: lv_select TYPE string.

    LOOP AT mt_select ASSIGNING FIELD-SYMBOL(<lv_select>).
      CLEAR: lv_select.
      IF sy-tabix = 1.
        lv_select = |SELECT { <lv_select> }|.
      ELSE.
        lv_select = |       { <lv_select> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_select ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_where.
    DATA: lv_where TYPE string.

    LOOP AT mt_where ASSIGNING FIELD-SYMBOL(<lv_where>).
      CLEAR: lv_where.
      IF sy-tabix = 1.
        lv_where = |WHERE { <lv_where> }|.
      ELSE.
        lv_where = |   { <lv_where> }|.
      ENDIF.

      ct_lines = VALUE #( BASE ct_lines ( lv_where ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_subroutine.
    DATA: lt_lines          TYPE zdbbr_string_t,
          lv_prog           TYPE string,
          lv_error_message  TYPE string,
          lv_code_line      TYPE string,
          lv_for_all_suffix TYPE string,
          lv_query_offset   TYPE i.

    lt_lines = VALUE #(
*... insert program start command
      ( |program.|                                   )
    ).

*... create type definition for association navigation if requested
    IF mf_create_for_all = abap_true.
      lt_lines = VALUE #( BASE lt_lines
        ( |TYPES: BEGIN OF lty_for_all_data,| )
      ).
      LOOP AT ms_assocication_target-fields ASSIGNING FIELD-SYMBOL(<ls_assoc_field>).
        lt_lines = VALUE #( BASE lt_lines
          ( |         { <ls_assoc_field>-name } TYPE { ms_assocication_target-ref_cds_view }-{ <ls_assoc_field>-name },| )
        ).
      ENDLOOP.
      lt_lines = VALUE #( BASE lt_lines
        ( |	      END OF lty_for_all_data.|                                               )
        ( |TYPES: ltt_for_all TYPE TABLE OF lty_for_all_data.| )
      ).

    ENDIF.


*... insert class definition to perform various kinds of selects
    lt_lines = VALUE #( BASE lt_lines
      ( |CLASS main DEFINITION.|                     )
      ( |  PUBLIC SECTION.|                          )
    ).
    lt_lines = VALUE #( BASE lt_lines
      ( |    CLASS-METHODS size|                     )
    ).
    IF mf_create_for_all = abap_true.
      lt_lines = VALUE #( BASE lt_lines
        ( |      IMPORTING|                            )
        ( |                it_for_all_data TYPE TABLE| )
      ).
    ENDIF.
    lt_lines = VALUE #( BASE lt_lines
      ( |      EXPORTING|                            )
      ( |                ev_size TYPE i|             )
      ( |      RAISING   zcx_dbbr_selection_common.|           )
    ) .
    IF mf_only_create_count_logic = abap_false.

      lt_lines = VALUE #( BASE lt_lines
        ( |    CLASS-METHODS select|                   )
      ).
      IF mf_create_for_all = abap_true.
        lt_lines = VALUE #( BASE lt_lines
          ( |      IMPORTING|                            )
          ( |                it_for_all_data TYPE TABLE| )
        ).
      ENDIF.
      lt_lines = VALUE #( BASE lt_lines
        ( |      EXPORTING|                            )
        ( |                et_data TYPE TABLE| )
        ( |      RAISING   zcx_dbbr_selection_common.| )
      ).
    ENDIF.

    lt_lines = VALUE #( BASE lt_lines
      ( |ENDCLASS.|                                  )
    ).


    lt_lines = VALUE #( BASE lt_lines
      ( |CLASS main IMPLEMENTATION.|                )

*... create coding for determing the found lines for a specific where condition
*... and from clause
      ( |  METHOD size.|                            )
    ).

    IF mf_create_for_all = abap_true.
      lt_lines = VALUE #( BASE lt_lines
        ( |  FIELD-SYMBOLS: <lt_for_all> TYPE ltt_for_all.| )
        ( |  ASSIGN it_for_all_data to <lt_for_all>.|       )
        ( ||                                                )
      ).
    ENDIF.

    lt_lines = VALUE #( BASE lt_lines
      ( |    TRY .|                                 )
      ( |        SELECT COUNT( * )|                 )
    ).

    fill_from( CHANGING ct_lines = lt_lines ).
    IF mf_create_for_all = abap_true.
      lt_lines = VALUE #( BASE lt_lines
        ( |          FOR ALL ENTRIES IN @<lt_for_all>| )
      ).
    ENDIF.
    fill_where( CHANGING ct_lines = lt_lines ).

    lt_lines = VALUE #( BASE lt_lines
      ( |        INTO @ev_size|                                  )
      ( |          BYPASSING BUFFER.|                            )
      ( |      CATCH cx_root INTO DATA(lx_root).|                )
      ( |        RAISE EXCEPTION TYPE zcx_dbbr_selection_common| )
      ( |          EXPORTING|                                    )
      ( |            previous = lx_root.|                        )
      ( |    ENDTRY.|                                            )
      ( |  ENDMETHOD.|                                           )
    ).

    IF mf_only_create_count_logic = abap_false.

*... creating logic for actually selecting lines from one or several tables or cds views
      lt_lines = VALUE #( BASE lt_lines
        ( |  METHOD select.|    )
      ).
      IF mf_create_for_all = abap_true.
        lt_lines = VALUE #( BASE lt_lines
          ( |  FIELD-SYMBOLS: <lt_for_all> TYPE ltt_for_all.| )
          ( |  ASSIGN it_for_all_data to <lt_for_all>.|       )
          ( ||                                                )
        ).
      ENDIF.
      lt_lines = VALUE #( BASE lt_lines
        ( |    TRY .|           )
      ).
      fill_select( CHANGING ct_lines = lt_lines ).
      fill_from( CHANGING ct_lines = lt_lines ).

      IF mf_create_for_all = abap_true.
        lt_lines = VALUE #( BASE lt_lines
          ( |          FOR ALL ENTRIES IN @<lt_for_all>| )
        ).
      ENDIF.

      fill_where( CHANGING ct_lines = lt_lines ).

      IF mf_create_for_all = abap_false.
        fill_group_by( CHANGING ct_lines = lt_lines ).
        fill_order_by( CHANGING ct_lines = lt_lines ).
      ENDIF.

      lt_lines = VALUE #( BASE lt_lines
        ( |        INTO TABLE @data(lt_data) UP TO { mv_max_size } ROWS|  )
        ( |          BYPASSING BUFFER.|                                   )
        ( |        MOVE-CORRESPONDING lt_data to et_data.|                )
        ( |      CATCH cx_root INTO DATA(lx_root).|                       )
        ( |        RAISE EXCEPTION TYPE zcx_dbbr_selection_common|        )
        ( |          EXPORTING|                                           )
        ( |            previous = lx_root.|                               )
        ( |    ENDTRY.|                                                   )
        ( |  ENDMETHOD.|                                                  )
      ).
    ENDIF.

    lt_lines = VALUE #( BASE lt_lines
      ( |ENDCLASS.|                                                     )
    ).

    ev_prog = 'TEMP1'.

    GENERATE SUBROUTINE POOL lt_lines
        NAME    ev_prog
        MESSAGE ev_error_message
        LINE    ev_error_line
        OFFSET  ev_error_offset
        WORD    ev_error_word.                         "#EC CI_GENERATE

  ENDMETHOD.


  METHOD select_data.
    DATA: lx_root TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_for_all_data> TYPE table.

    IF mf_create_for_all = abap_true.
      IF ir_t_for_all IS NOT BOUND.
        RETURN.
      ENDIF.

      ASSIGN ir_t_for_all->* TO <lt_for_all_data>.

      IF <lt_for_all_data> IS INITIAL.
        RETURN.
      ENDIF.

      CALL METHOD (mv_class)=>select
        EXPORTING
          it_for_all_data = <lt_for_all_data>
        IMPORTING
          et_data         = et_data.
    ELSE.
***      CALL METHOD (mv_class)=>select IMPORTING et_data = et_data.
      TRY.
          SELECT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            GROUP BY (mt_group_by)
            ORDER BY (mt_order_by)
          INTO CORRESPONDING FIELDS OF TABLE @et_data
            UP TO @mv_max_size ROWS.
        CATCH cx_root INTO lx_root.
          RAISE EXCEPTION TYPE zcx_dbbr_selection_common
            EXPORTING
              previous = lx_root.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD set_max_rows.
    mv_max_size = iv_max_rows.
  ENDMETHOD.


  METHOD update_from.
    mt_from = it_from.
  ENDMETHOD.

  METHOD get_select_sql.
    DATA: lt_sql_lines TYPE string_table.

    fill_select( CHANGING ct_lines = lt_sql_lines ).
    fill_from( CHANGING ct_lines = lt_sql_lines ).
    fill_where( CHANGING ct_lines = lt_sql_lines ).
    fill_group_by( CHANGING ct_lines = lt_sql_lines ).
    fill_order_by( CHANGING ct_lines = lt_sql_lines ).

    CONCATENATE LINES OF lt_sql_lines INTO rv_select_sql SEPARATED BY cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.

ENDCLASS.
