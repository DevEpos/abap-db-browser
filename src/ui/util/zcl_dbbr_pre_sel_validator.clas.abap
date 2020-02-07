CLASS zcl_dbbr_pre_sel_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_selection_fields TYPE zdbbr_selfield_itab
        ir_tabfields        TYPE REF TO zcl_dbbr_tabfield_list
        is_join_definition  TYPE zdbbr_join_def
        is_technical_infos  TYPE zdbbr_tech_info.
    METHODS validate.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_selection_fields   TYPE zdbbr_selfield_itab.
    DATA mr_tabfields          TYPE REF TO zcl_dbbr_tabfield_list.
    DATA ms_join_definition    TYPE zdbbr_join_def.
    DATA ms_technical_infos    TYPE zdbbr_tech_info.
    DATA: mf_virtual_join TYPE xsdboolean,
          mf_group_by     TYPE xsdboolean.
    METHODS determine_selection_status.
ENDCLASS.



CLASS zcl_dbbr_pre_sel_validator IMPLEMENTATION.


  METHOD constructor.
    mt_selection_fields   = it_selection_fields.
    mr_tabfields          = ir_tabfields.
    ms_join_definition    = is_join_definition.
    ms_technical_infos    = is_technical_infos.
  ENDMETHOD.


  METHOD determine_selection_status.
    mf_virtual_join = xsdbool( ms_join_definition IS NOT INITIAL AND
                               line_exists( ms_join_definition-tables[ is_virtual = abap_true ] ) ).

    LOOP AT mt_selection_fields ASSIGNING FIELD-SYMBOL(<ls_selfield>) WHERE group_by = abap_true
                                                                         OR aggregation <> ''
                                                                         OR totals = abap_true.
      EXIT.
    ENDLOOP.

    mf_group_by = xsdbool( sy-subrc <> 0 ).

  ENDMETHOD.


  METHOD validate.
    determine_selection_status( ).

    IF mf_group_by = abap_true AND mf_virtual_join = abap_true.
      " check if all

    ENDIF.
  ENDMETHOD.
ENDCLASS.
