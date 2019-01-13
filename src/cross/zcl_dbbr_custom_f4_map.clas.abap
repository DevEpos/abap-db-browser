CLASS ZCL_DBBR_custom_f4_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS read_custom_f4_definition
      IMPORTING
        iv_tablename             TYPE tabname
        iv_fieldname             TYPE fieldname
      EXPORTING
        et_custom_f4_definitions TYPE ZDBBR_f4_data_itab.
    METHODS read_custom_f4_definitions
      IMPORTING
        iv_tablename TYPE tabname.
    METHODS clear.
    METHODS entry_exists
      IMPORTING
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
      RETURNING
        VALUE(rf_exists) TYPE boolean.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_custom_f4_map TYPE ZDBBR_custom_f4_map_itab.
ENDCLASS.



CLASS ZCL_DBBR_custom_f4_map IMPLEMENTATION.

  METHOD clear.
    CLEAR mt_custom_f4_map.
  ENDMETHOD.

  METHOD read_custom_f4_definition.
*&---------------------------------------------------------------------*
*& Description: Reads existing f4 help definition definend in ZDBBROIN_F4
*&---------------------------------------------------------------------*
    ASSIGN mt_custom_f4_map[ tabname   = iv_tablename
                             fieldname = iv_fieldname ] TO FIELD-SYMBOL(<ls_custom_f4_map>).
    IF sy-subrc = 0.
      et_custom_f4_definitions = <ls_custom_f4_map>-f4_definitions.
    ENDIF.
  ENDMETHOD.

  METHOD entry_exists.
    rf_exists = xsdbool( line_exists( mt_custom_f4_map[ tabname   = iv_tabname
                                                        fieldname = iv_fieldname ] ) ).
  ENDMETHOD.

  METHOD read_custom_f4_definitions.
*&---------------------------------------------------------------------*
*& Description: Reads existing f4 help definitions definend in ZDBBROIN_F4
*&---------------------------------------------------------------------*

    IF NOT line_exists( mt_custom_f4_map[ tabname = iv_tablename ] ).
      DATA(lr_custom_f4_factory) = NEW ZCL_DBBR_custom_f4_factory( ).

      lr_custom_f4_factory->find_f4_for_table(
        EXPORTING iv_tabname = iv_tablename
        IMPORTING et_f4      = DATA(lt_f4)
      ).

      LOOP AT lt_f4 ASSIGNING FIELD-SYMBOL(<ls_f4>)
        GROUP BY ( tabname   = <ls_f4>-tabname
                   fieldname = <ls_f4>-fieldname )
        ASSIGNING FIELD-SYMBOL(<ls_f4_group>).

        INSERT VALUE #(
            tabname        = <ls_f4_group>-tabname
            fieldname      = <ls_f4_group>-fieldname
            f4_definitions = VALUE ZDBBR_f4_data_itab( FOR f4 IN GROUP <ls_f4_group> ( f4 ) )
         ) INTO TABLE mt_custom_f4_map.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
