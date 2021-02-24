"! <p class="shorttext synchronized" lang="en">Map for custom value help</p>
CLASS zcl_dbbr_custom_f4_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Reads custom F4 definitions for the given table-field</p>
    METHODS read_custom_f4_definition
      IMPORTING
        iv_tablename             TYPE tabname
        iv_fieldname             TYPE fieldname
        iv_rollname              TYPE rollname OPTIONAL
        is_built_in_type         TYPE zdbbr_built_in_data_type OPTIONAL
      EXPORTING
        et_custom_f4_definitions TYPE zdbbr_f4_data_itab.
    "! <p class="shorttext synchronized" lang="en">Reads custom f4 definitions for the given table</p>
    METHODS read_custom_f4_definitions
      IMPORTING
        iv_tablename TYPE tabname.
    "! <p class="shorttext synchronized" lang="en">Read and cache custom f4 w/o necessary field assignment</p>
    "!
    METHODS read_same_type_custom_f4_defs.
    "! <p class="shorttext synchronized" lang="en">Clears all buffered custom f4 definitions</p>
    METHODS clear.
    "! <p class="shorttext synchronized" lang="en">Clears all buffered F4 def. for the given table field</p>
    METHODS clear_f4_for_field
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname.
    "! <p class="shorttext synchronized" lang="en">Determine F4 for given table field</p>
    METHODS determine_f4_for_field
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname.
    "! <p class="shorttext synchronized" lang="en">Checks if there is a custom F4 for the given table field</p>
    METHODS entry_exists
      IMPORTING
        iv_tabname       TYPE tabname
        iv_fieldname     TYPE fieldname
        iv_rollname      TYPE rollname OPTIONAL
        is_built_in_type TYPE zdbbr_built_in_data_type OPTIONAL
      RETURNING
        VALUE(rf_exists) TYPE boolean.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_custom_f4_by_dt_map TYPE HASHED TABLE OF zdbbr_custom_f4_map WITH UNIQUE KEY rollname datatype length.
    DATA: mt_custom_f4_map TYPE zdbbr_custom_f4_map_itab.
ENDCLASS.



CLASS zcl_dbbr_custom_f4_map IMPLEMENTATION.

  METHOD clear.
    CLEAR mt_custom_f4_map.
  ENDMETHOD.

  METHOD clear_f4_for_field.
    DELETE mt_custom_f4_map WHERE tabname = iv_tabname
                              AND fieldname = iv_fieldname.
  ENDMETHOD.

  METHOD read_custom_f4_definition.
    IF iv_tablename = zif_dbbr_c_global=>c_parameter_dummy_table.
      ASSIGN mt_custom_f4_map[ fieldname = iv_fieldname ] TO FIELD-SYMBOL(<ls_custom_f4_map>).
    ELSE.
      ASSIGN mt_custom_f4_map[ tabname   = iv_tablename
                               fieldname = iv_fieldname ] TO <ls_custom_f4_map>.
    ENDIF.

    IF sy-subrc = 0.
      et_custom_f4_definitions = <ls_custom_f4_map>-f4_definitions.
    ELSE.
      IF iv_rollname IS NOT INITIAL.
        ASSIGN mt_custom_f4_by_dt_map[ rollname = iv_rollname ] TO <ls_custom_f4_map>.
      ELSE.
        ASSIGN mt_custom_f4_by_dt_map[ datatype = is_built_in_type-datatype
                                       length   = is_built_in_type-leng    ] TO <ls_custom_f4_map>.
      ENDIF.
      IF sy-subrc = 0.
        et_custom_f4_definitions = <ls_custom_f4_map>-f4_definitions.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD determine_f4_for_field.
    zcl_dbbr_custom_f4_factory=>find_f4_for_table(
      EXPORTING iv_tabname = iv_tabname
      IMPORTING et_f4      = DATA(lt_f4)
    ).

    DELETE lt_f4 WHERE fieldname <> iv_fieldname.

    CHECK lt_f4 IS NOT INITIAL.

    INSERT VALUE #(
        tabname        = iv_tabname
        fieldname      = iv_fieldname
        f4_definitions = lt_f4
     ) INTO TABLE mt_custom_f4_map.
  ENDMETHOD.


  METHOD entry_exists.
    rf_exists = xsdbool( line_exists( mt_custom_f4_map[ tabname   = iv_tabname
                                                        fieldname = iv_fieldname ] ) ).

    CHECK rf_exists = abap_false.

    IF iv_rollname IS INITIAL AND is_built_in_type IS INITIAL.
      RETURN.
    ENDIF.

*.. Check cache for already read F4 that match the passed data type
    IF iv_rollname IS NOT INITIAL.
      rf_exists = xsdbool( line_exists( mt_custom_f4_by_dt_map[ rollname = iv_rollname ] ) ).
*.... If the requesting field has a data element no further checks against
*.... the buffered value helps will be performed
      RETURN.
    ENDIF.

    rf_exists = xsdbool( line_exists( mt_custom_f4_by_dt_map[ datatype = is_built_in_type-datatype
                                                              length   = is_built_in_type-leng     ] ) ).
  ENDMETHOD.

  METHOD read_custom_f4_definitions.

    IF NOT line_exists( mt_custom_f4_map[ tabname = iv_tablename ] ).

      zcl_dbbr_custom_f4_factory=>find_f4_for_table(
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
            f4_definitions = VALUE zdbbr_f4_data_itab( FOR f4 IN GROUP <ls_f4_group> ( f4 ) )
         ) INTO TABLE mt_custom_f4_map.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read_same_type_custom_f4_defs.
    zcl_dbbr_custom_f4_factory=>find_f4_for_datatype(
        EXPORTING if_apply_to_same_data = abap_true
        IMPORTING et_f4                 = DATA(lt_f4)
    ).
    LOOP AT lt_f4 ASSIGNING FIELD-SYMBOL(<ls_f4>)
      GROUP BY ( rollname = <ls_f4>-rollname
                 datatype = <ls_f4>-datatype
                 length   = <ls_f4>-length )
      ASSIGNING FIELD-SYMBOL(<ls_f4_group>).

      INSERT VALUE #(
        rollname       = <ls_f4_group>-rollname
        datatype       = <ls_f4_group>-datatype
        length         = <ls_f4_group>-length
        f4_definitions = VALUE #( FOR f4 IN GROUP <ls_f4_group> ( f4 )  )
      ) INTO TABLE mt_custom_f4_by_dt_map.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
