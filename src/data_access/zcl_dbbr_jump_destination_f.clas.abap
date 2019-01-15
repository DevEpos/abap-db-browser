CLASS zcl_dbbr_jump_destination_f DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS save_jump_destinations
      IMPORTING
        !iv_query_id          TYPE zdbbr_query_id
        if_update_query_flag  TYPE abap_bool OPTIONAL
      CHANGING
        !ct_jump_destinations TYPE zdbbr_jumpdest_data_ui_itab .
    METHODS get_jump_destinations
      IMPORTING
        !iv_query_id                TYPE zdbbr_query_id
      RETURNING
        VALUE(rt_jump_destinations) TYPE zdbbr_jumpdest_data_ui_itab .
    METHODS delete_jumpdest_by_query_id
      IMPORTING
        !iv_query_id TYPE zdbbr_query_id .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS save_jump_destination
      CHANGING
        !cs_jump_destination TYPE zdbbr_jumpdest_data_ui .
    METHODS save_parameter
      IMPORTING
        !is_parameter TYPE zdbbr_jumpdestp .
    METHODS delete_parameters_for_jumpdest
      IMPORTING
        !iv_jumpdest_id TYPE zdbbr_jumpdest_id .
    METHODS delete_jumpdest
      IMPORTING
        !is_jump_definition TYPE zdbbr_jumpdest .
    METHODS read_jump_destinations
      IMPORTING
        !iv_query_id                TYPE zdbbr_query_id
      RETURNING
        VALUE(rt_jump_destinations) TYPE zdbbr_jumpdest_itab .
    METHODS read_params_for_jumpdest
      IMPORTING
        !iv_jumpdest_id  TYPE zdbbr_jumpdest_id
      RETURNING
        VALUE(rt_params) TYPE zdbbr_jumpparam_itab .
    METHODS delete_parameter_by_id
      IMPORTING
        !iv_parameter_id TYPE memoryid .
ENDCLASS.



CLASS zcl_dbbr_jump_destination_f IMPLEMENTATION.


  METHOD delete_jumpdest.
    DELETE zdbbr_jumpdest FROM is_jump_definition.
  ENDMETHOD.


  METHOD delete_jumpdest_by_query_id.
    DATA(lt_jump_destinations) = read_jump_destinations( iv_query_id = iv_query_id ).
    LOOP AT lt_jump_destinations ASSIGNING FIELD-SYMBOL(<ls_jump_dest>).
      delete_parameters_for_jumpdest( iv_jumpdest_id = <ls_jump_dest>-jumpdest_id ).
      delete_jumpdest( is_jump_definition = <ls_jump_dest> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD delete_parameters_for_jumpdest.
    DELETE FROM zdbbr_jumpdestp WHERE ref_jump_dest = iv_jumpdest_id.
  ENDMETHOD.


  METHOD delete_parameter_by_id.
    DELETE FROM zdbbr_jumpdestp WHERE parameter_id = iv_parameter_id.
  ENDMETHOD.


  METHOD get_jump_destinations.
    DATA: ls_transaction TYPE tstc.

    rt_jump_destinations = CORRESPONDING #( read_jump_destinations( iv_query_id = iv_query_id ) ).

    LOOP AT rt_jump_destinations ASSIGNING FIELD-SYMBOL(<ls_jump_destination>).
      SELECT SINGLE * FROM tstc
        INTO CORRESPONDING FIELDS OF ls_transaction
        WHERE tcode = <ls_jump_destination>-jump_target.

      IF sy-subrc = 0.
        <ls_jump_destination>-is_report_transaction = xsdbool( ls_transaction-cinfo = '80' ).
        <ls_jump_destination>-program_name = ls_transaction-pgmna.
      ENDIF.

    ENDLOOP.

    " fill parameters for each jump field
    LOOP AT rt_jump_destinations ASSIGNING FIELD-SYMBOL(<ls_jump_field>).
      <ls_jump_field>-parameters = read_params_for_jumpdest( iv_jumpdest_id = <ls_jump_field>-jumpdest_id ).
    ENDLOOP.
  ENDMETHOD.


  METHOD read_jump_destinations.
    SELECT * FROM zdbbr_jumpdest INTO TABLE @rt_jump_destinations
      WHERE ref_query_id = @iv_query_id.
  ENDMETHOD.


  METHOD read_params_for_jumpdest.
    SELECT * FROM zdbbr_jumpdestp INTO TABLE @rt_params
      WHERE ref_jump_dest = @iv_jumpdest_id.
  ENDMETHOD.


  METHOD save_jump_destination.
    DATA(ls_jumpdest) = CORRESPONDING zdbbr_jumpdest( cs_jump_destination ).

    MODIFY zdbbr_jumpdest FROM ls_jumpdest.

    DATA(lt_existing_params) = read_params_for_jumpdest( iv_jumpdest_id = ls_jumpdest-jumpdest_id ).
    LOOP AT lt_existing_params ASSIGNING FIELD-SYMBOL(<ls_existing_param>).
      IF NOT line_exists( cs_jump_destination-parameters[ parameter_id = <ls_existing_param>-parameter_id ] ).
        delete_parameter_by_id( <ls_existing_param>-parameter_id ).
      ENDIF.
    ENDLOOP.

    " save remaining parameters
    LOOP AT cs_jump_destination-parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
      save_parameter( <ls_param> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD save_jump_destinations.
    " first find all persisted jump fields for the given query
    DATA(lt_jumpdestinations) = read_jump_destinations( iv_query_id = iv_query_id ).

    " delete all jump fields that are not in the new list
    LOOP AT lt_jumpdestinations ASSIGNING FIELD-SYMBOL(<ls_existing_jumpdest>).
      IF NOT line_exists( ct_jump_destinations[ jumpdest_id = <ls_existing_jumpdest>-jumpdest_id ] ).
        " first delete dependent parameters
        delete_parameters_for_jumpdest( <ls_existing_jumpdest>-jumpdest_id ).
        delete_jumpdest( is_jump_definition = <ls_existing_jumpdest> ).
      ENDIF.

    ENDLOOP.

    " save remaining jump destinations
    LOOP AT ct_jump_destinations ASSIGNING FIELD-SYMBOL(<ls_jump_dest>).
      " is this a new jump destination ?
      IF <ls_jump_dest>-jumpdest_id IS INITIAL.
        <ls_jump_dest>-jumpdest_id = zcl_dbbr_system_helper=>create_guid_22( ).
      ENDIF.

      LOOP AT <ls_jump_dest>-parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
        <ls_param>-ref_jump_dest = <ls_jump_dest>-jumpdest_id.
      ENDLOOP.

      save_jump_destination(
        CHANGING
          cs_jump_destination = <ls_jump_dest>
      ).
    ENDLOOP.

    IF if_update_query_flag = abap_true.
      NEW zcl_dbbr_query_factory( )->update_query_flags(
        iv_query_id = iv_query_id if_has_jump_fields = xsdbool( ct_jump_destinations IS NOT INITIAL )
      ).
    ENDIF.

    " commit changes
    MESSAGE |{ 'Jump destinations were successfully saved!'(001) }| TYPE 'S'.
    COMMIT WORK.
  ENDMETHOD.


  METHOD save_parameter.
    MODIFY zdbbr_jumpdestp FROM is_parameter.
  ENDMETHOD.
ENDCLASS.
