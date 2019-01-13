class ZCL_DBBR_JUMP_DESTINATION_F definition
  public
  final
  create public .

public section.

  methods SAVE_JUMP_DESTINATIONS
    importing
      !IV_query_ID type ZDBBR_query_ID
    changing
      !CT_JUMP_DESTINATIONS type ZDBBR_JUMPDEST_DATA_UI_ITAB .
  methods GET_JUMP_DESTINATIONS
    importing
      !IV_query_ID type ZDBBR_query_ID
    returning
      value(RT_JUMP_DESTINATIONS) type ZDBBR_JUMPDEST_DATA_UI_ITAB .
  methods DELETE_JUMPDEST_BY_query_ID
    importing
      !IV_query_ID type ZDBBR_query_ID .
  PROTECTED SECTION.
private section.

  methods SAVE_JUMP_DESTINATION
    changing
      !CS_JUMP_DESTINATION type ZDBBR_JUMPDEST_DATA_UI .
  methods SAVE_PARAMETER
    importing
      !IS_PARAMETER type ZDBBR_JUMPDESTP .
  methods DELETE_PARAMETERS_FOR_JUMPDEST
    importing
      !IV_JUMPDEST_ID type ZDBBR_JUMPDEST_ID .
  methods DELETE_JUMPDEST
    importing
      !IS_JUMP_DEFINITION type ZDBBR_JUMPDEST .
  methods READ_JUMP_DESTINATIONS
    importing
      !IV_query_ID type ZDBBR_query_ID
    returning
      value(RT_JUMP_DESTINATIONS) type ZDBBR_JUMPDEST_ITAB .
  methods READ_PARAMS_FOR_JUMPDEST
    importing
      !IV_JUMPDEST_ID type ZDBBR_JUMPDEST_ID
    returning
      value(RT_PARAMS) type ZDBBR_JUMPPARAM_ITAB .
  methods DELETE_PARAMETER_BY_ID
    importing
      !IV_PARAMETER_ID type MEMORYID .
ENDCLASS.



CLASS ZCL_DBBR_JUMP_DESTINATION_F IMPLEMENTATION.


  METHOD delete_jumpdest.
    DELETE ZDBBR_jumpdest FROM is_jump_definition.
  ENDMETHOD.


  METHOD delete_jumpdest_by_query_id.
    DATA(lt_jump_destinations) = read_jump_destinations( iv_query_id = iv_query_id ).
    LOOP AT lt_jump_destinations ASSIGNING FIELD-SYMBOL(<ls_jump_dest>).
      delete_parameters_for_jumpdest( iv_jumpdest_id = <ls_jump_dest>-jumpdest_id ).
      delete_jumpdest( is_jump_definition = <ls_jump_dest> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD delete_parameters_for_jumpdest.
    DELETE FROM ZDBBR_jumpdestp WHERE ref_jump_dest = iv_jumpdest_id.
  ENDMETHOD.


  METHOD delete_parameter_by_id.
    DELETE FROM ZDBBR_jumpdestp WHERE parameter_id = iv_parameter_id.
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
    SELECT * FROM ZDBBR_jumpdest INTO TABLE @rt_jump_destinations
      WHERE ref_query_id = @iv_query_id.
  ENDMETHOD.


  METHOD read_params_for_jumpdest.
    SELECT * FROM ZDBBR_jumpdestp INTO TABLE @rt_params
      WHERE ref_jump_dest = @iv_jumpdest_id.
  ENDMETHOD.


  METHOD save_jump_destination.
    DATA(ls_jumpdest) = CORRESPONDING ZDBBR_jumpdest( cs_jump_destination ).

    MODIFY ZDBBR_jumpdest FROM ls_jumpdest.

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
        <ls_jump_dest>-jumpdest_id = ZCL_DBBR_system_helper=>create_guid_22( ).
      ENDIF.

      LOOP AT <ls_jump_dest>-parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
        <ls_param>-ref_jump_dest = <ls_jump_dest>-jumpdest_id.
      ENDLOOP.

      save_jump_destination(
        CHANGING
          cs_jump_destination = <ls_jump_dest>
      ).
    ENDLOOP.

    " commit changes
    MESSAGE |Jump destinations were successfully saved!| TYPE 'S'.
    COMMIT WORK.
  ENDMETHOD.


  METHOD save_parameter.
    MODIFY ZDBBR_jumpdestp FROM is_parameter.
  ENDMETHOD.
ENDCLASS.
