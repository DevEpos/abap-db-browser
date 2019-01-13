*&---------------------------------------------------------------------*
*& Report  zdbbr_test_join_manager
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdbbr_test_join_manager.

data(lr_s_join) = new zdbbr_join_def(
    primary_table = 'DD02L'
    tables        = value #(
      ( add_table         = 'TADIR'
        join_type         = zif_dbbr_c_join_types=>inner_join
        field_conditions  = value #(
          ( field      = 'OBJ_NAME'
            ref_field  = 'TABNAME'
            ref_table  = 'DD02L'
            operator   = '=' )
        )
        filter_conditions = value #(
         ( tabname   = 'DD02L'
           fieldname = 'AS4LOCAL'
           operator  = '='
           value     = 'A'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'AND' )
         ( tabname   = 'TADIR'
           fieldname = 'OBJECT'
           operator  = '='
           value     = 'TABL'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'AND' )
         ( tabname   = 'DD02L'
           fieldname = 'VIEWCLASS'
           operator  = '='
           value     = ''
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
         ( tabname   = 'DD02L'
           fieldname = 'VIEWCLASS'
           operator  = '='
           value     = 'D'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'AND' )
         ( tabname   = 'DD02L'
           fieldname = 'TABCLASS'
           operator  = '='
           value     = 'TRANSP'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
         ( tabname   = 'DD02L'
           fieldname = 'TABCLASS'
           operator  = '='
           value     = 'POOL'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
         ( tabname   = 'DD02L'
           fieldname = 'TABCLASS'
           operator  = '='
           value     = 'VIEW'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
        )
       )
    )
).

data(lr_join_manager) = new zcl_dbbr_join_manager(
    iv_primary_table = lr_s_join->primary_table
    ir_s_join_ref    = lr_s_join
).

lr_join_manager->zif_uitb_view~show( ).

IF lr_join_manager->was_updated( ).
  LOOP AT lr_s_join->tables ASSIGNING FIELD-SYMBOL(<ls_table>).
    cl_demo_output=>write_text( |Table { <ls_table>-add_table }| ).
    cl_demo_output=>write_text( |Fields for { <ls_table>-add_table }| ).
    cl_demo_output=>write_data( <ls_table>-field_conditions ).
    cl_demo_output=>write_text( |Filters for { <ls_table>-add_table }| ).
    cl_demo_output=>write_data( <ls_table>-filter_conditions ).
  ENDLOOP.

  cl_demo_output=>display( ).
ENDIF.
