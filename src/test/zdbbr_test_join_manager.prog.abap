*&---------------------------------------------------------------------*
*& Report  zdbbr_test_join_manager
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdbbr_test_join_manager.

DATA(lr_join) = NEW zdbbr_join_def(
    primary_table           = 'DD02L'
    primary_table_alias_alv = 'A'
    primary_table_alias     = 'TABLES'
    tables        = VALUE #(
      ( add_table         = 'TADIR'
        add_table_alias   = 'REPO'
        add_table_alias_alv = 'B'
        entity_type        = 'T'
        join_type         = zif_dbbr_c_join_types=>inner_join
        field_conditions  = VALUE #(
          ( field      = 'OBJ_NAME'
            ref_field  = 'TABNAME'
            ref_table  = 'DD02L'
            ref_table_alias = 'TABLES'
            operator   = '=' )
        )
        filter_conditions = VALUE #(
         ( tabname   = 'DD02L'
           tabname_alias = 'TABLES'
           fieldname = 'AS4LOCAL'
           operator  = '='
           value     = 'A'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'AND' )
         ( tabname   = 'TADIR'
           tabname_alias = 'REPO'
           fieldname = 'OBJECT'
           operator  = '='
           value     = 'TABL'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'AND' )
         ( tabname   = 'DD02L'
           tabname_alias = 'TABLES'
           fieldname = 'VIEWCLASS'
           operator  = '='
           value     = ''
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
         ( tabname   = 'DD02L'
           tabname_alias = 'TABLES'
           fieldname = 'VIEWCLASS'
           operator  = '='
           value     = 'D'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'AND' )
         ( tabname   = 'DD02L'
           tabname_alias = 'TABLES'
           fieldname = 'TABCLASS'
           operator  = '='
           value     = 'TRANSP'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
         ( tabname   = 'DD02L'
           tabname_alias = 'TABLES'
           fieldname = 'TABCLASS'
           operator  = '='
           value     = 'POOL'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
         ( tabname   = 'DD02L'
           tabname_alias = 'TABLES'
           fieldname = 'TABCLASS'
           operator  = '='
           value     = 'VIEW'
           value_type = zif_dbbr_c_join_cond_val_type=>typed_input
           and_or    = 'OR' )
        )
       )
    )
).

DATA(lo_join_manager) = NEW zcl_dbbr_join_manager(
  iv_primary_entity     = lr_join->primary_table
  iv_primary_entity_raw = lr_join->primary_table
  iv_entity_type        = 'T'
  ir_join_ref           = lr_join
).

lo_join_manager->show( ).

IF lo_join_manager->was_updated( ).
  cl_demo_output=>write_text( |Primary Table: { lr_join->primary_table }, Alias: { lr_join->primary_table_alias }, Alias-ALV: { lr_join->primary_table_alias_alv }| ).
  cl_demo_output=>write_text( || ).

  LOOP AT lr_join->tables ASSIGNING FIELD-SYMBOL(<ls_table>).
    cl_demo_output=>write_text( |Table: { <ls_table>-add_table }, Alias: { <ls_table>-add_table_alias }, Alias-ALV: { <ls_table>-add_table_alias_alv }| ).
    cl_demo_output=>write_text( |Fields for { <ls_table>-add_table }| ).
    cl_demo_output=>write_data( <ls_table>-field_conditions ).
    cl_demo_output=>write_text( |Filters for { <ls_table>-add_table }| ).
    cl_demo_output=>write_data( <ls_table>-filter_conditions ).
  ENDLOOP.

  cl_demo_output=>display( ).
ENDIF.
