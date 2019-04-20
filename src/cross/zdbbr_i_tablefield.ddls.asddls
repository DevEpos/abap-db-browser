@AbapCatalog.sqlViewName: 'ZDBBRITABFLD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Field in table'

define view ZDBBR_I_TableField
  as select from dd03l as Field
    inner join   dd02l as Table on  Field.tabname    = Table.tabname
                                and Table.as4local   = 'A'
                                and (
                                   Table.tabclass    = 'TRANSP'
                                   or Table.tabclass = 'VIEW'
                                 )
    inner join   tadir as Repo  on  Repo.obj_name = Table.tabname
                                and Repo.genflag  = ''
                                and (
                                   Repo.object    = 'TABL'
                                   or Repo.object = 'VIEW'
                                 )
{
  key Field.tabname   as TableName,
  key Field.fieldname as FieldName,
      Field.rollname  as RollName
}
where
  Field.as4local = 'A'
