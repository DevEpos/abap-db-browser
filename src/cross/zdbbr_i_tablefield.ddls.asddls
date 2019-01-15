@AbapCatalog.sqlViewName: 'ZDBBRITABFLD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Field in table'

define view ZDBBR_I_TableField
  as select from dd03l
{
  key tabname   as TableName,
  key fieldname as FieldName
}
where
  as4local = 'A'
