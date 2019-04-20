@AbapCatalog.sqlViewName: 'ZDBBRIFORKEYTAB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database tables of for. key definition'

define view ZDBBR_I_ForeignKeyTable
  with parameters
    p_language : abap.lang
  as select distinct from dd08l                                                       as ForeignKey
    left outer join       ZDBBR_I_DatabaseTable( p_language: $parameters.p_language ) as DatabaseTable on ForeignKey.checktable = DatabaseTable.TableName
{
  key ForeignKey.tabname    as TableName,
  key ForeignKey.checktable as ForeignKeyTable,
      DatabaseTable.CreatedBy,
      DatabaseTable.DevelopmentPackage,
      DatabaseTable.Description
}
where
  as4local = 'A'
