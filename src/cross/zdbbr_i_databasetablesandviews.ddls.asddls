@AbapCatalog.sqlViewName: 'ZDBBRIDBTABAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DB Tables and DB views'

define view ZDBBR_I_DatabaseTablesAndViews
  with parameters
    p_language : abap.lang
  as select from ZDBBR_I_DatabaseTable(p_language : $parameters.p_language)
{
  TableName as Entity,
  TableName as EntityRaw,
  Description,
  DescriptionUpper,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedDate,
  Type
}
union select from ZDBBR_I_DatabaseView(p_language : $parameters.p_language)
{
  ViewName as Entity,
  ViewName as EntityRaw,
  Description,
  DescriptionUpper,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedDate,
  Type
}
