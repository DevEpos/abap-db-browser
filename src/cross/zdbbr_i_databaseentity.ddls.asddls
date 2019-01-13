@AbapCatalog.sqlViewName: 'ZDBBRIDBENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database Entity like (View or Table)'

define view ZDBBR_I_DatabaseEntity
  with parameters
    p_language : abap.lang
  as select from ZDBBR_I_DatabaseTable(p_language : $parameters.p_language)
{
  TableName as Entity,
  TableName as EntityRaw,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  Type
}
union select from ZDBBR_I_DatabaseView(p_language : $parameters.p_language)
{
  ViewName as Entity,
  ViewName as EntityRaw,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  Type
}
union select from ZDBBR_I_CDSEntity(p_language : $parameters.p_language)
{
  EntityId    as Entity,
  RawEntityId as EntityRaw,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  Type
}
