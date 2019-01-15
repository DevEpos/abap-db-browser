@AbapCatalog.sqlViewName: 'ZDBBRICDSANNO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Annotations for CDS Views'
define view ZDBBR_I_CdsAnnotation
  as select from ddheadanno
{
  strucobjn                       as EntityId,
  name                            as Name,
  upper(replace(value, '''', '')) as Value
}
union select from ddfieldanno
{
  strucobjn                       as EntityId,
  name                            as Name,
  upper(replace(value, '''', '')) as Value
}
