@AbapCatalog.sqlViewName: 'ZDBBRDBEAGG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Aggr. Database Entities by package/type'

define view ZDBBR_I_DatabaseEntityAggr
  with parameters
    p_language : abap.lang
  as select from ZDBBR_I_DatabaseEntity( p_language: $parameters.p_language)
{
  //ZDBBR_I_DatabaseEntity
  Type,
  DevelopmentPackage,
  count(*) as EntityCount
}
group by
  Type,
  DevelopmentPackage
