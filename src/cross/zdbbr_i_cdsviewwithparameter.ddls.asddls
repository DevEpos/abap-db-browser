@AbapCatalog.sqlViewName: 'ZDBBRICDSVWP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS View that has parameters'

define view ZDBBR_I_CDSViewWithParameter
  as select distinct from dd10b
{
  key strucobjn as EntityId
}
where
  as4local = 'A'
