/*
    Provides all views that have field annotations that
    can be used as value helps
*/
@AbapCatalog.sqlViewName: 'ZDBBRICFVWVH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS View fields with assigned Value H.'

define view ZDBBR_I_CDSFieldValueWithVH
  as select from ddfieldanno
{
  key strucobjn  as Entity,
  key lfieldname as FieldName,
      value      as ValueHelpAssociation
}
where
     name = 'OBJECTMODEL.FOREIGNKEY.ASSOCIATION'
  or name = 'CONSUMPTION.VALUEHELP'
