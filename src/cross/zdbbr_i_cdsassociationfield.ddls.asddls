@AbapCatalog.sqlViewName: 'ZDBBRICDSASSFLD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'On Condition Field of Assoc. in CDS View'

define view ZDBBR_I_CDSAssociationField
  as select from dd05b
{
  key strucobjn                          as Entity,
  key associationname                    as AssociationName,
  key fieldname_t                        as TargetField,
  key fdposition                         as FieldPosition,
      fieldname                          as SourceField,
      @EndUserText.label: 'Signals literal condition'
      cast( case left(fieldname,1)
        when '''' then 'X'
        else           ' '
      end as bool )                      as IsLiteralCondition,
      operator                           as Operator,
      and_or                             as AndOrCondition
}
where
  as4local = 'A'
