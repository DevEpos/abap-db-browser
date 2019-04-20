@AbapCatalog.sqlViewName: 'ZDBBRIDOMFIXV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Domain fix values'

define view ZDBBR_I_DomainFixValue
  as select from    dd07l as FixedValue
    left outer join dd07t as ValueText on  FixedValue.domname    = ValueText.domname
                                       and FixedValue.domvalue_l = ValueText.domvalue_l
                                       and FixedValue.as4local   = ValueText.as4local
{
  key FixedValue.domname    as DomainName,
  key FixedValue.domvalue_l as Low,
  key ValueText.ddlanguage  as Language,
      ValueText.ddtext      as Text
}
where
  FixedValue.as4local = 'A' --Active
