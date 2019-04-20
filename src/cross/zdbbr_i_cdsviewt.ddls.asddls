@AbapCatalog.sqlViewName: 'ZDBBRICDSVT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Text for CDS View'

@VDM.viewType: #BASIC

define view ZDBBR_I_CDSViewT
  as select from ddddlsrc02bt as Text
{

  key Text.ddlname       as DdlName,
  key Text.ddlanguage    as Language,
      Text.ddtext        as Description,
      upper(Text.ddtext) as DescriptionUpper
}
where
  as4local = 'A'
