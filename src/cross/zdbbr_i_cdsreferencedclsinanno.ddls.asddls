@AbapCatalog.sqlViewName: 'ZDBBRICDSREFCA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Referenced clses in CDS views via Annot.'

define view ZDBBR_I_CDSReferencedClsInAnno
  as select from ZDBBR_I_CdsAnnotation
{
  EntityId,
  FieldName,
  Name,
  replace(Value, 'ABAP:', '') as Value
}
where
     Name like '.FILTER%.TRANSFORMEDBY'
  or Name like '.SORT%.TRANSFORMEDBY'
  or Name like '%.VIRTUALELEMENTCALCULATEDBY'
  or Name like 'ANALYTICS%.READCLASSNAME'
  or Name like 'ANALYTICS%.WRITEBACK.CLASSNAME'
