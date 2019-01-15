@AbapCatalog.sqlViewName: 'ZDBBRIAPST'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'API states'

define view ZDBBR_I_APIStates
  as select from cls_assignment as cls
{
  cls.trobjtype as ObjectType,
  cls.sobj_name as ObjectName,
  case
    when cls.attribute   = 'ARS_RELEASE_STATE_C0'                then '4' // Add Custom Fields
    when ( cls.attribute = 'RELEASE_STATE' and cls.value = '2' ) then '1' // Use in Key User apps
    when ( cls.attribute = 'RELEASE_STATE' and cls.value = '5' ) then '2' // Use in SAP Cloud Platform
    when cls.attribute = 'ARS_RELEASE_STATE_C2'                  then '3' // Use as Remote API
    else ''
  end as FilterValue,
  cast( 'RELEASED' as abap.char(184) ) as APIState  
}
where
  (
         cls.attribute = 'ARS_RELEASE_STATE_C0'
    and  cls.value     = 'RELEASED'
  )
  or(
         cls.attribute = 'RELEASE_STATE'
    and(
         cls.value     = '2'
      or cls.value     = '5'
    )
  )
  or(
         cls.attribute = 'ARS_RELEASE_STATE_C2'
    and  cls.value     = 'RELEASED'
  )
