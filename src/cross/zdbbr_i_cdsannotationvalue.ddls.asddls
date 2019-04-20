@AbapCatalog.sqlViewName: 'ZDBBRICDSANV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Possible Values for a CDS Annotation'
define view ZDBBR_I_CdsAnnotationValue
  as select from ddla_rt_header
{
  key ddlaname                           as DdlAnnotationName,
  key key_upper                          as AnnotationNameUpper,
  key key_raw                            as AnnotationNameRaw,
      cast( 'true' as ddannotation_val ) as Value
}
where
  value_type = 'Boolean'
union select from ddla_rt_header
{
  key ddlaname                            as DdlAnnotationName,
  key key_upper                           as AnnotationNameUpper,
  key key_raw                             as AnnotationNameRaw,
      cast( 'false' as ddannotation_val ) as Value
}
where
  value_type = 'Boolean'
union select from ddla_rt_header as Annotation
  inner join      ddla_rt_enums  as Enum on Annotation.key_guid = Enum.key_guid
{
  key Annotation.ddlaname              as DdlAnnotationName,
  key Annotation.key_upper             as AnnotationNameUpper,
  key Annotation.key_raw               as AnnotationNameRaw,
      concat('#',Enum.symbol)          as Value
}
