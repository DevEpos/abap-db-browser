@AbapCatalog.sqlViewName: 'ZDBBRICDSANNO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Annotations for CDS Views'
define view ZDBBR_I_CdsAnnotation
  as select from ddfieldanno
{
  strucobjn                       as EntityId,
  lfieldname                      as FieldName,
  name                            as Name,
  upper(replace(value, '''', '')) as Value
}
union select from ddheadanno
{
  strucobjn                       as EntityId,
  ''                              as FieldName,
  name                            as Name,
  upper(replace(value, '''', '')) as Value
}
/* Change on 2019/04/17
 *-------------------------
 * Also show CDS views if there meta data extension files
 * use the request annotation
 */
union select from ddlx_rt_header as MetaExtensionHeader
  inner join      ddlx_rt_data   as MetaExtensionAnno on MetaExtensionHeader.ddlxname = MetaExtensionAnno.ddlxname
{
  MetaExtensionHeader.extended_artifact as EntityId,
  MetaExtensionAnno.element             as FieldName,
  MetaExtensionAnno.name                as Name,
  upper(replace(value, '''', ''))       as Value
}
