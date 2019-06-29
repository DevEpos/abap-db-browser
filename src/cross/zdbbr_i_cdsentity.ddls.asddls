@AbapCatalog.sqlViewName: 'ZDBBRICDSEN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Entity in Data Definition'

define view ZDBBR_I_CDSEntity
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    p_language : abap.lang
  as select from    ddddlsrc            as CdsSource
    inner join      ZDBBR_P_CDSViewBase as Base         on CdsSource.ddlname = Base.DdlName
    left outer join ddddlsrc02bt        as Text         on  Text.ddlname    = Base.DdlName
                                                        and Text.ddlanguage = $parameters.p_language
    left outer join ddddlsrct           as FallbackText on  FallbackText.ddlname    = Base.DdlName
                                                        and FallbackText.ddlanguage = Base.OriginalLanguage
  association [0..1] to ZDBBR_I_APIStates as _ApiState on  Base.DdlName          =  _ApiState.ObjectName
                                                       and _ApiState.FilterValue <> '4'
{
  Base.EntityId,
  Base.RawEntityId,
  Base.DdlName,
  Base.ViewName,
  CdsSource.source_type    as SourceType,
  Base.DevelopmentPackage,
  $parameters.p_language   as Language,
  case
    when Text.ddtext is null then FallbackText.ddtext
    else Text.ddtext
  end                      as Description,
  case
    when Text.ddtext is null then upper( FallbackText.ddtext )
    else upper( Text.ddtext )
  end                      as DescriptionUpper,
  Base.CreatedBy,
  Base.CreatedDate,
  Base.ChangedDate,
  'C'                      as Type,
  _ApiState
}
where
      CdsSource.parentname = ''
  and CdsSource.as4local   = 'A'
