@AbapCatalog.sqlViewName: 'ZDBBRICDSEN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Entity in Data Definition'

define view ZDBBR_I_CDSEntity
  with parameters
    p_language : abap.lang
  as select from    ddddlsrc            as CdsSource
    inner join      ZDBBR_P_CDSViewBase as Base on CdsSource.ddlname = Base.DdlName
    left outer join ZDBBR_I_CDSViewT    as Text on  Base.DdlName  = Text.DdlName
                                                and Text.Language = $parameters.p_language
{
  Base.EntityId,
  Base.RawEntityId,
  Base.DdlName,
  Base.DevelopmentPackage,
  Text.Language,
  Text.Description,
  Text.DescriptionUpper,
  Base.CreatedBy,
  Base.CreatedDate,
  Base.ChangedDate,
  'C'                     as Type
}
where
      CdsSource.parentname = ''
  and CdsSource.as4local   = 'A'
