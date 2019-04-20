@AbapCatalog.sqlViewName: 'ZDBBRPCDSVB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Base view for CDS with Repository info.'

@VDM.private: true

define view ZDBBR_P_CDSViewBase
  as select from ZDBBR_I_DdlDependency as StructuredObject
    inner join   dd02b                 as CdsEntityHeader on  StructuredObject.EntityName = CdsEntityHeader.strucobjn
                                                          and CdsEntityHeader.as4local    = 'A'
    inner join   tadir                 as Repo            on  StructuredObject.DdlName = Repo.obj_name
                                                          and Repo.pgmid               = 'R3TR'
                                                          and object                   = 'DDLS'
{
  CdsEntityHeader.strucobjn     as EntityId,
  CdsEntityHeader.strucobjn_raw as RawEntityId,
  StructuredObject.DdlName,
  StructuredObject.ViewName,
  Repo.devclass                 as DevelopmentPackage,
  Repo.author                   as CreatedBy,
  Repo.created_on               as CreatedDate,
  Repo.masterlang               as OriginalLanguage,
  CdsEntityHeader.chgdate       as ChangedDate
}
where
  as4local = 'A'
