@AbapCatalog.sqlViewName: 'ZDBBRPCDSVB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Base view for CDS with Repository info.'

@VDM.private: true

define view ZDBBR_P_CDSViewBase
  as select from ddldependency as StructuredObject
    inner join   dd02b         as CdsEntityHeader on  StructuredObject.objectname = CdsEntityHeader.strucobjn
                                                  and CdsEntityHeader.as4local    = 'A'
    inner join   tadir         as Repo            on  StructuredObject.ddlname = Repo.obj_name
                                                  and Repo.pgmid               = 'R3TR'
                                                  and object                   = 'DDLS'
{
  CdsEntityHeader.strucobjn            as EntityId,
  CdsEntityHeader.strucobjn_raw        as RawEntityId,
  StructuredObject.ddlname             as DdlName,
  Repo.devclass                        as DevelopmentPackage,
  Repo.author                          as CreatedBy
}
where
      StructuredObject.objecttype = 'STOB'
  and StructuredObject.state      = 'A'
