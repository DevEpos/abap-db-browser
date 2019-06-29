@AbapCatalog.sqlViewName: 'ZDBBRIDBTAB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database Table'

define view ZDBBR_I_DatabaseTable
  with parameters
    p_language : abap.lang
  as select distinct from tadir as Repo
    inner join            dd02l as DbTable      on  Repo.obj_name = DbTable.tabname
                                                and Repo.pgmid    = 'R3TR'
                                                and Repo.object   = 'TABL'
    left outer join       dd02t as Text         on  DbTable.tabname = Text.tabname
                                                and Text.ddlanguage = $parameters.p_language
    left outer join       dd02t as FallBackText on  DbTable.tabname         = FallBackText.tabname
                                                and FallBackText.ddlanguage = Repo.masterlang
{
  key DbTable.tabname        as TableName,
      $parameters.p_language as Language,
      case
        when Text.ddtext is not null then Text.ddtext
        else FallBackText.ddtext
      end                    as Description,
      case
        when Text.ddtext is not null then upper(Text.ddtext)
        else upper(FallBackText.ddtext)
      end                    as DescriptionUpper,
      author                 as CreatedBy,
      Repo.created_on        as CreatedDate,
      as4date                as ChangedDate,
      devclass               as DevelopmentPackage,
      'T'                    as Type
}
where
      tabclass         = #tabclass.'TRANSP'
  and DbTable.as4local = 'A'
