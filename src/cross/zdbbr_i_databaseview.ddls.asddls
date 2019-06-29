@AbapCatalog.sqlViewName: 'ZDBBRIDBVIEW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database view'

define view ZDBBR_I_DatabaseView
  with parameters
    p_language : abap.lang
  as select from    tadir as Repo
    inner join      dd25l as DbView       on  Repo.obj_name = DbView.viewname
                                          and Repo.pgmid    = 'R3TR'
                                          and Repo.object   = 'VIEW'
    left outer join dd25t as Text         on  DbView.viewname = Text.viewname
                                          and Text.ddlanguage = $parameters.p_language
    left outer join dd25t as FallBackText on  DbView.viewname         = FallBackText.viewname
                                          and FallBackText.ddlanguage = Repo.masterlang
{
  DbView.viewname        as ViewName,
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
  'V'                    as Type
}
where
       DbView.as4local = 'A'
  and  genflag         = #genflag.' '
  and(
       viewclass       = #viewclass.' '
    or viewclass       = #viewclass.'D'
  )
