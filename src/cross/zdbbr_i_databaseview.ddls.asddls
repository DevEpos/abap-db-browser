@AbapCatalog.sqlViewName: 'ZDBBRIDBVIEW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database view'

define view ZDBBR_I_DatabaseView
  with parameters
    p_language : abap.lang
  as select from    tadir as Repo
    inner join      dd25l as DbView on  Repo.obj_name = DbView.viewname
                                    and Repo.pgmid    = 'R3TR'
                                    and Repo.object   = 'VIEW'
    left outer join dd25t as Text   on  DbView.viewname = Text.viewname
                                    and Text.ddlanguage = $parameters.p_language
{
  DbView.viewname   as ViewName,
  ddtext            as Description,
  ddlanguage        as Language,
  author            as CreatedBy,
  devclass          as DevelopmentPackage,
  'V'               as Type
}
where
       DbView.as4local = 'A'
  and  genflag         = #genflag.' '
  and(
       viewclass       = #viewclass.' '
    or viewclass       = #viewclass.'D'
  )
