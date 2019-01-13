@AbapCatalog.sqlViewName: 'ZDBBRIDBTAB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database Table'

define view ZDBBR_I_DatabaseTable
  with parameters
    p_language : abap.lang
  as select from    tadir as Repo
    inner join      dd02l as DbTable on  Repo.obj_name = DbTable.tabname
                                     and Repo.pgmid    = 'R3TR'
                                     and Repo.object   = 'TABL'
    left outer join dd02t as Text    on  DbTable.tabname = Text.tabname
                                     and Text.ddlanguage = $parameters.p_language
{
  DbTable.tabname   as TableName,
  ddtext            as Description,
  ddlanguage        as Language,
  author            as CreatedBy,
  devclass          as DevelopmentPackage,
  'T'               as Type
}
where
      tabclass         = #tabclass.'TRANSP'
  and DbTable.as4local = 'A'
