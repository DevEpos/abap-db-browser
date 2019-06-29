@AbapCatalog.sqlViewName: 'ZDBBRICDSFPE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Entities in Select From clause of CDS'

-- CDS views which have Database tables in select part
define view ZDBBR_I_CDSFromPartEntity
  as select from dd26s as BaseTable
    inner join   dd02l as DbTable on  BaseTable.tabname = DbTable.tabname
                                  and DbTable.tabclass  = 'TRANSP'
{
  BaseTable.viewname  as DdlViewName,
  BaseTable.tabname   as SourceEntity
}
-- CDS views which have Database Views (not DDL Views of CDS views) in select part
union select from dd26s as BaseTable
  inner join      tadir as Repo on  BaseTable.tabname = Repo.obj_name
                                and Repo.object       = 'VIEW'
                                and Repo.genflag      = ''
{
  BaseTable.viewname as DdlViewName,
  BaseTable.tabname  as SourceEntity
}
-- CDS views which have other CDS views in the select part
union select from dd26s                 as BaseTable
  inner join      ZDBBR_I_DdlDependency as DdlMap on BaseTable.tabname = DdlMap.ViewName
{
  BaseTable.viewname as DdlViewName,
  DdlMap.EntityName  as SourceEntity
}
-- CDS views which have table functions in the select part
union select from zdbbrdd26s_v as BaseTable
{
  BaseTable.ddlview   as DdlViewName,
  BaseTable.basetable as SourceEntity
}
where
  BaseTable.ddictype = 'STOB'
