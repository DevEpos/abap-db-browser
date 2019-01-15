@AbapCatalog.sqlViewName: 'ZDBBRICDSFPE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Entities in Select From clause of CDS'

define view ZDBBR_I_CDSFromPartEntity
  as select from dd26s as BaseTable
    inner join   dd02l as DbTable on  BaseTable.tabname = DbTable.tabname
                                  and DbTable.tabclass  = 'TRANSP'
{
  BaseTable.viewname  as DdlViewName,
  BaseTable.tabname   as SourceEntity
}
union select from dd26s as BaseTable
  inner join      tadir as Repo on  BaseTable.tabname = Repo.obj_name
                                and Repo.object       = 'VIEW'
                                and Repo.genflag      = ''
{
  BaseTable.viewname as DdlViewName,
  BaseTable.tabname  as SourceEntity
}
union select from dd26s         as BaseTable
  inner join      ddldependency as DdlMap on  BaseTable.tabname = DdlMap.objectname
                                          and DdlMap.objecttype = 'VIEW'
                                          and DdlMap.state      = 'A'
{
  BaseTable.viewname as DdlViewName,
  DdlMap.ddlname     as SourceEntity
}
