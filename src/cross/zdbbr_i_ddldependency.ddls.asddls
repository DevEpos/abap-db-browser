@AbapCatalog.sqlViewName: 'ZDBBRIDDLDEP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Resolved DDL Dependency'
define view ZDBBR_I_DdlDependency
  as select from    ddldependency as StructuredObject
    left outer join ddldependency as DdlDdicView on  StructuredObject.ddlname = DdlDdicView.ddlname
                                                 and DdlDdicView.objecttype   = 'VIEW'
                                                 and DdlDdicView.state        = 'A'
{
  StructuredObject.ddlname         as DdlName,
  DdlDdicView.objectname           as ViewName,
  StructuredObject.objectname      as EntityName
}
where
      StructuredObject.objecttype = 'STOB'
  and StructuredObject.state      = 'A'
