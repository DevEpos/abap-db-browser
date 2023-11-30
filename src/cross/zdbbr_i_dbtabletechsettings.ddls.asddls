@AbapCatalog.sqlViewName: 'ZDBBRIDBTTSET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Technical Settings of Db Table'

define view ZDBBR_I_DbTableTechSettings
  as select from    dd09l                 as TechSettings
    inner join      dd02l                 as Table             on TechSettings.tabname = Table.tabname
    left outer join ZSAT_I_DomainFixValue as SizeCategoryText  on  SizeCategoryText.Low        = TechSettings.tabkat
                                                               and SizeCategoryText.DomainName = 'TABKAT'
                                                               and SizeCategoryText.Language   = $session.system_language
    left outer join dartt                 as DataClassText     on  DataClassText.tabart  = TechSettings.tabart
                                                               and DataClassText.ddlangu = $session.system_language
    left outer join ZSAT_I_DomainFixValue as BufferTypeText    on  BufferTypeText.Low        = TechSettings.pufferung
                                                               and BufferTypeText.DomainName = 'PUFFERUNG'
                                                               and BufferTypeText.Language   = $session.system_language
    left outer join ZSAT_I_DomainFixValue as DeliveryClassText on  DeliveryClassText.Low        = Table.contflag
                                                               and DeliveryClassText.DomainName = 'CONTFLAG'
                                                               and DeliveryClassText.Language   = $session.system_language
{
  key Table.tabname          as TableName,
      Table.contflag         as DeliveryClass,
      DeliveryClassText.Text as DeliveryClassText,
      tabkat                 as SizeCategory,
      SizeCategoryText.Text  as SizeCategoryText,
      TechSettings.tabart    as DataClass,
      DataClassText.darttext as DataClassText,
      pufferung              as BufferingIndicator,
      BufferTypeText.Text    as BufferingIndicatorText,
      Table.as4user          as LastChangedBy,
      Table.as4date          as LastChangedDate

}
where
  TechSettings.as4local = 'A'
