@ClientHandling.type: #CLIENT_INDEPENDENT
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Field Hierarchy'

define table function ZDBBR_I_CdsFieldHierarchy
  with parameters
    p_cdsViewName  : ddstrucobjname,
    p_cdsFieldName : fieldname
returns
{
  key ViewName       : tabname;
  key ViewField      : fieldname;
  key Level          : abap.int1;
      EntityName     : tabname;
      ViewFieldRaw   : fieldname;
      BaseTable      : tabname;
      BaseEntityName : tabname;
      BaseField      : fieldname;
      BaseFieldRaw   : fieldname;

}
implemented by method
  zcl_dbbr_amdp_cds_field_hier=>get_cds_field_hierarchy;